package runtime

import (
	"fmt"
	"net/http"
	"net/http/httptest"
	"os"
	"path/filepath"
	goruntime "runtime"
	"testing"

	"github.com/glojurelang/glojure/pkg/glj"
	jsystem "github.com/glojurelang/glojure/pkg/javacompat/system"
	"github.com/glojurelang/glojure/pkg/lang"
)

func mapValue(value any, key string) any {
	return glj.Var("clojure.core", "get").Invoke(value, lang.NewKeyword(key))
}

func TestPlatformArgsPreferHostedCommandLineArgs(t *testing.T) {
	Load()
	argsVar := lang.NSCore.FindInternedVar(
		lang.NewSymbol("*command-line-args*"),
	)
	originalArgs := argsVar.Deref()
	argsVar.BindRoot(lang.NewList("first", "second"))
	defer argsVar.BindRoot(originalArgs)

	originalVersion := jsystem.SetProperty("babashka.version", "test")
	defer func() {
		if originalVersion == nil {
			jsystem.ClearProperty("babashka.version")
		} else {
			jsystem.SetProperty("babashka.version", originalVersion)
		}
	}()

	got := fmt.Sprint(glj.Var("ys.v0.platform", "args").Invoke())
	if got != `["first" "second"]` {
		t.Fatalf(
			"platform args: got %q, want %q",
			got,
			`["first" "second"]`,
		)
	}
}

func TestLoadAndNativeBackends(t *testing.T) {
	Load()
	target := lang.FindOrCreateNamespace(lang.NewSymbol("ys-v0-glj.runtime-test"))
	bindings := lang.NewMap(glj.Var("clojure.core", "*ns*"), target)
	glj.Var("clojure.core", "push-thread-bindings").Invoke(bindings)
	defer glj.Var("clojure.core", "pop-thread-bindings").Invoke()
	glj.Var("ys.v0", "init").Invoke()

	if got := fmt.Sprint(glj.Var("ys.v0.platform", "arch").Invoke()); got != goruntime.GOARCH {
		t.Fatalf("platform arch: got %q, want %q", got, goruntime.GOARCH)
	}
	wantCWD, err := os.Getwd()
	if err != nil {
		t.Fatal(err)
	}
	varGet := glj.Var("clojure.core", "var-get")
	if got := fmt.Sprint(varGet.Invoke(glj.Var("ys.v0.global", "CWD"))); got != wantCWD {
		t.Fatalf("runtime CWD: got %q, want %q", got, wantCWD)
	}
	if got := fmt.Sprint(varGet.Invoke(glj.Var("ys.v0.global", "RUN"))); got == "{}" {
		t.Fatal("runtime RUN was not initialized")
	}

	if got := fmt.Sprint(glj.Var("ys.v0.std", "len").Invoke("hello")); got != "5" {
		t.Fatalf("len: got %q, want 5", got)
	}
	glj.Var("ys.v0.std", "say").Invoke()

	dir := filepath.Join(t.TempDir(), "nested", "directory")
	glj.Var("ys.v0.fs", "mkdir-p").Invoke(dir)
	if got := glj.Var("ys.v0.fs", "exists?").Invoke(dir); got != true {
		t.Fatalf("filesystem backend did not create %s: %v", dir, got)
	}

	process := glj.Var("babashka.process", "sh").Invoke("printf", "hello")
	if got := fmt.Sprint(mapValue(process, "out")); got != "hello" {
		t.Fatalf("process stdout: got %q, want hello", got)
	}
	if got := fmt.Sprint(mapValue(process, "exit")); got != "0" {
		t.Fatalf("process exit: got %q, want 0", got)
	}

	data := lang.NewMap(lang.NewKeyword("answer"), int64(42))
	if got := fmt.Sprint(glj.Var("clojure.data.json", "write-str").Invoke(data)); got != `{"answer":42}` {
		t.Fatalf("json encoding: got %q", got)
	}

	yamlData := glj.Var("ys.v0.yaml", "load").Invoke(
		"first: 1\nsecond:\n  - true\n  - null\n",
	)
	if got := fmt.Sprint(
		glj.Var("clojure.core", "get").Invoke(yamlData, "first"),
	); got != "1" {
		t.Fatalf("yaml first value: got %q, want 1", got)
	}
	gotYAML := fmt.Sprint(glj.Var("ys.v0.yaml", "dump").Invoke(yamlData))
	if gotYAML != "first: 1\nsecond:\n  - true\n  - null\n" {
		t.Fatalf("yaml encoding: got %q", gotYAML)
	}

	server := httptest.NewServer(http.HandlerFunc(func(w http.ResponseWriter, _ *http.Request) {
		_, _ = w.Write([]byte("hello over http"))
	}))
	defer server.Close()
	response := glj.Var("babashka.http-client", "get").Invoke(server.URL)
	if got := fmt.Sprint(mapValue(response, "status")); got != "200" {
		t.Fatalf("http status: got %q, want 200", got)
	}
	if got := fmt.Sprint(mapValue(response, "body")); got != "hello over http" {
		t.Fatalf("http body: got %q", got)
	}
}
