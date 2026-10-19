package runtime

import (
	"encoding/json"
	"fmt"
	"net/http"
	"net/http/httptest"
	"os"
	"path/filepath"
	"reflect"
	goruntime "runtime"
	"testing"

	"github.com/glojurelang/glojure/pkg/glj"
	jsystem "github.com/glojurelang/glojure/pkg/javacompat/system"
	"github.com/glojurelang/glojure/pkg/lang"
)

func mapValue(value any, key string) any {
	return glj.Var("clojure.core", "get").Invoke(value, lang.NewKeyword(key))
}

func TestJSONMapKeys(t *testing.T) {
	Load()
	write := glj.Var("clojure.data.json", "write-str")
	for _, tc := range []struct {
		name  string
		value any
		want  string
	}{
		{"colon string", lang.NewMap(":foo", true), `{":foo":true}`},
		{"colon only", lang.NewMap(":", true), `{":":true}`},
		{"multiple colons", lang.NewMap("::foo", true), `{"::foo":true}`},
		{"empty string", lang.NewMap("", true), `{"":true}`},
		{"keyword", lang.NewMap(lang.NewKeyword("foo"), true), `{"foo":true}`},
		{"namespaced keyword", lang.NewMap(lang.NewKeyword("ns/foo"), true),
			`{"ns/foo":true}`},
		{"distinct string and keyword",
			lang.NewMap(":foo", "string", lang.NewKeyword("foo"), "keyword"),
			`{":foo":"string","foo":"keyword"}`},
		{"escaped string", lang.NewMap(":\"\\\n", true),
			`{":\"\\\n":true}`},
		{"nested map", lang.NewMap("outer", lang.NewMap(":inner", true)),
			`{"outer":{":inner":true}}`},
	} {
		t.Run(tc.name, func(t *testing.T) {
			encoded := fmt.Sprint(write.Invoke(tc.value))
			var got, want any
			if err := json.Unmarshal([]byte(encoded), &got); err != nil {
				t.Fatalf("invalid JSON %q: %v", encoded, err)
			}
			if err := json.Unmarshal([]byte(tc.want), &want); err != nil {
				t.Fatal(err)
			}
			if !reflect.DeepEqual(got, want) {
				t.Fatalf("got %s, want %s", encoded, tc.want)
			}
		})
	}
}

func TestMathFunctions(t *testing.T) {
	Load()
	eval := glj.Var("clojure.core", "eval")
	for _, tc := range []struct{ expression, want string }{
		{`(ys.v0.std/round -1.5)`, `-1`},
		{`(ys.v0.std/round -0.5)`, `0`},
		{`(ys.v0.std/round 1.5)`, `2`},
		{`(ys.v0.std/floor -1.5)`, `-2.0`},
		{`(ys.v0.std/pow 2 10)`, `1024`},
		{`(integer? (ys.v0.std/pow 2 10))`, `true`},
		{`(ys.v0.std/pow 4 0.5)`, `2.0`},
		{`(ys.v0.std/pow 2 -3)`, `0.125`},
		{`(ys.v0.std/pow 2 3 2)`, `512`},
		{`(ys.v0.std/sqrt 81)`, `9.0`},
		{`(clojure.math/get-exponent 8)`, `3`},
	} {
		t.Run(tc.expression, func(t *testing.T) {
			got := eval.Invoke(glj.Read(tc.expression))
			if !lang.Equals(got, glj.Read(tc.want)) {
				t.Fatalf("got %v, want %s", got, tc.want)
			}
		})
	}
}

func TestSetOperators(t *testing.T) {
	Load()
	cases := []struct {
		expression string
		want       string
	}{
		{`(ys.v0.std/add+ #{1 2} [2 3])`, `#{1 2 3}`},
		{`(ys.v0.std/add+ [1 1] #{2})`, `[1 1 2]`},
		{`(ys.v0.std/sub+ #{1 2 3} '(2 3))`, `#{1}`},
		{`(ys.v0.std/sub+ [nil false 1] #{nil false})`, `[1]`},
		{`(ys.v0.std/mul+ #{nil false 1} [nil false])`, `#{nil false}`},
		{`(ys.v0.std/mul+ [3 1 3 2] #{3})`, `[3 3]`},
		{`(ys.v0.std/mul+ '(3 1 3 2) #{3})`, `(3 3)`},
		{`(vector? (ys.v0.std/mul+ [1] #{}))`, `true`},
		{`(seq? (ys.v0.std/sub+ '(1) #{1}))`, `true`},
		{`(ys.v0.std/sub+ #{nil false 1} nil false)`, `#{1}`},
		{`(ys.v0.std/mul+ [3 1 3 2] #{1 3} #{3})`, `[3 3]`},
		{`(ys.v0.std/lt #{} #{1} #{1 2})`, `true`},
		{`(ys.v0.std/le #{1} #{1})`, `true`},
		{`(ys.v0.std/gt #{1 2} #{1} #{})`, `true`},
		{`(ys.v0.std/ge #{1} #{1})`, `true`},
		{`(ys.v0.std/lt #{1} #{2 3})`, `false`},
		{`(ys.v0.std/lt #{1} #{1})`, `false`},
		{`(ys.v0.std/gt #{1} #{1})`, `false`},
		{`((ys.v0.std/lt #{1 2}) #{1})`, `true`},
		{`((ys.v0.std/ge #{1}) #{1})`, `true`},
		{`(apply ys.v0.std/lt (range 10000))`, `true`},
		{`(ys.v0.std/lt 2 1 #{})`, `false`},
	}
	eval := glj.Var("clojure.core", "eval")
	for _, tc := range cases {
		t.Run(tc.expression, func(t *testing.T) {
			got := eval.Invoke(glj.Read(tc.expression))
			if !lang.Equals(got, glj.Read(tc.want)) {
				t.Fatalf("got %v, want %s", got, tc.want)
			}
		})
	}
	for _, op := range []string{"lt", "le", "gt", "ge"} {
		for _, args := range []string{`#{1} [1]`, `[1] #{1}`} {
			expression := fmt.Sprintf(
				`(try (ys.v0.std/%s %s) false (catch go/any _ true))`,
				op, args,
			)
			t.Run(expression, func(t *testing.T) {
				if got := eval.Invoke(glj.Read(expression)); got != true {
					t.Fatal("mixed set/sequence comparison did not throw")
				}
			})
		}
	}
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
	shortUse := lang.NewList(lang.NewSymbol("json"))
	normalized := glj.Var("ys.v0.ys", "normalize-use-forms").Invoke(shortUse)
	if got := fmt.Sprint(normalized); got != "((ys.json :as json))" {
		t.Fatalf("short use: got %q, want %q", got, "((ys.json :as json))")
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
		"first: 1\nsecond:\n  - true\n  - null\n" +
			"third: this long scalar must remain on one physical line " +
			"even when it is longer than eighty characters\n",
	)
	if got := fmt.Sprint(
		glj.Var("clojure.core", "get").Invoke(yamlData, "first"),
	); got != "1" {
		t.Fatalf("yaml first value: got %q, want 1", got)
	}
	gotYAML := fmt.Sprint(glj.Var("ys.v0.yaml", "dump").Invoke(yamlData))
	if gotYAML != "first: 1\nsecond:\n- true\n- null\n"+
		"third: this long scalar must remain on one physical line "+
		"even when it is longer than eighty characters\n" {
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
