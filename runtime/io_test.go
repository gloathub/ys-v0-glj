package runtime

import (
	"fmt"
	"os"
	"path/filepath"
	"strings"
	"testing"

	"github.com/glojurelang/glojure/pkg/glj"
)

func TestStandardReadFile(t *testing.T) {
	Load()
	t.Setenv("YS_MODULES", "fs")
	read := glj.Var("ys.v0.std", "read")
	for _, content := range []string{"", "What if YAML…?\nsecond line\n"} {
		t.Run(fmt.Sprintf("%q", content), func(t *testing.T) {
			path := filepath.Join(t.TempDir(), "input.txt")
			if err := os.WriteFile(path, []byte(content), 0600); err != nil {
				t.Fatal(err)
			}
			if got := read.Invoke(path); got != content {
				t.Fatalf("read: got %q, want %q", got, content)
			}
		})
	}
}

func TestStandardWriteFile(t *testing.T) {
	Load()
	t.Setenv("YS_MODULES", "fs")
	path := filepath.Join(t.TempDir(), "output.txt")
	write := glj.Var("ys.v0.std", "write")
	for _, content := range []string{"first contents", "YAML…\n", ""} {
		if got := write.Invoke(path, content); got != nil {
			t.Fatalf("write: got %v, want nil", got)
		}
		got, err := os.ReadFile(path)
		if err != nil {
			t.Fatal(err)
		}
		if string(got) != content {
			t.Fatalf("write: got %q, want %q", got, content)
		}
	}
}

func requirePanicContaining(t *testing.T, want string, call func()) {
	t.Helper()
	defer func() {
		got := recover()
		if got == nil || !strings.Contains(fmt.Sprint(got), want) {
			t.Errorf("got panic %v, want message containing %q", got, want)
		}
	}()
	call()
}

func TestStandardFileErrors(t *testing.T) {
	Load()
	t.Setenv("YS_MODULES", "fs")
	path := filepath.Join(t.TempDir(), "missing", "file.txt")
	for _, name := range []string{"read", "write"} {
		t.Run(name, func(t *testing.T) {
			requirePanicContaining(t, "Failed to "+name+" file: "+path, func() {
				if name == "read" {
					glj.Var("ys.v0.std", name).Invoke(path)
				} else {
					glj.Var("ys.v0.std", name).Invoke(path, "contents")
				}
			})
		})
	}
}

func TestStandardFileModuleAccess(t *testing.T) {
	Load()
	t.Setenv("YS_MODULES", "io")
	path := filepath.Join(t.TempDir(), "protected.txt")
	const content = "unchanged"
	if err := os.WriteFile(path, []byte(content), 0600); err != nil {
		t.Fatal(err)
	}
	for _, name := range []string{"read", "write"} {
		t.Run(name, func(t *testing.T) {
			requirePanicContaining(t, "ys.fs is disabled by YS_MODULES", func() {
				if name == "read" {
					glj.Var("ys.v0.std", name).Invoke(path)
				} else {
					glj.Var("ys.v0.std", name).Invoke(path, "replacement")
				}
			})
		})
	}
	got, err := os.ReadFile(path)
	if err != nil {
		t.Fatal(err)
	}
	if string(got) != content {
		t.Fatalf("disabled write changed contents to %q", got)
	}
}
