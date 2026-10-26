package runtime

import (
	"fmt"
	"strings"
	"testing"

	"github.com/glojurelang/glojure/pkg/glj"
	"github.com/glojurelang/glojure/pkg/lang"
)

func TestJSONRead(t *testing.T) {
	Load()
	read := glj.Var("clojure.data.json", "read-str")
	for _, tc := range []struct {
		name, input, want string
	}{
		{"null", `null`, `nil`},
		{"false", `false`, `false`},
		{"true", `true`, `true`},
		{"integer", `-123`, `-123`},
		{"decimal", `1.25e2`, `125.0`},
		{"empty string", `""`, `""`},
		{"empty array", `[]`, `[]`},
		{"empty object", `{}`, `{}`},
		{"whitespace", " \t\r\n[1, 2, 3] \n", `[1 2 3]`},
		{"nested", `{"a":[null,false,{"b":true}],"c":-2}`,
			`{"a" [nil false {"b" true}] "c" -2}`},
		{"escapes", `"\"\\\/\n\r\t"`, `"\"\\/\n\r\t"`},
		{"unicode", `{"é":"雪😀","a":["λ",42]}`,
			`{"é" "雪😀" "a" ["λ" 42]}`},
		// Preserve existing behavior; this change does not add JSON features.
		{"ignored trailing input", `true false`, `true`},
		{"unicode escape limitation", `"\u0041"`, `"u0041"`},
	} {
		t.Run(tc.name, func(t *testing.T) {
			got := read.Invoke(tc.input)
			if !lang.Equals(got, glj.Read(tc.want)) {
				t.Fatalf("got %v, want %s", got, tc.want)
			}
		})
	}
}

func TestJSONReadErrors(t *testing.T) {
	Load()
	read := glj.Var("clojure.data.json", "read-str")
	for _, tc := range []struct{ input, message string }{
		{"", "Unexpected end of input"},
		{" \n\t", "Unexpected end of input"},
		{`"unfinished`, "Unterminated string"},
		{`"unfinished\`, "Unterminated escape sequence"},
		{`[`, "Unterminated array"},
		{`{`, "Unterminated object"},
		{`{"a" 1}`, "Expected : after object key"},
		{`[1 2]`, "Unexpected character in array"},
		{`tru`, "Unexpected character"},
	} {
		t.Run(tc.input, func(t *testing.T) {
			defer func() {
				failure := recover()
				if failure == nil ||
					!strings.Contains(fmt.Sprint(failure), tc.message) {
					t.Fatalf("got %v, want error containing %q",
						failure, tc.message)
				}
			}()
			read.Invoke(tc.input)
		})
	}
}

func BenchmarkJSONRead(b *testing.B) {
	Load()
	read := glj.Var("clojure.data.json", "read-str")
	for _, size := range []int{1024, 4096, 16384, 32768} {
		for _, tc := range []struct{ name, input string }{
			{"array", "[" + strings.Repeat("1,", size/2) + "0]"},
			{"object", `{"a":"` + strings.Repeat("x", size) + `"}`},
			{"unicode", `"` + strings.Repeat("雪😀", size/7) + `"`},
			{"whitespace", strings.Repeat(" ", size) + "true"},
		} {
			b.Run(fmt.Sprintf("%s/%d", tc.name, size), func(b *testing.B) {
				b.SetBytes(int64(len(tc.input)))
				b.ReportAllocs()
				b.ResetTimer()
				for i := 0; i < b.N; i++ {
					read.Invoke(tc.input)
				}
			})
		}
	}
}
