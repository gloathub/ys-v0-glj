# ys-v0-go

`ys-v0-go` is the precompiled Glojure runtime for YAMLScript v0. It combines
the portable `org.yamlscript/ys.v0` sources with Go-native implementations of
the runtime backends that are unavailable on Glojure, then commits the
generated Go loaders as a normal Go module.

The generated `source/` tree is shipped too, allowing downstream Glojure AOT
compilers to analyze programs against the exact runtime used by the loaders.
The generated `bb/runtime.clj` bundles the unmodified portable upstream
sources for self-contained Babashka programs.

Go programs load the runtime before requiring generated application code:

```go
import ysv0 "github.com/gloathub/ys-v0-go/runtime"

func main() {
    ysv0.Load()
    // Require and invoke the generated application namespace.
}
```

## Development

```bash
make generate
make test
make check-generated
```

`YS_V0_SOURCE=/path/to/yamlscript/core/src make generate` uses an unpublished
YAMLScript source tree. Normal generation downloads the pinned Clojars
artifact. The patch in `patches/` is temporary until its AOT-safety changes
are released by YAMLScript.

The temporary YAMLScript changes are:

- Wrap host vars in forwarding functions instead of interning their values,
  so Glojure can AOT-link them.
- Route Glojure environment, working-directory, executable, argument, host,
  OS, architecture, and PID access through the native `ys.v0.platform`
  namespace.
- Skip host runtime-variable binding while `*compile-files*` is true; exports
  and aliases are still installed for application analysis, and normal
  binding runs when the generated loader executes.

## Release

Release from a clean `main` branch that contains the current `origin/main`:

```bash
make release VERSION=x.y.z
```

The target updates the module `VERSION` in `Makefile`, regenerates and tests
the runtime, verifies the committed generated files, commits the version
change, creates the annotated root tag `vx.y.z`, and atomically pushes `main`
and the tag. `YS-V0-VERSION` is the independently pinned YAMLScript runtime
source version and is not changed by a module release.
