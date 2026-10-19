package runtime

import (
	"sync"

	"github.com/glojurelang/glojure/pkg/glj"
	"github.com/glojurelang/glojure/pkg/lang"
	_ "github.com/glojurelang/glojure/pkg/stdlib/clojure/math"
	_ "github.com/glojurelang/glojure/pkg/stdlib/clojure/set"
	_ "github.com/glojurelang/glojure/pkg/stdlib/clojure/string"
	_ "github.com/glojurelang/glojure/pkg/stdlib/clojure/walk"

	_ "github.com/gloathub/ys-v0-glj/babashka/http_client"
	_ "github.com/gloathub/ys-v0-glj/babashka/process"
	_ "github.com/gloathub/ys-v0-glj/clj_yaml/core"
	_ "github.com/gloathub/ys-v0-glj/clojure/data/json"
	_ "github.com/gloathub/ys-v0-glj/yamlscript/common"
	_ "github.com/gloathub/ys-v0-glj/yamlscript/regex"
	_ "github.com/gloathub/ys-v0-glj/yamlscript/util"
	_ "github.com/gloathub/ys-v0-glj/ys/dwim"
	_ "github.com/gloathub/ys-v0-glj/ys/fs"
	_ "github.com/gloathub/ys-v0-glj/ys/http"
	_ "github.com/gloathub/ys-v0-glj/ys/ipc"
	_ "github.com/gloathub/ys-v0-glj/ys/json"
	_ "github.com/gloathub/ys-v0-glj/ys/std"
	_ "github.com/gloathub/ys-v0-glj/ys/v0"
	_ "github.com/gloathub/ys-v0-glj/ys/v0/clj"
	_ "github.com/gloathub/ys-v0-glj/ys/v0/common"
	_ "github.com/gloathub/ys-v0-glj/ys/v0/csv"
	_ "github.com/gloathub/ys-v0-glj/ys/v0/debug"
	_ "github.com/gloathub/ys-v0-glj/ys/v0/dwim"
	_ "github.com/gloathub/ys-v0-glj/ys/v0/ext"
	_ "github.com/gloathub/ys-v0-glj/ys/v0/fs"
	_ "github.com/gloathub/ys-v0-glj/ys/v0/global"
	_ "github.com/gloathub/ys-v0-glj/ys/v0/http"
	_ "github.com/gloathub/ys-v0-glj/ys/v0/imports"
	_ "github.com/gloathub/ys-v0-glj/ys/v0/io"
	_ "github.com/gloathub/ys-v0-glj/ys/v0/ipc"
	_ "github.com/gloathub/ys-v0-glj/ys/v0/json"
	_ "github.com/gloathub/ys-v0-glj/ys/v0/manifest"
	_ "github.com/gloathub/ys-v0-glj/ys/v0/platform"
	_ "github.com/gloathub/ys-v0-glj/ys/v0/pods"
	_ "github.com/gloathub/ys-v0-glj/ys/v0/pprint"
	_ "github.com/gloathub/ys-v0-glj/ys/v0/re"
	_ "github.com/gloathub/ys-v0-glj/ys/v0/std"
	_ "github.com/gloathub/ys-v0-glj/ys/v0/taptest"
	_ "github.com/gloathub/ys-v0-glj/ys/v0/util"
	_ "github.com/gloathub/ys-v0-glj/ys/v0/yaml"
	_ "github.com/gloathub/ys-v0-glj/ys/v0/ys"
)

var loadOnce sync.Once

// Load initializes the complete YAMLScript v0 runtime in dependency order.
// Generated programs still call ys.v0/init from their own namespace.
func Load() {
	loadOnce.Do(func() {
		require := glj.Var("clojure.core", "require")
		for _, namespace := range []string{
			"clojure.walk",
			"babashka.process",
			"babashka.http-client",
			"clj-yaml.core",
			"clojure.data.json",
			"ys.v0.clj",
			"ys.v0.platform",
			"ys.v0.global",
			"ys.v0.util",
			"ys.v0.debug",
			"ys.v0.common",
			"ys.v0.csv",
			"ys.v0.dwim",
			"ys.v0.yaml",
			"ys.v0.ext",
			"ys.v0.http",
			"ys.v0.json",
			"ys.v0.manifest",
			"ys.v0.imports",
			"ys.v0.re",
			"ys.v0.ys",
			"ys.v0.fs",
			"ys.v0.io",
			"ys.v0.ipc",
			"ys.v0.pods",
			"ys.v0.pprint",
			"ys.v0.std",
			"yamlscript.regex",
			"ys.v0.taptest",
			"ys.v0",
			"yamlscript.common",
			"yamlscript.util",
			"ys.fs",
			"ys.http",
			"ys.ipc",
			"ys.json",
			"ys.std",
			"ys.dwim",
		} {
			require.Invoke(lang.NewSymbol(namespace))
		}
	})
}
