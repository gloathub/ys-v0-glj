# YAMLScript runtime for Glojure

This module supplies Gloat's compiled YAMLScript standard library.
Its `source/` tree and Go loaders are generated together.
Change the upstream YAMLScript sources rather than editing generated files.

## Development

From this checkout, generate and test against a local YAMLScript source tree:

```sh
make test YS_V0_SOURCE=/absolute/path/to/yamlscript/core/src
```

Gloat automatically uses this development checkout when it is located at
`repos/ys-v0-glj` inside the Gloat repository.
Rebuild YAMLScript against that Gloat checkout with:

```sh
make -C ys build YAMLSCRIPT_ENGINE=glojure \
  GLOAT-DIR=/absolute/path/to/gloat \
  -W "$PWD/.cache/glojure-src/.prepared"
```

Run that command from the YAMLScript repository.
The resulting executable embeds the local generated runtime.
The `-W` option rebuilds an existing executable after runtime changes.

## Release dependencies

Without `YS_V0_SOURCE`, generation downloads the `org.yamlscript/ys.v0` JAR
pinned by `YS-V0-VERSION` in the Makefile.
Keep that version aligned with the source snapshot used for generation.
The operator and math changes in this snapshot require publishing the updated
library as part of the next YAMLScript release before releasing this module.
Until then, use `YS_V0_SOURCE` for every generation or test command.

Publish the updated YAMLScript library first, regenerate this module from the
published JAR, and verify that its generated files match the reviewed snapshot.
After releasing this module, update and release Gloat with the new runtime pin.
Then update and build the YAMLScript CLI with that Gloat release.
