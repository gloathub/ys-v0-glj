R := https://github.com/makeplus/makes
M := .cache/makes
$(shell [ -d '$M' ] || git clone -q $R '$M')

include $M/init.mk

GLOJURE-VERSION := 0.7.11
GO-VERSION := 1.24.0
YS-V0-VERSION := 0.2.29
VERSION := 0.1.1

include $M/glojure.mk
include $M/go.mk
include $M/clean.mk

GLOJURE-SRC := $(LOCAL-CACHE)/glojure-$(GLOJURE-VERSION)

.PHONY: generate test check-generated release

generate: $(GLJ) $(GO) $(GLOJURE-SRC)
	GLJ='$(abspath $(GLJ))' \
	GLOJURE_SRC='$(abspath $(GLOJURE-SRC))' \
	GO='$(abspath $(GO))' \
	YS_V0_VERSION='$(YS-V0-VERSION)' \
	YS_V0_SOURCE='$(YS_V0_SOURCE)' \
	./script/generate

test: generate
	$(GO) test ./...

check-generated: generate
	git diff --exit-code -- \
	  babashka bb clojure yamlscript ys source runtime/namespaces.edn

release:
	@$(if $(filter command line,$(origin VERSION)),,\
	  $(error VERSION is required on the command line))
	./script/release '$(VERSION)'

$(GLOJURE-SRC):
	git clone -q --depth 1 -b v$(GLOJURE-VERSION) \
	  --config advice.detachedHead=false \
	  https://github.com/glojurelang/glojure '$@'

clean::
	$(RM) -r .cache/build
