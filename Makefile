R := https://github.com/makeplus/makes
M := .cache/makes
$(shell [ -d '$M' ] || git clone -q $R '$M')

include $M/init.mk

GLOJURE-VERSION := 0.7.24
GO-VERSION := 1.24.0
YS-V0-VERSION := 0.3.5
VERSION := 0.1.5

include $M/glojure.mk
include $M/go.mk
include $M/perl.mk
include $M/clean.mk

GLOJURE-SRC := $(LOCAL-CACHE)/glojure-$(GLOJURE-VERSION)

generate: $(GLJ) $(GO) $(PERL) $(GLOJURE-SRC)
	GLJ='$(abspath $(GLJ))' \
	GLOJURE_SRC='$(abspath $(GLOJURE-SRC))' \
	GO='$(abspath $(GO))' \
	PERL='$(abspath $(PERL))' \
	YS_V0_VERSION='$(YS-V0-VERSION)' \
	YS_V0_SOURCE='$(YS_V0_SOURCE)' \
	./script/generate

test: generate
	$(GO) test ./...

check-generated: generate
	git diff --exit-code -- \
	  babashka bb clojure yamlscript ys source runtime/namespaces.edn

# Use v=VERSION as the short version form, a=1 to release from the current
# branch, and d=1 to preview release actions.
RELEASE-VERSION := \
  $(strip $(if $(filter command line,$(origin VERSION)),$(VERSION),\
    $(if $(filter command line,$(origin v)),$(v))))
ifdef a
export YS_V0_GLJ_RELEASE_ALLOW_BRANCH := 1
endif
ifdef d
export YS_V0_GLJ_RELEASE_DRYRUN := 1
endif

release:
	@$(if $(RELEASE-VERSION),,\
	  $(error v or VERSION is required on the command line))
	./script/release '$(RELEASE-VERSION)'

$(GLOJURE-SRC):
	git clone -q --depth 1 -b v$(GLOJURE-VERSION) \
	  --config advice.detachedHead=false \
	  https://github.com/glojurelang/glojure '$@'

clean::
	$(RM) -r .cache/build
