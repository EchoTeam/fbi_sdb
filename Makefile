REBAR := $(shell which ./rebar || which rebar)

CAMLSRC=ocaml_src
SDB=priv/fbi_sdb
OCAML_DEPS:=deps/ocaml-jskitlib

.PHONY: all get-deps compile clean install uninstall
.PHONY: test-unit test-ct check
.PHONY: ocaml clean-ocaml $(OCAML_DEPS) clean-ocaml-deps

all: get-deps compile install

get-deps:
	$(REBAR) get-deps

compile: $(OCAML_DEPS)
	$(REBAR) compile

$(OCAML_DEPS):
	$(MAKE) -C $@

install:
	cd $(CAMLSRC); $(MAKE) install

uninstall:
	cd $(CAMLSRC); $(MAKE) uninstall

ocaml:
	cd $(CAMLSRC); $(MAKE)

ocaml-clean:
	cd $(CAMLSRC); $(MAKE) clean

clean-ocaml:
	cd $(CAMLSRC); $(MAKE) clean

test-unit: all 
	$(REBAR) eunit skip_deps=true

test-ct: all
	$(REBAR) ct skip_deps=true

check: test-unit test-ct

clean-ocaml-deps:
	for ocamld in $(OCAML_DEPS); do \
		$(MAKE) -C $$ocamld clean || exit 1; \
	done

clean: clean-ocaml-deps
	$(REBAR) clean
	rm -f $(SDB)
	rm -fr .eunit
