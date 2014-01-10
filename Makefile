REBAR= `which ./rebar || rebar`

CAMLSRC=ocaml_src
SDB=priv/fbi_sdb
OCAML_DEPS:=deps/ocaml-jskitlib

.PHONY: all compile clean install uninstall
.PHONY: test-unit test-ct check
.PHONY: ocaml clean-ocaml $(OCAML_DEPS) clean-ocaml-deps

all: compile install

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

test-unit: install
	$(REBAR) eunit skip_deps=true

test-ct:
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
