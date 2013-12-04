REBAR= `which ./rebar || rebar`

.PHONY: ocaml clean-ocaml

CAMLSRC=ocaml_src
SDB=priv/fbi_sdb

all: compile install

compile:
	$(REBAR) compile

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

test: install
	$(REBAR) eunit

clean:
	$(REBAR) clean
	rm -f $(SDB)
	rm -fr .eunit
