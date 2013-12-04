OCAMLPATH="../../../deps/ocaml_erlang_port:../../../ocaml_deps"
REQUIRES = "str,num,ocaml-erlang-port,ocaml-jskitlib,unix"
OCAMLOPT = OCAMLPATH="$(OCAMLPATH)" ocamlfind ocamlopt -package $(REQUIRES)
OCAMLC = OCAMLPATH="$(OCAMLPATH)" ocamlfind ocamlc -package $(REQUIRES)
OCAMLLEX = OCAMLPATH="$(OCAMLPATH)" ocamllex
OCAMLYACC = OCAMLPATH="$(OCAMLPATH)" ocamlyacc

PARSER = fbi_conf_parser.mly
LEXER = fbi_conf_lexer.mll

PARSER_AND_LEXER_CMXS = ${PARSER:.mly=.cmx} ${LEXER:.mll=.cmx}

COMMON_CMXS = mavg.cmx fbi_conf_type.cmx fbi_packet.cmx fbi_conf.cmx fbi_storage.cmx threshold_eval.cmx event_loop.cmx
ETOBJS = $(PARSER_AND_LEXER_CMXS) $(COMMON_CMXS) test-sdb.cmx
EROBJS = $(PARSER_AND_LEXER_CMXS) $(COMMON_CMXS) fbi-sdb.cmx

all: fbi-sdb fbi-report

include .depends

fbi-sdb: compile-parser $(EROBJS)
	$(OCAMLOPT) -linkpkg -o fbi-sdb $(EROBJS)

fbi-report: fbi_report.cmx
	$(OCAMLOPT) -linkpkg -o fbi-report fbi_report.cmx

test-sdb: $(ETOBJS)
	$(OCAMLOPT) -linkpkg -o test-sdb $(ETOBJS)

depend:
	ocamldep *.ml *.mly *.mll > .depends

check: test-sdb
	./test-sdb

install: fbi-sdb
	mkdir -p ../priv/
	install -C ./fbi-sdb ../priv/

uninstall:
	rm -f ../priv/fbi-sdb

.PHONY: compile-parser
compile-parser:: fbi_conf_type.cmx ${PARSER:.mly=.ml} ${PARSER:.mly=.mli} ${PARSER:.mly=.cmi} ${PARSER:.mly=.cmx}

.PHONY: compile-lexer
compile-lexer:: ${LEXER:.mll=.ml} ${LEXER:.mll=.cmx}

.SUFFIXES: .ml .mli .mly .mll .cmx .cmo .cmi

.mly.ml:
	$(OCAMLYACC) -v $<

.mly.mli:
	$(OCAMLYACC) $<

.mll.ml:
	$(OCAMLLEX) $<

.ml.cmx:
	$(OCAMLOPT) -o $@ -c $<

.ml.cmo:
	$(OCAMLC) -o $@ -c $<

.mli.cmi:
	$(OCAMLC) -o $@ $<

clean:
	rm -f fbi_conf_type.mli
	rm -f ${PARSER_AND_LEXER_CMXS:.cmx=.ml}
	rm -f ${PARSER_AND_LEXER_CMXS:.cmx=.mli}
	rm -f *.cm* *.[ao]
	rm -f *.output

