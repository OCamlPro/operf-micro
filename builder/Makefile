
include ../Makefile.conf

WARN=-w +1..45-4-44-42
OPT= $(WARN) -g -bin-annot -principal

OBJS=\
  static_config.cmo utils.cmo \
  files.cmo parser.cmo lexer.cmo loc.cmo command.cmo \
  detect_config.cmo benchmark.cmo measurements.cmo \
  builder.cmo main.cmo

ifeq "${BEST}" "opt"
BIN=builder.opt$(EXEEXT)
endif

all: $(BIN)

static_config.ml: ../Makefile.conf
	echo "let name = \"$(NAME)\"" > static_config.ml
	echo "let prefix = \"$(PREFIX)\"" >> static_config.ml

parser.ml parser.mli: parser.mly
	ocamlyacc $<

lexer.ml: lexer.mll
	ocamllex $<

%.cmx: %.ml
	ocamlopt $(OPT) -c $<

%.cmo: %.ml
	ocamlc $(OPT) -c $<

%.cmi: %.mli
	ocamlc $(OPT) -c $<

builder$(EXEEXT): $(OBJS)
	ocamlc -g -o $@ unix.cma $^

builder.opt$(EXEEXT): $(OBJS:cmo=cmx)
	ocamlopt -g -o $@ unix.cmxa $^

install: $(BIN)
	mkdir -p $(BINDIR)
	cp -f $(BIN) $(BINDIR)/$(NAME)$(EXEEXT)

clean:
	rm -f static_config.ml
	rm -f parser.ml parser.mli lexer.ml
	rm -f *.o *.a *.cmx *.cmo *.cmi *.cmt *.cmti
	rm -f builder builder.opt

.PHONY: all install depend clean
depend: static_config.ml lexer.ml parser.ml parser.mli
	ocamldep *.ml *.mli > .depend

include .depend
