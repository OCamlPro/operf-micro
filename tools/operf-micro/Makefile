
ROOTDIR=../../

include $(ROOTDIR)/Makefile.conf

WARN=-w +1..45-4-44-42-40
OPT= $(WARN) -g -bin-annot -principal -I +unix

OBJS=\
  static_config.cmo utils.cmo \
  files.cmo parser.cmo lexer.cmo loc.cmo command.cmo \
  detect_config.cmo benchmark.cmo ransac.cmo \
  measurements.cmo builder.cmo main.cmo

ifeq "${NATIVE}" "true"
BIN=builder.opt$(EXEEXT)
else
BIN=builder$(EXEEXT)
endif

all: $(BIN)

static_config.ml: $(ROOTDIR)/Makefile.conf
	echo "let name = \"$(PKG)\"" > static_config.ml
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
	ocamlc -g -I +unix -o $@ unix.cma $^

builder.opt$(EXEEXT): $(OBJS:cmo=cmx)
	ocamlopt -g -I +unix -o $@ unix.cmxa $^

install: $(BIN)
	mkdir -p $(BINDIR)
	cp -f $(BIN) $(BINDIR)/$(PKG)$(EXEEXT)

uninstall:
	rm -f $(BINDIR)/$(PKG)$(EXEEXT)

clean:
	rm -f static_config.ml
	rm -f parser.ml parser.mli lexer.ml
	rm -f *.o *.a *.cmx *.cmo *.cmi *.cmt *.cmti
	rm -f builder builder.opt

.PHONY: all install depend clean
depend: static_config.ml lexer.ml parser.ml parser.mli
	ocamldep *.ml *.mli > .depend

include .depend
