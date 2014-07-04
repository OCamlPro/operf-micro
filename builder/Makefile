all: builder

parser.ml parser.mli: parser.mly
	ocamlyacc $<

lexer.ml: lexer.mll
	ocamllex $<

loc.cmi: loc.mli
	ocamlc -c $<

loc.cmo: loc.ml loc.cmi
	ocamlc -c $<

files.cmi files.cmo: files.ml
	ocamlc -c $<

parser.cmi: parser.mli files.cmi
	ocamlc -c $<

parser.cmo: parser.ml parser.cmi files.cmi
	ocamlc -c $<

lexer.cmi lexer.cmo: lexer.ml parser.cmi
	ocamlc -c $<

builder.cmo: builder.ml parser.cmi lexer.cmi loc.cmi
	ocamlc -c $<

builder: files.cmo parser.cmo lexer.cmo loc.cmo builder.cmo
	ocamlc -o $@ $^

clean:
	rm -f parser.ml parser.mli lexer.ml
	rm -f *.cmo *.cmi
	rm -f builder

.PHONY: clean
