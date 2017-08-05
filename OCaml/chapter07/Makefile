all:
	ocamllex lexer.mll
	menhir parser.mly
	ocamlopt parsing_information_mli \
           parsing_information.ml \
           term.ml \
           parser.mli \
           parser.ml \
           lexer.ml \
           test.ml

clean:
	$(RM) a.out *.cmi *.cmx *.o lexer.ml parser.mli parser.ml
