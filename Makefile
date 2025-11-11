
LEX_ML = /usr/bin/ocamllex
YACC_ML = /usr/bin/ocamlyacc
OCAMLC = /usr/bin/ocamlc

all: Evaluator

Evaluator: parser main.ml
	$(OCAMLC) -o main ast.cmo lexer.cmo parser.cmo main.ml

parser: ast.ml lexer.mll parser.mly
	$(OCAMLC) -c ast.ml
	$(LEX_ML) -o lexer.ml lexer.mll
	$(YACC_ML) -b parser parser.mly
	$(OCAMLC) -c parser.mli
	$(OCAMLC) -c lexer.ml
	$(OCAMLC) -c parser.ml

clean:
	rm -f *.cmo
	rm -f *.cmi
	rm -f prologTerm
	rm -f evaluator	
	rm -f lexer.ml
	rm -f parser.mli
	rm -f parser.ml