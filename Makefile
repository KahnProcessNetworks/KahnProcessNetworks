all:  
	ocamlc -thread unix.cma threads.cma kahn.ml example.ml test.ml
