all: kahn.ml miscellaneous.ml th.ml Test/test.ml
	ocamlc -thread unix.cma threads.cma unix.cma miscellaneous.ml th.ml pipe.ml network.ml sequential.ml kahn.ml Test/test.ml

edit:
	gedit README.md Makefile kahn.ml miscellaneous.ml thread.ml pipe.ml network.ml sequential.ml Test/test.ml &

clean:
	rm -f *~ *.cmi *.cmo *.out
