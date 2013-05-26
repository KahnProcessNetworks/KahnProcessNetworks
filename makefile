all: miscellaneous.ml th.ml pipe.ml server.ml network.ml sequential.ml kahn.ml Test/test.ml
	ocamlc -thread unix.cma threads.cma unix.cma miscellaneous.ml th.ml pipe.ml server.ml network.ml sequential.ml kahn.ml Test/test.ml

edit: README.md makefile kahn.ml miscellaneous.ml th.ml pipe.ml server.ml network.config network.ml sequential.ml Test/test.ml
	gedit README.md makefile kahn.ml miscellaneous.ml th.ml pipe.ml server.ml network.config network.ml sequential.ml Test/test.ml &

clean:
	rm -f *~ *.cmi *.cmo *.out
