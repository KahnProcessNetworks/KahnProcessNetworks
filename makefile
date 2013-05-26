all: miscellaneous.ml th.ml pipe.ml server.ml network.ml sequential.ml kahn.ml Test/test.ml
	clear
	rm -f *~ *.cmi *.cmo *.out
	ocamlopt unix.cmxa miscellaneous.ml pipe.ml server.ml network.ml sequential.ml kahn.ml Test/test.ml
	ls -l --color
	ps
	cat network.config

edit: README.md makefile kahn.ml miscellaneous.ml th.ml pipe.ml server.ml network.config network.ml sequential.ml Test/test.ml
	gedit README.md makefile kahn.ml miscellaneous.ml th.ml pipe.ml server.ml network.config network.ml sequential.ml Test/test.ml &

clean:
	rm -f *~ *.cmi *.cmo *.cmx *.o *.out
