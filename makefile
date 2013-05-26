all: miscellaneous.ml threads.ml pipe.ml server.ml network.ml sequential.ml kahn.ml Test/test.ml
	clear
	rm -f *~ *.cmi *.cmo *.out
	ocamlopt -thread unix.cmxa threads.cmxa miscellaneous.ml threads.ml pipe.ml server.ml network.ml sequential.ml kahn.ml Test/test.ml
	ls -l --color
	ps
	cat network.config

clean:
	rm -f *~ *.cmi *.cmo *.cmx *.o *.out
