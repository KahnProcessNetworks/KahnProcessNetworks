all: pipe

thread: kahn.ml Implementations/thread.ml test.ml
	cp Implementations/miscellaneous.ml miscellaneous.ml
	cp Implementations/thread.ml implementation.ml
	ocamlc -thread unix.cma threads.cma kahn.ml miscellaneous.ml implementation.ml test.ml

pipe: kahn.ml Implementations/pipe.ml test.ml
	cp Implementations/miscellaneous.ml miscellaneous.ml
	cp Implementations/pipe.ml implementation.ml
	ocamlc unix.cma kahn.ml miscellaneous.ml implementation.ml test.ml

network: kahn.ml Implementations/network.ml test.ml
	cp Implementations/miscellaneous.ml miscellaneous.ml
	cp Implementations/network.ml implementation.ml
	ocamlc unix.cma kahn.ml miscellaneous.ml implementation.ml test.ml

sequential: kahn.ml Implementations/sequential.ml test.ml
	cp Implementations/miscellaneous.ml miscellaneous.ml
	cp Implementations/sequential.ml implementation.ml
	ocamlc unix.cma kahn.ml miscellaneous.ml implementation.ml test.ml

edit:
	gedit README.md Makefile kahn.ml Implementations/miscellaneous.ml Implementations/thread.ml Implementations/pipe.ml Implementations/network.ml Implementations/sequential.ml test.ml &

clean:
	rm -f *.cmi *.cmo *.out implementation.ml miscellaneous.ml
