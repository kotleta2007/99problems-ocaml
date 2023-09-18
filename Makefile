all:
	ocamlopt -I +str -o table str.cmxa table.ml

clean:
	rm -rf table table.cmi table.cmx table.o
