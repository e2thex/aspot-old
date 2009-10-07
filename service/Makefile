
all: pordb

pordb: server.ml pordb.ml
	ocamlfind ocamlc -o pordb -package "netstring,netcgi2,unix,nethttpd-for-netcgi2,netplex" \
		-thread pordb.ml server.ml -linkpkg -g

test: test.ml database.ml
	ocamlopt -o progprog database.ml test.ml

clean:
	rm -f *.cmi *.cmo *.cmx *.o pordb
