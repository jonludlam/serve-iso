.PHONY: clean default

default: serve_iso.native

clean:	
	rm -rf *.native *~ _build

serve_iso.native: src/serve_iso.ml
	ocamlbuild -use-ocamlfind src/serve_iso.native

