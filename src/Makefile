PKGVERSION = $(shell git describe --always --dirty)

WEB = san@math.umons.ac.be:~/public_html/software/

MODULES=main command functions authors graphs mathfunc rsa
OBJECTS=$(MODULES:=.cmo)
MLS=$(MODULES:=.ml)
MLIS=$(MODULES:=.mli)
TEST=test.byte
MAIN=main.byte
OCAMLBUILD=ocamlbuild -use-ocamlfind

default: build
	utop

build:
	$(OCAMLBUILD) $(OBJECTS)

test:
	$(OCAMLBUILD) -tag 'debug' $(TEST) && ./$(TEST)

run:
	$(OCAMLBUILD) $(MAIN) && ./$(MAIN)

zip:
	zip -r a5src.zip *.ml* *.txt* *.csv* rml _tags .merlin .ocamlinit Makefile

docs: docs-public docs-private

docs-public: build
	mkdir -p doc.public
	ocamlfind ocamldoc -I _build -package ANSITerminal,csv,ocamlgraph,graphics,gnuplot,base,num,str \
		-html -stars -d doc.public $(MLIS)

docs-private: build
	mkdir -p doc.private
	ocamlfind ocamldoc -I _build -package ANSITerminal,csv,ocamlgraph,graphics,gnuplot,base,num,str \
		-html -stars -d doc.private \
		-inv-merge-ml-mli -m A -hide-warnings $(MLIS) $(MLS)

clean:
	ocamlbuild -clean
	rm -rf doc.public doc.private