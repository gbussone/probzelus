PROBZELUC=probzeluc

NAME=coin

all: build

$(NAME).ml main.ml: $(NAME).zls
	$(PROBZELUC) -apf -I `zeluc -where`-io -s main $+

clean:
	rm -f $(NAME).ml main.ml
	rm -f *.zci

## Generic rules

.phony: build exec exec_ds

exec: main.ml
	dune exec ./main.exe

build: main.ml
	dune build main.exe

%.zci: %.zli
	$(PROBZELUC) $<

cleanall: clean
	dune clean
	rm -f *~
