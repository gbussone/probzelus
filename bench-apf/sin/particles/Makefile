NAME=sin
FM=

all: build

%.zci: %.zli
	probzeluc $<

main.ml: $(NAME).zls time.zci
	probzeluc $(FM) -I `zeluc -where`-io -s main $(NAME).zls

build: main.ml
	dune build main.exe

exec: main.ml
	dune exec ./main.exe

clean:
	dune clean
	rm -f $(NAME).ml main.ml
	rm -f *.zci
