EX=sin
ALGO=ds_nogc
NAME=sin_ds_nogc

include ../../Makefile.common-config

CSV_FILES=perf-step.csv mem-ideal.csv

PROBZELUC += -I ../sinlib

all: build

bench: bench_per_steps bench_mem_ideal

include ../../Makefile.common-algo
