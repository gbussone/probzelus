EX=coin
ALGO=apf_is
NAME=coin_apf_is

include ../../Makefile.common-config

CSV_FILES=perf.csv perf-step.csv mem-ideal.csv accuracy.csv

PROBZELUC += -apf -I ../coinlib

all: build

bench: bench_per_particles bench_per_steps bench_mem_ideal

include ../../Makefile.common-algo
