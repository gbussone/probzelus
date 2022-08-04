EX=outlier
ALGO=fm_mm
NAME=outlier_fm_mm

include ../../Makefile.common-config

CSV_FILES=perf.csv perf-step.csv mem-ideal.csv accuracy.csv

PROBZELUC += -fm -I ../outlierlib

all: build

bench: bench_per_particles bench_per_steps bench_mem_ideal

include ../../Makefile.common-algo
