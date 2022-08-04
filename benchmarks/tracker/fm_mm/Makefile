EX=tracker
ALGO=fm_mm
NAME=tracker_fm_mm

include ../../Makefile.common-config

CSV_FILES=perf.csv perf-step.csv mem-ideal.csv accuracy.csv

PROBZELUC += -fm -I ../trackerlib

all: build

bench: bench_per_particles bench_per_steps bench_mem_ideal

include ../../Makefile.common-algo
