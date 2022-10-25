.DEFAULT_GOAL:=help

build:
	for ex in coin kalman radar sin split; do cd $$ex; for algo in particles importance apf_mm; do cd $$algo; make build; cd ..; done; cd ..; done

run:
	for ex in coin kalman radar sin split; do cd $$ex; for algo in particles importance apf_mm; do cd $$algo; make raw.csv & cd ..; done; cd ..; done; wait

csv:
	for ex in coin kalman radar sin split; do cd $$ex; for algo in particles importance apf_mm; do cd $$algo; make error_theta.csv metric_theta.csv error_x.csv; cd ..; done; cd ..; done

pdf:
	mkdir plots; for ex in coin kalman radar sin split; do cd $$ex; gnuplot *.gp; mv *.pdf ../plots; cd ..; done

bench: ## Generate benchmark figures in plots
	make build
	make run
	make csv
	make pdf

help:
	@awk 'BEGIN {FS = ":.*##"; printf "Usage:\n"} /^[a-zA-Z_-]+:.*?##/ { printf "  make %-20s# %s\n", $$1, $$2 }' $(MAKEFILE_LIST)
