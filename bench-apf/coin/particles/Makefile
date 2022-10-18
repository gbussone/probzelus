NAME=coin
APF=

all: build

%.zci: %.zli
	probzeluc $<

main.ml: $(NAME).zls time.zci
	probzeluc $(APF) -I `zeluc -where`-io -s main $(NAME).zls

build: main.ml
	dune build main.exe

exec: main.ml
	dune exec ./main.exe

raw.csv: main.ml
	-for i in `seq 1 10`; do cat ../data | dune exec ./main.exe >> raw.csv; done

error_theta.csv: raw.csv
	sed -e 's/\(.*\), \(.*\), \(.*\), \(.*\), \(.*\), \(.*\)/Step,\1,0,,,0,\2,0,0/' raw.csv > tmp.csv
	mv tmp.csv ../../../benchmarks
	cd ../../../benchmarks; dune exec tools/analyze_csv.exe -- -per_step tmp.csv -perf-step tmp2.csv; rm tmp.csv
	mv ../../../benchmarks/tmp2.csv error_theta.csv

metric_theta.csv: raw.csv
	sed -e 's/\(.*\), \(.*\), \(.*\), \(.*\), \(.*\), \(.*\)/Step,\1,0,,,0,\3,0,0/' raw.csv > tmp.csv
	mv tmp.csv ../../../benchmarks
	cd ../../../benchmarks; dune exec tools/analyze_csv.exe -- -per_step tmp.csv -perf-step tmp2.csv; rm tmp.csv
	mv ../../../benchmarks/tmp2.csv metric_theta.csv

error_x.csv: raw.csv
	sed -e 's/\(.*\), \(.*\), \(.*\), \(.*\), \(.*\), \(.*\)/Step,\1,0,,,0,\4,0,0/' raw.csv > tmp.csv
	mv tmp.csv ../../../benchmarks
	cd ../../../benchmarks; dune exec tools/analyze_csv.exe -- -per_step tmp.csv -perf-step tmp2.csv; rm tmp.csv
	mv ../../../benchmarks/tmp2.csv error_x.csv

metric_x.csv: raw.csv
	sed -e 's/\(.*\), \(.*\), \(.*\), \(.*\), \(.*\), \(.*\)/Step,\1,0,,,0,\5,0,0/' raw.csv > tmp.csv
	mv tmp.csv ../../../benchmarks
	cd ../../../benchmarks; dune exec tools/analyze_csv.exe -- -per_step tmp.csv -perf-step tmp2.csv; rm tmp.csv
	mv ../../../benchmarks/tmp2.csv metric_x.csv

time.csv: raw.csv
	sed -e 's/\(.*\), \(.*\), \(.*\), \(.*\), \(.*\), \(.*\)/Step,\1,0,,,0,\6,0,0/' raw.csv > tmp.csv
	mv tmp.csv ../../../benchmarks
	cd ../../../benchmarks; dune exec tools/analyze_csv.exe -- -per_step tmp.csv -perf-step tmp2.csv; rm tmp.csv
	mv ../../../benchmarks/tmp2.csv time.csv

clean:
	dune clean
	rm -f $(NAME).ml main.ml
	rm -f *.csv
	rm -f *.zci
