all: list-benchmarks.png

build=dist/build
pkg=parfold

%.png: %.csv
	barchart criterion \
		--title="Performance of parallel list folds" \
		$<

list-benchmarks.csv: $(build)/$(pkg)-list-benchmarks/$(pkg)-list-benchmarks
	./$< --summary=$@

$(build)/$(pkg)-list-benchmarks/$(pkg)-list-benchmarks: \
	parfold.cabal list-benchmarks.hs
	cabal configure && cabal build
