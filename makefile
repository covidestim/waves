results.csv: scripts/pullLatestResults.R
	Rscript scripts/pullLatestResults.R

alphas.csv: scripts/regression.jl neighbors.csv results.csv
	julia scripts/regression.jl

alphas_reformat.csv: scripts/transformResults.R alphas.csv
	Rscript scripts/transformResults.R

network.net fips-code-mapping.csv &: scripts/transformToMultiplex.R alphas_reformat.csv
	@rm -f network.net
	Rscript scripts/transformToMultiplex.R

network.tree network_states.tree network.log &: network.net
	@rm -f network.tree network_states.tree network.log
	docker run -it --rm \
	  -v $(shell pwd):/data/ \
	  mapequation/infomap:latest \
	  --multilayer-relax-rate 0.4 \
	  /data/network.net \
	  /data/ | tee network.log

network-processed.csv: network_states.tree
	./scripts/stripForObservable.sh < $^ > $@
