# Pull latest model results from the API, but only the important variables,
# and cut off the last ~1.5 months
results.csv: scripts/pullLatestResults.R
	Rscript scripts/pullLatestResults.R

# Fit the mixed-effects model to the data, and write the estimates of the 
# \alpha coefficients to disk
alphas.csv: scripts/regression.jl neighbors.csv results.csv
	julia scripts/regression.jl | tee julia.log

# Reformat the Julia output to make it more machine-readable
alphas_reformat.csv: scripts/transformResults.R alphas.csv
	Rscript scripts/transformResults.R

# Prepare the machine-readable Julia fit data for the InfoMap routine by
# transforming it into an explicitly graph-oriented representtation
network.net month-code-mapping.csv fips-code-mapping.csv &: scripts/transformToMultiplex.R alphas_reformat.csv
	@rm -f network.net
	Rscript scripts/transformToMultiplex.R

# Run InfoMap on the network and save its output and logs
network.tree network_states.tree network.log &: network.net
	@rm -f network.tree network_states.tree network.log
	docker run -it --rm \
	  -v $(shell pwd):/data/ \
	  mapequation/infomap:latest \
	  --multilayer-relax-rate 0.4 \
	  /data/network.net \
	  /data/ | tee network.log

# Do some minimal processing on the `.tree` file from InfoMap to prepare it
# for downstream use in the Observable notebook
network-processed.csv: network_states.tree
	./scripts/stripForObservable.sh < $^ > $@

