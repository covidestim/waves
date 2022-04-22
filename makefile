county_polygons := county_polygons.topojson
cbg_polygons := cbg-polygons/cb_2019_us_bg_500k.shp
cbg_popsize := cbg_popsize.csv

hexid-fips-map.csv hexes.shp &: scripts/hexbin.R $(county_polygons) $(cbg_polygons) $(cbg_popsize)
	Rscript scripts/hexbin.R \
          --save-csv hexid-fips-map.csv \
	  --save-shp hexes.shp \
	  --county-polygons $(county_polygons) \
	  --cbg-polygons $(cbg_polygons) \
	  --cbg-popsize $(cbg_popsize) \
	  --hexsize 25

# Reads the hex shapefile created in hexbin.R and creates neighbors dataframe
hex_polygons := hexes.shp

neighbors_hex.csv &: scripts/neighbors.R $(hex_polygons)
	Rscript scripts/neighbors.R \
	--hex-polygons $(hex_polygons)

# Pull model results and case counts from the API, but only the important
# variables, and cut off the last ~1.5 months
results.csv: scripts/pullInputData.R
	Rscript scripts/pullInputData.R

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

