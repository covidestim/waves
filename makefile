ds := data-sources
dp := data-products

county_polygons := $(ds)/county_polygons.topojson
cbg_polygons    := $(ds)/cbg-polygons/cb_2019_us_bg_500k.shp
cbg_popsize     := $(ds)/cbg_popsize.csv

clean:
	@rm -rf data-products/*

$(dp)/hexid-fips-map.csv $(dp)/hexes.shp &: scripts/hexbin.R $(county_polygons) $(cbg_polygons) $(cbg_popsize)
	Rscript scripts/hexbin.R \
          --save-csv $(dp)/hexid-fips-map.csv \
	  --save-shp $(dp)/hexes.shp \
	  --county-polygons $(county_polygons) \
	  --cbg-polygons $(cbg_polygons) \
	  --cbg-popsize $(cbg_popsize) \
	  --hexsize 25

all: counties hexes

counties: $(counties)/observable/network-processed.csv

# Reads the hex shapefile created in hexbin.R and creates neighbors dataframe
neighbors_hex.csv: scripts/neighbors.R hexes.shp
	Rscript scripts/neighbors.R -o $@ --hex-polygons hexes.shp

# Pull model results and case counts from the API, but only the important
# variables, and cut off the last ~1.5 months
$(dp)/covidestim-observations.csv: scripts/pullInputData.R
	Rscript scripts/pullInputData.R \
		-o $@ \
		--rundate 2021-12-02 \
                --clip-final-months 1

# Fit the mixed-effects model to the data, and write the estimates of the 
# \alpha coefficients to disk
$(dp)/alphas.csv: scripts/regression.jl $(ds)/fips-neighbors.csv $(dp)/covidestim-observations.csv
	julia scripts/regression.jl \
		-o $@ \
		--key fips \
		--neighbors $(ds)/fips-neighbors.csv \
		--observations $(dp)/covidestim-observations.csv | \
	tee $(dp)/julia.log

# Reformat the Julia output to make it more machine-readable
$(dp)/alphas-reformat.csv: scripts/transformResults.R $(dp)/alphas.csv
	Rscript scripts/transformResults.R -o $@ --alphas $(dp)/alphas.csv

# Prepare the machine-readable Julia fit data for the InfoMap routine by
# transforming it into an explicitly graph-oriented representtation
$(dp)/network.net $(dp)/month-code-mapping.csv $(dp)/fips-code-mapping.csv &: scripts/transformToMultiplex.R $(dp)/alphas_reformat.csv
	@rm -f network.net
	Rscript scripts/transformToMultiplex.R \
		--save-network $(dp)/network.net \
		--save-monthcode-mapping $(dp)/month-code-mapping.csv \
		--save-fips-mapping $(dp)/fips-code-mapping.csv \
	        --alphas-reformat $(dp)/alphas_reformat.csv

# Run InfoMap on the network and save its output and logs
$(dp)/network.tree $(dp)/network_states.tree $(dp)/network.log &: $(dp)/network.net
	@rm -f network.tree network_states.tree network.log
	docker run -it --rm \
	  -v $(shell pwd):/data/ \
	  -v $(shell pwd)/$(dp):/opt/out/ \
	  mapequation/infomap:latest \
	  --multilayer-relax-rate 0.4 \
	  /opt/in/network.net \
	  /opt/out/ | tee /opt/out/network.log

# Do some minimal processing on the `.tree` file from InfoMap to prepare it
# for downstream use in the Observable notebook
$(dp)/network-processed.csv: $(dp)/network_states.tree
	./scripts/stripForObservable.sh < $^ > $@

