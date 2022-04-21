ds := data-sources
dp := data-products

hexes    := $(dp)/geo-hexes
counties := $(dp)/geo-counties

county_polygons := $(ds)/county_polygons.topojson
cbg_polygons    := $(ds)/cbg-polygons/cb_2019_us_bg_500k.shp
cbg_popsize     := $(ds)/cbg_popsize.csv

clean:
	@rm -rf data-products/*

counties: $(counties)/observable/network-processed.csv

hexes: $(hexes)/observable/network-processed.csv

##############################################################################
## Recipes common to both the county and hex versions of the analysis       ##
##############################################################################

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

##############################################################################
## County-geography-only recipes                                            ##
##############################################################################

# Fit the mixed-effects model to the data, and write the estimates of the 
# \alpha coefficients to disk
$(counties)/mixedmodel/alphas.csv: scripts/regression.jl \
                                   $(ds)/fips-neighbors.csv \
                                   $(dp)/covidestim-observations.csv
	@mkdir -p $(counties)/mixedmodel
	julia scripts/regression.jl \
	  -o             $@ \
	  --key          fips \
	  --neighbors    $(ds)/fips-neighbors.csv \
	  --observations $(dp)/covidestim-observations.csv | \
	tee $(counties)/mixedmodel/julia.log

# Reformat the Julia output to make it more machine-readable
$(counties)/mixedmodel/alphas-reformat.csv: scripts/transformResults.R \
                                            $(counties)/mixedmodel/alphas.csv
	Rscript scripts/transformResults.R \
	  -o $@ \
	  --alphas $(counties)/mixedmodel/alphas.csv

# Prepare the machine-readable Julia fit data for the InfoMap routine by
# transforming it into an explicitly graph-oriented representation
counties_infomap_inputs := $(counties)/infomap/network.net \
			   $(counties)/infomap/month-code-mapping.csv \
			   $(counties)/infomap/fips-code-mapping.csv

$(counties_infomap_inputs)&: scripts/transformToMultiplex.R \
	                     $(counties)/mixedmodel/alphas-reformat.csv
	@rm -f network.net
	@mkdir -p $(counties)/infomap
	Rscript scripts/transformToMultiplex.R \
	  --save-network           $(counties)/infomap/network.net \
	  --save-monthcode-mapping $(counties)/infomap/month-code-mapping.csv \
	  --save-fips-mapping      $(counties)/infomap/fips-code-mapping.csv \
	  --alphas-reformat        $(counties)/mixedmodel/alphas-reformat.csv

# Run InfoMap on the network and save its output and logs
counties_infomap_outputs := $(counties)/infomap/network.tree \
			    $(counties)/infomap/network_states.tree \
			    $(counties)/infomap/network.log

$(counties_infomap_outputs)&: $(counties)/infomap/network.net
	@rm -f $(counties)/infomap/{network.tree,network_states.tree,network.log}
	docker run -it --rm \
	  -v $(shell pwd)/$(counties)/infomap/:/opt/data/ \
	  mapequation/infomap:latest \
	  --multilayer-relax-rate 0.4 \
	  /opt/data/network.net \
	  /opt/data/ | tee $(counties)/infomap/network.log

# Do some minimal processing on the `.tree` file from InfoMap to prepare it
# for downstream use in the Observable notebook
$(counties)/observable/network-processed.csv: $(counties)/infomap/network_states.tree
	@mkdir -p $(counties)/observable
	./scripts/stripForObservable.sh < $^ > $@

##############################################################################
## Hexagonal-geography-only recipes                                         ##
##############################################################################
hex_geo_deps := $(county_polygons) $(cbg_polygons) $(cbg_popsize)

$(hexes)/hexid-fips-map.csv $(hexes)/hexes.shp &: scripts/hexbin.R $(hex_geo_deps)
	Rscript scripts/hexbin.R \
          --save-mapping    $(hexes)/hexid-fips-map.csv \
	  --save-shp        $(hexes)/hexes.shp \
	  --county-polygons $(county_polygons) \
	  --cbg-polygons    $(cbg_polygons) \
	  --cbg-popsize     $(cbg_popsize) \
	  --hexsize         25

# The below version adds support for the as-yet-unimplemented --save-neighbors
#   option, which saves a CSV detailing each hexagon's neighboring hexagons.
#
#   It also adds support for per-hex observations, assuming we choose to locate
#   that functionality in this script.
#
# $(hexes)/hexid-fips-map.csv $(hexes)/hexes.shp $(hexes)/hexid-neighbors.csv $(hexes)/hexid-observations.csv &: scripts/hexbin.R $(county_polygons) $(cbg_polygons) $(cbg_popsize) $(dp)/covidestim-observations.csv
# 	Rscript scripts/hexbin.R \
#	  --save-observations $(hexes)/hexid-observations.csv \
#         --save-mapping      $(hexes)/hexid-fips-map.csv \
# 	  --save-neighbors    $(hexes)/hexid-neighbors.csv \
# 	  --save-shp          $(hexes)/hexes.shp \
# 	  --county-polygons   $(county_polygons) \
# 	  --cbg-polygons      $(cbg_polygons) \
# 	  --cbg-popsize       $(cbg_popsize) \
# 	  --fips-observations $(dp)/covidestim-observations.csv \
# 	  --hexsize           25

# Fit the mixed-effects model to the data, and write the estimates of the 
# \alpha coefficients to disk
$(hexes)/mixedmodel/alphas.csv: scripts/regression.jl \
	                        $(hexes)/hexid-neighbors.csv \
				$(hexes)/hexid-observations.csv
	@mkdir -p $(hexes)/mixedmodel
	julia scripts/regression.jl \
	  -o             $@ \
	  --key          hexid \
	  --neighbors    $(hexes)/hexid-neighbors.csv \
	  --observations $(hexes)/hexid-observations.csv | \
	  tee $(hexes)/mixedmodel/julia.log

# Reformat the Julia output to make it more machine-readable
$(hexes)/mixedmodel/alphas-reformat.csv: scripts/transformResults.R \
	                                 $(hexes)/mixedmodel/alphas.csv
	Rscript scripts/transformResults.R \
          -o $@ \
	  --alphas $(hexes)/mixedmodel/alphas.csv

# Prepare the machine-readable Julia fit data for the InfoMap routine by
# transforming it into an explicitly graph-oriented representation.
hexes_infomap_inputs := $(hexes)/infomap/network.net \
			$(hexes)/infomap/month-code-mapping.csv \
			$(hexes)/infomap/hexid-code-mapping.csv

$(hexes_infomap_inputs)&: scripts/transformToMultiplex.R \
	                  $(hexes)/mixedmodel/alphas-reformat.csv
	@rm -f network.net
	@mkdir -p $(hexes)/infomap
	Rscript scripts/transformToMultiplex.R \
	  --save-network           $(hexes)/infomap/network.net \
	  --save-monthcode-mapping $(hexes)/infomap/month-code-mapping.csv \
	  --save-geo-mapping       $(hexes)/infomap/hexid-code-mapping.csv \
	  --key                    hexid \
	  --alphas-reformat        $(hexes)/mixedmodel/alphas-reformat.csv

# Run InfoMap on the network and save its output and logs
hexes_infomap_outputs := $(hexes)/infomap/network.tree \
			 $(hexes)/infomap/network_states.tree \
			 $(hexes)/infomap/network.log

$(hexes_infomap_outputs)&: $(hexes)/infomap/network.net
	@rm -f $(hexes)/infomap/{network.tree,network_states.tree,network.log}
	docker run -it --rm \
	  -v $(shell pwd)/$(hexes)/infomap/:/opt/data/ \
	  mapequation/infomap:latest \
	  --multilayer-relax-rate 0.4 \
	  /opt/data/network.net \
	  /opt/data/ | tee $(hexes)/infomap/network.log

# Do some minimal processing on the `.tree` file from InfoMap to prepare it
# for downstream use in the Observable notebook
$(hexes)/observable/network-processed.csv: $(hexes)/infomap/network_states.tree
	@mkdir -p $(hexes)/observable
	./scripts/stripForObservable.sh < $^ > $@

