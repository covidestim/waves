# Delete files made by recipes which error
.DELETE_ON_ERROR:

ds := data-sources
dp := data-products

hexes    := $(dp)/geo-hexes
counties := $(dp)/geo-counties

county_polygons := $(ds)/county_polygons.topojson
cbg_polygons    := $(ds)/cbg-polygons/cb_2019_us_bg_500k.shp
cbg_popsize     := $(ds)/cbg_popsize.csv

clean:
	@rm -rf data-products/*

all: counties hexes

.PHONY: counties hexes all

counties_outputs := $(counties)/mixedmodel/alphas-reformat.csv \
		    $(counties)/observable/network-processed.csv

hexes_outputs := $(hexes)/mixedmodel/alphas-reformat.csv \
		 $(hexes)/observable/network-processed.csv \
		 $(hexes)/hexes-albers.topojson \
		 $(hexes)/observable/network-joined.topojson.gz \
		 $(hexes)/vectors/vectors.geojson

hexes: $(hexes_outputs)

counties: $(counties_outputs)

##############################################################################
## Recipes common to both the county and hex versions of the analysis       ##
##############################################################################

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
	  -o              $@ \
	  --key           fips \
	  --neighbors     $(ds)/fips-neighbors.csv \
	  --observations  $(dp)/covidestim-observations.csv \
	  --regardless-of-rt \
	  --predict       infectionsPC \
	  --predict-using infectionsPC | \
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
	  --save-geo-mapping       $(counties)/infomap/fips-code-mapping.csv \
	  --key                    fips \
	  --alphas-reformat        $(counties)/mixedmodel/alphas-reformat.csv

# Run InfoMap on the network and save its output and logs
counties_infomap_outputs := $(counties)/infomap/network.tree \
			    $(counties)/infomap/network_states.tree \
			    $(counties)/infomap/network.log

$(counties_infomap_outputs)&: $(counties)/infomap/network.net
	@rm -f $(counties)/infomap/{network.tree,network_states.tree,network.log}
	docker run --rm \
	  -v $(shell pwd)/$(counties)/infomap/:/opt/data/ \
	  mapequation/infomap:latest \
	  --multilayer-relax-rate 0.4 \
	  /opt/data/network.net \
	  /opt/data/ | tee $(counties)/infomap/network.log

# Do some minimal processing on the `.tree` file from InfoMap to prepare it
# for downstream use in the Observable notebook
$(counties)/observable/network-processed.csv: $(counties)/infomap/network_states.tree
	@mkdir -p $(counties)/observable
	./scripts/stripForObservable.sh < $< > $@

##############################################################################
## Hexagonal-geography-only recipes                                         ##
##############################################################################
hex_geo_deps := $(county_polygons) $(cbg_polygons) $(cbg_popsize)

$(hexes)/hexid-fips-map.csv $(hexes)/hexes.shp $(hexes)/hexes.geojson $(hexes)/hexid-neighbors.csv &: scripts/hexbin.R $(hex_heo_deps)
	@mkdir -p $(hexes)
	Rscript scripts/hexbin.R \
	  --save-mapping      $(hexes)/hexid-fips-map.csv \
	  --save-neighbors    $(hexes)/hexid-neighbors.csv \
	  --save-shp          $(hexes)/hexes.shp \
	  --save-geojson      $(hexes)/hexes.geojson \
	  --county-polygons   $(county_polygons) \
	  --cbg-polygons      $(cbg_polygons) \
	  --cbg-popsize       $(cbg_popsize) \
	  --hexsize           25 \
	  --lower-48

#######################################
## Pre-Infomap Geo/TopoJSON ops      ##
#######################################

# Convert the R-generated GeoJSON of hexes to an Albers-projected GeoJSON.
# This file DOES account for hexes that are excluded from the dataset during
# interpolation - all hexes in this GeoJSON actually participate in the
# mixed-model or InfoMap steps.
$(hexes)/hexes-albers.geojson: $(hexes)/hexes-participating.geojson
	geoproject "d3.geoAlbersUsa().scale(1300).translate([487.5, 305])" \
		< $(hexes)/hexes-participating.geojson > $@

# Convert the Albers-projected GeoJSON to TopoJSON by creating hexes layer,
# then simplifying to 1px (w.r.t. the projection), then quantizing.
$(hexes)/hexes-albers.topojson: $(hexes)/hexes-albers.geojson
	geo2topo hexes=$(hexes)/hexes-albers.geojson | \
		toposimplify -p 1 - | topoquantize 1e5 - > $@

# Generate a polygon composed from hexes showing their coverage.
#
# Note: these hexes are the set of hexes that existed pre-interpolation, so
#   there is missingness generated downstream during interpolation that is NOT
#   represented here.
$(hexes)/hex-coverage-albers.topojson: $(hexes)/hexes-albers.topojson
	topomerge hexes=hexes < $< > $@

#######################################
## Interpolation                     ##
#######################################

interpolation_products := $(hexes)/hexid-observations.csv \
			  $(hexes)/hexes-participating.geojson

interpolation_inputs := $(hexes)/hexid-fips-map.csv \
			$(dp)/covidestim-observations.csv \
			$(hexes)/hexes.geojson

$(interpolation_products)&: scripts/hex-interpolate.R $(interpolation_inputs)
	Rscript scripts/hex-interpolate.R \
	  --save-observations $(hexes)/hexid-observations.csv \
	  --save-excluded     $(hexes)/hexid-excluded.csv \
	  --save-geojson      $(hexes)/hexes-participating.geojson \
	  --hex-mapping       $(hexes)/hexid-fips-map.csv \
	  --observations      $(dp)/covidestim-observations.csv \
	  --exclude-threshold 50 \
	  --geojson           $(hexes)/hexes.geojson

#######################################
## Mixed-effects                     ##
#######################################

# Fit the mixed-effects model to the data, and write the estimates of the 
# \alpha coefficients to disk
$(hexes)/mixedmodel/alphas.csv: scripts/regression.jl \
	                        $(hexes)/hexid-neighbors.csv \
				$(hexes)/hexid-observations.csv
	@mkdir -p $(hexes)/mixedmodel
	julia scripts/regression.jl \
	  -o              $@ \
	  --key           hexid \
	  --neighbors     $(hexes)/hexid-neighbors.csv \
	  --observations  $(hexes)/hexid-observations.csv \
	  --regardless-of-rt \
	  --predict       infectionsPC \
	  --predict-using infectionsPC | \
	  tee $(hexes)/mixedmodel/julia.log

# Reformat the Julia output to make it more machine-readable
$(hexes)/mixedmodel/alphas-reformat.csv: scripts/transformResults.R \
	                                 $(hexes)/mixedmodel/alphas.csv
	Rscript scripts/transformResults.R \
          -o $@ \
	  --alphas $(hexes)/mixedmodel/alphas.csv

#######################################
## Vector-field                      ##
#######################################

$(hexes)/vectors/vectors.geojson: $(hexes)/hexid-neighbors.csv \
				  $(hexes)/mixedmodel/alphas-reformat.csv \
				  $(hexes)/hexes.geojson \
				  $(hexes)/hexid-observations.csv \
				  scripts/vectorfield.R
	@mkdir -p $(hexes)/vectors
	@rm -f $@
	Rscript scripts/vectorfield.R \
	  -o             $@ \
	  --neighbors    $(hexes)/hexid-neighbors.csv \
	  --alphas       $(hexes)/mixedmodel/alphas-reformat.csv \
	  --geos         $(hexes)/hexes.geojson \
	  --observations $(hexes)/hexid-observations.csv

#######################################
## Infomap                           ##
#######################################

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
	docker run --rm \
	  -v $(shell pwd)/$(hexes)/infomap/:/opt/data/ \
	  mapequation/infomap:latest \
	  --multilayer-relax-rate 0.4 \
	  /opt/data/network.net \
	  /opt/data/ | tee $(hexes)/infomap/network.log

# Do some minimal processing on the `.tree` file from InfoMap to prepare it
# for downstream use in the Observable notebook
$(hexes)/observable/network-processed.csv: $(hexes)/infomap/network_states.tree
	@mkdir -p $(hexes)/observable
	./scripts/stripForObservableHexid.sh < $< > $@

########################################
## Observable-focused Geo/TopoJSON ops #
########################################

# Take the network, and write it to a JSON that details the module assignments
# for each month as an array of objects inside an object that represents a hex.
$(hexes)/observable/network-processed.json: $(hexes)/observable/network-processed.csv
	Rscript -e "d <- readr::read_csv('$<', col_types = 'ccn'); d <- dplyr::group_by(d, hexid); d <- tidyr::nest(d, assignments = c(module, month)); jsonlite::write_json(d, '$@')"

# Convert to newline-delimited JSON to enable downstream operations
$(hexes)/observable/network-processed.ndjson: $(hexes)/observable/network-processed.json
	ndjson-split < $< > $@

# For each GeoJSON hex, join in the assignments for each month as a .properties
# key on each GeoJSON. Assemble this into a GeoJSON FeatureCollection.
$(hexes)/observable/network-joined.geojson: $(hexes)/observable/network-processed.ndjson $(hexes)/hexes-albers.geojson
	ndjson-split 'd.features' < $(hexes)/hexes-albers.geojson | \
	ndjson-join 'd.properties.hexid' 'd.hexid' - $(hexes)/observable/network-processed.ndjson | \
	ndjson-map 'd[0].properties = {assignments: d[1].assignments, hexid: d[1].hexid}, d[0]' | \
	ndjson-reduce | \
	ndjson-map '{type: "FeatureCollection", features: d}' > $@

# Convert the GeoJSON to a TopoJSON
$(hexes)/observable/network-joined.topojson: $(hexes)/observable/network-joined.geojson
	geo2topo hexes=$< | toposimplify -p 1 - | topoquantize 1e5 - > $@

# Gzip of the TopoJSON
$(hexes)/observable/network-joined.topojson.gz: $(hexes)/observable/network-joined.topojson
	gzip -c < $< > $@
