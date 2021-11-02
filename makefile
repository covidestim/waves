results.csv:
	curl -H "Accept: text/csv" \
	  'https://api.covidestim.org/latest_results?select=fips,date,Rt,infectionsPC' \
	  > $@

model.hdf5: results.csv regression.jl
	julia regression.jl results.csv
