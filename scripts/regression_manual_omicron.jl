using CSV;
using DataFrames;
using Dates;
using MixedModels;
using StatsModels;
using Pipe;
using CategoricalArrays;
using DocOpt;

## Geosymbol, to which unit the model will be fit
geosym = Symbol("hexid")

## Reading neighbors data
neighbors = CSV.read(
  "data-products/geo-hexes/hexid-neighbors.csv", DataFrame;
  # i = TO, j = FROM
  types = Dict(:i => String, :j => String)
)

## Reading hexid observations
results = CSV.read(
  "data-products/geo-hexes/hexid-observations_omicron.csv", 
  DataFrame;
  types=Dict(
    geosym        => String,
    :date         => Date,
    :cases        => Float64,
    :Rt           => Float64,
    :infections   => Float64,
    :infectionsPC => Float64
  )
)

## Inner Join 1, between the hexid grid and the observations
joined = innerjoin(
  neighbors, results; on = :i => geosym, renamecols = "" => "_i"
 )

rename!(joined, :date_i => :date)

## Inner join 2, between the joined dataframe and the results again, 
## to have a complete set of data structure to the observations and results
joined = innerjoin(
  joined, results; on = [:date, :j => geosym], renamecols = "" => "_j"
)

sort!(joined, :date)

## Predciting variable, can be 'infectionsPC', 'Rt', etc.
outcome_i_symbol = Symbol("infectionsPC" * "_i")
print(outcome_i_symbol)
## Covariate variable used to predict, can be 'infectionsPC', 'Rt', etc.
outcome_j_symbol = Symbol("infectionsPC" * "_j")
print(outcome_j_symbol)

## groupby the joined dataframe
joined = groupby(joined, [:i, :j])
## Transform the joined dataframe to create lags columns
#transform!(joined, outcome_i_symbol => (v -> lag(v, 1)) => :outcome_i_1)

## a Function to create the lag variable on the lag columns, outcome_j_
function lagsForVariable_j(df, variable, lags)
  transformers = map(
    x -> variable => (v -> lag(v, x)) => Symbol("outcome_j_" * string(x)),
    lags
  )

  return transform!(df, transformers)
end

## a Function to create the lag variable on the lag columns, outcome_i_
function lagsForVariable_i(df, variable, lags)
  transformers = map(
    x -> variable => (v -> lag(v, x)) => Symbol("outcome_i_" * string(x)),
    lags
  )

  return transform!(df, transformers)
end

# Assembling all lags for each observation
lags = [1, 2, 3] # As we are now with a weekly dataset, each lag it is a week lag, not a daily one
lagsForVariable_j(joined, outcome_j_symbol, lags)
lagsForVariable_i(joined, outcome_i_symbol, lags)

# ungroup and creating the predicting column
joined = select(joined, All(); ungroup=true)
rename!(joined, outcome_i_symbol => :outcome_i)

## Filtering out with R_t < 1, if the arg is set to
# if !args["--regardless-of-rt"]
  #filter!([:Rt_i] => rt -> rt >= 1, joined)
# end

# Performing categorical encoding of interaction term
transform!(joined, [:i, :j, :date] =>
  ((i, j, date) -> 
    categorical(string.(i, "-", j, "-", year.(date), "-", month.(date), "-", day.(date))))
  => :interactionTerm
)
# show(names(joined))
# select(joined, :interactionTerm)
# joined

autocorr = true
## Choosing between a autocorr term model or a without autocorr model term
 if autocorr #!args["--no-autocorr"]
     formula = @formula(
     outcome_i ~
       0 + 
  
       # Interaction term (random effect)
       (1 | interactionTerm) +
  
       # Lags (fixed effects)
       outcome_j_1 +
       outcome_j_2 +
       outcome_j_3 +
  
       # Autocorr (fixed effect)
       #outcome_i_1 +
       outcome_i_1 +
       outcome_i_2 +
       outcome_i_3
  
       # (EMPTY) covariates
  
   )
  else
    formula = @formula(
    outcome_i ~
      0 + 
  
      # Interaction term (random effect)
      (1 | interactionTerm) +
  
      # Lags (fixed effects)
      outcome_j_1 +
      outcome_j_2 +
      outcome_j_3 
  )
  end
##

print(formula)

# Fitting mixed model
model = fit(MixedModel, formula, joined, contrasts=Dict(:interactionTerm => Grouping()))
# println("Fit complete")

println(model)

## Separate the condiotional means of the random effects form the model
effects = DataFrame(only(raneftables(model)))

print(effects)

# Writing alphas .csv
if autocorr
  CSV.write("data-products/geo-hexes/mixedmodel/alphas_weekly_regardless_rt_omicronera.csv", effects)
else
  CSV.write("data-products/geo-hexes/mixedmodel/alphas_weekly_regardless_rt_omicronera_no_autocorr.csv", effects)
end

# CSV.write("data-products/geo-hexes/mixedmodel/joined-weekly.csv", joined)
