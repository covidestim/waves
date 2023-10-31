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
  "data-products/geo-hexes/hexid-observations.csv", 
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
## Covariate variable used to predict, can be 'infectionsPC', 'Rt', etc.
outcome_j_symbol = Symbol("infectionsPC" * "_j")

## groupby the joined dataframe
joined = groupby(joined, [:i, :j])
## Transform the joined dataframe to create lags columns
transform!(joined, outcome_i_symbol => (v -> lag(v, 1)) => :outcome_i_1)

## a Function to create the lag variable on the lag columns
function lagsForVariable(df, variable, lags)
  transformers = map(
    x -> variable => (v -> lag(v, x)) => Symbol("outcome_j_" * string(x)),
    lags
  )

  return transform!(df, transformers)
end

# Assembling all lags for each observation
lags = [1, 7, 14, 21]
lagsForVariable(joined, outcome_j_symbol, lags)

# ungroup:
joined = select(joined, All(); ungroup=true)
rename!(joined, outcome_i_symbol => :outcome_i)

filter([:i], => ==1, joined)

## Filtering out with R_t < 1, if the arg is set to
# if !args["--regardless-of-rt"]
  #filter!([:Rt_i] => rt -> rt >= 1, joined)
# end

# Performing categorical encoding of interaction term
transform!(joined, [:i, :j, :date] =>
  ((i, j, date) -> 
    categorical(string.(i, "-", j, "-", year.(date), "-", month.(date), "-", week.(date))))
  => :interactionTerm
)

# select(joined, :interactionTerm)
# joined

autocorr = false
## Choosing between a autocorr term model or a without autocorr model term
if autocorr #!args["--no-autocorr"]
    formula = @formula(
    outcome_i ~
      0 + 
  
      # Interaction term (random effect)
      (1 | interactionTerm) +
  
      # Lags (fixed effects)
      outcome_j_1 +
      outcome_j_7 +
      outcome_j_14 +
      outcome_j_21 #+
  
      # Autocorr (fixed effect)
      #outcome_i_1
  
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
      outcome_j_7 +
      outcome_j_14 +
      outcome_j_21 +
  
      # Autocorr (fixed effect)
      outcome_i_1
  
      # (EMPTY) covariates
  
  )
  end
##

print(formula)

# Fitting mixed model
model = fit(MixedModel, formula, joined, contrasts=Dict(:interactionTerm => Grouping()))
# println("Fit complete")

## Separate the condiotional means of the random effects form the model
effects = DataFrame(only(raneftables(model)))

# println(model)

# Writing alphas .csv
CSV.write("data-products/geo-hexes/mixedmodel/alphas_weekly_regardless_rt.csv", effects)

CSV.write("data-products/geo-hexes/mixedmodel/joined-weekly.csv", joined)
