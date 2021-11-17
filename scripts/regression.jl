using CSV;
using DataFrames;
using Dates;
using MixedModels;
using StatsModels;
using Pipe;
using CategoricalArrays;

println("Reading neighbors.csv")
neighbors = CSV.read(
  "neighbors.csv", DataFrame;
  # i = TO, j = FROM
  types = Dict(:i => String, :j => String)
)

println("Reading results.csv")
results = CSV.read(
  "results.csv", DataFrame;
  types=Dict(
    :fips         => String,
    :date         => Date,
    :cases        => Float64,
    :Rt           => Float64,
    :infections   => Float64
  )
)

println("Innerjoin #1")
joined = innerjoin(
  neighbors, results; on = :i => :fips, renamecols = "" => "_i"
 )

rename!(joined, :date_i => :date)

println("Innerjoin #2")
joined = innerjoin(
  joined, results; on = [:date, :j => :fips], renamecols = "" => "_j"
)

sort!(joined, :date)

joined = groupby(joined, [:i, :j])
transform!(joined, :cases_i => (v -> lag(v, 1)) => :cases_i_1)

function lagsForVariable(df, variable, lags)
  transformers = map(
    x -> variable => (v -> lag(v, x)) => Symbol(string(variable)*"_"*string(x)),
    lags
  )

  return transform!(df, transformers)
end

println("Assembling all lags for each observation")
lagsForVariable(joined, :cases_j, 1:20)

# ungroup:
joined = select(joined, All(); ungroup=true)

filter!([:Rt_i] => rt -> rt >= 1, joined)

println("Performing categorical encoding of interaction term")
transform!(joined, [:i, :j, :date] =>
  ((i, j, date) -> 
    categorical(string.(i, "-", j, "-", year.(date), "-", month.(date))))
  => :interactionTerm
)

println("Fitting mixed model")
model = fit(MixedModel, @formula(
  infections_i ~
    0 + 

    # Interaction term (random effect)
    (1 | interactionTerm) +

    # Lags (fixed effects)
    cases_j_1 +
    cases_j_2 +
    cases_j_3 +
    cases_j_4 +
    cases_j_5 +
    cases_j_6 +
    cases_j_7 +
    cases_j_8 +
    cases_j_9 +
    cases_j_10 +
    cases_j_11 +
    cases_j_12 +
    cases_j_13 +
    cases_j_14 +
    cases_j_15 +
    cases_j_16 +
    cases_j_17 +
    cases_j_18 +
    cases_j_19 +
    cases_j_20 +

    # Autocorr (fixed effect)
    cases_i_1

    # (EMPTY) covariates

), joined, contrasts=Dict(:interactionTerm => Grouping()))
println("Fit complete")

effects = DataFrame(only(raneftables(model)))

println(model)

println("Writing `alphas.csv`")
CSV.write("alphas.csv", effects)
