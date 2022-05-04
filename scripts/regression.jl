using CSV;
using DataFrames;
using Dates;
using MixedModels;
using StatsModels;
using Pipe;
using CategoricalArrays;
using DocOpt;

doc = """Waves project: mixed-model

Usage:
  regression.jl -o <path> --key <colname> --neighbors <path> --observations <path> [--predict-using <outcome>]
  regression.jl (-h | --help)
  regression.jl --version

Options:
  -o <path>                  Where to save the interaction terms and their intercepts
  --key <colname>            Key to group on ("fips" or "hexid")
  --neighbors <path>         Path to a CSV listing all neighbors [i, j]
  --observations <path>      Path to observations for each FIPS or hexid
  --predict-using <outcome>  Which outcome to predict infections from [default: cases]
  -h --help                  Show this screen.
  --version                  Show version.

"""

args = docopt(doc, version=v"0.1")

geosym = Symbol(args["--key"])
println("Geographic grouping unit is ", args["--key"])

println("Reading ", args["--neighbors"])
neighbors = CSV.read(
  args["--neighbors"], DataFrame;
  # i = TO, j = FROM
  types = Dict(:i => String, :j => String)
)

println("Reading ", args["--observations"])
results = CSV.read(
  args["--observations"], DataFrame;
  types=Dict(
    geosym      => String,
    :date       => Date,
    :cases      => Float64,
    :Rt         => Float64,
    :infections => Float64
  )
)

println("Innerjoin #1")
joined = innerjoin(
  neighbors, results; on = :i => geosym, renamecols = "" => "_i"
 )

rename!(joined, :date_i => :date)

println("Innerjoin #2")
joined = innerjoin(
  joined, results; on = [:date, :j => geosym], renamecols = "" => "_j"
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

println("Writing alphas: ", args["-o"])
CSV.write(args["-o"], effects)
