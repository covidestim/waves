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
  regression.jl -o <path> --key <colname> --neighbors <path> --observations <path> [--regardless-of-rt] [--no-autocorr] [--predict <outcome>] [--predict-using <outcome>]
  regression.jl (-h | --help)
  regression.jl --version

Options:
  -o <path>                  Where to save the interaction terms and their intercepts
  --key <colname>            Key to group on ("fips" or "hexid")
  --neighbors <path>         Path to a CSV listing all neighbors [i, j]
  --observations <path>      Path to observations for each FIPS or hexid
  --regardless-of-rt         Fit all observations, even if R_t < 1
  --no-autocorr              Take out the autcorrelation term
  --predict <outcome>        Which outcome to predict [default: cases]
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
    geosym        => String,
    :date         => Date,
    :cases        => Float64,
    :Rt           => Float64,
    :infections   => Float64,
    :infectionsPC => Float64
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

outcome_i_symbol = Symbol(args["--predict"] * "_i")
outcome_j_symbol = Symbol(args["--predict-using"] * "_j")

joined = groupby(joined, [:i, :j])
transform!(joined, outcome_i_symbol => (v -> lag(v, 1)) => :outcome_i_1)

function lagsForVariable(df, variable, lags)
  transformers = map(
    x -> variable => (v -> lag(v, x)) => Symbol("outcome_j_" * string(x)),
    lags
  )

  return transform!(df, transformers)
end

println("Assembling all lags for each observation")
lags = [1, 7, 14, 21]
lagsForVariable(joined, outcome_j_symbol, lags)

# ungroup:
joined = select(joined, All(); ungroup=true)
rename!(joined, outcome_i_symbol => :outcome_i)

if !args["--regardless-of-rt"]
  filter!([:Rt_i] => rt -> rt >= 1, joined)
end

println("Performing categorical encoding of interaction term")
transform!(joined, [:i, :j, :date] =>
  ((i, j, date) -> 
    categorical(string.(i, "-", j, "-", year.(date), "-", month.(date))))
  => :interactionTerm
)


## Choosing between a autocorr term model or a without autocorr model term
if args["--no-autocorr"]
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

println("The model formula is:")
print(formula)

println("Fitting mixed model")
model = fit(MixedModel, formula, joined, contrasts=Dict(:interactionTerm => Grouping()))
println("Fit complete")

effects = DataFrame(only(raneftables(model)))

println(model)

println("Writing alphas: ", args["-o"])
CSV.write(args["-o"], effects)
