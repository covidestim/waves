using CSV;
using DataFrames;
using Dates;
using GLM;
using Pipe;
using CategoricalArrays;

neighbors = CSV.read(
  "neighbors.csv", DataFrame;
  # i = TO, j = FROM
  types=Dict(:i => String, :j => String)
)

results = CSV.read(
  "results.csv", DataFrame;
  types=Dict(
    :fips         => String,
    :date         => Date,
    :Rt           => Float64,
    :infectionsPC => Float64
  )
)

joined = innerjoin(
  neighbors, results; on = :i => :fips, renamecols = "" => "_i"
 )

rename!(joined, :date_i => :date)

joined = innerjoin(
  joined, results; on = [:date, :j => :fips], renamecols = "" => "_j"
)

sort!(joined, :date)

joined = groupby(joined, [:i, :j])
transform!(joined, :infectionsPC_i => (v -> lag(v, 1)) => :infectionsPC_i_1)

function lagsForVariable(df, variable, lags)
  transformers = map(
    x -> variable => (v -> lag(v, x)) => Symbol(string(variable)*"_"*string(x)),
    lags
  )

  return transform!(df, transformers)
end

lagsForVariable(joined, :infectionsPC_j, 1:20)

# ungroup:
joined = select(joined, All(); ungroup=true)

filter!([:Rt_i] => rt -> rt >= 1, joined)

transform!(joined, [:i, :j, :date] =>
  ((i, j, date) -> 
    categorical(string.(i, "-", j, "-", year.(date), "-", month.(date))))
  => :interactionTerm
)

model = lm(@formula(
  infectionsPC_i ~
    # (EMPTY) Interaction term
    interactionTerm +
    # Lags
    infectionsPC_j_1 +
    infectionsPC_j_2 +
    infectionsPC_j_3 +
    infectionsPC_j_4 +
    infectionsPC_j_5 +
    infectionsPC_j_6 +
    infectionsPC_j_7 +
    infectionsPC_j_8 +
    infectionsPC_j_9 +
    infectionsPC_j_10 +
    infectionsPC_j_11 +
    infectionsPC_j_12 +
    infectionsPC_j_13 +
    infectionsPC_j_14 +
    infectionsPC_j_15 +
    infectionsPC_j_16 +
    infectionsPC_j_17 +
    infectionsPC_j_18 +
    infectionsPC_j_19 +
    infectionsPC_j_20 +

    # Autocorr
    infectionsPC_i_1

    # (EMPTY) covariates

), joined)
