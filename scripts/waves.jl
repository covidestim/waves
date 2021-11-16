using HTTP;
using DataFrames;
using Dates;
using CSV;
using Pipe;
using Tables;
using Statistics;
using StatsBase;
using Combinatorics;
using CSV;

function getLatestResults()
  resp = HTTP.get("https://covidestim.s3.amazonaws.com/latest/estimates.csv")

  return CSV.File(resp.body; types=Dict(:fips => String)) |> DataFrame
end

# Tuple of {start, end, vector of Rts}
waveentry = @NamedTuple{
  start::Date,
  finish::Date,
  rt::Vector{Float64},
  inf::Vector{Float64}
 }

function idWaves(df)
  waves        = Vector{waveentry}()
  wavestart    = Date(1970, 1, 1)
  wavestartidx = 0
  inwave       = false

  for (index, row) in enumerate(Tables.namedtupleiterator(df))
    if (row[:Rt] > 1 && !inwave)
      inwave       = true
      wavestart    = row[:date]
      wavestartidx = index
    end
    if (row[:Rt] < 1 && inwave)
      push!(waves, (
        start  = wavestart,
        finish = row[:date],
        rt     = df[wavestartidx:index, :Rt],
        inf    = df[wavestartidx:index, :infections]
      ))
      inwave = false
    end
  end

  return waves
end

function idAllWaves(df)
  wavesForFIPS = Dict{String, Vector{waveentry}}()

  for (key, subdf) in pairs(groupby(df, :fips))
    wavesForFIPS[key[:fips]] = idWaves(subdf)
  end

  return wavesForFIPS
end

function allCountyPairs(dict)
  return @pipe keys(dict) |> collect |> combinations(_, 2)
end

function indicesToCompareForFIPSPair(a, b, ndays)
  indices = Vector{Tuple{Int64, Int64}}()
  dt = Day(ndays)

  # i and j are Date's
  for (idxA, i) in enumerate(a)
    for (idxB, j) in enumerate(b)
      if (abs(i.start-j.start) < dt)
        push!(indices, (idxA, idxB))
      end
    end
  end

  return indices
end

function comparePairs(pairs, wavesDict)
  comparisons = Vector{@NamedTuple{
      fipsA::String,
      fipsB::String,
      waveIdA::Int64,
      waveIdB::Int64,
      wavestartA::Date,
      wavestartB::Date,
      offset::Int64,
      offsetDays::Day,
      energy::Float64
  }}()

  for pair in pairs
    a = pair[1] # This is a FIPS code
    b = pair[2]
    wavesForA = wavesDict[a]
    wavesForB = wavesDict[b]

    for waveIdxs in indicesToCompareForFIPSPair(wavesForA, wavesForB, 20)
      waveA = wavesForA[waveIdxs[1]]
      waveB = wavesForB[waveIdxs[2]]

      minLen = min(length(waveA.rts), length(waveB.rts))

      result = crosscor(
        waveA.rts[1:minLen],
        waveB.rts[1:minLen],
        -(minLen - 1):(minLen - 1)
       )

      peakIdx = argmax(result) - minLen

      push!(
        comparisons, (
          fipsA=a, fipsB=b,
          waveIdA=waveIdxs[1], waveIdB=waveIdxs[2],
          wavestartA=waveA.start, wavestartB=waveB.start,
          offset=peakIdx,
          offsetDays=(waveB.start - waveA.start) + Day(peakIdx),
          energy=result[argmax(result)]
        )
      )
    end
  end

  return comparisons
end

# result = getLatestResults()
# wavesdict = idAllWaves(result)
# countyPairs = allCountyPairs(wavesdict)
# comparisons = comparePairs(countyPairs, wavesdict)
# comparisonsdf = DataFrame(comparisons)
# filter([:fipsA, :fipsB] => (x, y) -> occursin(r"^(09001)|(09009)", x) &&
#   occursin(r"^(09009)|(09001)", y), comparisonsdf) 

function restrictToNeighbors(df)
  neighborSet = Set{Tuple{String, String}}()
  for row in CSV.File("adjacency_list.csv"; types=Dict(:fips=>String, :neighbor=>String))
    union!(neighborSet, Set([(row.fips, row.neighbor)]))
  end

  return filter(
    [:fipsA, :fipsB] => (a, b) -> Set([(a, b)]) ⊆ neighborSet, df
  )
end

# CSV.write("neighborwaves.csv", final; transform=(col,val)->col==8 ? Dates.value(val) : val)
