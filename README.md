# Waves

![Diagram of project dataflow](/img/diagram.png)

This is a repository of code related to the ongoing Waves project.

## Dependencies:

- A recent Julia install. Download it [here][julia] or install it through your
  package manager.

- A few Julia packages that don't ship with the standard distribution. You
  can install all of these from the repository root by running:  
  ```
  julia scripts/deps.jl
  ```

- [Docker][docker]. You may need root privileges to use Docker depending on
  your system.

- The R packages `sf` `tidyverse` `docopt` `cli` `glue` `geojsonio`. `sf` in
  particular is a heavy package and it is common for people to experience
  issues installing, see the [sf docs on
  installing](https://r-spatial.github.io/sf/#installing) first.  
  ```r
  install.packages(c('sf', 'tidyverse', 'docopt', 'cli', 'glue', 'geojsonio'))
  ```

- For exporting visualization-related files:
  ```
  npm install -g d3-geo-projection topojson ndjson-cli
  ```

- [Git LFS](https://git-lfs.com). Run `git lfs fetch && git lfs checkout` from
  the repository root.

## `makefile` / Getting started

The complete pipeline from fetching input data to producing results compatible
with the visualization code in the Observable notebooks
([vector field][observable0], [interactions][observable1], [modules][observable2]) is specified in the
`makefile`. This pipeline:

1. Fetches Covidestim input and results from the public Covidestim API. This
   produces a `.csv` with all the observations that will be used in the
   mixed-model.

2. Interpolates the Covidestim data to a hexagonal tiling of the US in a
   sensible manner.

2. Fits a mixed model to this data in Julia.

3. Transforms the interaction coefficients from the mixed-model output into a
   vector field that can be visualized in Observable or another GIS-like
   environment.

To run the entire pipeline, after satisfying dependencies:

```bash
make data-products/geo-hexes/vectors/vectors.geojson
```

This will produce `network-processed.csv` and `alphas_reformat.csv`. The
schema for these two files are described below.

## Output data schema

### `covidestim_observations.csv`

| Variable       | Type                 | Description                                                         |
|----------------|----------------------|---------------------------------------------------------------------|
| `fips`         | string               | FIPS code                                                           |
| `date`         | string, `YYYY-MM-DD` | Date                                                                |
| `cases`        | uint                 | Number of reported cases on date `date`                             |
| `Rt`           | float                | Covidestim estimated $R_t$ on date `date`                           |
| `infections`   | float                | Covidestim estimated infections occurring on date `date`            |
| `infectionsPC` | float                | Covidestim estimated infections per capita occurring on date `date` |

### `hexid-neighbors.csv`

| Variable | Type   | Description                 |
|----------|--------|-----------------------------|
| `i`      | string | Hexid of one neighbor       |
| `j`      | string | Hexid of the other neighbor |

Note: for all $i,j$, $i \neq j$ there exists a $j,i$.

`i`, `j` are always unsigned integers but it is recommended to parse them as
strings.

### `hexid-fips-map.csv`

| Variable               | Type   | Description                                                                    |
|------------------------|--------|--------------------------------------------------------------------------------|
| `hexid`                | string | Hexid |
| `fips`                 | string | FIPS code |
| `proportion_from_fips` | float  | **Proportion of `hexid`'s population that comes from FIPS code `fips`**. Needed for interpolating rate-expressed quantities. |
| `proportion_of_fips`   | float  | **Proportion of `fips`'s population that lies within hexagon `hexid`**. Needed for interpolating incidence observations. |

### `hexid-observations.csv`

| Variable       | Type                        | Description                                                                          |
|----------------|-----------------------------|--------------------------------------------------------------------------------------|
| `hexid`        | string                      | Hexid                                                                                |
| `date`         | string, `YYYY-MM-DD` format | Date                                                                                 |
| `cases`        | **float**                   | Number of reported cases on date `date`. No longer an integer, due to interpolation. |
| `infections`   | float                       | Covidestim estimated infections occurring on date `date`.                            |
| `infectionsPC` | float                       | Covidestim estimated infections per capita occurring on date `date`.                 |
| `Rt`           | float                       | Covidestim estimated $R_t$ for date `date`.                                          |

### `alphas_reformat.csv`

| Variable           | Type   | Description                                                   |
|--------------------|--------|---------------------------------------------------------------|
| `i`                | string | Hexid                                                         |
| `j`                | string | Hexid                                                         |
| `month`            | string | `YYYY-MM-01` format                                           |
| `alpha`            | float  | Value of the mixed-model interaction term for `i`-`j`-`month` |
| `value`            | float  | Vestigial, always equal to `alpha`.                           |
| `alpha_normalized` | float  | Normalized alpha, calculated as $\frac{\alpha_{i,j,m}}{\mathrm{mean}(\|\alpha_{i,j,m}\|)} \forall (i,j,m), j = $`j` |


[julia]: https://julialang.org/downloads/
[docker]: https://docker.com

[observable0]: https://observablehq.com/@covidestim/normalized-alphas-infectionspc
[observable1]: https://observablehq.com/d/d933941d0d76c7d4
[observable2]: https://observablehq.com/@marcusrussi/infomap-v2
