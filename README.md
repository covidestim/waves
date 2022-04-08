# Waves

![Diagram of project dataflow](/img/diagram.jpg)

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

- The R packages `tidyverse`, `glue`, `cli`, `lubridate`.

## `makefile` / Getting started

The complete pipeline from fetching input data to producing results compatible
with the visualization code in the Observable notebooks
([interactions][observable1], [modules][observable2]) is specified in the
`makefile`. This pipeline:

1. Fetches Covidestim input and results from the public Covidestim API. This
   produces a `.csv` with all the observations that will be used in the
   mixed-model.

2. Fits a mixed model to this data in Julia.

3. Transforms the interaction coefficients from the mixed-model output so that
   they can be fed to Infomap as edge weights between adjacent geographies.

4. Runs Infomap in [multilayer
   mode](https://www.mapequation.org/infomap/#InputMultilayerIntra) (one
   layer/month) in order to identify modules of geographies

5. Reformats this Infomap output a little bit so that it can be used more
   easily within the Observable environment.

To run the entire pipeline, after satisfying dependencies:

```bash
make network-processed.csv alphas_reformat.csv
```

This will produce `network-processed.csv` and `alphas_reformat.csv`. The
schema for these two files are described below.

## Output data schema

### `network-processed.csv`

| Variable | Type   | Description |
|----------|--------|-------------|
| `module` | string | The module assigned to that geographic unit for that month. Of the form `a:b:c:d`, where `a` is the highest-level module and `d` is the lowest-level module. `a`,`b`,`c`,`d` are integers. A geographic unit may sometimes not be assigned into the deepest level of the module hierarchy for a particular month, whcih means that some units are simply assigned a module of the form `a`, `a:b`, or `a:b:c`. |
| `fips`   | string | Geographic unit designator, currently the FIPS code of a county, but could be modified to instead represent a hexbin ID |
| `month`  | integer | Month. Currently month `1` is the first month in the dataset. **Does not correspond to month of the year.**|

### `alphas_reformat.csv`

| Variable | Type                 | Description                                                                    |
|----------|----------------------|--------------------------------------------------------------------------------|
| `i`      | string               | Geographic unit, currently FIPS code                                           |
| `j`      | string               | Geographic unit, currently FIPS code                                           |
| `month`  | string, `YYYY-MM-DD` | Month for which we calculated this interaction term                            |
| `value`  | string               | Value of the interaction term for `i,j,month` as determined by the mixed-model |

[julia]: https://julialang.org/downloads/
[docker]: https://docker.com

[observable1]: https://observablehq.com/d/d933941d0d76c7d4
[observable2]: https://observablehq.com/@marcusrussi/infomap-v2
