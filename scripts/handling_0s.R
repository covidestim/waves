## Reading the new dataset
hexObsPreOmicron <- vroom::vroom("data-products/geo-hexes/hexid-observations_preomicronNEW 2.csv")

### Save to a CSV
hexObsPreOmicron <- hexObsPreOmicron |> 
  mutate(infections2 = case_when(infections >= population ~ population,
                                population == 0 ~ 1,
                                TRUE ~ infections)) |> 
  mutate(infectionsPC = (infections2/population)*1e5) |> 
  # mutate(infections = if_else(population == 0, 0, infections),
  #        infectionsPC = if_else(population == 0, 0, infectionsPC)) |> 
  dplyr::select(-geometry) |> 
  tidyr::drop_na()

write_csv(hexObsPreOmicron, 
          file = "data-products/geo-hexes/hexid-observations_preomicronNEW.csv")

## Reading the new dataset
hexObsOmicronEra <- vroom::vroom("data-products/geo-hexes/hexid-observations_omicronNEW.csv")

### Save to a CSV
hexObsOmicronEra <- hexObsOmicronEra |> 
  mutate(infections2 = case_when(infections >= population ~ population,
                                 population == 0 ~ 1,
                                 TRUE ~ infections)) |> 
  mutate(infectionsPC = (infections2/population)*1e5) |> 
  # mutate(infections = if_else(population == 0, 0, infections),
  #        infectionsPC = if_else(population == 0, 0, infectionsPC)) |> 
  dplyr::select(-geometry) |> 
  tidyr::drop_na()

write_csv(hexObsOmicronEra, 
          file = "data-products/geo-hexes/hexid-observations_omicroneraNEW.csv")
