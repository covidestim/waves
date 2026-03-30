gc()
rm(list = ls())
###############################################################################
##### Load in the required packages                                       #####
##############################################################################|
suppressPackageStartupMessages( library(sf) )
suppressPackageStartupMessages( library(geojsonio) )
library(magrittr, warn.conflicts = F)
library(ggplot2,  warn.conflicts = F)
library(dplyr,    warn.conflicts = F)
library(stringr,  warn.conflicts = F)
library(docopt,   warn.conflicts = F)
library(cli,      warn.conflicts = F)
library(readr,    warn.conflicts = F)
library(purrr,    warn.conflicts = F)
library(progress, warn.conflicts = F)
library(here)
library(collapse)

###############################################################################
##### Load in the required data files                                     #####
##############################################################################|

##### Hexgrid |
hexgrid <- st_read("Data/data-products/geo-hexes/hexgrid_1100_km.shp") |> 
  st_transform(crs = 5070)

##### Population allocated across the interesections of counties and hexes |
population <- st_read(here::here("Data/data-products/geo-hexes/pop/interhex_pop_updated.shp")) %>%
  select(hexid_x, fips_x, sum_pop, geometry) %>%
  rename(population = sum_pop,
         hexid = hexid_x,
         fips = fips_x)

# ggplot() + geom_sf(data=population, aes(fill = population, geometry = geometry))

##### Sum up the population in each hexagon based on the intersection population

populationHexTotal <- population %>%
  st_drop_geometry() %>%
  reframe(population = sum(population, na.rm = TRUE),
          fips=fips,
          .by="hexid")

### Save the population file 
sf::st_write(obj = populationHexTotal %>% 
                   left_join(st_read("Data/data-products/geo-hexes/hexgrid_1100_km.shp")),
             dsn = "Data/data-products/geo-hexes/pop/hexgrid_1100_km_intersection_meta_pop.shp",
             delete_dsn = T,
             delete_layer = T)

# write.csv(populationHexTotal, 
#           "Data/data-products/geo-hexes/pop/hexgrid_1100_km_intersection_meta_pop.csv")

# ggplot() + geom_sf(data=populationHexTotal %>%
#                      left_join(hexgrid), aes(fill = population, geometry = geometry)) +
#            geom_sf(data=populationHexTotal %>%
#               left_join(hexgrid) %>% filter(population ==0),
#               aes(geometry = geometry), fill="green")


countyGeom  <- st_read(here("Data/data-sources/countyPolygons.shp")) %>% 
  mutate(fips = as.character(GEOID)) %>%
  filter(fips %in% population$fips) %>% 
  select(fips, geometry)

# ggplot() + geom_sf(data=countyGeom)

populationCnty <- population %>% 
  st_drop_geometry() %>%
  reframe(countyPop = sum(population, na.rm = TRUE), 
          .by = "fips")

populationFull <- population %>%
  full_join(populationCnty) %>%
  mutate(frcCountyPop = population/countyPop)

##### covidestim observations allocated across counties |
##### this will be used to populate the hexgrid with infections |
observationsFips <- st_read("Data/data-products/observations_preomicron.shp") %>% 
  st_drop_geometry() %>%
  filter(fips %in% unique(population$fips))

# observationsFips %>% 
#   filter(! fips %in% population$fips)

##### Also set a test date for the plots throughout the workflow |
testDate <- as.Date("2021-09-04")

##### Explore with a plot |
# ggplot() +
#   geom_sf(observationsFips |>
#             filter(date == testDate),
#           mapping=aes(fill=infctPC)) +
#   geom_sf(observationsFips %>%
#             filter(infctns == 0,
#                    date == testDate),
#           mapping = aes(),
#           fill="green")+
#   theme_minimal() +
#   scale_fill_viridis_c()


###############################################################################
#######################  Allocate infections    ###############################
##############################################################################|
### 
### Helper function that returns NA if all are NA but sums non NA values 
### otherwise
sumna <- function(x) {
  if(all(is.na(x))) NA else sum(x, na.rm = TRUE)
}

hexInfections <- data.frame()

for (i in unique(observationsFips$date)){
  print(as.Date(i))
  
  #### Infections for each county on day of model run 
  #### We join the countyGeom to get all fips, even those that are 
  #### missing infections estimates. 
  observationsFullCnty <- observationsFips %>% 
    filter(date == as.Date(i)) %>% 
    select(fips, date, infctns) %>% 
    full_join(countyGeom %>% st_drop_geometry())
  
  # ggplot() + geom_sf(data = observationsFullCnty %>% join(countyGeom), 
  #                    aes(geometry = geometry, fill = infctns))
  
  #### Create a vector of fips codes that have no infections for 
  #### the model run date. We use these to omit population in the 
  #### infections per capita calculation. 
  observationsMissing <- observationsFullCnty %>% 
    filter(is.na(infctns)) %>% select(fips) %>% 
    unlist() %>% as.vector()
  
  # ggplot() + geom_sf(data = countyGeom %>% filter(fips %in% observationsMissing))
  
  ##### Sum up the population in each hexagon based on the intersection population. 
  ##### Edge case consideration: for hexes that are shared across counties with 
  ##### missing infection observations, we want to only include the population of 
  ##### the counties with known infection estimates. In this way, we will not 
  ##### dilute the infections per capita estimates for these hexes. This consideration
  ##### requires us to do this within in the loop, not before. 
  populationHex <- population %>% 
    st_drop_geometry() %>%
    filter(!fips %in% observationsMissing) %>%
    reframe(population = sum(population, na.rm = TRUE), 
            .by="hexid")
  
  # ggplot() +
  # geom_sf(data=hexgrid, color="black") +
  # geom_sf(data=populationHex %>% left_join(hexgrid),
  #         aes(geometry=geometry), fill="salmon")
  
  hexInfections0 <- populationFull %>% 
    st_drop_geometry() %>%
    full_join(observationsFullCnty) %>%
    mutate(intersectInfections = frcCountyPop * infctns) %>% 
    reframe(infections = sumna(intersectInfections), 
            .by = "hexid") %>% 
    mutate(date = as.Date(i)) %>%
    left_join(populationHex) %>%
    mutate(infectionsPC = infections / population, 
           infectionsPC = ifelse(is.nan(infectionsPC), 0, infectionsPC))%>% 
    left_join(populationHexTotal %>% 
              rename(populationTotal = population)) %>%
    left_join(hexgrid %>% select(hexid, geometry))
  
  #### CHECK WITH A PLOT ####
  # ggplot() + geom_sf(data=hexInfections0, aes(fill = infectionsPC,
  #                                                geometry= geometry)) +
  # scale_fill_viridis_c(option = "magma",
  #                        name = "Estimated Infections/day", direction = -1)
  
  # ggplot() + 
  #   geom_sf(data=hexInfectionsGeom |> 
  #     filter(infectionsPC > 0), 
  #   aes(geometry = geometry, 
  #     fill=infectionsPC*1e5))+
  #   geom_sf(data=hexInfectionsGeom |> 
  #     filter(is.na(infectionsPC)), 
  #   aes(geometry = geometry),
  # fill = "orange")+
  #   geom_sf(data = hexInfectionsGeom |> 
  #     filter(infections == 0, infectionsPC == 0), 
  #   aes(geometry=geometry), fill = "deeppink1")+
  #   geom_sf(data = hexInfectionsGeom |> 
  #     filter(infections == 0, is.na(infectionsPC)), 
  #   aes(geometry=geometry), fill = "green")
  # geom_sf(data = hexInfectionsGeom |> 
  #   filter(infectionsPC == 0), 
  # aes(geometry=geometry), fill = "green")
  
  
  #### CHECKS FOR PERFORMANCE #### 
  if (round(sum(hexInfections0$population, na.rm = T)/1e6, 4) != 
      round(sum(population %>% st_drop_geometry() %>%
                filter(!fips %in% observationsMissing) %>% 
                select(population), na.rm = T)/1e6,4)){
    print("Population does not match.")
    break()
  }
  
  if (round(sum(hexInfections0$infections, na.rm = T),
            4) != 
      round(observationsFips %>% 
            filter(date == as.Date(i)) %>%
            pull(var = infctns) %>% 
            sum(na.rm = T),
            4)){
    print("Infections do not match")
    break()
  }
  
  if(nrow(hexInfections0 %>% filter(!is.na(infections))) != nrow(populationHex)){
    print("Number of hexes is incorrect")
    break()
  }
  
  #### Create one large dataframe
  hexInfections <- rbind(hexInfections, hexInfections0)
}

# population %>% select(population) %>% st_drop_geometry() %>% sum()/1e6
# populationFull %>% select(population) %>% st_drop_geometry() %>% sum()/1e6
# hexInfections %>% filter(date ==alpha_peak) %>% select(population) %>% sum()/1e6
# hexInfections %>% filter(date ==delta_peak) %>% select(population) %>% sum()/1e6
# 
# dim(hexInfections %>% filter(date ==alpha_peak))
# dim(hexInfections %>% filter(date ==delta_peak))
# dim(populationHex)
hexInfections <- hexInfections %>% filter(! is.na(date))

write_csv(hexInfections %>% st_drop_geometry(), 
          file = here("Data/data-products/geo-hexes/hexid-observations_preomicron_intersection_hexgrid1100km.csv"))

##############################################################################|
#### Check that things are working as expected ###############################
##############################################################################|
### Population fractions sum to 1 |
### If you want to run this check, we have to keep frcCountyPop 
### in the allocate infections step. 

# hexInfections %>% filter(fips == "04015",
#                          date == testDate) %>%
#                   st_drop_geometry() %>%
#                   ungroup() %>%
#                   select(frcCountyPop) %>% sum()

### Infections in intersections sum to county level infections |
### If you want to run this check, we have to keep intersectionInfections 
### in the allocate infections step. 

# hexInfections %>% filter(fips == "04015",
#                          date == testDate) %>%
#   st_drop_geometry() %>%
#   ungroup() %>%
#   select(intersectInfections) %>% sum()


######## CHECK TOTAL INFECTIONS SUM UP #####################################|
hexInfections %>% filter(date == testDate) %>%
  st_drop_geometry() %>% 
  ungroup() %>%
  select(infections) %>% sum(na.rm=TRUE)

observationsFips %>% filter(date == testDate) %>%
  st_drop_geometry() %>% 
  select(infctns) %>% sum()

######## CHECK TOTAL POPULATION #####################################|
hexInfections %>% filter(date == testDate) %>%
  st_drop_geometry() %>% 
  ungroup() %>%
  select(population) %>% sum(na.rm=TRUE)

population %>% 
  st_drop_geometry() %>% 
  ungroup() %>%
  select(population) %>% sum(na.rm=TRUE)

###############################################################################
##### Create a gif of the infections allocation for the first wave        #####
###############################################################################
# alphaDates <- seq.Date(from = as.Date("2020-11-19")-63, 
#                        to = as.Date("2020-11-19"), by = "day")
# hexObservationsAllSF <- hexObservationsAllSF %>% mutate(infectionsPC = infectionsPC*1e5)
# 
# for(i in 1:63){
#   print(i)
#   plotDate <- alphaDates[i]
#   hexInfPlot <-
#     ggplot() + 
#     geom_sf(data=hexObservationsAllSF %>% filter(date == plotDate), aes(fill = infectionsPC)) + 
#     scale_fill_viridis_b(option = "magma",
#                          # name = "Estimated Infections/1000/week",
#                          direction = -1,
#                          na.value = "white",
#                          breaks = c(0,seq(500,3000, 500)),
#                          labels = c("500< ",seq(500,2500, 500), ' >2500'),
#                          limits = c(0,3000),
#     ) +
#     theme_bw() + 
#     ggtitle(paste(plotDate))
#   
#   png(here(paste0("figures/infection_allocation/firstWaveHexes/plot", plotDate, ".png")))
#   print(hexInfPlot)
#   dev.off()
# }
# 
# distFrontPlots <- list.files(here("figures/infection_allocation/firstWaveHexes"), full.names = TRUE)
# distFrontPlotList <- lapply(distFrontPlots, image_read)
# 
# ## join the images together 
# distFrontJoined <- image_join(distFrontPlotList)
# 
# ## animate at 2 frames per second
# distFrontAnimated <- image_animate(distFrontJoined, fps = 2)
# 
# distFrontAnimated
# 
# image_write(image = distFrontAnimated,
#             path = here("figures/infection_allocation/firstWaveHexes/infections.gif"))
# #######################################################################################
# for(i in 1:63){
#   print(i)
#   plotDate <- alphaDates[i]
#   hexInfPlot <-
#     ggplot() + 
#     geom_sf(data=observationFips %>% filter(date == plotDate), aes(fill = infectionsPC)) + 
#     scale_fill_viridis_b(option = "magma",
#                          # name = "Estimated Infections/1000/week",
#                          direction = -1,
#                          na.value = "white",
#                          breaks = c(0,seq(500,3000, 500)),
#                          labels = c("500< ",seq(500,2500, 500), ' >2500'),
#                          limits = c(0,3000),
#     ) +
#     theme_bw() + 
#     ggtitle(paste(plotDate))
#   
#   png(here(paste0("figures/infection_allocation/firstWaveCounties/plot", plotDate, ".png")))
#   print(hexInfPlot)
#   dev.off()
# }
# 
# distFrontPlots <- list.files(here("figures/infection_allocation/firstWaveCounties"), full.names = TRUE)
# distFrontPlotList <- lapply(distFrontPlots, image_read)
# 
# ## join the images together 
# distFrontJoined <- image_join(distFrontPlotList)
# 
# ## animate at 2 frames per second
# distFrontAnimated <- image_animate(distFrontJoined, fps = 2)
# 
# distFrontAnimated
# 
# image_write(image = distFrontAnimated,
#             path = here("figures/infection_allocation/firstWaveCounties/infectionsPC.gif"))
# 