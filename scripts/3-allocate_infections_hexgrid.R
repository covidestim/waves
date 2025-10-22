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
population <- st_read(here("Data/data-products/geo-hexes/pop/interhex_pop_updated.shp")) %>%
  select(hexid_x, fips_x, sum_pop, geometry) %>%
  rename(population = sum_pop,
         hexid = hexid_x,
         fips = fips_x)

populationHex <- population %>% 
                 st_drop_geometry() %>%
                 reframe(population = sum(population, na.rm = TRUE), 
                         .by="hexid")

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

observationsFips %>% 
  filter(! fips %in% population$fips)
##### Also set a test date for the plots throughout the workflow |
testDate <- "2021-03-26"

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

hexInfections <- data.frame()
for (i in unique(observationsFips$date)){
  print(as.Date(i))
  hexInfections0 <- populationFull %>% 
    st_drop_geometry() %>%
    full_join(observationsFips %>% 
                filter(date == as.Date(i)) %>% 
                select(fips, date, infctns)) %>% 
    mutate(intersectInfections = frcCountyPop * infctns) %>% 
    reframe(infections = sum(intersectInfections, na.rm=TRUE), 
            .by = "hexid") %>% 
    mutate(date = as.Date(i)) %>%
    left_join(populationHex) %>% 
    mutate(infectionsPC = infections / population)
    
    #### CHECKS FOR PERFORMANCE #### 
  
    if (round(sum(hexInfections0$population)/1e6, 4) != 
        round(sum(population$population)/1e6,4)){
      print("Population does not match.")
      break()
    }
  
    if (round(sum(hexInfections0$infections),4) != 
        round(observationsFips %>% filter (date == i) %>%
        select(infctns) %>% sum(),4)){
      print("Infections do not match")
      break()
    }
  
    if(nrow(hexInfections0) != nrow(populationHex)){
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

ggplot() + 
  # geom_sf(data = population)+
  geom_sf(data=hexInfections %>% 
            filter(date == alpha_peak,
                   ! hexid %in% deltaHexID) %>%
          left_join(hexgrid, by="hexid"))

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
  select(hexInfections) %>% sum(na.rm=TRUE)

observationFips %>% filter(date == testDate) %>%
  st_drop_geometry() %>% 
  select(infections) %>% sum()

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