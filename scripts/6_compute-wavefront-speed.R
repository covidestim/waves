###############################################################################
###########################        SETUP        ###############################
###############################################################################
setwd("~/waves")

### load packages
library(dplyr)
library(magick)
library(sf)
library(ggplot2)
library(here)

### define the date of the first and second peaks
first_peak <- as.Date("2020-11-19")
second_peak <- as.Date("2021-09-04")

### Read in the stable hex grid
hexgrid <- sf::st_read(here("data-products/geo-hexes/hexgrid2025.shp")) %>%
  mutate(hexid = as.character(hexid))

### Create a single polygon of the hexgrid for use in distance calculations
hexgrid %>% st_union() -> simpHexgrid
st_crs(hexgrid)
### Load in the new observations from the CAR model
# obs <- readRDS(here("data-products/tsa_meta30m_run_preomicron_daily.rds"))  %>% 
obs <- read.csv(here("data-products/tsa_meta30m_run_preomicron_daily.csv"), 
                sep = "\t") %>% 
  mutate(hexid = as.character(hexid), 
         date = as.Date(date), 
         mean = as.numeric(mean))

### Define the two areas prior to the peaks  
CAR_lag_first <- obs %>% filter(date %in% seq.Date(from = (first_peak - 63), 
                                                   to = first_peak, 
                                                   length.out = 64)) %>% 
  left_join(hexgrid, by = "hexid") %>% st_as_sf() 

CAR_lag_second <- obs %>% filter(date %in% seq.Date(from = (second_peak - 63), 
                                                    to = second_peak, 
                                                    length.out = 64))  %>% 
  left_join(hexgrid, by = "hexid") %>% st_as_sf() 

### Set the infection threshold for the boundary definition
inf_threshold <- 165

### Source in the wavefront script that defines the boundaries
source(here("scripts/5_identify-wavefronts.R"))

### Set up our palette 
gradientPal <- viridis::magma(n=7)

###############################################################################
###########################     first WAVE      ###############################
###############################################################################

### Calculate the boundary / leading edge of the wave
boundList_w1 <- list()
for(i in -63:0){
  date <- first_peak + i 
  # print(date)
  boundList_w1[[i+64]] <-  idWavefront(hexObs = CAR_lag_first, 
                                       hexgrid = hexgrid, 
                                       infThreshold = inf_threshold, 
                                       t = date)
  
  png(here(paste0("figures/wavefronts/firstWave/plot", date, ".png")))
  print(boundList_w1[[i+64]]["boundary plot"])
  dev.off()
}

saveRDS(boundList_w1, file = here("data-products/wavefronts/boundaryData_firstWave.rds"), version = 2)
# boundList_w1 <- readRDS(here("data-products/wavefronts/boundaryData_firstWave.rds"))

###############################################################################
########### CALCULATE AND PLOT DISTANCE TO NEAREST POINT ######################
########### ON THE FRONTIER FOR EACH LINESTRING AT EACH #######################
########### DATE EXAMINED FOR THE PEAK. 
###############################################################################

### Empty vectors for calculations ### 
growth_w1 <- vector() ### Change in boundary length
waveEdge_w1 <- vector()
arealExpansion_w1 <- vector()
distanceToFrontier_w1 <- list()
for (i in 2:length(boundList_w1)){
  ### wave's edge length
  waveEdge_w1[i-1] <- as.numeric(boundList_w1[[i-1]]["boundary length"]) / 1000
  
  ### change in wave's edge length
  growth_w1[i-1] <- as.numeric(boundList_w1[[i]]["boundary length"]) - as.numeric(boundList_w1[[i-1]]["boundary length"])
  
  ### change in area 
  arealExpansion_w1[i-1] <- (nrow(boundList_w1[[i]][["wave"]]) - nrow(boundList_w1[[i-1]][["wave"]]))*64.75
  ### distance between wavefronts
  dist_wf <- st_distance(boundList_w1[[i-1]][["boundary"]][,"geometry"], boundList_w1[[i]][["boundary"]][,"geometry"])
  ### distance between wavefronts and edge of the United States
  dist_us <- st_distance(boundList_w1[[i-1]][["boundary"]][,"geometry"], st_boundary(simpHexgrid))
  dist_min <-cbind(boundList_w1[[i-1]][["boundary"]][,c("hexid", "date")], 
                   apply(X=dist_wf, MARGIN=1, FUN=min, na.rm = TRUE),
                   apply(X=dist_us, MARGIN=1, FUN=min, na.rm = TRUE))
  
  colnames(dist_min)[3] <- "distToFront0"; colnames(dist_min)[4] <- "distToUS"
  
  speed <- dist_min %>% rowwise() %>% mutate(distToFront = min(distToFront0, distToUS))
  distanceToFrontier_w1[[i-1]] <- speed
}

#### Calculate the weekly speed as well to confirm that the mean summary 
#### not particularly sensitive to time 
distanceToFrontier_w1_weekly <- list()
for (i in 8:length(boundList_w1)){
  x1<- st_distance(boundList_w1[[i-7]][["boundary"]][,"geometry"], boundList_w1[[i]][["boundary"]][,"geometry"]) /1000
  x2 <-cbind(boundList_w1[[i-7]][["boundary"]][,c("hexid", "date")], apply(X=x1, MARGIN=1, FUN=min, na.rm = TRUE))
  colnames(x2)[3] <- "distToFront"
  distanceToFrontier_w1_weekly[[i-7]] <- x2
}

### Save for future use ### 
saveRDS(distanceToFrontier_w1, here("data-products/wavefronts/distanceToFrontier_firstWave.rds"), version = 2)
saveRDS(distanceToFrontier_w1_weekly, here("data-products/wavefronts/distanceToFrontier_firstWaveWkly.rds"), version = 2)

### Read in the distance data ### 
# distanceToFrontier_w1 <- readRDS(here("data-products/wavefronts/distanceToFrontier_firstWave.rds"))

for(i in 1:63){
  date <- unique(distanceToFrontier_w1[[i]]$date)
  plotDistFront <- ggplot() + 
    geom_sf(data=hexgrid, mapping=aes(geometry= geometry), fill="grey80") +
    geom_sf(data=boundList_w1[[i]][["wave"]], mapping=aes(geometry= geometry), fill="grey50") +
    geom_sf(data=distanceToFrontier_w1[[i]], mapping=aes(geometry= geometry, 
                                                         color=distToFront/1000), size=1.5) +
    geom_sf(data=distanceToFrontier_w1[[i]] %>% filter(distToFront ==0), 
            mapping=aes(geometry= geometry), color = "black", size=1.5) +
    scale_color_viridis_b(limits=c(0, 800), option = "C", 
                          breaks=c(0,10,25,50,100,200, 400, 800), 
                          name = "Distance to nearest point\non boundary at t+1\n(in kms)") +
    ggtitle(paste(date))
  
  png(here(paste0("figures/wavefronts/firstWave/distFront-singleScale/plot", date, ".png")))
  print(plotDistFront)
  dev.off()
}

distFrontPlots <- list.files(here("figures/wavefronts/firstWave/distFront-singleScale/"), full.names = TRUE)
distFrontPlotList <- lapply(distFrontPlots, image_read)

## join the images together 
distFrontJoined <- image_join(distFrontPlotList)

## animate at 2 frames per second
distFrontAnimated <- image_animate(distFrontJoined, fps = 2)

distFrontAnimated

image_write(image = distFrontAnimated,
            path = here("figures/wavefronts/firstWave/distFront-singleScale/distFrontSingleAnimated.gif"))

###############################################################################
###########################     second WAVE      ###############################
###############################################################################
boundList_w2 <- list()
for(i in -63:0){
  date <- second_peak + i 
  # print(date)
  boundList_w2[[i+64]] <-  idWavefront(hexObs = CAR_lag_second, 
                                       hexgrid = hexgrid, 
                                       infThreshold = inf_threshold, 
                                       t = date)
  
  png(here(paste0("figures/wavefronts/secondWave/plot", date, ".png")))
  print(boundList_w2[[i+64]]["boundary plot"])
  dev.off()
}

# saveRDS(boundList_w2, file = here("data-products/wavefronts/boundaryData_secondWave.rds"), version = 2)
boundList_w2 <- readRDS(here("data-products/wavefronts/boundaryData_secondWave.rds"))

## calculate growth of wave front 
growth_w2 <- vector()
waveEdge_w2 <- vector()
arealExpansion_w2 <- vector()
distanceToFrontier_w2 <- list()
for (i in 2:length(boundList_w2)){
  ### Calculate the change in the wave boundary length
  growth_w2[i-1] <- as.numeric(boundList_w2[[i]]["boundary length"]) - as.numeric(boundList_w2[[i-1]]["boundary length"])
  ### Calculate the boundary length 
  waveEdge_w2[i-1] <- as.numeric(boundList_w2[[i-1]]["boundary length"]) / 1000
  arealExpansion_w2[i-1] <- (nrow(boundList_w2[[i]][["wave"]]) - nrow(boundList_w2[[i-1]][["wave"]]))*64.75
  
  ### distance between wavefronts
  dist_wf <- st_distance(boundList_w2[[i-1]][["boundary"]][,"geometry"], boundList_w2[[i]][["boundary"]][,"geometry"])
  ### distance between wavefronts and edge of the United States
  dist_us <- st_distance(boundList_w2[[i-1]][["boundary"]][,"geometry"], st_boundary(simpHexgrid))
  dist_min <-cbind(boundList_w2[[i-1]][["boundary"]][,c("hexid", "date")], 
                   apply(X=dist_wf, MARGIN=1, FUN=min, na.rm = TRUE),
                   apply(X=dist_us, MARGIN=1, FUN=min, na.rm = TRUE))
  
  colnames(dist_min)[3] <- "distToFront0"; colnames(dist_min)[4] <- "distToUS"
  
  speed <- dist_min %>% rowwise() %>% mutate(distToFront = min(distToFront0, distToUS))
  distanceToFrontier_w2[[i-1]] <- speed
}

#### Calculate the weekly speed as well to confirm that the mean summary 
#### not particularly sensitive to time 
distanceToFrontier_w2_weekly <- list()
for (i in 8:length(boundList_w2)){
  x1<- st_distance(boundList_w2[[i-7]][["boundary"]][,"geometry"], boundList_w2[[i]][["boundary"]][,"geometry"]) /1000
  x2 <-cbind(boundList_w2[[i-7]][["boundary"]][,c("hexid", "date")], apply(X=x1, MARGIN=1, FUN=min, na.rm = TRUE))
  colnames(x2)[3] <- "distToFront"
  distanceToFrontier_w2_weekly[[i-7]] <- x2
}

### Save for future use ### 
saveRDS(distanceToFrontier_w2, here("data-products/wavefronts/distanceToFrontier_secondWave.rds"), version = 2)
saveRDS(distanceToFrontier_w2_weekly, here("data-products/wavefronts/distanceToFrontier_secondWaveWkly.rds"), version = 2)

###############################################################################
########### CALCULATE AND PLOT DISTANCE TO NEAREST POINT ######################
########### ON THE FRONTIER FOR EACH LINESTRING AT EACH #######################
########### DATE EXAMINED FOR THE PEAK. 
###############################################################################

for(i in 1:63){
  date <- unique(distanceToFrontier_w2[[i]]$date)
  plotDistFront <-
    ggplot() + 
    geom_sf(data=hexgrid, mapping=aes(geometry= geometry), fill="grey80") +
    geom_sf(data=boundList_w2[[i]][["wave"]], mapping=aes(geometry= geometry), fill="grey50") +
    geom_sf(data=distanceToFrontier_w2[[i]], mapping=aes(geometry= geometry, 
                                                         color=distToFront/1000), size=1.5) +
    geom_sf(data=distanceToFrontier_w2[[i]] %>% filter(distToFront ==0), 
            mapping=aes(geometry= geometry), color = "black", size=1.5) +
    scale_color_viridis_b(limits=c(0, 800), option = "C", 
                          breaks=c(0,10,25,50,100,200, 400, 800), 
                          name = "Distance to nearest point\non boundary at t+1\n(in kms)") +
    theme_bw() + 
    ggtitle(paste(date))
  
  png(here(paste0("figures/wavefronts/secondWave/distFront-singleScale/plot", date, ".png")))
  print(plotDistFront)
  dev.off()
}

distFrontPlots <- list.files(here("figures/wavefronts/secondWave/distFront-singleScale"), full.names = TRUE)
distFrontPlotList <- lapply(distFrontPlots, image_read)

## join the images together 
distFrontJoined <- image_join(distFrontPlotList)

## animate at 2 frames per second
distFrontAnimated <- image_animate(distFrontJoined, fps = 2)

distFrontAnimated

image_write(image = distFrontAnimated,
            path = here("figures/wavefronts/secondWave/distFront-singleScale/distFrontSingleAnimated.gif"))
