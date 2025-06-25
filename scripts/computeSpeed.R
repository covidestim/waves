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
hexgrid <- readRDS(here("data-products/hexgrid.rds")) %>% 
  mutate(hexid = as.integer(hexid))

### Load in the new observations from the CAR model
obs <- read.csv("~/Downloads/tsa_meta30m_run_preomicron_daily.csv", 
                sep = "\t") %>% 
  mutate(hexid = as.integer(hexid), 
         date = as.Date(date), 
         mean = as.numeric(mean))

### Define the two areas prior to the peaks  
CAR_lag_first <- obs %>% filter(date %in% seq.Date(from = (first_peak - 63), 
                                                   to = first_peak, 
                                                   length.out = 64)) %>% 
  left_join(hexgrid, by = "hexid") %>% st_as_sf() %>% 
  rename(geometry = x)


CAR_lag_second <- obs %>% filter(date %in% seq.Date(from = (second_peak - 63), 
                                                   to = second_peak, 
                                                   length.out = 64))  %>% 
  left_join(hexgrid, by = "hexid") %>% st_as_sf() %>% 
  rename(geometry = x)

### Set the infection threshold for the boundary definition
inf_threshold <- 165

### Source in the wavefront script that defines the boundaries
source(here("scripts/wavefronts.R"))

### Set up our palette 
gradientPal <- viridis::magma(n=7)


###############################################################################
###########################     first WAVE      ###############################
###############################################################################

### Calculate the boundary / leading edge of the wave
boundList <- list()
for(i in -63:0){
    date <- first_peak + i 
    # print(date)
    boundList[[i+64]] <-  idWavefront(hexObs = CAR_lag_first, 
                                      hexgrid = hexgrid, 
                                      infThreshold = inf_threshold, 
                                      t = date)
    
    png(here(paste0("boundaryPlots/firstNewModel/plot", date, ".png")))
    print(boundList[[i+64]]["boundary plot"])
    dev.off()
}

saveRDS(boundList, file = here("boundaryPlots/firstNewModel/boundaryDatafirst.rds"), version = 2)
# boundList <- readRDS("boundaryPlots/first/boundaryDatafirst.rds")

###############################################################################
########### CALCULATE AND PLOT DISTANCE TO NEAREST POINT ######################
########### ON THE FRONTIER FOR EACH LINESTRING AT EACH #######################
########### DATE EXAMINED FOR THE PEAK. 
###############################################################################

growth <- vector()
speed <- vector()
speedHex <- vector()
distanceToFrontier <- list()
for (i in 2:length(boundList)){
  growth[i-1] <- as.numeric(boundList[[i]]["boundary length"]) - as.numeric(boundList[[i-1]]["boundary length"])
  speed[i-1] <- (as.numeric(boundList[[i]]["boundary length"]) - as.numeric(boundList[[i-1]]["boundary length"]))/as.numeric(boundList[[i-1]]["boundary length"])
  speedHex[i-1] <- ((nrow(boundList[[i]][["wave"]]) - nrow(boundList[[i-1]][["wave"]]))/(boundList[[i-1]][["boundary length"]]/1000))*64.75
  x1<- st_distance(boundList[[i-1]][["boundary"]][,"geometry"], boundList[[i]][["boundary"]][,"geometry"])
  x2 <-cbind(boundList[[i-1]][["boundary"]][,c("hexid", "date")], apply(X=x1, MARGIN=1, FUN=min, na.rm = TRUE))
  colnames(x2)[3] <- "distToFront"
  distanceToFrontier[[i-1]] <- x2
}

### Save for future use ### 
saveRDS(distanceToFrontier, here("boundaryPlots/firstNewModel/firstDistanceToFrontier.rds"), version = 2)

### Read in the distance data ### 
# distanceToFrontier <- readRDS(here("boundaryPlots/firstDistanceToFrontier.rds"))

for(i in 1:63){
  date <- unique(distanceToFrontier[[i]]$date)
  plotDistFront <- ggplot() + 
    geom_sf(data=hexgrid %>% rename(geometry=x), mapping=aes(geometry= geometry), fill="grey80") +
    geom_sf(data=boundList[[i]][["wave"]], mapping=aes(geometry= geometry), fill="grey50") +
    geom_sf(data=distanceToFrontier[[i]], mapping=aes(geometry= geometry, 
                                                      color=distToFront/1000), size=1.5) +
    geom_sf(data=distanceToFrontier[[i]] %>% filter(distToFront ==0), 
            mapping=aes(geometry= geometry), color = "black", size=1.5) +
    scale_color_viridis_b(limits=c(0, 1500), option = "C", 
                          breaks=c(0, 50, 100, 200, 500, 1000, 1500), 
                          name = "Distance to nearest point\non boundary at t+1\n(in kms)") +
    ggtitle(paste(date))
  
  png(here(paste0("boundaryPlots/firstNewModel/distFront-singleScale/plot", date, ".png")))
  print(plotDistFront)
  dev.off()
}

distFrontPlots <- list.files(here("boundaryPlots/firstNewModel/distFront-singleScale"), full.names = TRUE)
distFrontPlotList <- lapply(distFrontPlots, image_read)

## join the images together 
distFrontJoined <- image_join(distFrontPlotList)

## animate at 2 frames per second
distFrontAnimated <- image_animate(distFrontJoined, fps = 2)

distFrontAnimated

image_write(image = distFrontAnimated,
            path = "~/waves/boundaryPlots/firstNewModel/distFront-singleScale/distFrontSingleAnimated.gif")

###############################################################################
###########################     second WAVE      ###############################
###############################################################################
boundList <- list()
for(i in -63:0){
  date <- second_peak + i 
  # print(date)
  boundList[[i+64]] <-  idWavefront(hexObs = CAR_lag_second, 
                                    hexgrid = hexgrid, 
                                    infThreshold = inf_threshold, 
                                    t = date)
  
  png(paste0("~/waves/boundaryPlots/secondNewModel/plot", date, ".png"))
  print(boundList[[i+64]]["boundary plot"])
  dev.off()
}

saveRDS(boundList, file = "boundaryPlots/secondNewModel/boundaryDatasecond.rds", version = 2)

# boundList <- readRDS("boundaryPlots/second/boundaryDatasecond.rds")

## calculate growth of wave front 
growth <- vector()
speed <- vector()
speedHex <- vector()
distanceToFrontier <- list()
for (i in 2:length(boundList)){
  growth[i-1] <- as.numeric(boundList[[i]]["boundary length"]) - as.numeric(boundList[[i-1]]["boundary length"])
  speed[i-1] <- (as.numeric(boundList[[i]]["boundary length"]) - as.numeric(boundList[[i-1]]["boundary length"]))/as.numeric(boundList[[i-1]]["boundary length"])
  speedHex[i-1] <- ((nrow(boundList[[i]][["wave"]]) - nrow(boundList[[i-1]][["wave"]]))/(boundList[[i-1]][["boundary length"]]/1000))*64.75
  x1<- st_distance(boundList[[i-1]][["boundary"]][,"geometry"], boundList[[i]][["boundary"]][,"geometry"])
  x2 <-cbind(boundList[[i-1]][["boundary"]][,c("hexid", "date")], apply(X=x1, MARGIN=1, FUN=min, na.rm = TRUE))
  colnames(x2)[3] <- "distToFront"
  distanceToFrontier[[i-1]] <- x2
}
###############################################################################
########### CALCULATE AND PLOT DISTANCE TO NEAREST POINT ######################
########### ON THE FRONTIER FOR EACH LINESTRING AT EACH #######################
########### DATE EXAMINED FOR THE PEAK. 
###############################################################################

tmpDF<-data.frame()
for (i in 1:63){
  tmpDF <- rbind(tmpDF, distanceToFrontier[[i]]$distToFront)
}


for(i in 1:63){
  date <- unique(distanceToFrontier[[i]]$date)
  plotDistFront <-
    ggplot() + 
    geom_sf(data=hexgrid %>% rename(geometry=x), mapping=aes(geometry= geometry), fill="grey80") +
    geom_sf(data=boundList[[i]][["wave"]], mapping=aes(geometry= geometry), fill="grey50") +
    geom_sf(data=distanceToFrontier[[i]], mapping=aes(geometry= geometry, 
                                                      color=distToFront/1000), size=1.5) +
    geom_sf(data=distanceToFrontier[[i]] %>% filter(distToFront ==0), 
            mapping=aes(geometry= geometry), color = "black", size=1.5) +
    scale_color_viridis_b(limits=c(0, 1500), option = "C", 
                          breaks=c(0,50,100,200, 500, 1000, 1500), 
                          name = "Distance to nearest point\non boundary at t+1\n(in kms)") +
    theme_bw() + 
    ggtitle(paste(date))
  
  png(paste0("~/waves/boundaryPlots/secondNewModel/distFront-singleScale/plot", date, ".png"))
  print(plotDistFront)
  dev.off()
}

distFrontPlots <- list.files("~/waves/boundaryPlots/secondNewModel/distFront-singleScale", full.names = TRUE)
distFrontPlotList <- lapply(distFrontPlots, image_read)

## join the images together 
distFrontJoined <- image_join(distFrontPlotList)

## animate at 2 frames per second
distFrontAnimated <- image_animate(distFrontJoined, fps = 2)

distFrontAnimated

image_write(image = distFrontAnimated,
            path = "~/waves/boundaryPlots/secondNewModel/distFront-singleScale/distFrontSingleAnimated.gif")


