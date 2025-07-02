setwd("~/waves")

### load packages
library(dplyr)
library(magick)
library(sf)
library(ggplot2)
library(here)

boundList1 <- readRDS("boundaryPlots/firstNewModel/boundaryDatafirst.rds")
### Read in the first peak speeds
distanceToFrontier_firstWave <- readRDS(here("boundaryPlots/firstNewModel/firstDistanceToFrontier.rds"))

### Read in the second peak speeds 
distanceToFrontier_secondWave <- readRDS(here("boundaryPlots/secondNewModel/secondDistanceToFrontier.rds"))

### Read in the stable hex grid
### This is the file sent to Yu Lan for population distribution
hexgrid <- st_read("data-products/geo-hexes/hexgrid2025.shp") 

wave1Panel1 <- ggplot() + 
    geom_sf(data=hexgrid, mapping=aes(geometry= geometry), fill="grey90") +
    geom_sf(data=boundList1[[1]][["wave"]], mapping=aes(geometry= geometry), fill="grey50") +
    geom_sf(data=distanceToFrontier_firstWave[[1]], mapping=aes(geometry= geometry, 
                                                      color=distToFront/1000), size=1.5) +
    geom_sf(data=distanceToFrontier_firstWave[[1]] %>% filter(distToFront==0), 
            mapping=aes(geometry= geometry), color = "black", size=1.5) +
    scale_color_viridis_b(limits=c(0, 1500), option = "C",
                          breaks=c(0, 50, 100, 200, 500, 1000, 1500),
                          name = "Distance to nearest point\non boundary at t+1\n(in kms)") + 
    theme_void() +
    theme(legend.position = "none") + 
    ggtitle(paste(unique(distanceToFrontier_firstWave[[1]]$date)))


wave1Panel2 <- ggplot() + 
    geom_sf(data=hexgrid, mapping=aes(geometry= geometry), fill="grey90") +
    geom_sf(data=boundList1[[22]][["wave"]], mapping=aes(geometry= geometry), fill="grey50") +
    geom_sf(data=distanceToFrontier_firstWave[[22]], mapping=aes(geometry= geometry, 
                                                                color=distToFront/1000), size=1.5) +
    geom_sf(data=distanceToFrontier_firstWave[[22]] %>% filter(distToFront==0),  
            mapping=aes(geometry= geometry), color = "black", size=1.5) +
    scale_color_viridis_b(limits=c(0, 1500), option = "C",
                          breaks=c(0, 50, 100, 200, 500, 1000, 1500),
                          name = "Distance to nearest point\non boundary at t+1\n(in kms)") + 
    theme_void() +
    theme(legend.position = "none") + 
    ggtitle(paste(unique(distanceToFrontier_firstWave[[22]]$date)))

wave1Panel3 <- ggplot() + 
    geom_sf(data=hexgrid, mapping=aes(geometry= geometry), fill="grey90") +
    geom_sf(data=boundList1[[43]][["wave"]], mapping=aes(geometry= geometry), fill="grey50") +
    geom_sf(data=distanceToFrontier_firstWave[[43]], mapping=aes(geometry= geometry, 
                                                                 color=distToFront/1000), size=1.5) +
    geom_sf(data=distanceToFrontier_firstWave[[43]] %>% filter(distToFront==0), 
            mapping=aes(geometry= geometry), color = "black", size=1.5) +
    scale_color_viridis_b(limits=c(0, 1500), option = "C",
                          breaks=c(0, 50, 100, 200, 500, 1000, 1500),
                          name = "Distance to nearest point\non boundary at t+1\n(in kms)") + 
    theme_void() +
    theme(legend.position = "none") + 
    ggtitle(paste(unique(distanceToFrontier_firstWave[[43]]$date)))

wave1Panel4 <- ggplot() + 
    geom_sf(data=hexgrid, mapping=aes(geometry= geometry), fill="grey90") +
    geom_sf(data=boundList1[[46]][["wave"]], mapping=aes(geometry= geometry), fill="grey50") +
    geom_sf(data=distanceToFrontier_firstWave[[46]], mapping=aes(geometry= geometry, 
                                                                 color=distToFront/1000), size=1.5) +
    geom_sf(data=distanceToFrontier_firstWave[[46]] %>% filter(distToFront==0), 
            mapping=aes(geometry= geometry), color = "black", size=1.5) +
    scale_color_viridis_b(limits=c(0, 1500), option = "C",
                          breaks=c(0, 50, 100, 200, 500, 1000, 1500),
                          name = "Distance to nearest point\non boundary at t+1\n(in kms)") + 
    theme_void() +
    theme(legend.position = "none") + 
    ggtitle(paste(unique(distanceToFrontier_firstWave[[46]]$date)))

# rm(boundList1)
###############################################################################
# boundList2 <- readRDS(here("boundaryPlots/secondNewModel/boundaryData_secondWave.rds"))

wave2Panel1 <- ggplot() + 
    geom_sf(data=hexgrid, mapping=aes(geometry= geometry), fill="grey90") +
    geom_sf(data=boundList2[[1]][["wave"]], mapping=aes(geometry= geometry), fill="grey50") +
    geom_sf(data=distanceToFrontier_secondWave[[1]], mapping=aes(geometry= geometry, 
                                                                color=distToFront/1000), size=1.5) +
    geom_sf(data=distanceToFrontier_secondWave[[1]] %>% filter(distToFront==0), 
            mapping=aes(geometry= geometry), color = "black", size=1.5) +
    scale_color_viridis_b(limits=c(0, 1500), option = "C",
                          breaks=c(0, 50, 100, 200, 500, 1000, 1500),
                          name = "Distance to nearest point\non boundary at t+1\n(in kms)") + 
    theme_void() +
    theme(legend.position = "none") + 
    ggtitle(paste(unique(distanceToFrontier_secondWave[[1]]$date)))


wave2Panel2 <- ggplot() + 
    geom_sf(data=hexgrid, mapping=aes(geometry= geometry), fill="grey90") +
    geom_sf(data=boundList2[[22]][["wave"]], mapping=aes(geometry= geometry), fill="grey50") +
    geom_sf(data=distanceToFrontier_secondWave[[22]], mapping=aes(geometry= geometry, 
                                                                 color=distToFront/1000), size=1.5) +
    geom_sf(data=distanceToFrontier_secondWave[[22]] %>% filter(distToFront==0), 
            mapping=aes(geometry= geometry), color = "black", size=1.5) +
    scale_color_viridis_b(limits=c(0, 1500), option = "C",
                          breaks=c(0, 50, 100, 200, 500, 1000, 1500),
                          name = "Distance to nearest point\non boundary at t+1\n(in kms)") + 
    theme_void() +
    theme(legend.position = "none") + 
    ggtitle(paste(unique(distanceToFrontier_secondWave[[22]]$date)))

wave2Panel3 <- ggplot() + 
    geom_sf(data=hexgrid, mapping=aes(geometry= geometry), fill="grey90") +
    geom_sf(data=boundList2[[43]][["wave"]], mapping=aes(geometry= geometry), fill="grey50") +
    geom_sf(data=distanceToFrontier_secondWave[[43]], mapping=aes(geometry= geometry, 
                                                                 color=distToFront/1000), size=1.5) +
    geom_sf(data=distanceToFrontier_secondWave[[43]] %>% filter(distToFront==0), 
            mapping=aes(geometry= geometry), color = "black", size=1.5) +
    scale_color_viridis_b(limits=c(0, 1500), option = "C",
                          breaks=c(0, 50, 100, 200, 500, 1000, 1500),
                          name = "Distance to nearest point\non boundary at t+1\n(in kms)") + 
    theme_void() +
    theme(legend.position = "none") + 
    ggtitle(paste(unique(distanceToFrontier_secondWave[[43]]$date)))

wave2Panel4 <- ggplot() + 
    geom_sf(data=hexgrid, mapping=aes(geometry= geometry), fill="grey90") +
    geom_sf(data=boundList2[[48]][["wave"]], mapping=aes(geometry= geometry), fill="grey50") +
    geom_sf(data=distanceToFrontier_secondWave[[48]], mapping=aes(geometry= geometry, 
                                                                  color=distToFront/1000), size=1.5) +
    geom_sf(data=distanceToFrontier_secondWave[[48]] %>% filter(distToFront==0), 
            mapping=aes(geometry= geometry), color = "black", size=1.5) +
    scale_color_viridis_b(limits=c(0, 1500), option = "C",
                          breaks=c(0, 50, 100, 200, 500, 1000, 1500),
                          name = "Distance to nearest point\non boundary at t+1\n(in kms)") + 
    theme_void() +
    theme(legend.position = "none") + 
    ggtitle(paste(unique(distanceToFrontier_secondWave[[48]]$date)))

###############################################################################
######################## Create a grid plot ###################################
###############################################################################

library(gridExtra)

allPlot <- grid.arrange(wave1Panel1, 
             wave1Panel2,
             wave1Panel3,
             wave1Panel4, 
             wave2Panel1, 
             wave2Panel2,
             wave2Panel3,
             wave2Panel4, ncol=2, widths = c(30, 30))

###############################################################################
######################## Save the grid plot ###################################
###############################################################################

ggsave(plot = allPlot, filename = here("boundaryPlots/figure4.png"), dpi = 600)