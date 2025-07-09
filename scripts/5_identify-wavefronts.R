###############################################################################
### This function is used to approximate the leading edge of an infection wave 
### and key properties, such as length and estimated speed of expansion. 
### It will identify hexes above a defined threshold (infThreshold) at time 
### (t) for a set of infections per capita estimates distributed
### across a hexgrid (hexObs)
###############################################################################

idWavefront <- function(hexObs,
                        hexgrid,
                        infThreshold,
                        t){
###############################################################################
### Load packages
###############################################################################  
    library(dplyr)
###############################################################################
### Filter the dataset
###############################################################################
    filtHexObs <- hexObs %>% dplyr::filter(date == t) %>%
                             mutate(wavefront = ifelse(mean > infThreshold, 
                                                       TRUE, FALSE),
                                    ### Find the centroid of each hex  
                                    centroid = st_centroid(geometry))

### Check that the data set is nonempty: 
    if (sum(filtHexObs$wavefront) == 0) {
        print(paste("No hexes identified above threshold of", infThreshold, 
                    "infections per day. Maximum infections estimated on", t,
                    "is", 
                    round(max((hexObs %>% dplyr::filter(date == t))[,"mean"])*1e5)))
        # break()
    } else{
        print(paste("Identified", sum(filtHexObs$wavefront), 
                    "hexes with daily infections per capita greater than", 
                    infThreshold,".", 
                    "This represents", round(sum(filtHexObs$wavefront)/length(unique(filtHexObs$hexid)),2)*100, 
                    "% of all hexes."))
    }

### Check with a plot
    ggplot() + 
        geom_sf(data=filtHexObs, mapping=aes(fill=wavefront)) +
        geom_sf(data=filtHexObs %>% filter(wavefront==TRUE),
                mapping=aes(geometry= centroid), size =.01)
    
wave <- filtHexObs %>% filter(wavefront == TRUE) %>%
                       mutate(waveNumb = ifelse(as.numeric(hexid) > 2400, 1, 1)) %>% 
                       filter(waveNumb == 1)

notWave <- filtHexObs %>% filter(wavefront == FALSE) %>%
    mutate(waveNumb = ifelse(as.numeric(hexid) > 2400, 1, 1)) %>% 
    filter(waveNumb == 1)

boundary <- st_intersection(wave, notWave) %>% st_simplify()

plotBound <- ggplot() + 
            geom_sf(data=wave, mapping=aes(geometry= geometry), fill="salmon") +
            geom_sf(data=notWave, mapping=aes(geometry= geometry), fill="orange") +
            geom_sf(data=boundary, mapping=aes(geometry= geometry), color="dodgerblue") + 
            ggtitle(paste(t))


### Calculate the length of the boundary 
boundLength <- sum(st_length(boundary))

### Create a list of the relevant outputs 
waveFrontList <- list("wave" = wave, 
                      "not wave" = notWave, 
                      "boundary" = boundary, 
                      "boundary length" = boundLength,
                      "boundary plot" = plotBound)

return(waveFrontList)

}
