rm(list = ls())
gc()

library(tidyverse)
library(vroom)
library(sf)

## 
hexes <- sf::st_read("data-products/geo-hexes/hexes.shp")

alpha_peak <- as.Date("2020-11-19")
delta_peak <- as.Date("2021-09-08")

dataset <- "preomicron"

## If reruns need to be made from a saved df, read it as a data.frame and use the following code to rebuilty it as a list to rerun at the above for loop
CAR_df <- vroom::vroom(paste0("data-products/tsa_",
                              dataset, 
                              ".csv"))

## Returning the data.frame into a list format
CAR_list <- CAR_df |> 
  group_split(date) |> 
  as.list()


## Breakdowns of each peaks, Pre-Omicron
breaks_plt <- c(0,seq(150,300, 20))
labels_plt <- c("150< ",seq(150,280, 20), ' >300')
limits_plt <- c(0,350)
color_option <- "magma"

## function to plot hexes with infections
plt_fun <- \(week, hexes, CAR_df, plot_img = TRUE) {
  
  plot_alphas <- ggplot() +
    geom_sf(data = hexes |> 
              mutate(hexid = as.character(hexid)) |> 
              cbind(predCAR_B = CAR_df |> filter(date == week) |> pull(var = "mean")) |> 
              st_transform(crs = 26915),
            aes(fill = predCAR_B))+
    scale_fill_viridis_b(option = color_option,
                         # name = "Estimated Infections/1000/week",
                         direction = -1,
                         breaks = breaks_plt,
                         labels = labels_plt,
                         limits = limits_plt,
    )+
    # scale_fill_viridis_b(option = color_option,
    #                      n.breaks = 10,
    #                      direction = -1)+
    theme_minimal()+
    guides(fill = guide_bins(title = "Trend surface estimated infections per capita/100k",
                             title.position = "top",
                             title.vjust = 0.5))+
    theme(legend.position = "bottom",
          legend.title.position = "top",
          legend.key.width = grid::unit(1, "cm"))+
    coord_sf(xlim = st_bbox(hexes |> 
                              st_transform(crs = 26915))[c(1,3)],
             ylim = st_bbox(hexes |> 
                              st_transform(crs = 26915))[c(2,4)])
  
  library(patchwork)
  # plot <- (plot_infections | plot_alphas)
  plot <- plot_alphas
  
  if(plot_img){
    plot <- plot &
      labs(tag = paste("Week:", week))
  }
  
  # # Save the plot as a temporary file
  tmp_file <- tempfile(fileext = ".png")
  ggsave(tmp_file, plot, width = 16, height = 9, dpi = 100)
  
  # Return the temporary file path
  return(tmp_file)
}

weeks <- sort(unique(CAR_df$date))

## Size of gifs, in days

size <- 63

weeks_alpha <- seq.Date(from = (alpha_peak - size),
                        to = (alpha_peak + size),
                        by = "week")

weeks_delta <- seq.Date(from = (delta_peak - size),
                        to = (delta_peak + size),
                        by = "week")

weeks_plt <- weeks[which(weeks %in% weeks_delta)]

hexes <- hexes |> 
  filter(as.integer(hexid) < 7662)

frame_files <- lapply(weeks_plt, 
                      plt_fun, 
                      hexes,
                      bind_rows(CAR_list),
                      TRUE)

frame_files <- frame_files |> 
  unlist()

animation5 <- magick::image_animate(magick::image_read(frame_files), 
                                    # delay = 1,
                                    fps = 10, ## Change to a bigger number to a higher rate of fps and vice-versa
                                    optimize = T)
animation5

# Specify the output file path, the pattern nomenclature should be: daily_hex_tsa_preomicron.gif, if doing daily gif over some specific periods. DO NOT DO DAILY GIF FOR THE WHOLE PRE-OMICRON PERIOD! Or weekly_hex_tsa_preomicron.gif and weekly_hex_tsa_omicronera.gif for the gif over the entirety of both datasets

output_file <- "img/extra_figures/tsa_delta_fps10.gif"

# Save the GIF animation
magick::image_write(animation5, output_file)


magick::image_write_gif(image = animation5, 
                        path = output_file,
                        # delay = 1/10 # frame per second inverse
)

output_file <- "img/extra_figures/tsa_alpha_fps10.mp4"

magick::image_write_video(image = animation5, 
                          path = output_file,
                          delay = 1/10 # frame per second inverse
)

#