plt_preomicron <- \(week, dataset, hexes, plot_img = TRUE) {
  
  ## Stopping if not dataset and/or hexes
  if(missing(dataset) | missing(hexes) | missing(week)){
    stop("'week' and/or 'dataset' and/or 'hexes' is missing")
  }
  
  # Filter the data for the current week
  data <- dataset[dataset$date_week == week, ]
  
  # Create a ggplot2 plot
  plot <- ggplot()+
    geom_sf(data = hexes, 
            fill = "transparent")+
    # geom_sf(data = data,
    #         aes(color = infectionsPC_avg))+
    geom_segment(data = data,
                 aes(x = start_coord_x, xend = end_coord_x,
                     y = start_coord_y, yend = end_coord_y,
                     color = infectionsPC_avg),
                 arrow = grid::arrow(length = unit(x = .5, units = "mm"), type = "closed"))+
    theme_void()+
    # scale_y_continuous(limits = c(24,50))+
    # scale_x_continuous(limits = c(-124, -66))+
    scale_color_viridis_c(option = "turbo",
                          # midpoint = 600,
                          name = "average Infections per capita",
                          breaks = seq(0,500, 50),
                          labels = c(seq(0,450, 50), "500+"),
                          limits = c(0,500),
                          oob = scales::squish,
                          guide = metR::guide_colorstrip(title.position = "top",
                                                         title.hjust = 0.5,
                                                         barwidth = grid::unit(12, "cm")))+
    theme(legend.position = "none")+
    coord_sf(ylim = c(24,50), xlim = c(-124, -66))
  # plot
  
  if(plot_img){
    plot <- plot + 
      labs(tag = paste("Week:", week))+
      theme(legend.position = "top")
  }
  
  # # Save the plot as a temporary file
  tmp_file <- tempfile(fileext = ".png")
  ggsave(tmp_file, plot, width = 16, height = 9)
  
  # Return the temporary file path
  return(tmp_file)
}

plt_omicronera <- \(week, dataset, hexes, plot_img = TRUE) {
  ## Stopping if not dataset and/or hexes
  if(missing(dataset) | missing(hexes) | missing(week)){
    stop("'week' and/or 'dataset' and/or 'hexes' is missing")
  }
  
  # Filter the data for the current week
  data <- dataset[dataset$date_week == week, ]
  
  # Create a ggplot2 plot
  plot <- ggplot()+
    geom_sf(data = hexes, 
            fill = "transparent")+
    # geom_sf(data = data,
    #         aes(color = infectionsPC_avg))+
    geom_segment(data = data,
                 aes(x = start_coord_x, xend = end_coord_x,
                     y = start_coord_y, yend = end_coord_y,
                     color = infectionsPC_avg),
                 arrow = grid::arrow(length = unit(x = .5, units = "mm"), type = "closed"))+
    theme_void()+
    # scale_y_continuous(limits = c(24,50))+
    # scale_x_continuous(limits = c(-124, -66))+
    scale_color_viridis_c(option = "turbo",
                          # midpoint = 600,
                          name = "average Infections per capita",
                          breaks = seq(0,1000, 100),
                          labels = c(seq(0,900, 100), "1000+"),
                          limits = c(0,1000),
                          oob = scales::squish,
                          guide = metR::guide_colorstrip(title.position = "top",
                                                         title.hjust = 0.5,
                                                         barwidth = grid::unit(12, "cm")))+
    theme(legend.position = "none")+
    coord_sf(ylim = c(24,50), xlim = c(-124, -66))
  # plot

  if(plot_img){
    plot <- plot + 
      labs(tag = paste("Week:", week))+
      theme(legend.position = "top")
  }
    
  # # Save the plot as a temporary file
  tmp_file <- tempfile(fileext = ".png")
  ggsave(tmp_file, plot, width = 16, height = 9)
  
  # Return the temporary file path
  return(tmp_file)
}

## Scatterplot to alphas
## Reading the weekly model output
## function to build the joined alphas correctly
joined_alphas <- \(alphas, date_col){
  alphas <- alphas |> 
    ## Writing the hexbin code as a 4-digit number
    dplyr::mutate(hex_i = sprintf("%04d", i),
                  hex_j = sprintf("%04d", j)) |> 
    ## Creating a 8-digit hexbin code to identifying uniquely them
    dplyr::mutate(i_to_j = str_c(hex_i, hex_j),
                  j_to_i = str_c(hex_j, hex_i))
  
  ## Plotting
  alphas_i_to_j <- alphas |> 
    dplyr::group_by(i_to_j, 
                    {{ date_col }}) |> 
    dplyr::summarise(raw = alpha,
                     normalized = alpha_normalized) 
  # |>
  #   mutate(standarlized = (raw - mean(raw, na.rm = T))/sd(raw, na.rm = T)) |> 
  #   pivot_longer(cols = raw:standarlized,
  #                names_to = "type",
  #                values_to = "values")
  
  alphas_j_to_i <- alphas |> 
    dplyr::group_by(j_to_i, 
                    {{ date_col }}) |> 
    dplyr::summarise(raw = alpha,
                     normalized = alpha_normalized)
  # |>
  #   mutate(standarlized = (raw - mean(raw, na.rm = T))/sd(raw, na.rm = T)) |> 
  #   pivot_longer(cols = raw:standarlized,
  #                names_to = "type",
  #                values_to = "values")
  
  joined_ij <- dplyr::inner_join(alphas_i_to_j, 
                                 alphas_j_to_i, 
                                 by = c("i_to_j" = "j_to_i", "date_week"), 
                                 suffix = c(".itoj", ".jtoi"))
  return(joined_ij)
}
