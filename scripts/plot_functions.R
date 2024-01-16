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
