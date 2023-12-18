rm(list = ls())
gc()

library(tidyverse)

## Reading the weekly model output
alphas_week <- vroom::vroom("data-products/geo-hexes/mixedmodel/alphas_weekly_regardless_rt-reformat.csv")

alphas_week <- alphas_week |> 
  ## Writing the hexbin code as a 4-digit number
  mutate(hex_i = sprintf("%04d", i),
         hex_j = sprintf("%04d", j)) |> 
  ## Creating a 8-digit hexbin code to identifying uniquely them
  mutate(i_to_j = str_c(hex_i, hex_j),
         j_to_i = str_c(hex_j, hex_i))

## Splitting the alphas into i to j and j to i
## Plotting
alphas_i_to_j_week <- alphas_week |> 
  group_by(i_to_j, date_week) |> 
  summarise(raw = alpha,
            normalized = alpha_normalized) |>
  mutate(standarlized = (raw - mean(raw, na.rm = T))/sd(raw, na.rm = T)) |> 
  pivot_longer(cols = raw:standarlized,
               names_to = "type",
               values_to = "values")

alphas_j_to_i_week <- alphas_week |> 
  group_by(j_to_i, date_week) |> 
  summarise(raw = alpha,
            normalized = alpha_normalized) |>
  mutate(standarlized = (raw - mean(raw, na.rm = T))/sd(raw, na.rm = T)) |> 
  pivot_longer(cols = raw:standarlized,
               names_to = "type",
               values_to = "values")

## Transition rate matrix viz

dates <- unique(alphas_week$date_week)

tr_matrix_list <- lapply(sort(dates), function(x){
  tmp <- alphas_week |> 
    filter(date_week == x) |> 
    reframe(alpha = alpha, 
            .by = c(i,j)) |> 
    spread(i,alpha) |> 
    select(-j)
  })

names(tr_matrix_list) <- dates

# library(foreach)
# library(doParallel)
# cores<-detectCores() - 1
# cl<-makeCluster(cores)
# # clusterExport(cl)
# registerDoParallel(cl)
# tr_matrix_test <- foreach(x = sort(dates),
#                           .packages = c("dplyr", "tidyr")) %do% {
#   tmp <- alphas_week |> 
#     dplyr::filter(date_week == x) |> 
#     dplyr::reframe(alpha = alpha, 
#             .by = c(i,j)) |> 
#     tidyr::spread(i,alpha) |> 
#     dplyr::select(-j)
#   return(tmp)
#                           }
# stopCluster(cl)

# alphas_week |> 
#   filter(date_week == x) |> 
#   reframe(alpha = alpha, 
#           .by = c(i,j)) |> 
#   spread(i,alpha) |> 
#   select(-j)

## Alphas x alphas
alphas_edgelist <- alphas_week |> 
  reframe(alpha = sum(alpha, na.rm = T), 
          .by = c(i,j)) |> 
  spread(i,alpha)

rownames(alphas_edgelist) <- alphas_edgelist$j

m <- as.matrix(alphas_edgelist[,-1])

m_test <- m
m_test[is.na(m_test)] <- 0

pheatmap::pheatmap(m, 
                   display_numbers=F, 
                   show_colnames=F,
                   cluster_rows=F, 
                   cluster_cols=F)

alphas_tile <- alphas_week |> 
  reframe(alpha = sum(alpha, na.rm = T), 
          .by = c(i,j)) |> 
  complete(i,j)

alphas_tile |>
  filter(i %in% c(1:150),
         j %in% c(1:150)) |>
  ggplot(aes(x = i, y = j, fill = alpha))+
  geom_tile()+
  theme_minimal()+
  scale_fill_viridis_c(option = "plasma", na.value = "black")+
  coord_fixed()+
  theme(legend.position = "bottom")


graph_alphas <- igraph::graph_from_adjacency_matrix(test)

# igraph::plot.igraph(graph_alphas)
lwcc <- igraph::largest_component(graph = graph_alphas, 
                                  mode = c("weak"))
igraph::plot.igraph(lwcc)

lscc <- igraph::largest_component(graph = graph_alphas, 
                                  mode = c("strong"))

igraph::plot.igraph(lscc)
