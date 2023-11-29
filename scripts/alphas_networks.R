## Reading the weekly model output
alphas_week <- vroom::vroom("data-products/geo-hexes/mixedmodel/alphas_weekly_regardless_rt-reformat.csv")

alphas_week <- alphas_week |> 
  ## Writing the hexbin code as a 4-digit number
  mutate(hex_i = sprintf("%04d", i),
         hex_j = sprintf("%04d", j)) |> 
  ## Creating a 8-digit hexbin code to identifying uniquely them
  mutate(i_to_j = str_c(hex_i, hex_j),
         j_to_i = str_c(hex_j, hex_i))

## Alphas x alphas
alphas_edgelist <- alphas_week |> 
  select(i,j, alpha) |> 
  reframe(alpha = sum(alpha, na.rm = T), 
          .by = c(i,j))

graph_alphas <- igraph::graph_from_data_frame(alphas_edgelist)

igraph::plot

# igraph::plot.igraph(graph_alphas)
lwcc <- igraph::largest_component(graph = graph_alphas, 
                                  mode = c("weak"))

lscc <- igraph::largest_component(graph = graph_alphas, 
                                  mode = c("strong"))