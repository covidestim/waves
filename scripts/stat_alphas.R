rm(list = ls())
gc()

library(tidyverse)
library(sf)

# alphas <- vroom::vroom("data-products/geo-hexes/mixedmodel/alphas_weekly.csv")
# 
# 
# joined <- vroom::vroom("data-products/geo-hexes/mixedmodel/joined-weekly.csv")

## Reading the monthly model output
alphas_month <- vroom::vroom("data-products/geo-hexes/mixedmodel/alphas-reformat.csv",
                             col_types = c("d", "d", "D", "d", "d", "d"))

## Creating unique variable to each hex number
alphas_month <- alphas_month |> 
  ## Writing the hexbin code as a 4-digit number
  mutate(hex_i = sprintf("%04d", i),
         hex_j = sprintf("%04d", j)) |> 
  ## Creating a 8-digit hexbin code to identifying uniquely them
  mutate(i_to_j = str_c(hex_i, hex_j),
         j_to_i = str_c(hex_j, hex_i))

## Plotting
alphas_i_to_j <- alphas_month |> 
  group_by(i_to_j, month) |> 
  summarise(raw = alpha,
            normalized = alpha_normalized) |>
  mutate(standarlized = (raw - mean(raw, na.rm = T))/sd(raw, na.rm = T)) |> 
  pivot_longer(cols = raw:standarlized,
               names_to = "type",
               values_to = "values")

alphas_j_to_i <- alphas_month |> 
  group_by(j_to_i, month) |> 
  summarise(raw = alpha,
            normalized = alpha_normalized) |>
  mutate(standarlized = (raw - mean(raw, na.rm = T))/sd(raw, na.rm = T)) |> 
  pivot_longer(cols = raw:standarlized,
               names_to = "type",
               values_to = "values")

joined_ij <- inner_join(alphas_i_to_j, 
                        alphas_j_to_i, 
                        by = c("i_to_j" = "j_to_i", 
                               "month", "type"), 
                        suffix = c(".itoj", ".jtoi")) 

scatterplot_raw <- joined_ij |> 
  filter(type == "raw") |> 
  ggplot(aes(x = values.itoj, 
             y = values.jtoi))+
  geom_point(alpha = 0.01)+
  theme_minimal()+
  labs(title = "Monthly model",
       x = expression(alpha~"i to j"),
       y = expression(alpha~"j to i"))+
  theme(axis.title = element_text(size = 14))+
  theme(legend.position = "none")
scatterplot_raw

ggsave(filename = "img/scatterplot_raw_alphas_monthly.png",
       plot = scatterplot_raw,
       width = 16,
       height = 9, 
       dpi = 100)

scatterplot_month <- joined_ij |> 
  ggplot(aes(x = values.itoj, 
             y = values.jtoi, 
             col = type))+
  geom_point(alpha = 0.01)+
  theme_minimal()+
  labs(title = "Monthly model",
       x = expression(alpha~"i to j"),
       y = expression(alpha~"j to i"))+
  theme(axis.title = element_text(size = 14))+
  facet_wrap(type~.)+
  theme(legend.position = "none")
scatterplot_month

ggsave(filename = "img/scatterplot_facetted_alphas_monthly.png",
       plot = scatterplot_month,
       width = 16,
       height = 9, 
       dpi = 100)

hist_month <- joined_ij |> 
  pivot_longer(cols = values.itoj:values.jtoi,
               names_to = "relation",
               values_to = "values") |> 
  mutate(relation = str_remove(relation, "values.")) |> 
  ggplot(aes(x = values, fill = relation))+
  geom_histogram(bins = 100)+
  facet_wrap(type~., scales = "free")+
  theme_minimal()
hist_month

ggsave(filename = "img/histogram_alphas_monthly.png",
       plot = hist_month,
       width = 16,
       height = 9, 
       dpi = 100)

## Reading the weekly model output
alphas_week <- vroom::vroom("data-products/geo-hexes/mixedmodel/alphas_weekly_regardless_rt-reformat.csv")

alphas_week <- alphas_week |> 
  ## Writing the hexbin code as a 4-digit number
  mutate(hex_i = sprintf("%04d", i),
         hex_j = sprintf("%04d", j)) |> 
  ## Creating a 8-digit hexbin code to identifying uniquely them
  mutate(i_to_j = str_c(hex_i, hex_j),
         j_to_i = str_c(hex_j, hex_i))

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

joined_ij_week <- inner_join(alphas_i_to_j_week, 
                             alphas_j_to_i_week, 
                             by = c("i_to_j" = "j_to_i", 
                                    "date_week", "type"), 
                             suffix = c(".itoj", ".jtoi"))

scatterplot_raw_week <- joined_ij_week |> 
  filter(type == "raw") |> 
  ggplot(aes(x = values.itoj, 
             y = values.jtoi))+
  geom_point(alpha = 0.01)+
  theme_minimal()+
  labs(title = "Weekly model",
       x = expression(alpha~"i to j"),
       y = expression(alpha~"j to i"))+
  theme(axis.title = element_text(size = 14))+
  theme(legend.position = "none")
scatterplot_raw_week

ggsave(filename = "img/scatterplot_raw_alphas_weekly.png",
       plot = scatterplot_raw_week,
       width = 16,
       height = 9, 
       dpi = 100)

scatterplot_weekly <- joined_ij_week |> 
  ggplot(aes(x = values.itoj, 
             y = values.jtoi, 
             col = type))+
  geom_point(alpha = 0.01)+
  theme_minimal()+
  labs(title = "Weekly model",
       x = expression(alpha~"i to j"),
       y = expression(alpha~"j to i"))+
  theme(axis.title = element_text(size = 14))+
  facet_wrap(type~.)+
  theme(legend.position = "none")
scatterplot_weekly

ggsave(filename = "img/scatterplot_facetted_alphas_weekly.png",
       plot = scatterplot_weekly,
       width = 16,
       height = 9, 
       dpi = 100)

hist_weekly <- joined_ij_week |> 
  pivot_longer(cols = values.itoj:values.jtoi,
               names_to = "relation",
               values_to = "values") |> 
  mutate(relation = str_remove(relation, "values.")) |> 
  ggplot(aes(x = values, fill = relation))+
  geom_histogram(bins = 1000)+
  facet_wrap(type~., scales = "free")+
  theme_minimal()
hist_weekly

ggsave(filename = "img/histogram_alphas_weekly.png",
       plot = hist_weekly,
       width = 16,
       height = 9, 
       dpi = 100)


# 
# 
# 
# joined_i_to_j_j_to_i |> 
#   filter(alpha_normalizedi_to_j > 0, 
#          alpha_normalizedj_to_i > 0) |> 
#   ggplot(aes(x = alpha_normalizedi_to_j, y = alpha_normalizedj_to_i))+
#   geom_point(alpha = 0.01) +
#   theme_minimal()
# 
# 
# 
# |>
#   pivot_longer(cols = c(alpha, alpha_normalized),
#                names_to = "normalized",
#                values_to = "alphas") |> 
#   mutate(normalized = case_when(normalized == "alpha" ~ "non-normalized",
#                                 normalized == "alpha_normalized" ~ "normalized")) |>
#   # filter(alphas != -1, alphas != 1) |> 
#   ggplot(aes(x = alphas, fill = normalized))+
#   geom_histogram(binwidth = .1)+
#   # geom_density()+
#   theme_minimal()+
#   labs(title = "monthly model")+
#   lims(x = c(-15,15))
# 
# alphas_week |> 
#   mutate(i_j = str_c(i, j, sep = "_")) |> 
#   pivot_longer(cols = c(alpha, alpha_normalized),
#                names_to = "normalized",
#                values_to = "alphas") |> 
#   mutate(normalized = if_else(normalized == "alpha", "non-normalized", "normalized")) |> 
#   ggplot(aes(x = alphas, fill = normalized))+
#   geom_histogram(binwidth = .1)+
#   # geom_density(alpha = .3)+
#   theme_minimal()+
#   labs(title = "weekly model")+
#   lims(x = c(-15,15))
# 
# alphas_week_regardless|> 
#   mutate(i_j = str_c(i, j, sep = "_")) |> 
#   pivot_longer(cols = c(alpha, alpha_normalized),
#                names_to = "normalized",
#                values_to = "alphas") |> 
#   mutate(normalized = if_else(normalized == "alpha", "non-normalized", "normalized")) |> 
#   ggplot(aes(x = alphas, fill = normalized))+
#   geom_histogram(binwidth = .1)+
#   # geom_density(alpha = .3)+
#   theme_minimal()+
#   labs(title = "weekly model", subtitle = "regardles Rt")+
#   lims(x = c(-15,15))
# 
# ## Processing to compare non-normalized/normalized alphas
# alphas_i_month <- alphas_month |> 
#   reframe(alpha_i = sum(alpha), 
#           alpha_i_normalized = sum(alpha_normalized),
#           .by = c(i, month)) |>
#   pivot_longer(cols = -"month",
#                names_to = "alphas",
#                values_to = "values")
# 
# alphas_j_month <- alphas_month |> 
#   reframe(alpha_j = sum(alpha), 
#           alpha_j_normalized = sum(alpha_normalized),
#           .by = c(j,month)) |>
#   pivot_longer(cols = -"month",
#                names_to = "alphas",
#                values_to = "values")
# 
# ## Rejoining i and j alphas to have a wider version
# alphas_month_rejoined <- bind_rows(alphas_i_month, alphas_j_month) |> 
#   separate_wider_delim(alphas, delim = "_", 
#                        names = c("alpha", "hex", "norm"), 
#                        too_few = "align_start",
#                        too_many = "merge") |> 
#   mutate(norm = ifelse(is.na(norm), "non-normalized", norm))
# 
# alphas_months_plt <- alphas_month_rejoined |> 
#   ggplot(aes(x = values, fill = hex))+
#   geom_vline(xintercept = 0)+
#   geom_histogram(bins = 100)+
#   facet_wrap(norm~.)+
#   lims(x = c(-20, 50))+
#   theme_minimal()+
#   labs(title = "Monthly model")
# alphas_months_plt
# 
# ## Weeks
# alphas_i_week <- alphas_week |> 
#   reframe(alpha_i = sum(alpha), 
#           alpha_i_normalized = sum(alpha_normalized),
#           .by = c(i, week)) |> 
#   select(-i) |> 
#   pivot_longer(cols = -"week", 
#                names_to = "alphas", 
#                values_to = "values")
# 
# alphas_j_week <- alphas_week |> 
#   reframe(alpha_j = sum(alpha), 
#           alpha_j_normalized = sum(alpha_normalized),
#           .by = c(j,week)) |> 
#   select(-j) |> 
#   pivot_longer(cols = -"week", 
#                names_to = "alphas", 
#                values_to = "values")
# 
# alphas_week_rejoined <- bind_rows(alphas_i_week, alphas_j_week) |> 
#   separate_wider_delim(alphas, delim = "_", 
#                        names = c("alpha", "hex", "norm"), 
#                        too_few = "align_start",
#                        too_many = "merge") |> 
#   mutate(norm = ifelse(is.na(norm), "non-normalized", norm))
# 
# alphas_weeks_plt <- alphas_week_rejoined |> 
#   ggplot(aes(x = values, fill = hex))+
#   geom_histogram(bins = 100)+
#   geom_vline(xintercept = 0, linewidth = .5)+
#   facet_wrap(norm~.)+
#   lims(x = c(-50, 70))+
#   theme_minimal()+
#   labs(title = "Weekly basis model")
# alphas_weeks_plt
# 
