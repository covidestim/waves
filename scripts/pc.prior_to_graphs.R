# Pre-simulation codes 
rm(list=ls(all=TRUE))

##--------- Load all packages
suppressPackageStartupMessages(library(sf))
suppressPackageStartupMessages(library(sp))
suppressPackageStartupMessages(library(spdep))
suppressPackageStartupMessages(library(INLA))
suppressPackageStartupMessages(library(ggplot2))
suppressPackageStartupMessages(library(dplyr))
suppressPackageStartupMessages(library(patchwork))
suppressPackageStartupMessages(library(cowplot))


##--------- Load shapefile of hexgrid shapefile (7661)
hexes_sf <- st_read("data-products/geo-hexes/hexes.shp") |> 
  filter(as.integer(hexid) < 7662)
hexes_sp <- as(hexes_sf, "Spatial")

##--------- Generate neighboorhood graphs
# W1 - Queen's row-standardized weight matrix. 
W1.nb <- poly2nb(hexes_sf)
nb2INLA("data-products/geo-hexes/W1.nb_graph",W1.nb)
W1.nb_graph <- inla.read.graph(filename="data-products/geo-hexes/W1.nb_graph")

# W2 is based on neighbors falling within the interval (0, 70km]. 
coords <- coordinates(hexes_sp)
W2.nb <- dnearneigh(coords, d1= 0, d2 = 70, longlat = TRUE)
nb2INLA("data-products/geo-hexes/W2.nb_graph",W2.nb)
W2.nb_graph <-inla.read.graph(filename="data-products/geo-hexes/W2.nb_graph")

# W3 is based on neighbors falling within the interval $(0,140km]$ resulting in a denser weight matrix. 
W3.nb <-  dnearneigh(coords, 0, 140, longlat = TRUE)
nb2INLA("data-products/geo-hexes/W3.nb_graph",W3.nb)
W3.nb_graph<-inla.read.graph(filename="data-products/geo-hexes/W3.nb_graph")

# Plot of neighborhood graphs
p1 <- image(inla.graph2matrix(W1.nb_graph), main = "Spatial Graph 1", axes = FALSE)
p2 <- image(inla.graph2matrix(W2.nb_graph), main = "Spatial Graph 2", axes = FALSE)
p3 <- image(inla.graph2matrix(W3.nb_graph), main = "Spatial Graph 3", axes = FALSE)
cowplot::plot_grid(p1, p2, p3, nrow=1)

##--------- Setting parameter values
n <- nrow(GHmap_sf) 
beta <- c(2, 0.25, -2)
phi <- c(0.30, 0.90)
tau <- c(4/9, 4)

##--------- Simulating design matrix 
set.seed(7380)
X1 <- round(runif(n, 1, 10),3)
X2 <- round(log(runif(n, 1, 2)),3)
X <- cbind(rep(1, n), X1, X2)

##--------- Save parameters along with in an existing folder
list.sim.par <- list(phi = phi, 
                     W = list(W1.nb, W2.nb, W3.nb), 
                     tau = tau)

save(list.sim.par, beta, X,
     file=paste("/Users/soms/Documents/TEXfiles/PhDThesis/Codes/Simulation/Param.RData"))


##--------- Graph PC priors for the precision and for the mixing parameter
prec <- seq(0.001, 4, by = 0.001)
alpha <- c(0.90, 0.50, 0.10)
pc.prec <- lapply(alpha, function(a) {
  inla.pc.dprec(prec, u=1, lambda=a)
})
pc.prec <- do.call(c, pc.prec)
tab <- data.frame(prec = rep(prec, length(alpha)), pc.prec = pc.prec,
                  alpha = rep(as.character(alpha), each = length(prec)))

ggplot(tab, aes(x = prec, y = pc.prec, linetype = alpha, color = alpha)) + 
  ylim(0,3) + geom_line(na.rm=TRUE, linewidth = 1.5) +
  xlab(expression(tau)) + ylab("density") +
  labs(color = expression(alpha), linetype = expression(alpha)) +
  scale_linetype_discrete(name = expression(alpha)) + theme_bw() + 
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        axis.text = element_text(size = 25),
        axis.title = element_text(size = 30),
        axis.ticks.length = unit(0.3, "cm"),
        panel.border = element_rect(linewidth = 1.5),
        legend.text = element_text(size = 20),
        legend.title = element_text(size = 25),
        legend.key.size = unit(2, "cm"),
        legend.key.width = unit(2,"cm"),
        legend.position = "top",
        legend.box.background = element_rect(colour = "black", linewidth = 1.5)) -> p1

phi = seq(0.0001, 0.9999, by=0.0001)
alpha <- c(0.90, 0.50, 0.10)
pc <- lapply(alpha, function(a) {
  INLA:::inla.pc.bym.phi(graph=W1.nb_graph, u=0.5, alpha=a)(phi)
})
pc <- do.call(c, pc)
tab <- data.frame(phi = rep(phi, length(alpha)), pc = pc,
                  alpha = rep(as.character(alpha), each = length(phi)))

ggplot(tab, aes(x = phi, y = pc, linetype = alpha, colour = alpha)) + 
  geom_line(linewidth = 1.5) +
  xlab(expression(paste(phi))) +
  ylab("density") +
  scale_linetype_discrete(name = expression(alpha)) + 
  labs(color  = expression(alpha), linetype = expression(alpha)) + 
  theme_bw() + 
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        axis.text = element_text(size = 25),
        axis.title = element_text(size = 30),
        axis.ticks.length = unit(0.3, "cm"),
        panel.border = element_rect(linewidth = 1.5),
        legend.text = element_text(size = 20),
        legend.title = element_text(size = 25),
        legend.key.size = unit(2, "cm"),
        legend.key.width = unit(2,"cm"),
        legend.position = "top",
        legend.box.background = element_rect(colour = "black", linewidth = 1.5)) -> p2

phi = seq(0.0001, 0.9999, by=0.0001)
alpha <- c(0.90, 0.50, 0.10)
pc <- lapply(alpha, function(a) {
  INLA:::inla.pc.bym.phi(graph=W2.nb_graph, u=0.5, alpha=a)(phi)
})
pc <- do.call(c, pc)
tab <- data.frame(phi = rep(phi, length(alpha)), pc = pc,
                  alpha = rep(as.character(alpha), each = length(phi)))

ggplot(tab, aes(x = phi, y = pc, linetype = alpha, colour = alpha)) + 
  geom_line(linewidth = 1.5) +
  xlab(expression(paste(phi))) +
  ylab("density") +
  scale_linetype_discrete(name = expression(alpha)) + 
  labs(color  = expression(alpha), linetype = expression(alpha)) + 
  theme_bw() + 
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        axis.text = element_text(size = 25),
        axis.title = element_text(size = 30),
        axis.ticks.length = unit(0.3, "cm"),
        panel.border = element_rect(linewidth = 1.5),
        legend.text = element_text(size = 20),
        legend.title = element_text(size = 25),
        legend.key.size = unit(2, "cm"),
        legend.key.width = unit(2,"cm"),
        legend.position = "top",
        legend.box.background = element_rect(colour = "black", linewidth = 1.5)) -> p3

phi = seq(0.0001, 0.9999, by=0.0001)
alpha <- c(0.90, 0.50, 0.10)
pc <- lapply(alpha, function(a) {
  INLA:::inla.pc.bym.phi(graph=W3.nb_graph, u=0.5, alpha=a)(phi)
})
pc <- do.call(c, pc)
tab <- data.frame(phi = rep(phi, length(alpha)), pc = pc,
                  alpha = rep(as.character(alpha), each = length(phi)))

ggplot(tab, aes(x = phi, y = pc, linetype = alpha, colour = alpha)) + 
  geom_line(linewidth = 1.5) +
  xlab(expression(paste(phi))) +
  ylab("density") +
  scale_linetype_discrete(name = expression(alpha)) + 
  labs(color  = expression(alpha), linetype = expression(alpha)) + 
  theme_bw() + 
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        axis.text = element_text(size = 25),
        axis.title = element_text(size = 30),
        axis.ticks.length = unit(0.3, "cm"),
        panel.border = element_rect(linewidth = 1.5),
        legend.text = element_text(size = 20),
        legend.title = element_text(size = 25),
        legend.key.size = unit(2, "cm"),
        legend.key.width = unit(2,"cm"),
        legend.position = "top",
        legend.box.background = element_rect(colour = "black", linewidth = 1.5)) -> p4

p1

plot_grid(p2, p3, p4, ncol = 3)