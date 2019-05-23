# Metric Entropy

library("MCMCpack")
library("ggplot2")
library("dplyr")

width = 4

# visualization of the entropy of a dirichlet distribution
alpha = c(3, 5, 3)

# use 3d x to plot in 2d as the third dimension is degenerate as x1 + x2 + x3 = 1
x = expand.grid(x1 = seq(0, 1, 0.01), x2 = seq(0, 1, 0.01))
x[,3] = 1 - x[,1] - x[,2]

# compute density
dens = apply(x, 1, ddirichlet, alpha = alpha)
density_data = data.frame(x1 = x[,1], x2 = x[,2], x3 = x[,3], z = dens)

# compute entropy
# helper function
Beta = function(alpha) {
  prod(gamma(alpha)) / gamma(sum(alpha))
}
# Shannon entropy of dirichlet
dir_ent = function(alpha) {
  alpha0 = sum(alpha)
  K = length(alpha)
  log(Beta(alpha)) + (alpha0 - K) * digamma(alpha0) - sum((alpha - 1) * digamma(alpha))
}
dir_ent(alpha)

# Renyi entropy of 3d (i.e. 2d) dirichlet
# alpha is 3d parameter of gamma distribution, lambda is parameter of renyi entropy
dir_renyi = function (alpha, lambda) {
  G_lambda = gamma(sum(alpha))^lambda * prod(gamma(lambda * alpha - lambda + 1)) /
    (gamma(lambda * (sum(alpha) - 3) + 3) * (prod(gamma(alpha))^lambda))
  return(log(G_lambda) / (1 - lambda))
}
# test
dir_renyi(alpha, 2)

# compute renyi entropy for different parameters
q = c(0.01, 0.5, 0.99, 1.01, 2, 10)
dirichlet_renyi_q = sapply(q, dir_renyi, alpha = alpha)
dirichlet_renyi_q
round(dirichlet_renyi_q, 3)
# shannon entropy
round(dir_ent(alpha), 3)


# plot density
ggplot(data = density_data) + 
  geom_raster(aes(x = x1, y = x2, fill = z)) +
  scale_fill_gradient2("Density", low = "white", high = "dodgerblue4") + 
  geom_contour(aes(x = x1, y = x2, z = z), color = "black") +
  theme_classic() + 
  coord_fixed(xlim = c(0, 1), ylim = c(0, 1))
ggsave("plots/dirichlet_density.pdf", width = width, height = 0.9 * width)

###########################################################################################
# set seed 1234
set.seed(1234)
# draw a random sample
dir_data = rdirichlet(n = 30, alpha = alpha) %>% as.data.frame()
colnames(dir_data) = c("x1", "x2", "x3")
ggplot(data = dir_data, aes(x = x1, y = x2)) +
  geom_point() + 
  coord_fixed()
ggsave("plots/dirichlet_sample.pdf", width = width, height = 0.9 * width)


library("ggforce")
colnames(dir_data) = c("x1", "x2", "x3")
dir_data$r = 0.1
dir_data$id = 1:nrow(dir_data)

# euclidean covering = 9
selected = c(1, 2, 5, 11, 15, 18, 13, 22, 27)
length(selected)
dir_data$selected = (1:nrow(dir_data)) %in% selected
ggplot() +
  geom_point(aes(x = x1, y = x2), data = dir_data, size = 2) +
  # geom_text(aes(x = x1, y = x2, label = id), data = dir_data, col = "red", size = 5) + 
  geom_circle(aes(x0 = x1, y0 = x2, r = r), 
              color = "dodgerblue4", alpha = 0.5,
              data = filter(dir_data, selected)) +
  coord_fixed(xlim = c(-0.05, 0.75), ylim = c(0, 0.75))
ggsave("plots/l2_covering.pdf", width = width, height = 0.9 * width)


# l1 covering = 12
selected = c(2, 3, 4, 5, 11, 18, 19, 21, 25, 13, 22, 27)
length(selected)
dir_data$selected = (1:nrow(dir_data)) %in% selected
ggplot() +
  geom_point(aes(x = x1, y = x2), data = dir_data, size = 2) +
  # geom_text(aes(x = x1, y = x2, label = id), data = dir_data, col = "red", size = 5) + 
  geom_circle(aes(x0 = x1, y0 = x2, r = r), n = 4, 
              color = "dodgerblue4", alpha = 0.5,
              data = filter(dir_data, selected)) +
  coord_fixed(xlim = c(-0.05, 0.75), ylim = c(0, 0.75))
ggsave("plots/l1_covering.pdf", width = width, height = 0.9 * width)


# l0 covering = 30
ggplot(data = dir_data) +
  geom_hline(aes(yintercept = x2), color = "dodgerblue4", alpha = 0.5) +
  geom_vline(aes(xintercept = x1), color = "dodgerblue4", alpha = 0.5) + 
  geom_point(aes(x = x1, y = x2), size = 2) +
  coord_fixed(xlim = c(-0.05, 0.75), ylim = c(0, 0.75))
ggsave("plots/l0_covering.pdf", width = width, height = 0.9 * width)
















