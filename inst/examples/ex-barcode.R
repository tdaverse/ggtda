#####EXAMPLE 1#####

# toy example
toy.data <- data.frame(
  appear = c(0, 0, 0, 1, 2),
  disappear = c(5, 3, 3, 5, 3),
  dim = c("0", "0", "0", "2", "1")
)
# topological barcode using the geom layer (and minimalist theme)
ggplot(toy.data,
       aes(start = appear, end = disappear, colour = dim, shape = dim)) +
  geom_barcode() +
  theme_barcode()

#####EXAMPLE 2#####
# load library and dataset for comprehensive example
library("ripserr")
angles <- runif(100, 0, 2 * pi)
circle2d <- cbind(cos(angles), sin(angles)) # unit circle (Betti-1 number = 1)
# calculate persistence homology and format
circ.phom <- as.data.frame(vietoris_rips(circle2d))
circ.phom$dimension <- as.factor(circ.phom$dimension)
# pretty topological barcode with geom layer
ggplot(circ.phom, aes(start = birth, end = death,
                      colour = dimension)) +
  geom_barcode() +
  theme_barcode()

#####EXAMPLE 3#####
# point cloud with features at different scales
set.seed(1255)
beads <- replicate(12L, {
  angles <- runif(20L, 0, 2 * pi)
  cbind(cos(angles), sin(angles)) * 1/6
}, simplify = FALSE)
knots <- cbind(cos(2 * pi * seq(12L) / 12), sin(2 * pi * seq(12L) / 12))
necklace <- do.call(rbind, lapply(seq(12L), function(i) {
  sweep(beads[[i]], 2L, knots[i, , drop = FALSE], "+")
}))
plot(necklace, asp = 1, pch = 16, cex = .5)
necklace.phom <- as.data.frame(vietoris_rips(necklace, dim = 1L))
necklace.phom$dimension <- factor(necklace.phom$dimension)
# conventional barcode plot
ggplot(necklace.phom,
       aes(xmin = birth, xmax = death, color = dimension, shape = dimension)) +
  theme_barcode() +
  geom_barcode()
# barcode plot on logarithmic scale (requires `xmin` and `xmax` aesthetics)
ggplot(necklace.phom,
       aes(xmin = birth, xmax = death, color = dimension, shape = dimension)) +
  theme_barcode() +
  geom_barcode() +
  scale_x_continuous(trans = "log") +
  coord_cartesian(xlim = c(.05, 1))
