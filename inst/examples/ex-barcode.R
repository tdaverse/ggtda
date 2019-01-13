#####EXAMPLE 1#####
# toy example
toy.data <- data.frame(
  appear = c(0, 0, 1, 2),
  disappear = c(5, 3, 5, 3),
  dim = c("0", "0", "2", "1")
)

# topological barcode using the stat layer
ggplot(toy.data,
       aes(xmin = appear, xmax = disappear, colour = dim, shape = dim)) +
  stat_barcode()

# topological barcode using the geom layer (and minimalist theme)
ggplot(toy.data,
       aes(x = appear, xend = disappear, colour = dim, shape = dim)) +
  geom_barcode() +
  theme_tda()

#####EXAMPLE 2#####
# load library and dataset for comprehensive example
library("TDAstats")
data("circle2d") # unit circle (betti-1 number = 1)

# calculate persistence homology and format
circ.phom <- as.data.frame(calculate_homology(circle2d))
circ.phom$dimension <- as.factor(circ.phom$dimension)

# pretty topological barcode with stat layer
ggplot(circ.phom, aes(xmin = birth, xmax = death,
                      colour = dimension)) +
  stat_barcode() +
  theme_barcode()

# pretty topological barcode with geom layer
ggplot(circ.phom, aes(x = birth, xend = death,
                      colour = dimension)) +
  geom_barcode() +
  theme_barcode()
