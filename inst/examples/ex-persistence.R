#####EXAMPLE 1#####
# toy example
toy.data <- data.frame(
  birth = c(0, 0, 1, 2, 1.5),
  death = c(5, 3, 5, 3, 6),
  dim = c("0", "0", "2", "1", "1")
)
# flat persistence diagram
ggplot(toy.data,
       aes(start = birth, end = death, colour = dim, shape = dim)) +
  theme_tda() +
  stat_persistence() +
  lims(x = c(0, NA), y = c(0, NA))
# diagonal persistence diagram with frontier
ggplot(toy.data,
       aes(start = birth, end = death, colour = dim, shape = dim)) +
  theme_tda() + coord_equal() +
  stat_persistence(diagram = "diagonal") +
  stat_frontier(diagram = "diagonal") +
  lims(x = c(0, NA), y = c(0, NA))
# landscape persistence frontier
ggplot(toy.data,
       aes(start = birth, end = death, colour = dim, shape = dim)) +
  theme_tda() + coord_equal() +
  stat_frontier(diagram = "landscape") +
  lims(x = c(0, NA), y = c(0, NA))

#####EXAMPLE 2#####
# load library and dataset for comprehensive example
library("TDAstats")
data("annulus2d")  # noisy unit circle (Betti-1 number = 1)
# calculate persistence homology and format
annulus.phom <- as.data.frame(calculate_homology(annulus2d))
annulus.phom$dimension <- as.factor(annulus.phom$dimension)
# pretty flat persistence diagram
ggplot(annulus.phom, aes(start = birth, end = death,
                         shape = dimension, colour = dimension)) +
  stat_persistence(diagram = "flat") +
  theme_persist()
# pretty diagonal persistence diagram
ggplot(annulus.phom, aes(start = birth, end = death,
                         shape = dimension, colour = dimension)) +
  stat_persistence(diagram = "diagonal") +
  theme_persist()
# pretty landscape persistence diagram
ggplot(annulus.phom, aes(start = birth, end = death,
                         shape = dimension, colour = dimension)) +
  stat_frontier(diagram = "landscape") +
  theme_persist()
