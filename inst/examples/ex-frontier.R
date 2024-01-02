# toy example
toy.data <- data.frame(
  birth = c(0, 0, 1, 2, 1.5),
  death = c(5, 3, 5, 3, 6),
  dim = c("0", "0", "2", "1", "1")
)
# diagonal persistence diagram with frontier
ggplot(toy.data,
       aes(start = birth, end = death, colour = dim, shape = dim)) +
  theme_persist() +
  coord_equal() +
  stat_persistence() +
  stat_frontier() +
  lims(x = c(0, NA), y = c(0, NA))
# persistence frontier
ggplot(toy.data,
       aes(start = birth, end = death, colour = dim, shape = dim)) +
  theme_persist() +
  coord_equal() +
  stat_frontier(diagram = "landscape") +
  lims(x = c(0, NA), y = c(0, NA))

# load library and generate dataset for comprehensive example
library("ripserr")
# noisy unit circle (Betti-1 number = 1)
n <- 100L; sd <- 0.1
set.seed(7)
t <- stats::runif(n = n, min = 0, max = 2*pi)
annulus.df <- data.frame(
  x = cos(t) + stats::rnorm(n = n, mean = 0, sd = sd),
  y = sin(t) + stats::rnorm(n = n, mean = 0, sd = sd)
)
# calculate persistence homology and format
annulus.phom <- as.data.frame(vietoris_rips(annulus.df))
annulus.phom$dimension <- as.factor(annulus.phom$dimension)
# pretty landscape persistence diagram
ggplot(annulus.phom, aes(start = birth, end = death,
                         shape = dimension, colour = dimension)) +
  stat_frontier(diagram = "landscape") +
  theme_persist()
