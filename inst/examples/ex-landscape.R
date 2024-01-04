# toy example
toy.data <- data.frame(
  birth = c(0, 0, 1, 3, 4, 1.5),
  death = c(5, 3, 5, 4, 6, 3),
  dim = factor(c(0, 0, 1, 1, 2, 2))
)
# diagonal persistence diagram with frontier
ggplot(toy.data,
       aes(start = birth, end = death, colour = dim, shape = dim)) +
  theme_persist() +
  coord_equal() +
  stat_landscape(aes(alpha = -after_stat(level)), diagram = "landscape") +
  stat_persistence(diagram = "landscape") +
  lims(x = c(0, NA), y = c(0, NA)) +
  guides(alpha = "none")

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
# pretty diagonal persistence diagram
ggplot(annulus.phom, aes(start = birth, end = death,
                         shape = dimension, colour = dimension)) +
  stat_persistence(diagram = "landscape") +
  theme_persist()
# pretty landscape persistence diagram
ggplot(annulus.phom, aes(start = birth, end = death,
                         shape = dimension, colour = dimension)) +
  stat_landscape(diagram = "landscape") +
  theme_persist()
