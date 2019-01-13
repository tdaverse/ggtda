#####EXAMPLE 1#####
# toy example
toy.data <- data.frame(
  birth = c(0, 0, 1, 2),
  death = c(5, 3, 5, 3),
  dim = c("0", "0", "2", "1")
)

# flat persistence diagram using the stat layer
ggplot(toy.data,
       aes(start = birth, end = death, colour = dim, shape = dim)) +
  stat_flat()

# diagonal persistence diagram using the geom layer
ggplot(toy.data,
       aes(start = birth, end = death, colour = dim, shape = dim)) +
  geom_persist()

# flat persistence diagram using the geom layer with minimalist theme
ggplot(toy.data,
       aes(start = birth, end = death, colour = dim, shape = dim)) +
  geom_persist(stat = "flat") +
  theme_tda()

#####EXAMPLE 2#####
# load library and dataset for comprehensive example
library("TDAstats")
data("annulus2d") # noisy unit circle (betti-1 number = 1)

# calculate persistence homology and format
annulus.phom <- as.data.frame(calculate_homology(annulus2d))
annulus.phom$dimension <- as.factor(annulus.phom$dimension)

# pretty flat persistence diagram
ggplot(annulus.phom, aes(start = birth, end = death,
                         shape = dimension, colour = dimension)) +
  stat_flat() +
  theme_persist()

# pretty diagonal persistence diagram
ggplot(annulus.phom, aes(start = birth, end = death,
                         shape = dimension, colour = dimension)) +
  geom_persist() +
  theme_persist()

# another way to get a pretty flat persistence diagram
ggplot(annulus.phom, aes(start = birth, end = death,
                         shape = dimension, colour = dimension)) +
  geom_persist(stat = "flat") +
  theme_persist()
