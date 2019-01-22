#####EXAMPLE 1#####
# toy example
toy.data <- data.frame(
  birth = c(0, 0, 1, 2, 1.5),
  death = c(5, 3, 5, 3, 6),
  dim = c("0", "0", "2", "1", "1")
)

# persistence diagram
ggplot(toy.data,
       aes(start = birth, end = death, colour = dim, shape = dim)) +
  geom_persistence()
# flat persistence diagram using the `stat_flat` layer method
ggplot(toy.data,
       aes(start = birth, end = death, colour = dim, shape = dim)) +
  geom_persistence(stat = "flat")
ggplot(toy.data,
       aes(start = birth, end = death, colour = dim, shape = dim)) +
  stat_flat(geom = "persistence")
# flat persistence diagram using the `flat = TRUE` parameter method
ggplot(toy.data,
       aes(start = birth, end = death, colour = dim, shape = dim)) +
  geom_persistence(flat = TRUE)

# flat persistence diagram using the geom layer with minimalist theme
ggplot(toy.data,
       aes(start = birth, end = death, colour = dim, shape = dim)) +
  geom_persistence(stat = "flat") +
  theme_tda()

#####EXAMPLE 2#####
# load library and dataset for comprehensive example
library("TDAstats")
data("annulus2d") # noisy unit circle (betti-1 number = 1)

# calculate persistence homology and format
annulus.phom <- as.data.frame(calculate_homology(annulus2d))
annulus.phom$dimension <- as.factor(annulus.phom$dimension)

# pretty diagonal persistence diagram
ggplot(annulus.phom, aes(start = birth, end = death,
                         shape = dimension, colour = dimension)) +
  geom_persistence() +
  theme_persist()

# another way to get a pretty flat persistence diagram
ggplot(annulus.phom, aes(start = birth, end = death,
                         shape = dimension, colour = dimension)) +
  geom_persistence(stat = "flat") +
  theme_persist()
ggplot(annulus.phom, aes(start = birth, end = death,
                         shape = dimension, colour = dimension)) +
  geom_persistence(flat = TRUE) +
  theme_persist()
