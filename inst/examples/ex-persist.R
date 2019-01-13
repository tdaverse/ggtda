# toy example
data <- data.frame(
  birth = c(0, 0, 1, 2),
  death = c(5, 3, 5, 3),
  dim = c("0", "0", "2", "1")
)
# using the stat layer
ggplot(data,
       aes(start = birth, end = death, colour = dim, shape = dim)) +
  stat_flat()

# using the geom layer
ggplot(data,
       aes(start = birth, end = death, colour = dim, shape = dim)) +
  geom_persist()

# using the geom layer to get flat diagram
ggplot(data,
       aes(start = birth, end = death, colour = dim, shape = dim)) +
  geom_persist(stat = "flat")
