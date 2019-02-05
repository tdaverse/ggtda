# toy example
toy.data <- data.frame(
  birth = c(0, 0, 1, 2, 1.5),
  death = c(5, 3, 5, 3, 6),
  dim = c("0", "0", "2", "1", "1")
)

# compare the plots below to their analogues using `geom_persist()`

# persistence landscape for each dimension
ggplot(toy.data, aes(start = birth, end = death)) +
  coord_fixed() +
  facet_wrap(~ dim) +
  stat_landscape()

# combined persistence frontier
ggplot(toy.data, aes(start = birth, end = death)) +
  coord_fixed() +
  stat_frontier()

# grouped persistence frontier
ggplot(toy.data, aes(start = birth, end = death, color = dim)) +
  coord_fixed() +
  stat_frontier()

# overlay landscape with frontier
ggplot(toy.data, aes(start = birth, end = death, color = dim)) +
  coord_fixed() +
  stat_landscape(linetype = "dashed") +
  stat_frontier()

# SUGGESTION: `stat_identity/landscape()` w/ `geom_point/segment()`
