# toy example
toy.data <- data.frame(
  birth = c(0, 0, 1, 2, 1.5),
  death = c(5, 3, 5, 3, 6),
  dim = c("0", "0", "2", "1", "1")
)

# persistence landscape for each dimension
ggplot(toy.data, aes(start = birth, end = death)) +
  coord_fixed() +
  facet_wrap(~ dim) +
  stat_landscape()
