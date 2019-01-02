# toy example
data <- data.frame(
  birth = c(0, 0, 1, 2),
  death = c(5, 3, 5, 3),
  dim = c("0", "0", "2", "1")
)
ggplot(data,
       aes(xmin = birth, xmax = death, group = dim, color = dim)) +
  stat_barcode()
