
# toy example
toy.data <- data.frame(
  birth = c(0, 0, 1, 2, 1.5),
  death = c(5, 3, 5, 3, 6),
  dim = c("0", "0", "2", "1", "1")
)
# diagonal persistence diagram, coding persistence to transparency
ggplot(toy.data,
       aes(start = birth, end = death, colour = dim, shape = dim)) +
  theme_persist() +
  coord_equal() +
  stat_persistence(aes(alpha = after_stat(persistence)),
                   diagram = "diagonal", size = 3) +
  geom_abline(intercept = 0, slope = 1) +
  lims(x = c(0, 6), y = c(0, 6)) +
  guides(alpha = "none")
# diagonal persistence diagram with fundamental boxes
ggplot(toy.data,
       aes(start = birth, end = death, colour = dim, shape = dim)) +
  theme_persist() +
  coord_equal() +
  stat_persistence() +
  geom_abline(intercept = 0, slope = 1) +
  geom_fundamental_box(t = c(1.5, 5.5),
                       color = "goldenrod", fill = "goldenrod") +
  lims(x = c(0, 6), y = c(0, 6)) +
  guides(alpha = "none")
# flat persistence diagram, coding dimension to numeral
ggplot(toy.data,
       aes(start = birth, end = death, label = dim)) +
  theme_persist() +
  stat_persistence(diagram = "flat", geom = "text") +
  lims(x = c(0, NA), y = c(0, NA))

## Experimental examples to test new functionality

ggplot(toy.data, aes(start = birth, end = death, colour = dim, shape = dim)) +
  theme_persist() +
  coord_equal() +
  stat_persistence(
    geom = "text",
    aes(label = after_stat(feature_id), alpha = after_stat(persistence)),
    diagram = "flat", size = 3
  ) +
  guides(alpha = "none")

ggplot(toy.data, aes(start = birth, end = death, color = dim)) +
  theme_persist() + coord_equal() +
  stat_persistence(geom = "frontier")

ggplot(toy.data, aes(start = birth, end = death, color = dim)) +
  theme_persist() + coord_equal() +
  stat_persistence(geom = "frontier", diagram = "flat")

ggplot(toy.data, aes(start = birth, end = death, color = dim)) +
  theme_persist() + coord_equal() +
  stat_persistence(geom = "frontier", diagram = "landscape")

# this is silly but a good test
raw_data <- data.frame(obj = I(list(eurodist, 10*swiss, Nile)))
raw_data$class <- vapply(raw_data$obj, class, "")
ggplot(raw_data, aes(dataset = obj)) +
  stat_persistence(aes(shape = factor(after_stat(dimension)), color = class))
