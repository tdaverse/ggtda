# toy example
toy.data <- data.frame(
  birth = c(0, 0, 1, 2, 1.5),
  death = c(5, 3, 5, 3, 6),
  dim = c("0", "0", "2", "1", "1")
)

# Basic visualization of persistence
ggplot(toy.data, aes(birth = birth, death = death, color = dim, shape = dim)) +
  geom_persistence(size = 3) +
  geom_abline(intercept = 0, slope = 1) +
  coord_equal(xlim = c(0, 6), ylim = c(0, 6)) 

# Stylized visualization, mapping persistence to alpha (opacity)
ggplot(toy.data, aes(birth = birth, death = death, color = dim, shape = dim)) +
  geom_persistence(aes(alpha = after_stat(persistence)), size = 3) +
  geom_abline(intercept = 0, slope = 1) +
  coord_equal(xlim = c(0, 6), ylim = c(0, 6)) +
  guides(alpha = "none") +
  theme_persist() +
  labs(
    x = "Birth",
    y = "Death",
    color = "Dimension",
    shape = "Dimension"
  )

# diagonal persistence diagram with fundamental boxes
ggplot(toy.data, aes(birth = birth, death = death, color = dim, shape = dim)) +
  geom_persistence(aes(alpha = after_stat(persistence)), size = 3) +
  geom_fundamental_box(t = c(1.5, 5.5), color = "goldenrod", fill = "goldenrod") +
  geom_abline(intercept = 0, slope = 1) +
  coord_equal(xlim = c(0, 6), ylim = c(0, 6)) +
  guides(alpha = "none") +
  theme_persist() +
  labs(
    x = "Birth",
    y = "Death",
    color = "Dimension",
    shape = "Dimension"
  )

# flat persistence diagram, mapping dimension to numeral
ggplot(toy.data, aes(birth = birth, death = death)) +
  geom_persistence(diagram = "flat") +
  geom_label(aes(label = dim), stat = "persistence", diagram = "flat", nudge_y = .15) +
  guides(alpha = "none") +
  theme_persist() +
  labs(
    x = "Birth",
    y = "Persistence"
  )