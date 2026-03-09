# 100 points at random intervals on S1
set.seed(1); angles <- runif(100, 0, 2 * pi)
circle2d <- cbind(x = cos(angles), y = sin(angles)) 

# persistence diagram with landscape overlaid
ggplot(df_pointcloud, aes(dataset = data, colour = after_stat(dimension))) +
  coord_equal() +
  geom_persistence() +
  geom_landscape(diagram = "diagonal", na.rm = TRUE) +
  scale_color_brewer(type = "qual", palette = 2) +
  labs(x = "Birth", y = "Death") +
  guides(alpha = "none")

# persistence landscape with diagram overlaid
ggplot(df_pointcloud, aes(dataset = data, colour = after_stat(dimension))) +
  coord_equal() +
  geom_landscape(na.rm = TRUE) +
  geom_persistence(diagram = "landscape") +
  scale_color_brewer(type = "qual", palette = 2) +
  guides(alpha = "none")

