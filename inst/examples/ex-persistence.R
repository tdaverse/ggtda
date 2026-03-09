theme_set(theme_persist())

# 100 points at random intervals on S1
set.seed(1); angles <- runif(100, 0, 2 * pi)
circle2d <- cbind(x = cos(angles), y = sin(angles)) 

ggplot(circle2d, aes(x, y)) + 
  geom_point() +
  coord_fixed()

# Tibble with list column containing S1 pointcloud data 
df_pointcloud <- tibble::tibble(data = list(circle2d))

# Compute and plot persistence with `dataset` aesthetic of `geom_persistence()`:
ggplot(df_pointcloud, aes(dataset = data, colour = after_stat(dimension))) +
  geom_persistence(size = 3) +
  geom_abline(intercept = 0, slope = 1) +
  coord_fixed(xlim = c(0, 3), ylim = c(0, 3))  +
  scale_color_brewer(type = "qual", palette = 2) +
  labs(x = "Birth", y = "Death")

# Alternatively, plot pre-computed filtration with `birth` and `death` aesthetics:
df_phom <- as.data.frame(ripserr::vietoris_rips(circle2d))
df_phom$dimension <- ordered(df_phom$dimension, levels = c(0, 1))

ggplot(df_phom, aes(birth = birth, death = death, color = dimension)) +
  geom_persistence(size = 3) +
  geom_abline(intercept = 0, slope = 1) +
  coord_fixed(xlim = c(0, 3), ylim = c(0, 3))  +
  scale_color_brewer(type = "qual", palette = 2) +
  labs(x = "Birth", y = "Death")

# Emphasize regions of interest with `geom_fundamental_box()`:
ggplot(df_phom, aes(birth = birth, death = death, color = dimension)) +
  geom_fundamental_box(t = c(.5, 1.5), color = "goldenrod", fill = "goldenrod") +
  geom_persistence(size = 3) +
  geom_abline(intercept = 0, slope = 1) +
  coord_fixed(xlim = c(0, 3), ylim = c(0, 3))  +
  scale_color_brewer(type = "qual", palette = 2) +
  labs(x = "Birth", y = "Death")
  

# Specify how infinite features are plotted with `infinity_break`:
ggplot(df_pointcloud, aes(dataset = data, colour = after_stat(dimension))) +
  geom_hline(yintercept = 3, linetype = "dashed") +
  geom_persistence(size = 3, infinity_break = 3) +
  geom_abline(intercept = 0, slope = 1) +
  coord_fixed(xlim = c(0, 3), ylim = c(0, 3))  +
  scale_color_brewer(type = "qual", palette = 2) +
  labs(x = "Birth", y = "Death")

# Further specify with computed variable `after_stat(infinite)`:
ggplot(df_pointcloud, aes(dataset = data, colour = after_stat(dimension))) +
  geom_hline(yintercept = 3, linetype = "dashed") +
  geom_persistence(aes(shape = after_stat(infinite)), infinity_break = 3, size = 3, stroke = 2) +
  geom_abline(intercept = 0, slope = 1) +
  scale_shape_manual(values = c("TRUE" = 8, "FALSE" = 20)) +
  scale_color_brewer(type = "qual", palette = 2) +
  coord_fixed(xlim = c(0, 3), ylim = c(0, 3))  +
  guides(shape = "none") +
  labs(x = "Birth", y = "Death")


# The `dataset` aesthetic allows for the simultaneous plotting of multiple persistences
sim_s2 <- function(n) {
  angles <- runif(n, 0, 2 * pi)
  cbind(x = cos(angles), y = sin(angles)) 
}

set.seed(1)
df_pointcloud_n <- tibble::tibble(
  n = c(10, 20, 40),
  data = lapply(n, sim_s2)
)

# One facet per `dataset`
ggplot(df_pointcloud_n, aes(dataset = data, colour = after_stat(dimension))) +
  geom_persistence(size = 3) +
  geom_abline(intercept = 0, slope = 1) +
  coord_fixed(xlim = c(0, 3), ylim = c(0, 3))  +
  scale_color_brewer(type = "qual", palette = 2) +
  facet_wrap(vars(n)) +
  labs(x = "Birth", y = "Death")

# Plotting together, observe how the persistence of the 1-dimensional feature increases with n
ggplot(df_pointcloud_n, aes(dataset = data, color = factor(n), shape = after_stat(as.character(dimension)))) +
  geom_persistence(size = 3) +
  geom_abline(intercept = 0, slope = 1) +
  scale_color_brewer(type = "seq", palette = 'RdPu') +
  coord_fixed(xlim = c(0, 3), ylim = c(0, 3))  +
  labs(x = "Birth", y = "Death", shape = "Dimension", color = "N")


