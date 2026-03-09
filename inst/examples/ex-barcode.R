theme_set(theme_barcode())

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
  geom_barcode(linewidth = 1) +
  scale_color_brewer(type = "qual", palette = 2)

# Alternatively, plot pre-computed filtration with `birth` and `death` aesthetics:
df_phom <- as.data.frame(ripserr::vietoris_rips(circle2d))
df_phom$dimension <- ordered(df_phom$dimension, levels = c(0, 1))

ggplot(df_phom, aes(birth = birth, death = death, color = dimension)) +
  geom_barcode(linewidth = 1) +
  scale_color_brewer(type = "qual", palette = 2)

# Specify how infinite features are plotted with `infinity_break`:
ggplot(df_pointcloud, aes(dataset = data, colour = after_stat(dimension))) +
  geom_vline(xintercept = 2, linetype = "dashed") +
  geom_barcode(linewidth = 1, infinity_break = 2) +
  scale_color_brewer(type = "qual", palette = 2)

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
# Avoid empty space by specifying `scales = "free_y"` and `space = "free_y"`
ggplot(df_pointcloud_n, aes(dataset = data, colour = after_stat(dimension))) +
  geom_barcode(linewidth = 1) +
  scale_color_brewer(type = "qual", palette = 2) +
  facet_grid(rows = vars(n), scales = "free_y", space = "free_y")

