# function to generate noisy 2D circles
make_noisy_circle <- function(n, sd = .01) {
  theta <- stats::runif(n = n, min = 0, max = 2 * pi)
  cbind(x = cos(theta), y = sin(theta)) +
    MASS::mvrnorm(n = n, mu = c(0, 0), Sigma = diag(x = sd, nrow = 2))
}

# generate a noisy 2D circle
set.seed(1)
d <- as.data.frame(make_noisy_circle(n = 36, sd = .05))

# plot balls beneath points
ggplot(d, aes(x = x, y = y)) +
  theme_bw() +
  coord_fixed() +
  stat_disk(radius = .35, fill = "aquamarine3", alpha = .15) +
  geom_point()

# plot 1-skeleton
ggplot(d, aes(x = x, y = y)) +
  theme_bw() +
  coord_fixed() +
  stat_vietoris1(diameter = .7, alpha = .25) +
  geom_point()

# plot 2-skeleton
ggplot(d, aes(x = x, y = y)) +
  theme_bw() +
  coord_fixed() +
  stat_vietoris2(diameter = .7, fill = "darkgoldenrod", alpha = .1) +
  stat_vietoris1(diameter = .7, alpha = .25) +
  geom_point()

# plot 1-skeleton atop balls
ggplot(d, aes(x = x, y = y)) +
  theme_bw() +
  coord_fixed() +
  stat_disk(radius = .35, fill = "aquamarine3", alpha = .15) +
  stat_vietoris1(diameter = .7, size = .2) +
  geom_point(size = .3)

# plot 2-skeleton atop balls
ggplot(d, aes(x = x, y = y)) +
  theme_bw() +
  coord_fixed() +
  stat_disk(radius = .35, fill = "aquamarine3", alpha = .15) +
  stat_vietoris2(diameter = .7, fill = "darkgoldenrod", alpha = .1) +
  stat_vietoris1(diameter = .7, size = .2) +
  geom_point(size = .3)
