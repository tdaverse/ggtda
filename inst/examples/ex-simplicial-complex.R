
# function to generate noisy 2D circles
make_noisy_circle <- function(n, sd = .01) {
  theta <- stats::runif(n = n, min = 0, max = 2*pi)
  cbind(x = cos(theta) + stats::rnorm(n, 0, sd),
        y = sin(theta) + stats::rnorm(n, 0, sd))
}

# generate a noisy 2D circle
set.seed(1)
d <- as.data.frame(make_noisy_circle(n = 40, sd = .15))

# plot balls beneath points
ggplot(d, aes(x = x, y = y)) +
  theme_bw() +
  coord_fixed() +
  stat_disk(radius = .35, fill = "aquamarine3") +
  geom_point()

# plot Vietoris 1-skeleton
ggplot(d, aes(x = x, y = y)) +
  theme_bw() +
  coord_fixed() +
  stat_vietoris1(diameter = .7, alpha = .25) +
  stat_vietoris0()

# plot Vietoris 2-skeleton
ggplot(d, aes(x = x, y = y)) +
  theme_bw() +
  coord_fixed() +
  stat_vietoris2(diameter = .7, fill = "darkgoldenrod") +
  stat_vietoris1(diameter = .7, alpha = .25) +
  stat_vietoris0()

# plot Čech 2-skeleton
ggplot(d, aes(x = x, y = y)) +
  theme_bw() +
  coord_fixed() +
  stat_cech2(diameter = .7, fill = "darkgoldenrod") +
  stat_cech1(diameter = .7, alpha = .25) +
  stat_cech0()

# plot Vietoris 1-skeleton atop balls
ggplot(d, aes(x = x, y = y)) +
  theme_bw() +
  coord_fixed() +
  stat_disk(radius = .35, fill = "aquamarine3") +
  stat_vietoris1(diameter = .7, size = .2) +
  stat_vietoris0(size = .3)

# plot Vietoris 2-skeleton atop balls
ggplot(d, aes(x = x, y = y)) +
  theme_bw() +
  coord_fixed() +
  stat_disk(radius = .35, fill = "aquamarine3") +
  stat_vietoris2(diameter = .7, fill = "darkgoldenrod") +
  stat_vietoris1(diameter = .7, size = .2) +
  stat_vietoris0(size = .3)

# plot Čech 2-skeleton atop balls
ggplot(d, aes(x = x, y = y)) +
  theme_bw() +
  coord_fixed() +
  stat_disk(radius = .35, fill = "aquamarine3") +
  stat_cech2(diameter = .7, fill = "darkgoldenrod") +
  stat_cech1(diameter = .7, size = .2) +
  stat_cech0(size = .3)
