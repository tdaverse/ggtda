set.seed(1)

s <- seq(0, 2*pi, length.out = 40)
df <- data.frame(
  x = cos(s) + rnorm(40, 0, .1),
  y = sin(s) + rnorm(40, 0, .1)
)

# default, visualizing dim w/ alpha:
ggplot(df, aes(x, y)) +
  geom_simplicial_complex(radius = .4)

# visualizing dim w/ fill:
ggplot(df, aes(x, y)) +
  geom_simplicial_complex(
    mapping = aes(fill = after_stat(dim)),
    alpha = .5, radius = .4
  )

# Visualizing multiple groups together
s <- c(s, s)
df_mix <- data.frame(
  x = cos(s) + rnorm(80, 0, .1),
  y = sin(s) + rnorm(80, 0, .1)
)

df_mix$x <- df_mix$x + rep(c(-2, 2), length.out = 80)
df_mix$lab <- rep(c("a", "b"), length.out = 80)

ggplot(df_mix, aes(x, y, fill = lab)) +
  geom_simplicial_complex(radius = .4)

# FIXME: incorporate or remove old examples below

# function to generate noisy 2D circles
make_noisy_circle <- function(n, sd = .01) {
  theta <- stats::runif(n = n, min = 0, max = 2*pi)
  cbind(x = cos(theta) + stats::rnorm(n, 0, sd),
        y = sin(theta) + stats::rnorm(n, 0, sd))
}

# generate a noisy 2D circle
set.seed(1)
d <- as.data.frame(make_noisy_circle(n = 40, sd = .15))
r <- 1/3

# plot balls beneath points
ggplot(d, aes(x = x, y = y)) +
  theme_bw() +
  coord_fixed() +
  stat_disk(radius = r, fill = "aquamarine3") +
  geom_point()

# plot Vietoris 1-skeleton
ggplot(d, aes(x = x, y = y)) +
  theme_bw() +
  coord_fixed() +
  stat_vietoris1(diameter = 2*r, alpha = .25) +
  stat_vietoris0()

# plot Vietoris 2-skeleton
ggplot(d, aes(x = x, y = y)) +
  theme_bw() +
  coord_fixed() +
  stat_vietoris2(diameter = 2*r, fill = "darkgoldenrod") +
  stat_vietoris1(diameter = 2*r, alpha = .25) +
  stat_vietoris0()

# plot Čech 2-skeleton
ggplot(d, aes(x = x, y = y)) +
  theme_bw() +
  coord_fixed() +
  stat_cech2(diameter = 2*r, fill = "darkgoldenrod") +
  stat_cech1(diameter = 2*r, alpha = .25) +
  stat_cech0()

# plot Vietoris 1-skeleton atop balls
ggplot(d, aes(x = x, y = y)) +
  theme_bw() +
  coord_fixed() +
  stat_disk(radius = r, fill = "aquamarine3") +
  stat_vietoris1(diameter = 2*r, size = .2) +
  stat_vietoris0(size = .3)

# plot alpha 1-skeleton
ggplot(d, aes(x, y)) +
  theme_bw() +
  coord_fixed() +
  stat_alpha2(radius = r, fill = "darkgoldenrod") +
  stat_alpha1(radius = r, size = .2) +
  stat_alpha0()
