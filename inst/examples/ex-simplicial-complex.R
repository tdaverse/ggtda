
set.seed(1)
s <- seq(0, 2*pi, length.out = 40)
df <- data.frame(
  x = cos(s) + rnorm(40, 0, .1),
  y = sin(s) + rnorm(40, 0, .1)
)

# default, visualizing dimension w/ alpha:
ggplot(df, aes(x, y)) +
  coord_fixed() +
  geom_simplicial_complex(radius = .4)
# visualizing dimension w/ fill:
ggplot(df, aes(x, y)) +
  coord_fixed() +
  geom_simplicial_complex(
    mapping = aes(fill = after_stat(dimension)),
    alpha = .5, radius = .4
  )

# with a zero radius or diameter
ggplot(df, aes(x = x, y = y)) +
  coord_fixed() +
  stat_simplicial_complex(radius = 0)
# with a too-small radius or diameter
ggplot(df, aes(x = x, y = y)) +
  coord_fixed() +
  stat_simplicial_complex(radius = 0.01)

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
