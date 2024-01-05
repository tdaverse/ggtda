
# generate a noisy 2D circle
set.seed(2)
theta <- stats::runif(n = 40L, min = 0, max = 2*pi)
d <- data.frame(x = cos(theta) + stats::rnorm(40L, 0, .15),
                y = sin(theta) + stats::rnorm(40L, 0, .15))
r <- 1/3

# overlay ball cover and Vietoris-Rips complex with points
ggplot(d, aes(x = x, y = y)) +
  theme_bw() +
  coord_fixed() +
  geom_disk(radius = r, fill = "aquamarine3") +
  geom_simplicial_complex(
    radius = r, fill = "darkgoldenrod",
    complex = "Vietoris", engine = "base"
  ) +
  geom_point()
# use the Čech complex instead
ggplot(d, aes(x = x, y = y)) +
  theme_bw() +
  coord_fixed() +
  geom_disk(radius = r, fill = "aquamarine3") +
  geom_simplicial_complex(
    radius = r, fill = "darkgoldenrod",
    complex = "Cech", engine = "base"
  ) +
  geom_point()
