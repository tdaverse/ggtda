
# function to generate noisy 2D circles
make_noisy_circle <- function(n, sd = .01) {
  theta <- stats::runif(n = n, min = 0, max = 2*pi)
  cbind(x = cos(theta) + stats::rnorm(n, 0, sd),
        y = sin(theta) + stats::rnorm(n, 0, sd))
}

# generate a noisy 2D circle
set.seed(1)
d <- as.data.frame(make_noisy_circle(n = 36L, sd = .15))
r <- 1/6

# plot disks beneath points
ggplot(d, aes(x = x, y = y)) +
  theme_bw() +
  coord_fixed() +
  geom_disk(radius = r, fill = "aquamarine3") +
  geom_point()

# use `diameter` parameter
ggplot(d, aes(x = x, y = y)) +
  theme_bw() +
  coord_fixed() +
  geom_disk(diameter = 2*r, fill = "aquamarine3") +
  geom_point()
