# function to generate noisy 2D circles
make_noisy_circle <- function(n, sd = .01) {
  theta <- stats::runif(n = n, min = 0, max = 2 * pi)
  cbind(x = cos(theta), y = sin(theta)) +
    MASS::mvrnorm(n = n, mu = c(0, 0), Sigma = diag(x = sd, nrow = 2))
}

# generate a noisy 2D circle
d <- as.data.frame(make_noisy_circle(n = 360, sd = .02))

# plot skeleton below points
ggplot(d, aes(x = x, y = y)) +
  theme_bw() +
  stat_skeleton(radius = .2, alpha = .25) +
  geom_point()

# plot skeleton atop balls
# ERROR
ggplot(d, aes(x = x, y = y)) +
  theme_bw() +
  ggforce::geom_circle(aes(x0 = x, y0 = y, r = .2)) +
  stat_skeleton(radius = .2, alpha = .25)
