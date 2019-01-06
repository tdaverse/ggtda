## save a data frame containing 100 points forming an annulus (noisy unit
##   circle) used as an example in vignettes, etc.

# reproducibility
set.seed(7)

# create points around unit circle
angles <- runif(100, 0, 2 * pi)
x.val <- cos(angles)
y.val <- sin(angles)

# add noise
x.val <- x.val + runif(100, -0.2, 0.2)
y.val <- y.val + runif(100, -0.2, 0.2)

# create dataset and add to package using devtools
annulus2d <- cbind(x.val, y.val)
devtools::use_data(annulus2d)