#####EXAMPLE 1#####

# toy example
toy.data <- data.frame(
  appear = c(0, 0, 0, 1, 2),
  disappear = c(5, 3, 3, 5, 3),
  dim = c("0", "0", "0", "2", "1")
)
# topological barcode using the geom layer (and minimalist theme)
ggplot(toy.data,
       aes(start = appear, end = disappear, colour = dim, shape = dim)) +
  geom_barcode() +
  theme_barcode()

#####EXAMPLE 2#####
# load library and dataset for comprehensive example
library("ripserr")
angles <- runif(100, 0, 2 * pi)
circle2d <- cbind(cos(angles), sin(angles)) # unit circle (Betti-1 number = 1)
# calculate persistence homology and format
circ.phom <- as.data.frame(vietoris_rips(circle2d))
circ.phom$dimension <- as.factor(circ.phom$dimension)
# pretty topological barcode with geom layer
ggplot(circ.phom, aes(start = birth, end = death,
                      colour = dimension)) +
  geom_barcode() +
  theme_barcode()
