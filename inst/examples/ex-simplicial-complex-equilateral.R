
# equilateral triangle
et <- data.frame(x = cos(2*pi*c(0,1/3,2/3)), y = sin(2*pi*c(0,1/3,2/3)))
# small perturbations from key values
eps <- .00000001

# Vietoris-Rips
ggplot(et, aes(x, y)) +
  coord_fixed() +
  geom_simplicial_complex(diameter = sqrt(3) - eps)
ggplot(et, aes(x, y)) +
  coord_fixed() +
  geom_simplicial_complex(diameter = sqrt(3) + eps)

# Čech
ggplot(et, aes(x, y)) +
  coord_fixed() +
  geom_simplicial_complex(complex = "Cech", diameter = sqrt(3) - eps)
ggplot(et, aes(x, y)) +
  coord_fixed() +
  geom_simplicial_complex(complex = "Cech", diameter = sqrt(3) + eps)

# alpha
ggplot(et, aes(x, y)) +
  coord_fixed() +
  geom_simplicial_complex(complex = "alpha", diameter = sqrt(3) - eps)
ggplot(et, aes(x, y)) +
  coord_fixed() +
  geom_simplicial_complex(complex = "alpha", diameter = sqrt(3) + eps)
