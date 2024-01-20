
# equilateral triangle
equilateral_triangle <- 
  data.frame(x = cos(2*pi*c(0,1/3,2/3)), y = sin(2*pi*c(0,1/3,2/3)))
# small perturbations from key values
eps <- .00000001

# Vietoris-Rips
ggplot(equilateral_triangle, aes(x, y)) +
  coord_fixed() +
  geom_simplicial_complex(diameter = sqrt(3) - eps)
ggplot(equilateral_triangle, aes(x, y)) +
  coord_fixed() +
  geom_simplicial_complex(diameter = sqrt(3) + eps)

# Čech
ggplot(equilateral_triangle, aes(x, y)) +
  coord_fixed() +
  geom_simplicial_complex(complex = "Cech", diameter = sqrt(3) - eps)
ggplot(equilateral_triangle, aes(x, y)) +
  coord_fixed() +
  geom_simplicial_complex(complex = "Cech", diameter = sqrt(3) + eps)

# alpha
ggplot(equilateral_triangle, aes(x, y)) +
  coord_fixed() +
  geom_simplicial_complex(complex = "alpha", diameter = sqrt(3) - eps)
ggplot(equilateral_triangle, aes(x, y)) +
  coord_fixed() +
  geom_simplicial_complex(complex = "alpha", diameter = sqrt(3) + eps)
