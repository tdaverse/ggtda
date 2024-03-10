
# regular septagon
sevenths <- (seq_len(7) - 1) * 2 * pi / 7
septagon <- data.frame(x = cos(sevenths), y = sin(sevenths))
ggplot(septagon, aes(x, y)) +
  geom_simplicial_complex(
    diameter = 1.6, dimension_max = 5L, one_simplices = "all",
    engine = "simplextree",
    aes(color = after_stat(factor(dimension)),
        fill = after_stat(factor(dimension))),
    alpha = .5
  )

# regular pentagon
fifths <- (seq_len(5) - 1) * 2 * pi / 5
pentagon <- data.frame(x = cos(fifths), y = sin(fifths))
ggplot(pentagon, aes(x, y)) +
  geom_simplicial_complex(
    diameter = 2, dimension_max = 5L, one_simplices = "all",
    engine = "simplextree",
    aes(color = after_stat(factor(dimension)),
        fill = after_stat(factor(dimension))),
    alpha = .5
  )
