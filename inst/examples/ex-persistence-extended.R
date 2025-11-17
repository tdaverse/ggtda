
# toy extended persistence data, adapted from Carriere & Oudot (2015)
eph.data <- data.frame(
  dimension = c(0L, 1L, 0L, 1L),
  part = c("Ord", "Rel", "Ext+", "Ext–"),
  birth = c(1, 9, 1, 8),
  death = c(5, 7, 11, 3)
)
# extended persistence diagram
ggplot(eph.data,
       aes(start = birth, end = death, color = factor(dimension))) +
  theme_persist() +
  coord_equal() +
  stat_persistence(aes(shape = part), size = 3) +
  geom_abline(intercept = 0, slope = 1) +
  lims(x = c(0, 11), y = c(0, 11)) +
  labs(color = "Dimension", shape = "Homology")
# extended barcode
ggplot(eph.data,
       aes(start = birth, end = death, color = factor(dimension))) +
  theme_barcode() +
  geom_barcode(aes(linetype = part))
