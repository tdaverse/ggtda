
# this is silly but a good test
raw_data <- data.frame(obj = I(list(eurodist, 10*swiss, Nile)))
raw_data$class <- vapply(raw_data$obj, class, "")
ggplot(raw_data, aes(dataset = obj)) +
  stat_persistence(aes(color = factor(after_stat(dimension)), shape = class))
