
# this is silly but a good test
raw_data <- data.frame(obj = I(list(eurodist, 10*swiss, Nile)))
raw_data$class <- vapply(raw_data$obj, class, "")
# exclude time series data if {ripserr} v0.1.1 is installed
if ("ripserr" %in% rownames(utils::installed.packages()) &&
    utils::packageVersion("ripserr") == "0.1.1")
  raw_data[c(1L, 2L), ]
# persistence diagram
ggplot(raw_data, aes(dataset = obj)) +
  stat_persistence(aes(color = factor(after_stat(dimension)), shape = class))
# persistence landscape
ggplot(raw_data, aes(dataset = obj)) +
  facet_wrap(facets = vars(class), scales = "free") +
  stat_landscape(aes(color = factor(after_stat(dimension)))) +
  theme(legend.position = "bottom")
