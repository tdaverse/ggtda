
# this is silly but a good test
raw_data <- data.frame(obj = I(list(eurodist, 10*swiss, Nile)))
raw_data$class <- vapply(raw_data$obj, class, "")
# exclude time series data if {ripserr} v0.1.1 is installed
if (.ripserr_version == "0.1.1") raw_data <- raw_data[c(1L, 2L), ]
ggplot(raw_data, aes(dataset = obj)) +
  stat_persistence(aes(color = factor(after_stat(dimension)), shape = class))
