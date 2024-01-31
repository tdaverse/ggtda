
# list-column of data sets to 'dataset' aesthetic
raw_data <- data.frame(obj = I(list(eurodist, 10*swiss, Nile)))
raw_data$class <- vapply(raw_data$obj, class, "")

if ("TDA" %in% rownames(utils::installed.packages())) {
  
  # barcodes
  ggplot(raw_data, aes(dataset = obj)) +
    geom_barcode(aes(color = factor(after_stat(dimension))),
                 engine = "TDA") +
    facet_wrap(facets = vars(class))
  # persistence diagram
  ggplot(raw_data, aes(dataset = obj)) +
    stat_persistence(aes(color = factor(after_stat(dimension)), shape = class),
                     engine = "GUDHI")
  # persistence landscape
  ggplot(raw_data, aes(dataset = obj)) +
    facet_wrap(facets = vars(class), scales = "free") +
    stat_landscape(aes(color = factor(after_stat(dimension))),
                   engine = "Dionysus") +
    theme(legend.position = "bottom")
  
}

if ("ripserr" %in% rownames(utils::installed.packages())) {
  
  # exclude time series data if {ripserr} v0.1.1 is installed
  if (utils::packageVersion("ripserr") == "0.1.1")
    raw_data <- raw_data[c(1L, 2L), ]
  # barcodes
  ggplot(raw_data, aes(dataset = obj)) +
    geom_barcode(aes(color = factor(after_stat(dimension))),
                 engine = "ripserr") +
    facet_wrap(facets = vars(class))
  # persistence diagram
  ggplot(raw_data, aes(dataset = obj)) +
    stat_persistence(aes(color = factor(after_stat(dimension)), shape = class),
                     engine = "ripserr")
  # persistence landscape
  ggplot(raw_data, aes(dataset = obj)) +
    facet_wrap(facets = vars(class), scales = "free") +
    stat_landscape(aes(color = factor(after_stat(dimension))),
                   engine = "ripserr") +
    theme(legend.position = "bottom")
  
}
