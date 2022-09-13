# query installed version of {simplextree} (`NULL` if not installed)
on_load(
  .simplextree_version <-
    if ("simplextree" %in% rownames(utils::installed.packages())) {
      utils::packageVersion("simplextree")
    }
)
.onLoad <- function(...) {
  run_on_load()
}
