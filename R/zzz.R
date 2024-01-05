# installed versions of {simplextree} and {ripserr}, or `NULL` if not installed
on_load(
  .simplextree_version <-
    if ("simplextree" %in% rownames(utils::installed.packages())) {
      utils::packageVersion("simplextree")
    }
)
on_load(
  .ripserr_version <-
    if ("ripserr" %in% rownames(utils::installed.packages())) {
      utils::packageVersion("ripserr")
    }
)
.onLoad <- function(...) {
  run_on_load()
}
