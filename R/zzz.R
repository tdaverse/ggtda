# installed versions of {simplextree} and {ripserr}, or `NA` if not installed
.onLoad <- function(...) {
  run_on_load()
}
on_load(
  .simplextree_version <-
    if ("simplextree" %in% rownames(utils::installed.packages())) {
      utils::packageVersion("simplextree")
    } else NA_character_
)
on_load(
  .ripserr_version <-
    if ("ripserr" %in% rownames(utils::installed.packages())) {
      utils::packageVersion("ripserr")
    } else NA_character_
)
