#' @title Base `ggproto` classes for *ggtda*
#' 
#' @name ggtda-ggproto
#' @seealso [`ggplot2::ggplot2-ggproto`]
#' @keywords internal
NULL

#' A *ggplot2* Extension for Persistence Data
#'
#' *ggtda* is a user-friendly *ggplot2* extension for visualizing data arising
#' from topological data analysis, including persistence data and topological
#' constructions on planar data.
#'
#' @import ggplot2
#' @name ggtda
#' @docType package
NULL

# installed versions of {simplextree} and {ripserr}, or `NULL` if not installed
.onLoad <- function(...) {
  rlang::run_on_load()
}
rlang::on_load(
  .simplextree_version <-
    if ("simplextree" %in% rownames(utils::installed.packages())) {
      utils::packageVersion("simplextree")
    } else NA_character_
)
rlang::on_load(
  .ripserr_version <-
    if ("ripserr" %in% rownames(utils::installed.packages())) {
      utils::packageVersion("ripserr")
    } else NA_character_
)
