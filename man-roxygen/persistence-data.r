#' @section Persistence data:
#'
#'   Persistence data encode the values of an underlying parameter
#'   \eqn{\epsilon} at which topological features appear ("birth") and disappear
#'   ("death"). The difference between the birth and the death of a feature is
#'   called its "persistence". As topological features may be of different
#'   dimensions, persistence data sets usually also include the dimension of
#'   each feature.
#' 
#' Persistence data can be specified in two ways in **ggtda**:
#' 
#' 1. **The `birth` and `death` aesthetics**. Pre-computed persistence homologies
#' can be supplied directly to layer functions in **ggtda**, the columns of `data`
#' with the birth and death values for each feature must be mapped to the 
#' `birth` and `death` aesthetics, respectively.
#' 
#' 2. **The `dataset` aesthetic**. Instead of a pre-computed persistence homology,
#'  **ggtda** will compute the persistence homologies for arbitrary data sets.
#'  This requires the use of a nested `tibble` with list columns, the `data` 
#'  provided to either `ggplot()` or the layer function should be a `tibble` 
#'  with a column whose entries are data sets in a format that one of the 
#'  **ggtda** engines supports. This list column must be mapped to the `dataset` aesthetic.
#' 
#' See the examples included below or
#' \code{vignette("grouped-list-data", package = "ggtda")} for an extended
#' discussion of the `dataset` aesthetic's use.
#' 
#' @template ref-chazal2017
