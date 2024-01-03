#' @title Persistence landscapes
#'
#' @description Visualize persistence data as a persistence landscape.
#'   

#' @details
#'
#' {*Persistence landscapes*}, anticipated by some alternative coordinatizations
#' of persistence diagrams, were proposed as Lipschitz functions that demarcate
#' the Pareto frontiers of persistence diagrams. They can be averaged over the
#' diagrams obtained from multiple data sets designed or hypothesized to have
#' been generated from the same underlying topological structure.
#'
#' Persistence landscapes do not currently recognize extended persistence data.
#' 

#' @template ref-bubenik2015
#' @template ref-chazal2017

#' @eval rd_sec_aesthetics(
#'   stat_landscape = StatLandscape
#' )

#' @eval rd_sec_computed_vars(
#'   stat = "landscape",
#'   "level" = "position of each frontier, starting from the uppermost."
#' )
