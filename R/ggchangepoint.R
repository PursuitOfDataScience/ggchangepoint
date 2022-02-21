#' \code{ggchangepoint} package
#'
#' Combines Changepoint Anaysis with 'ggplot2'.
#'
#' ggchangepoint tries to offer several changepoint R packages in a tidy
#' format and output the ggplot2 plots so that the tidyverse users can gain some
#' familiarity to work with the changepoint analysis. For the moment, I only
#' include three changepoint packages ('changepoint', 'changepoint.np' and 'ecp'
#' ). More changepoint packages will be included as time progresses.
#'
#' @docType package
#' @name ggchangepoint
#' @import utils
NULL


if(getRversion() >= "2.15.1")  utils::globalVariables(c("cp",
                                                        ".",
                                                        "cp_value",
                                                        "raw_value"))
