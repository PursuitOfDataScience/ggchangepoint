#' \code{ggchangepoint} package
#'
#' Combines Changepoint Anaysis with 'ggplot2'.
#'
#' @docType package
#' @name ggchangepoint
#' @import utils
NULL


if(getRversion() >= "2.15.1")  utils::globalVariables(c("cp",
                                                        ".",
                                                        "cp_value",
                                                        "raw_value"))
