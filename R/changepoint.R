#' Changepoint wrapper
#'
#' This function wraps a number of \code{cpt} functions from the changepoint
#' package and the \code{cpt.np()} function from the changepoint.np package.
#' It is handy that users can use this function to get the same changepoint
#' results as these functions output individually. Moreover, it returns a tibble
#' that inherits the tidyverse sytle. Functions from the changepoint package do
#' require data normality assumption by default, yet changepoint.np is a
#' non-parametric way to detect changepoints and let data speak by itself.
#' If user sets \code{change_in} as \code{cpt_np}, a seed should be set before
#' using the function for the sake of reproducibility. For more details on the
#' changepoint and changepoint.np packages, please refer to their documentation.
#'
#' @param data A vector.
#' @param change_in Choice of \code{mean_var}, \code{mean}, \code{var}, and
#'   \code{cpt_np}. Each choice corresponds to \code{cpt.meanvar()},
#'   \code{cpt.mean()}, \code{cpt.var()} and \code{cpt.np()} respectively. The
#'   default is \code{mean_var}.
#' @param cp_method A wide range of choices (i.e., \code{AMOC}, \code{PELT},
#'   \code{SegNeigh} or \code{BinSeg}). Please note when \code{change_in} is
#'   \code{cpt_np}, \code{PELT} is the only option.
#' @param ... Extra arguments for each \code{cpt} function mentioned in the
#'   \code{change_in} section.
#'
#' @return A tibble includes which point(s) is/are the changepoint along with
#'   raw changepoint value corresponding to that changepoint.
#' @import changepoint
#' @import changepoint.np
#' @import tibble
#' @import Rdpack
#' @references
#' \insertRef{killick2014changepoint}{ggchangepoint}
#' @export
#'
#' @examples
#' set.seed(2022)
#' cpt_wrapper(c(rnorm(100,0,1),rnorm(100,0,10)))
#' cpt_wrapper(c(rnorm(100,0,1),rnorm(100,10,1)))
#'


cpt_wrapper <- function(data,
                        change_in = "mean_var",
                        cp_method = "PELT",
                        ...){

  if (change_in == "mean_var"){

    return(tibble::tibble(cp = changepoint::cpts(changepoint::cpt.meanvar(data,
                                                                          method = cp_method,
                                                                          ...)),
                          cp_value = data[cp]))
  }

  else if (change_in == "mean"){
    return(tibble::tibble(cp = changepoint::cpts(changepoint::cpt.mean(data,
                                                                       method = cp_method,
                                                                       ...)),
                          cp_value = data[cp]))
  }

  else if (change_in == "var"){
    return(tibble::tibble(cp = changepoint::cpts(changepoint::cpt.var(data,
                                                                      method = cp_method,
                                                                      ...)),
                          cp_value = data[cp]))

  }

  else if (change_in == "cpt_np"){
    return(tibble::tibble(cp = changepoint::cpts(changepoint.np::cpt.np(data,
                                                                        method = cp_method,
                                                                        ...)),
                          cp_value = data[cp]))
  }
  else {
    stop("Invalid Changepoint Method, must be mean_var, mean or var.")
  }
}



#' Plot for the changepoint package
#'
#' The plot for changepoints detected by the changepoint package is a line plot
#' for the raw data and the vertical lines representing each changepoint. The
#' x-axis is the row number of the raw data in the original data vector. The
#' plot inherits ggplot2, meaning users can add ggplot2 functions on top the
#' changepoint plot for customization.
#'
#' @inheritParams  cpt_wrapper
#' @param cptline_alpha The value of alpha for the vertical changepoint line(s),
#'   default is 1, meaning no transparency.
#' @param cptline_color The color for the vertical changepoint line(s), default
#'   is \code{blue}.
#' @param cptline_type The linetype for the vertical changepoint line(s),
#'   default is \code{solid}.
#' @param cptline_size The size for the vertical changepoint line(s), default is
#'   \code{0.5}.
#'
#' @return A line plot with data points along with the vertical lines
#'   representing changepoints.
#' @export
#' @import ggplot2
#' @import dplyr
#' @import tibble
#' @examples
#' ggcptplot(c(rnorm(100,0,1),rnorm(100,0,10)))
#' ggcptplot(c(rnorm(100,0,1),rnorm(100,10,1)))
#'


ggcptplot <- function(data,
                      change_in = "mean_var",
                      cp_method = "PELT",
                      ...,
                      cptline_alpha = 1,
                      cptline_color = "blue",
                      cptline_type = "solid",
                      cptline_size = 0.5){

  # join the changepoint data and raw data together

  joined_data <- data %>%
    tibble::tibble(raw_value = .) %>%
    dplyr::mutate(row_number = dplyr::row_number()) %>%
    dplyr::left_join(
      cpt_wrapper(data, change_in, cp_method, ...),
      by = c("row_number" = "cp")
    )
  # filter out the row numbers that do not have changepoint

  cp_data <- joined_data %>%
    dplyr::filter(!is.na(cp_value))

  # constructing the changepoint plot

  ggplot2::ggplot(data = joined_data,
                  ggplot2::aes(row_number, raw_value)) +
    ggplot2::geom_line() +
    ggplot2::geom_point() +
    ggplot2::geom_linerange(data = cp_data,
                            ggplot2::aes(x = row_number,
                                         ymin = -Inf,
                                         ymax = cp_value),
                            alpha = cptline_alpha,
                            color = cptline_color,
                            linetype = cptline_type,
                            size = cptline_size)
}









