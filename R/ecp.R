#' ecp wrapper
#'
#' The ecp package provides a non-parametric way to detect changepoints. Unlike
#' the changepoint package, it does not assume raw data to have any formal
#' distribution. This wrapper function wraps two functions from the ecp package,
#' i.e., \code{e.divisive()} and \code{e.agglo()}. Users can use either function
#' by switching the \code{algorithm} argument. Before using the wrapper function
#' , seed should be set for the sake of reproducibility.
#'
#' @param data A vector.
#' @param algorithm Either \code{divisive} or \code{agglo}. \code{divisive} is
#'   the default.
#' @param min_size Minimum number of observations between change points. By
#'   default is 2. This argument is only applied when \code{algorithm =
#'   "divisive"}.
#' @param ... Extra arguments to pass on either from \code{e.divisive()} or
#'   \code{e.agglo()}.
#'
#' @return A tibble includes which point(s) is/are the changepoint along with
#'   raw changepoint value corresponding to that changepoint.
#' @import tibble
#' @import ecp
#' @import Rdpack
#' @references
#' \insertRef{james2013ecp}{ggchangepoint}
#' @export
#'
#' @examples
#' set.seed(2022)
#' ecp_wrapper(c(rnorm(100,0,1),rnorm(100,0,10)))
#' ecp_wrapper(c(rnorm(100,0,1),rnorm(100,10,1)))
#'


ecp_wrapper <- function(data,
                        algorithm = "divisive",
                        min_size = 2,
                        ...){
  if (algorithm == "divisive") {

    cp <- ecp::e.divisive(as.matrix(data), min.size = min_size, ...)$estimates

    if (length(cp) > 2) {

      cp <- cp[2 : (length(cp) - 1)]
    }

    return(tibble::tibble(cp = cp,
                          cp_value = data[cp]))

  }
  else if (algorithm == "agglo") {

    cp <- ecp::e.agglo(as.matrix(data), ...)$estimates

    if (length(cp) > 2) {

      cp <- cp[2 : (length(cp) - 1)]
    }

    return(tibble::tibble(cp = cp,
                          cp_value = data[cp]))

  }

  else {

    stop("Invalid algorithm, must be divisive or agglo.")

    }

}






#' Plot for the ecp package
#'
#' The plot for changepoints detected by the ecp package is a line plot for the
#' raw data and the vertical lines representing each changepoint. The x-axis is
#' the row number of the raw data in the original data vector. The plot inherits
#' ggplot2, meaning users can add ggplot2 functions on top the changepoint plot
#' for customization.
#'
#' @inheritParams ecp_wrapper
#' @inheritParams ggcptplot
#'
#' @return A line plot with data points along with the vertical lines
#'   representing changepoints.
#' @export
#'
#' @examples
#' ggecpplot(c(rnorm(100,0,1),rnorm(100,0,10)))
#' ggecpplot(c(rnorm(100,0,1),rnorm(100,0,10)))


ggecpplot <- function(data,
                      algorithm = "divisive",
                      min_size = 2,
                      ...,
                      cptline_alpha = 1,
                      cptline_color = "blue",
                      cptline_type = "solid",
                      cptline_size = 0.5){


  joined_data <- data %>%
    tibble::tibble(raw_value = .) %>%
    dplyr::mutate(row_number = dplyr::row_number()) %>%
    dplyr::left_join(
      ecp_wrapper(data, algorithm, min_size, ...),
      by = c("row_number" = "cp")
    )

  cp_data <- joined_data %>%
    dplyr::filter(!is.na(cp_value))

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
