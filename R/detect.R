#' Unified changepoint detection dispatcher
#'
#' Runs one or more changepoint detection methods on a sequence and returns
#' a tidy \code{ggcpt} result object. This is the recommended entry point
#' for most users.
#'
#' @param x A numeric vector (the data series).
#' @param method Detection method. One of \code{"pelt"}, \code{"binseg"},
#'   \code{"segneigh"}, \code{"amoc"}, \code{"fpop"}, \code{"wbs"},
#'   \code{"wbs2"}, \code{"not"}, \code{"mosum"}, \code{"idetect"},
#'   \code{"tguh"}, \code{"smuce"}, \code{"hsmuce"}, \code{"gfpop"},
#'   \code{"np"}, \code{"ecp"}, \code{"kcp"}, \code{"cpm"},
#'   \code{"robust"}, \code{"decafs"}, \code{"sn"}, \code{"inspect"},
#'   \code{"sbs"}, \code{"bcp"}, \code{"bocpd"}, \code{"strucchange"},
#'   \code{"segmented"}. Defaults to \code{"pelt"}.
#' @param change_in What to detect change in. One of \code{"mean"},
#'   \code{"var"}, \code{"meanvar"}, \code{"slope"}, \code{"distribution"}.
#'   Defaults to \code{"mean"}.
#' @param penalty Penalty type or value. Either a character string
#'   (\code{"MBIC"}, \code{"BIC"}, \code{"AIC"}, \code{"Hannan-Quinn"})
#'   or a numeric penalty value. Defaults to \code{"MBIC"}. Numeric penalties
#'   are honoured by the functional-pruning methods (\code{fpop},
#'   \code{gfpop}); the \code{changepoint}-based methods expect one of the
#'   character options.
#' @param ... Additional arguments passed to the specific wrapper.
#'
#' @return A \code{ggcpt} object.
#' @export
#'
#' @examples
#' set.seed(2022)
#' x <- c(rnorm(100, 0, 1), rnorm(100, 10, 1))
#' result <- cpt_detect(x, method = "pelt", change_in = "mean")
#' result
#' ggplot2::autoplot(result)
cpt_detect <- function(x,
                       method = "pelt",
                       change_in = "mean",
                       penalty = "MBIC",
                       ...) {

  method <- match.arg(method, c(
    "pelt", "binseg", "segneigh", "amoc", "fpop", "wbs", "wbs2",
    "not", "mosum", "idetect", "tguh", "smuce", "hsmuce", "gfpop",
    "np", "ecp", "kcp", "cpm", "robust", "decafs", "sn",
    "inspect", "sbs", "bcp", "bocpd", "strucchange", "segmented"
  ))

  change_in <- match.arg(change_in, c("mean", "var", "meanvar", "slope", "distribution"))

  validate_data(x)
  data_vec <- as.numeric(x)

  if (method %in% c("pelt", "binseg", "segneigh", "amoc", "np")) {
    ci <- change_in_mapping(change_in)
    cp_method <- switch(method,
      pelt    = "PELT",
      binseg  = "BinSeg",
      segneigh = "SegNeigh",
      amoc    = "AMOC",
      np      = "PELT"
    )
    if (method == "np") ci <- "np"
    # The changepoint package does not implement the MBIC penalty for the
    # Segment Neighbourhood method; fall back to SIC (which it does support)
    # when the user keeps the default penalty.
    if (method == "segneigh" && identical(penalty, "MBIC")) {
      penalty <- "SIC"
    }
    # Pass penalty string directly so upstream changepoint pkg handles it
    res <- wrap_cpt_to_ggcpt(data_vec, ci, cp_method, method, penalty = penalty, ...)
    return(res)
  }

  if (method == "ecp") {
    return(wrap_ecp_to_ggcpt(data_vec, ...))
  }

  # Convert penalty to numeric for methods that need it
  pen_val <- if (is.numeric(penalty)) {
    penalty
  } else if (is.character(penalty) && penalty %in% c("BIC", "SIC", "MBIC", "AIC", "Hannan-Quinn")) {
    cpt_penalty(penalty, n = length(data_vec))
  } else {
    NULL
  }

  result <- switch(method,
    fpop     = fpop_wrapper(data_vec, penalty = pen_val, ...),
    gfpop    = gfpop_wrapper(data_vec, ...),
    wbs      = wbs_wrapper(data_vec, ...),
    wbs2     = wbs2_wrapper(data_vec, ...),
    not      = not_wrapper(data_vec, ...),
    mosum    = mosum_wrapper(data_vec, ...),
    idetect  = idetect_wrapper(data_vec, ...),
    tguh     = tguh_wrapper(data_vec, ...),
    stop("Method '", method, "' not yet implemented in this release. ",
         "It will be added in a future version.", call. = FALSE)
  )

  result
}

change_in_mapping <- function(change_in) {
  switch(change_in,
    mean         = "mean",
    var          = "var",
    meanvar      = "mean_var",
    slope        = "mean",
    distribution = "np"
  )
}

# Internal: describe a penalty argument as a list(type, value)
penalty_descriptor <- function(penalty) {
  if (is.numeric(penalty)) {
    list(type = "Manual", value = as.numeric(penalty))
  } else {
    list(type = as.character(penalty), value = NA_real_)
  }
}

# Internal: wrap cpt_wrapper result into a ggcpt object
wrap_cpt_to_ggcpt <- function(x, change_in, cp_method, method_name = NULL,
                              penalty = "MBIC", ...) {
  data_vec <- as.numeric(x)
  tbl <- cpt_wrapper(data_vec, change_in = change_in, cp_method = cp_method,
                     penalty = penalty, ...)

  data_tbl <- tibble::tibble(index = seq_along(data_vec), value = data_vec)

  if (is.null(method_name)) method_name <- tolower(cp_method)

  if (nrow(tbl) == 0) {
    res <- ggcpt_empty(data_vec, method_name)
    res$change_in <- change_in
    res$penalty <- penalty_descriptor(penalty)
    return(res)
  }

  cp_indices <- sort(tbl$cp)
  segments <- build_segments(data_vec, cp_indices)

  new_ggcpt(
    changepoints = tbl,
    segments = segments,
    data = data_tbl,
    method = method_name,
    change_in = change_in,
    penalty = penalty_descriptor(penalty),
    fit = NULL,
    call = match.call(),
    cp_convention = "left"
  )
}

# Internal: wrap ecp_wrapper result into a ggcpt object
wrap_ecp_to_ggcpt <- function(x, ...) {
  data_vec <- as.numeric(x)
  tbl <- ecp_wrapper(data_vec, ...)

  data_tbl <- tibble::tibble(index = seq_along(data_vec), value = data_vec)

  if (nrow(tbl) == 0) {
    return(ggcpt_empty(data_vec, "ecp"))
  }

  cp_indices <- sort(tbl$cp)
  # Normalize ecp convention (first index of right segment) to
  # changepoint convention (last index of left segment)
  cp_indices_left <- cp_indices - 1
  cp_indices_left <- cp_indices_left[cp_indices_left > 0]

  if (length(cp_indices_left) == 0) {
    return(ggcpt_empty(data_vec, "ecp"))
  }

  cp_value_left <- data_vec[cp_indices_left]
  segments <- build_segments(data_vec, cp_indices_left)

  cp_tbl <- tibble::tibble(cp = cp_indices_left, cp_value = cp_value_left)

  new_ggcpt(
    changepoints = cp_tbl,
    segments = segments,
    data = data_tbl,
    method = "ecp",
    change_in = "distribution",
    penalty = list(type = "permutation", value = NA_real_),
    fit = NULL,
    call = match.call(),
    cp_convention = "left"
  )
}

#' Construct changepoint penalties
#'
#' Helper to construct standard penalty values for use with changepoint
#' detection methods. Returns a numeric penalty value.
#'
#' @param type Penalty type: \code{"None"}, \code{"BIC"} (or \code{"SIC"}),
#'   \code{"MBIC"}, \code{"AIC"}, \code{"Hannan-Quinn"}, \code{"sSIC"}, or
#'   \code{"Manual"}.
#' @param n Series length. Required for BIC, MBIC, AIC, Hannan-Quinn, sSIC.
#' @param k Number of parameters per changepoint (typically 2 for
#'   mean+variance, 1 for mean-only). Defaults to 1.
#' @param value Numeric value for \code{Manual} type.
#'
#' @return A numeric penalty value.
#' @export
#'
#' @examples
#' cpt_penalty("BIC", n = 100)
#' cpt_penalty("AIC", n = 100)
#' cpt_penalty("Manual", value = 5)
cpt_penalty <- function(type, n = NULL, k = 1, value = NULL) {
  type <- match.arg(type, c("None", "BIC", "SIC", "MBIC", "AIC", "Hannan-Quinn", "sSIC", "Manual"))

  if (type == "None") return(0)
  if (type == "Manual") {
    if (is.null(value)) stop("`value` must be supplied for Manual type.", call. = FALSE)
    return(value)
  }

  if (is.null(n)) stop("`n` must be supplied for ", type, " penalty.", call. = FALSE)

  switch(type,
    BIC            = k * log(n),
    SIC            = k * log(n),
    MBIC           = 0.5 * (k + 1) * log(n) + lchoose(n, k),
    AIC            = 2 * k,
    `Hannan-Quinn` = 2 * k * log(log(n)),
    sSIC           = k * log(n) * 0.5
  )
}
