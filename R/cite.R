#' Cite the method behind a result
#'
#' Returns the bibliographic reference(s) for the method behind a
#' \code{ggcpt} result (or a method name), so an analysis can cite the right
#' methodological paper without leaving R.
#'
#' @param x A \code{ggcpt} object, a method name (e.g. \code{"pelt"}), or
#'   missing — in which case references for every known method are returned.
#'   A method registered with \code{\link{cpt_register_method}()} returns
#'   the citation that registration supplied, or a plain statement that none
#'   was given.
#' @return A tibble with columns \code{method} and \code{reference},
#'   invisibly; the references are also printed.
#' @export
#' @examples
#' cpt_cite("pelt")
#' res <- cpt_detect(c(rnorm(50), rnorm(50, 5)), method = "pelt")
#' cpt_cite(res)
cpt_cite <- function(x) {
  refs <- cpt_references()

  if (missing(x)) {
    out <- refs
  } else {
    method <- if (is_ggcpt(x)) x$method else as.character(x)
    # A ggcpt built by hand may carry no method at all; comparing against a
    # zero-length or NA value yields a zero-length/NA subscript, which
    # surfaces as tibble's "Can't subset rows with ..." rather than saying
    # what is wrong.
    if (length(method) != 1L || is.na(method) || !nzchar(method)) {
      stop("`x` must be a method name, or a ggcpt object whose `method` is ",
           "set. Call cpt_cite() with no argument for the full table.",
           call. = FALSE)
    }
    # The registry is an environment, so its lookup is CASE-SENSITIVE, and
    # cpt_register_method() stores the name verbatim. Lowercasing first
    # meant a registered "MyDetector" was never found -- cpt_detect()
    # dispatched it fine and cpt_cite() told the user to register a
    # citation, which is exactly what they had done. Try the name as given
    # before folding case for the built-in table.
    registered <- registry_get(method)
    method_given <- method
    method <- tolower(method)
    out <- refs[refs$method == method, , drop = FALSE]
    if (nrow(out) == 0) {
      # A user-registered detector is cited from what the registration
      # supplied -- or plainly not cited at all. Never invent one, and never
      # let a registered method borrow the credibility of a wired one.
      entry <- registered %||% registry_get(method)
      if (!is.null(entry)) {
        out <- tibble::tibble(
          method = method_given,
          reference = entry$citation %||% paste0(
            "No citation was supplied when `", method, "` was registered ",
            "with cpt_register_method(). This method is user-supplied: ",
            "ggchangepoint validated the shape of its output, not its ",
            "statistics."
          )
        )
      } else {
        stop("No reference recorded for method '", method, "'. ",
             "Call cpt_cite() with no argument for the full table. If this ",
             "result came from as_ggcpt() or an external detector, the ",
             "citation is yours to supply: register the detector with ",
             "cpt_register_method(citation = ...), or cite it directly.",
             call. = FALSE)
      }
    }
  }

  for (i in seq_len(nrow(out))) {
    cat("[", out$method[i], "] ", out$reference[i], "\n\n", sep = "")
  }
  invisible(out)
}

# Internal: the method -> reference table backing cpt_cite().
#' @noRd
cpt_references <- function() {
  tibble::tribble(
    ~method, ~reference,
    "pelt", "Killick, R., Fearnhead, P. and Eckley, I. A. (2012). Optimal detection of changepoints with a linear computational cost. Journal of the American Statistical Association, 107(500), 1590-1598.",
    "binseg", "Scott, A. J. and Knott, M. (1974). A cluster analysis method for grouping means in the analysis of variance. Biometrics, 30(3), 507-512.",
    "segneigh", "Auger, I. E. and Lawrence, C. E. (1989). Algorithms for the optimal identification of segment neighborhoods. Bulletin of Mathematical Biology, 51(1), 39-54.",
    "amoc", "Hinkley, D. V. (1970). Inference about the change-point in a sequence of random variables. Biometrika, 57(1), 1-17.",
    "np", "Haynes, K., Fearnhead, P. and Eckley, I. A. (2017). A computationally efficient nonparametric approach for changepoint detection. Statistics and Computing, 27(5), 1293-1305.",
    "ecp", "Matteson, D. S. and James, N. A. (2014). A nonparametric approach for multiple change point analysis of multivariate data. Journal of the American Statistical Association, 109(505), 334-345.",
    "fpop", "Maidstone, R., Hocking, T., Rigaill, G. and Fearnhead, P. (2017). On optimal multiple changepoint algorithms for large data. Statistics and Computing, 27(2), 519-533.",
    "wbs", "Fryzlewicz, P. (2014). Wild binary segmentation for multiple change-point detection. Annals of Statistics, 42(6), 2243-2281.",
    "wbs2", "Fryzlewicz, P. (2020). Detecting possibly frequent change-points: Wild Binary Segmentation 2 and steepest-drop model selection. Journal of the Korean Statistical Society, 49, 1027-1070.",
    "not", "Baranowski, R., Chen, Y. and Fryzlewicz, P. (2019). Narrowest-over-threshold detection of multiple change points and change-point-like features. Journal of the Royal Statistical Society: Series B, 81(3), 649-672.",
    "mosum", "Eichinger, B. and Kirch, C. (2018). A MOSUM procedure for the estimation of multiple random change points. Bernoulli, 24(1), 526-564.",
    "idetect", "Anastasiou, A. and Fryzlewicz, P. (2022). Detecting multiple generalized change-points by isolating single ones. Metrika, 85, 141-174.",
    "tguh", "Fryzlewicz, P. (2018). Tail-greedy bottom-up data decompositions and fast multiple change-point detection. Annals of Statistics, 46(6B), 3390-3421.",
    "crops", "Haynes, K., Eckley, I. A. and Fearnhead, P. (2017). Computationally efficient changepoint detection for a range of penalties. Journal of Computational and Graphical Statistics, 26(1), 134-143.",
    "smuce", "Frick, K., Munk, A. and Sieling, H. (2014). Multiscale change point inference. Journal of the Royal Statistical Society: Series B, 76(3), 495-580.",
    "hsmuce", "Pein, F., Sieling, H. and Munk, A. (2017). Heterogeneous change point inference. Journal of the Royal Statistical Society: Series B, 79(4), 1207-1227.",
    "bcp", "Barry, D. and Hartigan, J. A. (1993). A Bayesian analysis for change point problems. Journal of the American Statistical Association, 88(421), 309-319. See also Erdman, C. and Emerson, J. W. (2007). bcp: An R package for performing a Bayesian analysis of change point problems. Journal of Statistical Software, 23(3), 1-13.",
    "bocpd", "Adams, R. P. and MacKay, D. J. C. (2007). Bayesian online changepoint detection. arXiv:0710.3742.",
    "beast", "Zhao, K., Wulder, M. A., Hu, T., et al. (2019). Detecting change-point, trend, and seasonality in satellite time series data to track abrupt changes and nonlinear dynamics: A Bayesian ensemble algorithm. Remote Sensing of Environment, 232, 111181.",
    "cpm", "Ross, G. J. (2015). Parametric and nonparametric sequential change detection in R: The cpm package. Journal of Statistical Software, 66(3), 1-20.",
    "kcp", "Arlot, S., Celisse, A. and Harchaoui, Z. (2019). A kernel multiple change-point algorithm via model selection. Journal of Machine Learning Research, 20(162), 1-56. See also Cabrieto, J., Adolf, J., Tuerlinckx, F., Kuppens, P. and Ceulemans, E. (2018). Detecting long-lived autodependency changes in a multivariate system via change point detection and regime switching models. Scientific Reports, 8, 15637.",
    "npmojo", "McGonigle, E. T. and Cho, H. (2025). Nonparametric data segmentation in multivariate time series via joint characteristic functions. Biometrika, 112(2), asaf024.",
    "decafs", "Romano, G., Rigaill, G., Runge, V. and Fearnhead, P. (2022). Detecting abrupt changes in the presence of local fluctuations and autocorrelated noise. Journal of the American Statistical Association, 117(540), 2147-2162.",
    "sn", "Zhao, Z., Jiang, F. and Shao, X. (2022). Segmenting time series via self-normalisation. Journal of the Royal Statistical Society: Series B, 84(5), 1699-1725.",
    "inspect", "Wang, T. and Samworth, R. J. (2018). High dimensional change point estimation via sparse projection. Journal of the Royal Statistical Society: Series B, 80(1), 57-83.",
    "ocd", "Chen, Y., Wang, T. and Samworth, R. J. (2022). High-dimensional, multiscale online changepoint detection. Journal of the Royal Statistical Society: Series B, 84(1), 234-266.",
    "geomcp", "Grundy, T., Killick, R. and Mihaylov, G. (2020). High-dimensional changepoint detection via a geometrically inspired mapping. Statistics and Computing, 30, 1155-1166.",
    "cpop", "Fearnhead, P., Maidstone, R. and Letchford, A. (2019). Detecting changes in slope with an L0 penalty. Journal of Computational and Graphical Statistics, 28(2), 265-275. See also Fearnhead, P. and Grose, D. (2024). cpop: Detecting changes in piecewise-linear signals. Journal of Statistical Software, 109(7), 1-30.",
    "strucchange", "Bai, J. and Perron, P. (2003). Computation and analysis of multiple structural change models. Journal of Applied Econometrics, 18(1), 1-22. See also Zeileis, A., Leisch, F., Hornik, K. and Kleiber, C. (2002). strucchange: An R package for testing for structural change in linear regression models. Journal of Statistical Software, 7(2), 1-38.",
    "segmented", "Muggeo, V. M. R. (2003). Estimating regression models with unknown break-points. Statistics in Medicine, 22(19), 3055-3071.",
    "envcpt", "Beaulieu, C. and Killick, R. (2018). Distinguishing trends and shifts from memory in climate data. Journal of Climate, 31(23), 9519-9543.",
    "fastcpd", "Li, X. and Zhang, X. (2024). fastcpd: Fast change point detection in R. arXiv:2404.05933.",

    # ---- 0.5.0 engine wave -------------------------------------------------
    "nsp", "Fryzlewicz, P. (2024). Narrowest Significance Pursuit: inference for multiple change-points in linear models. Journal of the American Statistical Association, 119(546), 1633-1646.",
    "mcp", "Lindel\u00f8v, J. K. (2020). mcp: An R package for regression with multiple change points. OSF Preprints. doi:10.31219/osf.io/fzqxv.",
    "esac", "Moen, P. A. J., Glad, I. K. and Tveten, M. (2024). Efficient sparsity adaptive changepoint estimation. Electronic Journal of Statistics, 18(2), 3975-4038.",
    "pilliat", "Pilliat, E., Carpentier, A. and Verzelen, N. (2023). Optimal multiple change-point detection for high-dimensional data. Electronic Journal of Statistics, 17(1), 1240-1315.",
    "hdcov", "Wang, D., Yu, Y. and Rinaldo, A. (2021). Optimal covariance change point localization in high dimensions. Bernoulli, 27(1), 554-575.",
    "network", "Yu, Y., Padilla, O. H. M., Wang, D. and Rinaldo, A. (2021). Optimal network online change point localisation. arXiv:2101.05477.",
    "var", "Wang, D., Yu, Y., Rinaldo, A. and Willett, R. (2019). Localizing changes in high-dimensional vector autoregressive processes. arXiv:1909.06359.",
    "hdreg", "Rinaldo, A., Wang, D., Wen, Q., Willett, R. and Yu, Y. (2021). Localizing changes in high-dimensional regression models. Proceedings of the 24th International Conference on Artificial Intelligence and Statistics, PMLR 130, 2089-2097.",
    "fmean", "Aue, A., Rice, G. and S\u00f6nmez, O. (2018). Detecting and dating structural breaks in functional data without dimension reduction. Journal of the Royal Statistical Society: Series B, 80(3), 509-529.",
    "fcov", "Aue, A., Rice, G. and S\u00f6nmez, O. (2020). Structural break analysis for spectrum and trace of covariance operators. Environmetrics, 31(1), e2617. See also Aue, Rice and S\u00f6nmez (2018), JRSS-B 80(3), 509-529.",
    "kwc", "Ramsay, K. and Chenouri, S. (2025). Robust changepoint detection in the variability of multivariate functional data. Journal of Nonparametric Statistics. doi:10.1080/10485252.2025.2503891.",
    "fabisearch", "Ondrus, M. and Cribben, I. (2024). fabisearch: a package for change point detection in and visualization of the network structure of multivariate high-dimensional time series in R. Neurocomputing, 578, 127321.",
    "wbsts", "Korkas, K. K. and Fryzlewicz, P. (2017). Multiple change-point detection for non-stationary time series using wild binary segmentation. Statistica Sinica, 27(1), 287-311.",
    "bfast", "Verbesselt, J., Hyndman, R., Newnham, G. and Culvenor, D. (2010). Detecting trend and seasonal changes in satellite image time series. Remote Sensing of Environment, 114(1), 106-115.",
    "pettitt", "Pettitt, A. N. (1979). A non-parametric approach to the change-point problem. Journal of the Royal Statistical Society: Series C, 28(2), 126-135.",
    "buishand", "Buishand, T. A. (1982). Some methods for testing the homogeneity of rainfall records. Journal of Hydrology, 58(1-2), 11-27.",
    "snht", "Alexandersson, H. (1986). A homogeneity test applied to precipitation data. Journal of Climatology, 6(6), 661-675.",
    "taylor", "Taylor, W. A. (2000). Change-Point Analysis: A Powerful New Tool for Detecting Changes. Taylor Enterprises, Libertyville, Illinois.",
    "binsegrcpp", "Hocking, T. D. (2024). Finite sample complexity analysis of binary segmentation. arXiv:2410.08654.",

    # ---- tools that are methods in their own right -------------------------
    "copps", "Zou, C., Wang, G. and Li, R. (2020). Consistent selection of the number of change-points via sample-splitting. Annals of Statistics, 48(1), 413-439.",
    "mbic", "Zhang, N. R. and Siegmund, D. O. (2007). A modified Bayes information criterion with applications to the analysis of comparative genomic hybridization data. Biometrics, 63(1), 22-32.",
    "influence", "Wilms, I., Killick, R. and Matteson, D. S. (2022). Graphical influence diagnostics for changepoint models. Journal of Computational and Graphical Statistics, 31(3), 753-765.",
    "penalty_learning", "Hocking, T. D., Rigaill, G., Vert, J.-P. and Bach, F. (2013). Learning sparse penalties for change-point detection using max margin interval regression. Proceedings of the 30th International Conference on Machine Learning, 28, 172-180.",
    "edetector", "Shin, J., Ramdas, A. and Rinaldo, A. (2023). E-detectors: a nonparametric framework for sequential change detection. The New England Journal of Statistics in Data Science, 1(2), 229-260. (Implemented natively in ggchangepoint; every other method here wraps a separately maintained package.)",
    "benchmark", "van den Burg, G. J. J. and Williams, C. K. I. (2020). An evaluation of change point detection algorithms. arXiv:2003.06222.",
    "critical_difference", "Demsar, J. (2006). Statistical comparisons of classifiers over multiple data sets. Journal of Machine Learning Research, 7, 1-30."
  )
}
