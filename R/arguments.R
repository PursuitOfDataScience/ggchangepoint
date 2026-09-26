# ---------------------------------------------------------------------------
# Argument matching with a suggestion.
#
# `match.arg()` was reachable from 40 of the 130 exports, and its refusal is
# the commonest first error the package produced: `cpt_detect(x, method =
# "PELT")` answered "'arg' should be one of" followed by all fifty method
# names. The message named neither the argument (`arg` is match.arg()'s own
# variable) nor the function, dumped every valid value, and offered no
# correction, although `utils::adist()` resolves every typo tried against the
# method names at an edit distance of two or less. cpt_match_arg() keeps
# match.arg()'s semantics (partial matching, the full default vector meaning
# its first element, `several.ok`) and changes only the refusal.
# ---------------------------------------------------------------------------

# Internal: the closest values to `value` among `choices`, nearest first,
# with their edit distances. Case is ignored for the distance, so "PELT" is
# at distance 0 from "pelt": suggested, never silently accepted.
#' @noRd
nearest_choices <- function(value, choices, n = 3L) {
  if (length(choices) == 0L) {
    return(list(values = character(0), dist = numeric(0)))
  }
  d <- utils::adist(tolower(value), tolower(choices))[1, ]
  ord <- order(d, choices)
  keep <- ord[seq_len(min(n, length(ord)))]
  list(values = choices[keep], dist = d[keep])
}

# Internal: "a", "b" or "c", quoted.
#' @noRd
quoted_or <- function(x, conj = "or") {
  x <- paste0("\"", x, "\"")
  if (length(x) <= 1L) return(x)
  paste0(paste(x[-length(x)], collapse = ", "), " ", conj, " ", x[length(x)])
}

# Internal: the did-you-mean sentence for an unmatched value, or "".
#' @noRd
did_you_mean <- function(value, choices, max_dist = 2) {
  near <- nearest_choices(value, choices, n = length(choices))
  if (!length(near$values) || min(near$dist) > max_dist) return("")
  best <- near$values[near$dist == min(near$dist)]
  paste0(" Did you mean ", quoted_or(utils::head(best, 3L)), "?")
}

#' Match an argument against its choices, suggesting a fix on failure
#'
#' A drop-in for \code{match.arg()}: the same partial matching, the same
#' reading of an untouched default vector as its first element, the same
#' \code{several.ok}. Only the refusal differs: it names the argument, says
#' what was given, suggests the nearest valid value, and shows at most
#' \code{max_show} choices (pointing at \code{hint} for the rest).
#' @param arg The value to match.
#' @param choices Valid values; taken from the calling function's formal
#'   default when omitted, as in \code{match.arg()}.
#' @param several.ok Whether several values may be given.
#' @param name The argument's name, for the message.
#' @param class Condition kind for the refusal.
#' @param hint Sentence appended when the choice list is truncated.
#' @param suggest_from Values to suggest from, when wider than the ones
#'   that match (registered method names, say).
#' @param max_show The most choices to list in full.
#' @noRd
cpt_match_arg <- function(arg, choices, several.ok = FALSE, name = NULL,
                          class = "bad_argument", hint = NULL,
                          suggest_from = choices, max_show = 8L) {
  if (is.null(name)) name <- deparse(substitute(arg))
  if (missing(choices)) {
    sys_p <- sys.parent()
    formal_args <- formals(sys.function(sys_p))
    choices <- eval(formal_args[[as.character(substitute(arg))]],
                    envir = sys.frame(sys_p))
    if (missing(suggest_from)) suggest_from <- choices
  }
  if (is.null(arg)) return(choices[1L])
  if (!is.character(arg)) {
    cpt_abort("`", name, "` must be a character string, not ",
              class(arg)[1], ".", class = "bad_argument",
              data = list(argument = name, choices = choices))
  }
  if (!several.ok) {
    if (identical(arg, choices)) return(arg[1L])
    if (length(arg) != 1L) {
      cpt_abort("`", name, "` must be one value, not ", length(arg), ": ",
                quoted_or(utils::head(arg, 5L), "and"), ".",
                class = "bad_argument",
                data = list(argument = name, value = arg, choices = choices))
    }
  } else if (length(arg) == 0L) {
    cpt_abort("`", name, "` must name at least one value.",
              class = "bad_argument",
              data = list(argument = name, choices = choices))
  }
  i <- pmatch(arg, choices, nomatch = 0L, duplicates.ok = TRUE)
  if (any(i == 0L)) {
    bad <- arg[i == 0L][1L]
    prefixed <- choices[startsWith(choices, bad)]
    if (nzchar(bad) && length(prefixed) > 1L) {
      cpt_abort("`", name, " = \"", bad, "\"` matches more than one value: ",
                quoted_or(utils::head(prefixed, max_show)),
                ". Write more of it.", class = "bad_argument",
                data = list(argument = name, value = bad, choices = choices))
    }
    listed <- if (length(choices) <= max_show) {
      paste0(" It should be one of ", quoted_or(choices), ".")
    } else {
      near <- nearest_choices(bad, choices)$values
      paste0(" The nearest of the ", length(choices), " valid values are ",
             quoted_or(near), ".", if (!is.null(hint)) paste0(" ", hint))
    }
    cpt_abort("Unknown `", name, " = \"", bad, "\"`.",
              did_you_mean(bad, suggest_from), listed,
              class = class,
              data = list(argument = name, value = bad, choices = choices,
                          suggestion = nearest_choices(bad, suggest_from,
                                                       n = 1L)$values))
  }
  choices[i]
}
