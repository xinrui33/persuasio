#' Print method for lpr4ytz objects
#' @param x object of class "lpr4ytz"
#' @param ... unused
#' @export
print.lpr4ytz <- function(x, digits = 4, ...) {

  cat("\nLocal Persuasion Rate\n\n")

  cat("Outcome:    ", x$outcome, "\n", sep = "")
  cat("Treatment:  ", x$treatment, "\n", sep = "")
  cat("Instrument: ", x$instrument, "\n", sep = "")
  cat("Model:      ", x$model, "\n", sep = "")
  cat("Observations: ", x$n, "\n", sep = "")

  cat("\nEstimates:\n")

  ci_lb <- if (!is.null(x$ci) && length(x$ci) >= 1) x$ci[1] else NA_real_
  ci_ub <- if (!is.null(x$ci) && length(x$ci) >= 2) x$ci[2] else NA_real_

  out <- data.frame(
    Estimate = round(x$lpr, digits),
    `Std. Error` = round(x$se, digits),
    `95% CI Lower` = round(ci_lb, digits),
    `95% CI Upper` = round(ci_ub, digits)
  )

  cat(format(out, row.names = FALSE), sep = "\n")

  if (is.na(x$se) || is.null(x$se)) {
    cat("\nNote: Standard errors are not available for this specification (bootstrap recommended).\n")
  }

  cat("\n")

  invisible(x)
}
