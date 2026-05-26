#' Print method for lpr4ytz objects
#' @param x object of class "lpr4ytz"
#' @param digits number of decimal places to display (default is 4)
#' @param ... unused
#' @export
print.lpr4ytz <- function(x, digits = 4, ...) {

  cat("\nlpr: Local Persuasion Rate\n\n")  # test expects this string

  cat("Outcome:      ", x$outcome, "\n", sep = "")
  cat("Treatment:    ", x$treatment, "\n", sep = "")
  cat("Instrument:   ", x$instrument, "\n", sep = "")
  cat("Model:        ", x$model, "\n", sep = "")
  cat("Observations: ", x$n, "\n", sep = "")

  cat("\nEstimates:\n")
  out <- data.frame(
    Estimate       = round(as.numeric(x$lpr), digits),
    `Std. Error`   = round(as.numeric(x$se), digits),
    `95% CI Lower` = round(as.numeric(x$ci_lb), digits),
    `95% CI Upper` = round(as.numeric(x$ci_ub), digits),
    check.names = FALSE
  )

  print(out, row.names = FALSE)

  if (is.null(x$lb_se) || length(x$lb_se) == 0 || all(is.na(x$lb_se))) {
    cat("\nStandard errors not available for this specification.\n")
  }
  cat("\n")

  invisible(x)
}
