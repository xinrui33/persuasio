#' Print method for aprlb objects
#' @param x object of class "aprlb"
#' @param digits number of decimal places to display (default is 4)
#' @param ... unused
#' @export
print.aprlb <- function(x, digits = 4, ...) {

  cat("\n")
  cat("aprlb: Lower Bound of Average Persuasion Rate\n\n")

  cat("Outcome:    ", x$outcome, "\n", sep = "")
  cat("Instrument: ", x$instrument, "\n", sep = "")
  if (!is.null(x$covariates)) {
    cat("Covariates: ", paste(x$covariates, collapse = ", "), "\n", sep = "")
  } else {
    cat("Covariates: None\n")
  }
  cat("Model:        ", x$model, "\n", sep = "")
  cat("Observations: ", x$n, "\n", sep = "")
  cat("\n")

  cat("Estimates:\n")
  out <- data.frame(
    Estimate       = round(as.numeric(x$lb_coef), digits),
    `Std. Error`   = round(as.numeric(x$lb_se), digits),
    `95% CI Lower` = round(as.numeric(x$ci_lb), digits),
    `95% CI Upper` = round(as.numeric(x$ci_ub), digits),
    check.names = FALSE
  )

  print(out, row.names = FALSE)

  if (is.null(x$lb_se) || length(x$lb_se) == 0 || all(is.na(x$lb_se))) {
    cat("\nStandard errors not available for this specification.\n")
  }
  cat("\n")

  cat("Note: It is recommended to use the 'persuasio' command.\n")

  invisible(x)
}
