#' Print method for aprub objects
#' @param x object of class "aprub"
#' @param digits number of decimal places to display (default is 4)
#' @param ... unused
#' @export
print.aprub <- function(x, digits = 4, ...) {

  cat("\n")
  cat("aprub: Upper Bound of Average Persuasion Rate\n\n")

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
  df <- data.frame(
    Estimate       = round(as.numeric(x$ub_coef), digits),
    `Std. Error`   = round(as.numeric(x$ub_se), digits),
    `95% CI Lower` = round(as.numeric(x$ci_lb), digits),
    `95% CI Upper` = round(as.numeric(x$ci_ub), digits),
    check.names = FALSE
  )
  cat(format(df, row.names = FALSE), sep = "\n")

  if (is.na(x$lb_se)) {
    cat("\nStandard errors not available for this specification.\n")
  }
  cat("\n")

  cat("Note: It is recommended to use the 'persuasio' command.\n")

  invisible(x)
}
