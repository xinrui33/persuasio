#' Print method for aprlb objects
#' @param x object of class "aprlb"
#' @param ... unused
#' @export
print.aprlb <- function(x, digits = 4, ...) {

  cat("\n")
  cat("Lower Bound of Average Persuasion Rate\n\n")

  cat("Outcome:    ", x$outcome, "\n", sep = "")
  cat("Instrument: ", x$instrument, "\n", sep = "")

  if (!is.null(x$covariates)) {
    cat("Covariates: ", paste(x$covariates, collapse = ", "), "\n", sep = "")
  } else {
    cat("Covariates: None\n", sep = "")
  }

  cat("Model:             ", x$model, "\n", sep = "")
  cat("Observations:      ", x$n, "\n", sep = "")

  cat("\n")
  cat("Estimates:\n")

  cat(
    format(
      data.frame(
        Estimate = round(x$lb_coef, digits),
        `Std. Error` = round(x$lb_se, digits),
        `95% CI Lower` = round(x$ci_lb, digits),
        `95% CI Upper` = round(x$ci_ub, digits)
      ),
      row.names = FALSE
    ),
    sep = "\n"
  )

  if (is.na(x$lb_se)) {
    cat("\n\nStandard errors not available for this specification.")
  }

  cat("\n")

  cat("Note: It is recommended to use the 'persuasio' command.\n")

  invisible(x)
}
