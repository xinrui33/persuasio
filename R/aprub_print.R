#' Print method for aprub objects
#' @param x object of class "aprub"
#' @param ... unused
#' @export
print.aprub <- function(x, ...) {

  cat("\n")
  cat(strrep("-", 65), "\n")
  cat("aprub: Upper Bound of Average Persuasion Rate\n")
  cat(strrep("-", 65), "\n\n")

  cat("Outcome:    ", x$outcome, "\n", sep = "")
  cat("Instrument: ", x$instrument, "\n", sep = "")

  if (!is.null(x$covariates)) {
    cat("Covariates: ", paste(x$covariates, collapse = ", "), "\n\n", sep = "")
  } else {
    cat("Covariates: None\n\n")
  }

  cat(sprintf("Estimate: %10.6f\n", x$ub_coef))

  if (!is.null(x$ub_se) && !is.na(x$ub_se)) {
    cat(sprintf("Std. Err.: %10.6f\n", x$ub_se))
    cat(sprintf("95%% CI:    [%10.6f, %10.6f]\n", x$ci_lb, x$ci_ub))
  } else {
    cat(sprintf("Std. Err.: %10s\n", "."))
    cat("95% CI:    [., .]\n")
  }

  cat("\nNote: Estimated using persuasio APR upper bound estimator.\n")
  cat(strrep("-", 65), "\n\n")

  invisible(x)
}
