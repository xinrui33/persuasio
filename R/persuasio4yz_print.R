#' Print method for persuasio4yz
#'
#' @export
print.persuasio4yz <- function(x, ...) {

  cat("\n")
  cat(strrep("-", 70), "\n")
  cat("persuasio4yz: Average Persuasion Rate Inference\n")
  cat(strrep("-", 70), "\n\n")

  if (!is.null(x$title)) cat("Title:", x$title, "\n\n")

  cat("Outcome:    ", x$outcome, "\n", sep = "")
  cat("Instrument: ", x$instrument, "\n", sep = "")
  cat("Model:      ", x$model, "\n")
  cat("Method:     ", x$method, "\n\n")

  cat(sprintf("Lower bound: %10.6f\n", x$lb_coef))
  cat(sprintf("Upper bound: %10.6f\n\n", x$ub_coef))

  cat(sprintf("CI (level %.0f%%): [%.6f, %.6f]\n",
              x$level * 100, x$ci_lb, x$ci_ub))

  cat(strrep("-", 70), "\n\n")

  invisible(x)
}
