#' Print method for persuasio4ytz
#' @param x object of class "persuasio4ytz"
#' @param ... unused
#' @export
print.persuasio4ytz <- function(x, ...) {

  cat("\n")
  cat(strrep("-", 70), "\n")
  cat("persuasio4ytz: Average Persuasion Rate Inference\n")
  cat(strrep("-", 70), "\n\n")

  if (!is.null(x$title)) cat("Title:", x$title, "\n\n")

  cat("Outcome:     ", x$outcome, "\n", sep = "")
  cat("Treatment:   ", x$treatment, "\n", sep = "")
  cat("Instrument:  ", x$instrument, "\n", sep = "")
  cat("Model:       ", x$model, "\n")
  cat("Method:      ", x$method, "\n\n")

  cat(sprintf("Lower Bound: %10.6f\n", x$lb_coef))
  cat(sprintf("Upper Bound: %10.6f\n\n", x$ub_coef))

  cat(sprintf("CI (level %.0f%%): [%.6f, %.6f]\n",
              x$level * 100, x$ci_lb, x$ci_ub))

  cat("\nNote: Based on persuasio4ytz bounding framework.\n")
  cat(strrep("-", 70), "\n\n")

  invisible(x)
}
