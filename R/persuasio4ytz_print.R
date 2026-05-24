#' Print method for persuasio4ytz
#' @param x object of class "persuasio4ytz"
#' @param ... unused
#' @export
print.persuasio4ytz <- function(x, digits = 4, ...) {

  cat("\n")
  cat("Average Persuasion Rate Inference for binary outcomes, binary treatments and binary instruments\n\n")

  if (!is.null(x$title)) {
    cat("Title: ", x$title, "\n\n", sep = "")
  }

  cat("Outcome:    ", x$outcome, "\n", sep = "")
  cat("Treatment:  ", x$treatment, "\n", sep = "")
  cat("Instrument: ", x$instrument, "\n", sep = "")
  cat("Model:      ", x$model, "\n", sep = "")
  cat("Method:     ", x$method, "\n", sep = "")
  cat("Observations: ", x$n, "\n", sep = "")

  cat("\nEstimates:\n")

  out <- data.frame(
    `Lower Bound` = round(x$lb_coef, digits),
    `Upper Bound` = round(x$ub_coef, digits),
    `CI Lower` = round(x$ci_lb, digits),
    `CI Upper` = round(x$ci_ub, digits)
  )

  cat(format(out, row.names = FALSE), sep = "\n")

  cat("\n")
  cat(sprintf("Confidence level: %.0f%%\n", x$level * 100))

  if (x$method == "bootstrap") {
    cat(sprintf("Bootstrap replications: %s\n", x$nboot))
  }

  cat("\nNote: Based on persuasio4ytz bounding framework.\n")

  invisible(x)
}
