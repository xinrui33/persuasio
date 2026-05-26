#' Print method for persuasio4ytz2lpr
#' @param x object of class "persuasio4ytz2lpr"
#' @param digits number of decimal places to display (default is 4)
#' @param ... unused
#' @export
print.persuasio4ytz2lpr <- function(x, digits = 4, ...) {

  cat("\n")
  cat("Local persuasion rate for binary outcomes, binary treatments and binary instruments \n\n")

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
    `LPR` = round(as.numeric(x$lpr), digits),
    `CI Lower` = round(as.numeric(x$ci_lb), digits),
    `CI Upper` = round(as.numeric(x$ci_ub), digits),
    check.names = FALSE
  )

  cat(format(out, row.names = FALSE), sep = "\n")

  cat("\n")
  cat(sprintf("Confidence level: %.0f%%\n", x$level * 100))

  if (x$method == "bootstrap") {
    cat(sprintf("Bootstrap replications: %s\n", x$nboot))
  }

  invisible(x)
}
