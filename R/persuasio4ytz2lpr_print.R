#' Print method for persuasio4ytz2lpr
#' @param x object of class "persuasio4ytz2lpr"
#' @param ... unused
#' @export
print.persuasio4ytz2lpr <- function(x, ...) {

  cat("\n")
  cat(strrep("-", 70), "\n")
  cat("persuasio4ytz2lpr: Local Persuasion Rate Inference\n")
  cat(strrep("-", 70), "\n\n")

  if (!is.null(x$title)) cat("Title:", x$title, "\n\n")

  cat("Outcome:     ", x$outcome, "\n", sep = "")
  cat("Treatment:   ", x$treatment, "\n", sep = "")
  cat("Instrument:  ", x$instrument, "\n", sep = "")
  cat("Model:       ", x$model, "\n")
  cat("Method:      ", x$method, "\n\n")

  cat(sprintf("LPR estimate: %10.6f\n\n", x$lpr))

  cat(sprintf("CI (level %.0f%%): [%.6f, %.6f]\n",
              x$level * 100, x$ci_lb, x$ci_ub))

  if (is.na(x$se)) {
    cat("\nNote: SE not available. Bootstrap recommended.\n")
  }

  cat(strrep("-", 70), "\n\n")

  invisible(x)
}
