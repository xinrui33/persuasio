#' Print method for lpr4ytz objects
#'
#' @export
print.lpr4ytz <- function(x, ...) {

  cat("\nLocal Persuasion Rate (lpr4ytz)\n")
  cat("--------------------------------\n\n")

  cat("Outcome:", x$outcome, "\n")
  cat("Treatment:", x$treatment, "\n")
  cat("Instrument:", x$instrument, "\n")
  cat("Model:", x$model, "\n\n")

  cat(sprintf("LPR estimate: %.4f\n", x$lpr))

  if (!is.na(x$se)) {
    cat(sprintf("SE: %.4f\n", x$se))
    cat(sprintf("CI: [%.4f, %.4f]\n", x$ci[1], x$ci[2]))
  } else {
    cat("SE: NA (use bootstrap recommended)\n")
  }

  invisible(x)
}
