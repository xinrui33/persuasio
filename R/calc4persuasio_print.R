#' @title Print method for calc4persuasio
#'
#' @export

print.calc4persuasio <- function(x, ...) {

  cat("\ncalc4persuasio results\n")
  cat("----------------------\n\n")

  cat("Case:", x$case, "\n\n")

  cat("APR bounds:\n")
  cat(sprintf("  [%0.4f, %0.4f]\n", x$apr["lower"], x$apr["upper"]))

  cat("\nLPR bounds:\n")
  cat(sprintf("  [%0.4f, %0.4f]\n", x$lpr["lower"], x$lpr["upper"]))

  cat("\nInputs:\n")
  cat(sprintf("  y1 = %s, y0 = %s\n", x$inputs$y1, x$inputs$y0))
  cat(sprintf("  e1 = %s, e0 = %s\n", x$inputs$e1, x$inputs$e0))

  if (x$case == "case_2") {
    cat("\nNote: exposure rates Pr(t=1|z) not provided.\n")
  }

  invisible(x)
}
