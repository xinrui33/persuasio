#' Print method for calc4persuasio
#' @param x object of class "calc4persuasio"
#' @param digits number of decimal places to display (default is 4)
#' @param ... unused
#' @export
print.calc4persuasio <- function(x, digits = 4, ...) {

  cat("\ncalc4persuasio: APR and LPR bounds given Pr(y=1|z) and optionally Pr(t=1|z) for z=0,1\n\n")

  cat("Case: ", x$case, "\n\n", sep = "")

  # APR
  cat("APR bounds:\n")
  apr_df <- data.frame(
    Lower = round(x$apr["lower"], digits),
    Upper = round(x$apr["upper"], digits),
    check.names = FALSE
  )
  cat(format(apr_df, row.names = FALSE), sep = "\n")

  # LPR
  cat("\nLPR bounds:\n")
  lpr_df <- data.frame(
    Lower = round(x$lpr["lower"], digits),
    Upper = round(x$lpr["upper"], digits),
    check.names = FALSE
  )
  cat(format(lpr_df, row.names = FALSE), sep = "\n")

  # Inputs
  cat("\nInputs:\n")
  cat(
    "y1 = ", x$inputs$y1, ", y0 = ", x$inputs$y0, "\n",
    sep = ""
  )
  cat(
    "e1 = ", x$inputs$e1, ", e0 = ", x$inputs$e0, "\n",
    sep = ""
  )

  if (x$case == "no exposure rates") {
    cat("\nNote: Exposure rates, Pr(t=1|z) for z=0,1, are missing.\n")
  }

  cat("\n")

  invisible(x)
}
