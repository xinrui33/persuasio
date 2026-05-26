#' Print method for calc4persuasio
#' @param x object of class "calc4persuasio"
#' @param digits number of decimal places to display (default is 4)
#' @param ... unused
#' @export
print.calc4persuasio <- function(x, digits = 4, ...) {

  cat("\ncalc4persuasio: APR and LPR bounds given Pr(y=1|z) and optionally Pr(t=1|z)) for z=0,1\n\n")

  cat("Case: ", x$case, "\n\n", sep = "")
  cat("APR bounds:\n")
  cat(
    format(
      data.frame(
        Lower = round(as.numeric(x$apr[["lower"]]), digits),
        Upper = round(as.numeric(x$apr[["upper"]]), digits),
        check.names = FALSE
      ),
      row.names = FALSE
    ),
    sep = "\n"
  )

  cat("\nLPR bounds:\n")
  cat(
    format(
      data.frame(
        Lower = round(as.numeric(x$lpr[["lower"]]), digits),
        Upper = round(as.numeric(x$lpr[["upper"]]), digits),
        check.names = FALSE
      ),
      row.names = FALSE
    ),
    sep = "\n"
  )

  cat("\nInputs:\n")
  cat("y1 = ", as.numeric(x$inputs[["y1"]]), ", y0 = ", as.numeric(x$inputs[["y0"]]), "\n", sep = "")
  cat("e1 = ", as.numeric(x$inputs[["e1"]]), ", e0 = ", as.numeric(x$inputs[["e0"]]), "\n", sep = "")

  if (x$case == "no exposure rates") {
    cat("\nNote: Exposure rates, Pr(t=1|z) for z=0,1, are missing.\n")
  }
  cat("\n")

  invisible(x)
}
