#' Main persuasio dispatcher
#'
#' @export
persuasio <- function(est = c("apr", "lpr", "yz", "calc"),
                      varlist,
                      data,
                      subset = NULL,
                      ...) {

  est <- match.arg(est)

  if (!is.null(subset)) {
    data <- data[subset, , drop = FALSE]
  }

  y <- varlist[1]
  t <- varlist[2]
  z <- varlist[3]
  x <- if (length(varlist) > 3) varlist[4:length(varlist)] else NULL

  if (est == "apr") {

    return(persuasio4ytz(
      data = data,
      y = y, t = t, z = z, x = x,
      ...
    ))

  } else if (est == "lpr") {

    return(persuasio4ytz2lpr(
      data = data,
      y = y, t = t, z = z, x = x,
      ...
    ))

  } else if (est == "yz") {

    return(persuasio4yz(
      data = data,
      y = y, z = z, x = x,
      ...
    ))

  } else if (est == "calc") {

    y1 <- mean(data[[y]][data[[z]] == 1], na.rm = TRUE)
    y0 <- mean(data[[y]][data[[z]] == 0], na.rm = TRUE)

    e1 <- mean(data[[t]][data[[z]] == 1], na.rm = TRUE)
    e0 <- mean(data[[t]][data[[z]] == 0], na.rm = TRUE)

    return(calc4persuasio(
      y1 = y1,
      y0 = y0,
      e1 = e1,
      e0 = e0
    ))
  }

  stop("Invalid estimator type")
}
