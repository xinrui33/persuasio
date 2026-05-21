#' Unified Interface for Persuasion Rate Estimation
#'
#' Main wrapper function for the persuasio package. Provides a unified entry point
#' to different estimators of persuasion-related causal parameters, including:
#' Average Persuasion Rate (APR), Local Persuasion Rate (LPR), and reduced-form
#' bounds based on summary moments.
#'
#' The function routes to different estimators depending on \code{est}.
#'
#' @param est character. Type of estimator:
#' \itemize{
#'   \item \code{"apr"}: Average Persuasion Rate bounds (\code{persuasio4ytz})
#'   \item \code{"lpr"}: Local Persuasion Rate (\code{persuasio4ytz2lpr})
#'   \item \code{"yz"}: Reduced-form persuasion bound (\code{persuasio4yz})
#'   \item \code{"calc"}: Direct plug-in calculation (\code{calc4persuasio})
#' }
#'
#' @param varlist character vector of variable names in the order:
#' \code{c(Y, T, Z, X1, X2, ...)} where:
#' \itemize{
#'   \item Y = outcome
#'   \item T = treatment
#'   \item Z = instrument
#'   \item X = optional covariates
#' }
#'
#' @param data data.frame containing all variables
#' @param subset optional logical or index vector for subsetting data
#' @param ... additional arguments passed to downstream estimators
#'
#' @return A list or data.frame depending on \code{est}:
#' \itemize{
#'   \item APR / LPR / YZ: model-based estimation results
#'   \item CALC: closed-form moment-based bounds
#' }
#'
#' @details
#' This function does not perform estimation itself. It only:
#' \enumerate{
#'   \item Parses variable inputs
#'   \item Applies optional subsetting
#'   \item Routes to the appropriate estimator
#' }
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
