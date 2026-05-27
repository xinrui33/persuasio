#' @title Unified Interface for Causal Inference on Persuasion Effects
#'
#' @description Main wrapper for the \pkg{persuasio} package. Provides a
#'   unified entry point to all persuasion effect estimators. The function
#'   parses inputs, optionally subsets the data, and dispatches to the
#'   appropriate estimator based on \code{est}.
#'
#'   Variables in \code{varlist} must be supplied in order:
#'   \code{c(y, t, z, x, ...)} where \code{y} is the binary outcome,
#'   \code{t} is the binary treatment, \code{z} is the binary instrument,
#'   and \code{x} are optional covariates. For \code{est = "yz"} and
#'   \code{est = "calc"}, \code{t} is optional.
#'
#' @param est character. Estimator type:
#' \itemize{
#'   \item \code{"apr"}: Average persuasion rate bounds for binary outcome, treatment and instrument
#'   \item \code{"lpr"}: Local persuasion rate bounds for binary outcome, treatment and instrument
#'   \item \code{"yz"}: Average and local persuasion rate bounds using binary outcome and instrument only
#'   \item \code{"calc"}: Average and local persuasion rate calculation from summary statistics
#' }
#'
#' @param varlist character vector of variable names in the order:
#' \code{c(y, t, z, x, ...)} where:
#' \itemize{
#'   \item y = binary outcome
#'   \item t = binary treatment (optional for \code{"yz"} and \code{"calc"})
#'   \item z = binary instrument
#'   \item x = optional covariates
#' }
#'
#' @param data data.frame containing variables
#' @param subset optional vector of row indices or logical filter
#' @param ... additional arguments passed to downstream estimators
#'
#' @return An object of class depending on \code{est}:
#' \itemize{
#'   \item \code{"apr"}: APR estimation object
#'   \item \code{"lpr"}: LPR estimation object
#'   \item \code{"yz"}: reduced-form bound object
#'   \item \code{"calc"}: summary-statistics-based object
#' }
#'
#' @details
#' This function only performs:
#' \enumerate{
#'   \item input parsing
#'   \item optional subsetting
#'   \item method dispatch
#' }
#'
#'
#' @references Sung Jae Jun and Sokbae Lee (2023). Identifying the Effect of
#'   Persuasion. _Journal of Political Economy_, 131(8).
#'   <doi:10.1086/724114>
#'
#' @seealso \code{\link{aprlb}}, \code{\link{aprub}}, \code{\link{lpr4ytz}},
#'   \code{\link{calc4persuasio}}
#'
#' @examples
#' # Example 1: Average persuasion rate (APR) with normal inference
#' persuasio(
#'   est     = "apr",
#'   varlist = c("voteddem_all", "readsome", "post"),
#'   data    = GKB,
#'   level   = 0.80,
#'   method  = "normal"
#' )
#'
#' # Example 2: Local persuasion rate (LPR) with normal inference
#' persuasio(
#'   est     = "lpr",
#'   varlist = c("voteddem_all", "readsome", "post"),
#'   data    = GKB,
#'   level   = 0.80,
#'   method  = "normal"
#' )
#'
#' # Example 3: Outcome-instrument bounds with covariate and bootstrap inference
#' persuasio(
#'   est     = "yz",
#'   varlist = c("voteddem_all", "readsome", "post", "MZwave2"),
#'   data    = GKB,
#'   level   = 0.80,
#'   model   = "interaction",
#'   method  = "bootstrap",
#'   nboot   = 1000
#' )
#'
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
  t <- if (length(varlist) >= 2) varlist[2] else NULL
  z <- if (length(varlist) >= 3) varlist[3] else NULL
  x <- if (length(varlist) > 3) varlist[4:length(varlist)] else NULL

  switch(est,

         apr = {
           return(persuasio4ytz(
             data = data,
             y = y, t = t, z = z, x = x,
             ...
           ))
         },

         lpr = {
           return(persuasio4ytz2lpr(
             data = data,
             y = y, t = t, z = z, x = x,
             ...
           ))
         },

         yz = {
           return(persuasio4yz(
             data = data,
             y = y, z = z, x = x,
             ...
           ))
         },

         calc = {

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
  )

  stop("Invalid estimator type")
}
