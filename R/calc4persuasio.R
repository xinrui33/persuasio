#' Calculate the effect of persuasion when information on Pr(y=1|z) and
#' optimally Pr(t=1|z) for each z=0,1 is available
#'
#' __calc4persuasio__ calculates the effect of persuasion when information on
#' Pr(y=1|z) and optimally Pr(t=1|z) for each z=0,1 is available. The inputs are
#' y1, y0, e1, and e0, corresponding to estimates of \eqn{P(y=1 \mid z=1)},
#' \eqn{P(y=1 \mid z=0)}, \eqn{P(t=1 \mid z=1)}, and \eqn{P(t=1 \mid z=0)}.
#'
#' The outputs of this command are the lower and upper bounds on the average
#' persuasion rate (APR) as well as the lower and upper bounds on the local
#' persuasion rate (LPR).
#'
#'
#' @param y1 mean outcome under z = 1
#' @param y0 mean outcome under z = 0
#' @param e1 (optional) mean treatment under z = 1
#' @param e0 (optional) mean treatment under z = 0
#'
#' @return An object of class \code{calc4persuasio}, a list containing:
#'   \item{apr}{numeric vector: lower and upper bound} \item{lpr}{numeric
#'   vector: lower and upper bound} \item{inputs}{input values} \item{case}{case
#'   identifier}
#'
#' @export

calc4persuasio <- function(y1, y0, e1 = NULL, e0 = NULL) {

  # Input validation
  check01 <- function(x, name) {
    if (!is.null(x) && (x < 0 || x > 1)) {
      stop(paste0(name, "must be in [0,1]"))
    }
  }

  check01(y1, "y1")
  check01(y0, "y0")
  check01(e1, "e1")
  check01(e0, "e0")

  # Case detection
  has_exposure <- !is.null(e1) && !is.null(e0)

  # Computation
  if (has_exposure) {

    # APR lower bound
    apr_lb <- (y1 - y0) / (1 - y0)

    # APR upper bound
    ub_num <- min(1, y1 + 1 - e1) - max(0, y0 - e0)
    ub_den <- 1 - max(0, y0 - e0)
    apr_ub <- ub_num / ub_den

    # LATE
    late <- (y1 - y0) / (e1 - e0)

    # LPR bounds
    lpr_lb <- max(apr_lb, late)
    lpr_ub <- 1

    case <- "with exposure rates"

  } else {

    lb <- (y1 - y0) / (1 - y0)

    apr_lb <- lb
    apr_ub <- 1

    lpr_lb <- lb
    lpr_ub <- 1

    case <- "no exposure rates"
  }

  res <- list(
    apr = c(lower = as.numeric(apr_lb), upper = as.numeric(apr_ub)),
    lpr = c(lower = as.numeric(lpr_lb), upper = as.numeric(lpr_ub)),
    inputs = list(y1 = y1, y0 = y0, e1 = e1, e0 = e0),
    case = case
  )

  class(res) <- "calc4persuasio"
  return(res)
}
