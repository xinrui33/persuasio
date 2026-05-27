#' @title Causal Inference on Persuasion Effects Using Outcome and Instrument Only
#'
#' @description Estimates bounds on the Average Persuasion Rate (APR) using only
#'   a binary outcome \code{y} and a binary instrument \code{z}. Combines lower
#'   and upper bound estimation via \code{\link{aprlb}} and \code{\link{aprub}}
#'   under the YZ formulation with inference using either a Stoye (2009)-style
#'   normal approximation or bootstrap resampling.
#'
#'   This function is appropriate when treatment variables are unavailable
#'   or when the researcher wishes to bound the APR using only the
#'   reduced-form relationship between the instrument and the outcome. When
#'   treatment data are available, use \code{\link{persuasio4ytz}} instead.
#'
#'   When covariates are absent, both inference methods are available. When
#'   covariates are present, analytic standard errors are unavailable and
#'   \code{method = "bootstrap"} is required.
#'
#' @param data data.frame containing variables
#' @param y character, outcome variable name (binary 0/1)
#' @param z character, instrument variable name (binary 0/1)
#' @param x optional character vector of covariates
#' @param model model specification: \code{"no_interaction"} or
#'   \code{"interaction"}
#' @param method inference method: \code{"normal"} or \code{"bootstrap"}
#' @param level confidence level (default 0.95)
#' @param nboot number of bootstrap replications (default 50)
#' @param title optional title for output display
#' @param subset optional index or logical vector for subsetting data
#' @param seed optional random seed for bootstrap reproducibility
#'
#' @return An object of class \code{persuasio4yz} containing:
#' \describe{
#'   \item{lb_coef}{lower bound estimate}
#'   \item{ub_coef}{upper bound estimate}
#'   \item{ci_lb}{lower confidence bound}
#'   \item{ci_ub}{upper confidence bound}
#'   \item{level}{confidence level}
#'   \item{method}{inference method used}
#'   \item{n}{sample size}
#'   \item{outcome}{Y variable name}
#'   \item{instrument}{Z variable name}
#'   \item{covariates}{covariates used}
#'   \item{model}{model specification}
#'   \item{nboot}{number of bootstrap replications (if applicable)}
#'   \item{title}{optional title}
#' }
#'
#' @details
#' When \code{method = "normal"}, the function applies a Stoye (2009)-style
#' correction. If either standard errors from \code{\link{aprlb}} or
#' \code{\link{aprub}} is \code{NA} (which occurs when covariates are present),
#' use \code{method = "bootstrap"} instead.
#'
#' When \code{method = "bootstrap"}, the function constructs the confidence
#' interval from empirical quantiles of jointly resampled lower and upper bound
#' estimates. Set \code{seed} for reproducible results.
#'
#' @references Sung Jae Jun and Sokbae Lee (2023). Identifying the Effect of
#'   Persuasion. _Journal of Political Economy_, 131(8).
#'   <doi:10.1086/724114>
#'
#'
#' @seealso \code{\link{aprlb}}, \code{\link{aprub}},
#'   \code{\link{persuasio4ytz}}, \code{\link{persuasio}}
#'
#' @examples
#' # Example 1: No covariates, normal inference
#' persuasio4yz(
#'   data   = GKB,
#'   y      = "voteddem_all",
#'   z      = "post",
#'   method = "normal",
#'   level  = 0.80
#' )
#'
#' # Example 2: No covariates, bootstrap inference
#' persuasio4yz(
#'   data   = GKB,
#'   y      = "voteddem_all",
#'   z      = "post",
#'   method = "bootstrap",
#'   level  = 0.80,
#'   nboot  = 1000
#' )
#'
#' # Example 3: With covariate, interaction model, bootstrap inference
#' persuasio4yz(
#'   data   = GKB,
#'   y      = "voteddem_all",
#'   z      = "post",
#'   x      = "MZwave2",
#'   model  = "interaction",
#'   method = "bootstrap",
#'   level  = 0.80,
#'   nboot  = 1000
#' )
#'
#' @export
persuasio4yz <- function(data, y, z, x = NULL,
                         model = "no_interaction",
                         method = "normal",
                         level = 0.95,
                         nboot = 50,
                         title = NULL,
                         subset = NULL,
                         seed = NULL) {

  if (!is.null(seed)) set.seed(seed)

  if (!is.null(subset)) {
    data <- data[subset, , drop = FALSE]
  }

  lb <- aprlb(data, y, z, x, model)

  lb_coef <- lb$lb_coef
  ub_coef <- 1

  lb_se <- lb$lb_se
  ub_se <- NA

  n <- nrow(data)
  alpha <- 1 - level

  res <- NULL

  # Normal approximation
  if (method == "normal") {

    if (is.na(lb_se)) {
      stop("Normal approximation not available: lower-bound SE is NA (likely due to covariates). Use method='bootstrap'.")
    }

    cv <- qnorm(level)

    ci_lb <- lb_coef - cv * lb_se
    ci_ub <- 1

    res <- list(
      lb_coef = lb_coef,
      ub_coef = ub_coef,
      ci_lb = ci_lb,
      ci_ub = ci_ub,
      level = level,
      method = method,
      n = n,
      outcome = y,
      instrument = z,
      covariates = x,
      model = model,
      title = title
    )
  }


  # Bootstrap (Stata-style percentile)
  if (method == "bootstrap") {

    lb_boot <- numeric(nboot)

    for (b in seq_len(nboot)) {
      idx <- sample(seq_len(n), size = n, replace = TRUE)
      d_b <- data[idx, , drop = FALSE]
      lb_b <- try(aprlb(d_b, y, z, x, model), silent = TRUE)
      lb_boot[b] <- if (inherits(lb_b, "try-error")) NA else lb_b$lb_coef
    }

    ci_lb <- quantile(lb_boot, probs = alpha, na.rm = TRUE)
    ci_ub <- 1

    res <- list(
      lb_coef = lb_coef,
      ub_coef = 1,
      ci_lb = ci_lb,
      ci_ub = ci_ub,
      level = level,
      method = method,
      n = n,
      outcome = y,
      instrument = z,
      covariates = x,
      model = model,
      nboot = nboot,
      title = title
    )
  }

  class(res) <- "persuasio4yz"
  res
}
