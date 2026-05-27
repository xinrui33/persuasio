#' @title Causal Inference on the Local Persuasion Rate
#'
#' @description Estimates the Local Persuasion Rate (LPR) and constructs
#'   confidence intervals for binary outcome \code{y}, binary treatment
#'   \code{t}, and binary instrument \code{z}. Wraps \code{\link{lpr4ytz}}
#'   for point estimation and performs inference using either a standard
#'   normal approximation or bootstrap resampling.
#'
#'   The LPR measures the persuasion effect among compliers — those whose
#'   treatment status is switched by the instrument. Unlike the APR (see
#'   \code{\link{persuasio4ytz}}), the LPR is a point-identified quantity
#'   under the assumptions of Jun and Lee (2023), so a single confidence
#'   interval rather than a bound interval is returned.
#'
#'   When covariates are absent and \code{method = "normal"}, a delta-method
#'   standard error from \code{\link{lpr4ytz}} is used. When covariates are
#'   present, analytic standard errors are unavailable and
#'   \code{method = "bootstrap"} is required.
#'
#' @param data data.frame containing variables
#' @param y character, outcome variable name (binary 0/1)
#' @param t character, treatment variable name (binary 0/1)
#' @param z character, instrument variable name (binary 0/1)
#' @param x optional character vector of covariates
#' @param level confidence level (default 0.95)
#' @param model model specification: \code{"no_interaction"} or
#'   \code{"interaction"}
#' @param method inference method: \code{"normal"} or \code{"bootstrap"}
#' @param nboot number of bootstrap replications (default 50)
#' @param title optional title for printed output
#' @param seed optional random seed for bootstrap reproducibility
#'
#' @return An object of class \code{persuasio4ytz2lpr} containing:
#' \describe{
#'   \item{lpr}{local persuasion rate estimate}
#'   \item{ci_lb}{lower confidence bound}
#'   \item{ci_ub}{upper confidence bound}
#'   \item{se}{standard error (NA if bootstrap)}
#'   \item{level}{confidence level}
#'   \item{method}{inference method used}
#'   \item{n}{sample size}
#'   \item{outcome}{Y variable name}
#'   \item{treatment}{T variable name}
#'   \item{instrument}{Z variable name}
#'   \item{covariates}{covariates used}
#'   \item{model}{model specification}
#'   \item{nboot}{number of bootstrap replications (if applicable)}
#'   \item{title}{optional title}
#' }
#'
#' @details
#' When \code{method = "normal"}, the confidence interval is constructed as
#' \deqn{\hat{\theta}_{LPR} \pm z_{\alpha/2} \cdot \widehat{se}}
#' where \eqn{\widehat{se}} is the delta-method standard error returned by
#' \code{\link{lpr4ytz}}. This requires \code{se} to be non-missing; if
#' \code{se = NA} (which occurs when covariates are present), the normal
#' method is not available and \code{method = "bootstrap"} must be used.
#'
#' When \code{method = "bootstrap"}, the confidence interval is constructed
#' from empirical quantiles of bootstrap replications of the LPR estimate.
#' Set \code{seed} for reproducible results.
#'
#' @references
#' Sung Jae Jun and Sokbae Lee (2023). Identifying the Effect of
#'   Persuasion. \emph{Journal of Political Economy}, 131(8).
#'   \doi{10.1086/724114}
#'
#' @seealso \code{\link{lpr4ytz}}, \code{\link{persuasio4ytz}},
#'   \code{\link{persuasio}}
#'
#' @examples
#' # Example 1: No covariates, normal inference
#' persuasio4ytz2lpr(
#'   data   = GKB,
#'   y      = "voteddem_all",
#'   t      = "readsome",
#'   z      = "post",
#'   method = "normal",
#'   level  = 0.80
#' )
#'
#' # Example 2: No covariates, bootstrap inference
#' persuasio4ytz2lpr(
#'   data   = GKB,
#'   y      = "voteddem_all",
#'   t      = "readsome",
#'   z      = "post",
#'   method = "bootstrap",
#'   level  = 0.80,
#'   nboot  = 1000
#' )
#'
#' # Example 3: With covariate, interaction model, bootstrap inference
#' persuasio4ytz2lpr(
#'   data   = GKB,
#'   y      = "voteddem_all",
#'   t      = "readsome",
#'   z      = "post",
#'   x      = "MZwave2",
#'   model  = "interaction",
#'   method = "bootstrap",
#'   level  = 0.80,
#'   nboot  = 1000
#' )
#'
#' @export
persuasio4ytz2lpr <- function(data, y, t, z, x = NULL,
                              model = "no_interaction",
                              method = "normal",
                              level = 0.95,
                              nboot = 50,
                              title = NULL,
                              seed = NULL) {

  if (!is.null(seed)) set.seed(seed)

  # core estimation
  res <- lpr4ytz(data, y, t, z, x, model)

  lpr_coef <- res$lpr
  se <- res$se

  n <- nrow(data)
  alpha <- 1 - level

  # Normal approximation
  if (method == "normal") {

    if (is.na(se)) {
      stop("Normal approximation not available: lower-bound SE is NA (likely due to covariates). Use method='bootstrap'.")
    }

    z_crit <- qnorm(1 - alpha / 2)

    ci_lb <- max(0, lpr_coef - z_crit * se)
    ci_ub <- min(1, lpr_coef + z_crit * se)

    res <- list(
      lpr = as.numeric(lpr_coef),
      ci_lb = as.numeric(ci_lb),
      ci_ub = as.numeric(ci_ub),
      se = as.numeric(se),
      level = level,
      method = method,
      n = n,
      outcome = y,
      treatment = t,
      instrument = z,
      covariates = x,
      model = model,
      title = title
    )

    class(res) <- "persuasio4ytz2lpr"
    return(res)
  }

  # Bootstrap
  if (method == "bootstrap") {

    boot <- numeric(nboot)

    for (b in seq_len(nboot)) {

      idx <- sample(seq_len(n), size = n, replace = TRUE)
      d_b <- data[idx, , drop = FALSE]

      r <- try(lpr4ytz(d_b, y, t, z, x, model), silent = TRUE)

      boot[b] <- if (inherits(r, "try-error")) NA else r$lpr
    }

    boot <- boot[!is.na(boot)]

    ci_lb <- quantile(boot, probs = alpha / 2)
    ci_ub <- quantile(boot, probs = 1 - alpha / 2)

    res <- list(
      lpr = lpr_coef,
      ci_lb = ci_lb,
      ci_ub = ci_ub,
      se = NA,
      level = level,
      method = "bootstrap",
      n = n,
      outcome = y,
      treatment = t,
      instrument = z,
      covariates = x,
      model = model,
      nboot = nboot,
      title = title
    )
  }

  class(res) <- "persuasio4ytz2lpr"
  return(res)
}
