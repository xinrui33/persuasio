#' Conduct causal inference on the local persuasion rate for binary outcomes
#' _y_, binary treatments _t_ and binary instruments _z_
#'
#' Computes the Local Persuasion Rate (LPR) and constructs confidence intervals
#' using either asymptotic normal approximation or bootstrap methods.
#'
#' This wrapper calls \code{lpr4ytz()} for point estimation and then performs
#' inference based on the selected method.
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
#' @details For \code{method = "normal"}, the confidence interval is based on a
#' standard normal approximation using the delta-method standard error from
#' \code{lpr4ytz()}.
#'
#' For \code{method = "bootstrap"}, the interval is computed using empirical
#' quantiles of bootstrap replications.
#'
#' When \code{se = NA}, the normal approximation is not available and the
#' bootstrap method should be used.
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
