#' Average Persuasion Rate Inference
#'
#' Estimates the Average Persuasion Rate (APR) and constructs confidence intervals
#' using either asymptotic normal approximation or bootstrap methods.
#'
#' This function combines:
#' \itemize{
#'   \item lower bound estimation via \code{aprlb()}
#'   \item upper bound estimation via \code{aprub()}
#'   \item inference using either Stoye-style normal approximation or bootstrap
#' }
#'
#' @param data data.frame containing variables
#' @param y character, outcome variable name (binary 0/1)
#' @param t character, treatment variable name (binary 0/1)
#' @param z character, instrument variable name (binary 0/1)
#' @param x optional character vector of covariates
#' @param level confidence level (default 0.95)
#' @param model model specification: \code{"no_interaction"} or \code{"interaction"}
#' @param method inference method: \code{"normal"} or \code{"bootstrap"}
#' @param nboot number of bootstrap replications (default 50)
#' @param title optional title for printed output
#' @param subset optional index or logical vector for subsetting data
#'
#' @return An object of class \code{persuasio4ytz} containing:
#' \describe{
#'   \item{lb_coef}{lower bound estimate}
#'   \item{ub_coef}{upper bound estimate}
#'   \item{ci_lb}{lower confidence bound}
#'   \item{ci_ub}{upper confidence bound}
#'   \item{level}{confidence level}
#'   \item{method}{inference method used}
#'   \item{n}{sample size}
#'   \item{outcome}{Y variable name}
#'   \item{treatment}{T variable name}
#'   \item{instrument}{Z variable name}
#'   \item{covariates}{covariates used}
#'   \item{model}{model specification}
#'   \item{nboot}{number of bootstrap draws (if applicable)}
#'   \item{title}{optional title}
#' }
#'
#' @details
#' If \code{method = "normal"}, the function uses a Stoye (2009)-style correction
#' for partially identified parameters. Standard errors must be available from
#' both \code{aprlb()} and \code{aprub()}.
#'
#' If \code{method = "bootstrap"}, inference is based on empirical quantiles
#' from resampled estimates.
#'
#' @export
persuasio4ytz <- function(data, y, t, z, x = NULL,
                          level = 0.95,
                          model = "no_interaction",
                          method = "normal",
                          nboot = 50,
                          title = NULL,
                          subset = NULL) {

  # subset handling
  if (!is.null(subset)) {
    data <- data[subset, , drop = FALSE]
  }

  # core estimation
  lb <- aprlb(data, y, z, x, model)
  ub <- aprub(data, y, t, z, x, model)

  lb_coef <- lb$lb_coef
  ub_coef <- ub$ub_coef

  lb_se <- lb$lb_se
  ub_se <- ub$ub_se

  n <- nrow(data)

  alpha <- 1 - level

  # CASE 1: NORMAL APPROXIMATION (Stoye-style CI)
  if (method == "normal") {

    if (is.na(lb_se) || is.na(ub_se)) {
      stop("Normal method requires SEs (not available with this model). Use method='bootstrap'.")
    }

    cv1 <- qnorm(1 - alpha)
    cv2 <- qnorm(1 - alpha / 2)

    correction <- (ub_coef - lb_coef) / max(lb_se, ub_se)

    grid <- seq(cv1 - 0.01, cv2 + 0.01, length.out = n)

    loss <- abs(
      pnorm(grid + correction) - pnorm(-grid) - (1 - alpha)
    )

    cv_star <- mean(grid[loss == min(loss)])

    ci_lb <- lb_coef - cv_star * lb_se
    ci_ub <- ub_coef + cv_star * ub_se

    res <- list(
      lb_coef = as.numeric(lb_coef),
      ub_coef = as.numeric(ub_coef),
      ci_lb = as.numeric(ci_lb),
      ci_ub = as.numeric(ci_ub),
      level = level,
      method = "normal",
      n = n,
      outcome = y,
      treatment = t,
      instrument = z,
      covariates = x,
      model = model,
      title = title
    )

    class(res) <- "persuasio4ytz"
    return(res)
  }

  # CASE 2: BOOTSTRAP
  if (method == "bootstrap") {

    set.seed(NULL)

    lb_boot <- numeric(nboot)
    ub_boot <- numeric(nboot)

    for (b in seq_len(nboot)) {

      idx <- sample(seq_len(n), size = n, replace = TRUE)
      d_b <- data[idx, , drop = FALSE]

      lb_b <- try(aprlb(d_b, y, z, x, model, quiet = TRUE), silent = TRUE)
      ub_b <- try(aprub(d_b, y, t, z, x, model), silent = TRUE)

      lb_boot[b] <- if (inherits(lb_b, "try-error")) NA else lb_b$lb_coef
      ub_boot[b] <- if (inherits(ub_b, "try-error")) NA else ub_b$ub_coef
    }

    lb_boot <- lb_boot[!is.na(lb_boot)]
    ub_boot <- ub_boot[!is.na(ub_boot)]

    ci_lb <- quantile(lb_boot, probs = alpha)
    ci_ub <- quantile(ub_boot, probs = 1 - alpha)

    res <- list(
      lb_coef = lb_coef,
      ub_coef = ub_coef,
      ci_lb = ci_lb,
      ci_ub = ci_ub,
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

  class(res) <- "persuasio4ytz"
  return(res)
}
