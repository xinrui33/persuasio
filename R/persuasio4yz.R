#' Conduct causal inference on persuasive effects for binary outcomes _y_ and
#' binary instruments _z_
#'
#' Computes bounds for the Average Persuasion Rate using the YZ formulation,
#' combining lower and upper bound estimators (\code{aprlb} and \code{aprub})
#' with inference via either normal approximation or bootstrap.
#'
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
#' @details When \code{method = "normal"}, confidence intervals are constructed
#' using a Stoye-style correction that accounts for dependence between lower and
#' upper bounds.
#'
#' When \code{method = "bootstrap"}, inference is based on joint resampling of
#' lower and upper bound estimators.
#'
#' If either bound has missing standard errors, the bootstrap method is
#' recommended.
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
  ub <- aprub(data, y, NULL, z, x, model)

  lb_coef <- lb$lb_coef
  ub_coef <- ub$ub_coef

  lb_se <- lb$lb_se
  ub_se <- ub$ub_se

  n <- nrow(data)
  alpha <- 1 - level

  if (method == "normal") {

    if (is.na(lb_se) || is.na(ub_se)) {
      stop("Normal CI not available. Use bootstrap.")
    }

    cv1 <- qnorm(1 - alpha)
    cv2 <- qnorm(1 - alpha / 2)

    correction <- (ub_coef - lb_coef) / max(lb_se, ub_se)

    grid <- seq(cv1 - 0.01, cv2 + 0.01, length.out = n)

    loss <- abs(pnorm(grid + correction) - pnorm(-grid) - (1 - alpha))

    cv_star <- mean(grid[loss == min(loss)])

    ci_lb <- lb_coef - cv_star * lb_se
    ci_ub <- ub_coef + cv_star * ub_se

    res <- list(
      lb_coef = lb_coef,
      ub_coef = ub_coef,
      ci_lb = ci_lb,
      ci_ub = ci_ub,
      level = level,
      method = "normal",
      n = n,
      outcome = y,
      instrument = z,
      covariates = x,
      model = model,
      title = title
    )

    class(res) <- "persuasio4yz"
    return(res)
  }

  if (method == "bootstrap") {

    lb_boot <- numeric(nboot)
    ub_boot <- numeric(nboot)

    for (b in seq_len(nboot)) {

      idx <- sample(seq_len(n), size = n, replace = TRUE)
      d_b <- data[idx, , drop = FALSE]

      lb_b <- try(aprlb(d_b, y, z, x, model, quiet = TRUE), silent = TRUE)
      ub_b <- try(aprub(d_b, y, NULL, z, x, model), silent = TRUE)

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
