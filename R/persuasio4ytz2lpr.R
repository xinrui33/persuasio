#' Local Persuasion Rate Inference Wrapper
#'
#' Wrapper for LPR estimation and inference.
#'
#' @export
persuasio4ytz2lpr <- function(data, y, t, z, x = NULL,
                              level = 0.95,
                              model = "no_interaction",
                              method = "normal",
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

  # =========================================================
  # NORMAL APPROXIMATION
  # =========================================================
  if (method == "normal") {

    if (is.na(se)) {
      stop("Normal CI not available (SE is NA). Use method='bootstrap'.")
    }

    z_crit <- qnorm(1 - alpha / 2)

    ci_lb <- max(0, lpr_coef - z_crit * se)
    ci_ub <- min(1, lpr_coef + z_crit * se)

    result <- list(
      lpr = lpr_coef,
      ci_lb = ci_lb,
      ci_ub = ci_ub,
      se = se,
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
  }

  # =========================================================
  # BOOTSTRAP
  # =========================================================
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

    result <- list(
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

  class(result) <- "persuasio4ytz2lpr"
  return(result)
}
