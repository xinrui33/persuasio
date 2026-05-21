#' Average Persuasion Rate Inference
#'
#' Main wrapper for estimating APR bounds and inference.
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

    result <- list(
      lb_coef = lb_coef,
      ub_coef = ub_coef,
      ci_lb = ci_lb,
      ci_ub = ci_ub,
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

    result <- list(
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

  # attach class
  class(result) <- "persuasio4ytz"
  return(result)
}
