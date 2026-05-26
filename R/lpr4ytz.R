#' Estimate the local persuasion rate
#'
#' __lpr4ytz__ estimates the local persuasion rate (LPR). _veclist_ should
#' include _depvar_ _treatrvar_ _instrvar_ _covariates_ in order. Here, _depvar_
#' is binary outcomes (_y_), _treatrvar_ is binary treatments (_t_), _instrvar_
#' is binary instruments (_z_), and _covariates_ (_x_) are optional. There are
#' two cases: (i) _covariates_ are absent and (ii) _covariates_ are present.
#'
#' @param data data.frame
#' @param y outcome variable (binary)
#' @param t treatment variable (binary)
#' @param z instrument variable (binary)
#' @param x covariates (optional character vector)
#' @param model "no_interaction" or "interaction"
#'
#' @return A list with:
#' \itemize{
#'   \item \code{lpr}: Estimated Local Persuasion Rate
#'   \item \code{se}: Standard error of the estimate (NA under interaction model)
#'   \item \code{ci_lb}: Lower bound of the confidence interval
#'   \item \code{ci_ub}: Upper bound of the confidence interval
#'   \item \code{n}: Sample size
#'   \item \code{outcome}: Outcome variable name
#'   \item \code{treatment}: Treatment variable name
#'   \item \code{instrument}: Instrument variable name
#'   \item \code{covariates}: Covariates used in estimation
#'   \item \code{model}: Model specification used
#'   \item \code{case}: Estimation case used ("interaction" or "no_interaction")
#'   \item \code{class}: S3 class label ("lpr4ytz")
#' }
#'
#' @export
lpr4ytz <- function(data, y, t, z, x = NULL, model = "no_interaction") {

  model <- match.arg(model, c("no_interaction", "interaction"))

  if (model == "interaction" && is.null(x)) {
    warning("model='interaction' ignored because X is NULL")
    model <- "no_interaction"
  }

  y_vec <- data[[y]]
  t_vec <- data[[t]]
  z_vec <- data[[z]]

  if (!all(y_vec %in% c(0,1))) stop(paste0(y, "must be binary"))
  if (!all(t_vec %in% c(0,1))) stop(paste0(t, "must be binary"))
  if (!all(z_vec %in% c(0,1))) stop(paste0(z, "must be binary"))

  # lpr denominator
  data$den_lpr <- (1 - y_vec) * (1 - t_vec)

  # No covariates or no interaction

  if (is.null(x) || model == "no_interaction") {

    fmla <- if (!is.null(x)) paste(c(z, x), collapse = " + ") else z

    fit_num <- lm(as.formula(paste(y, "~", fmla)), data = data)
    fit_den <- lm(as.formula(paste("den_lpr ~", fmla)), data = data)

    X1 <- model.matrix(fit_num)
    X2 <- model.matrix(fit_den)

    e1 <- residuals(fit_num)
    e2 <- residuals(fit_den)

    # sandwich vcov
    n <- nrow(data)
    B1 <- solve(crossprod(X1))
    B2 <- solve(crossprod(X2))

    k1 <- ncol(X1)
    k2 <- ncol(X2)
    k <- k1 + k2

    B <- matrix(0, k, k)
    B[1:k1, 1:k1] <- B1
    B[(k1+1):k, (k1+1):k] <- B2

    S1 <- X1 * e1
    S2 <- X2 * e2

    M <- rbind(
      cbind(crossprod(S1), crossprod(S1, S2)),
      cbind(t(crossprod(S1, S2)), crossprod(S2))
    )

    V <- B %*% M %*% B
    V <- V * (n / (n - 1)) # Stata small sample adjustment

    # extract coefficients
    beta_num <- coef(fit_num)[z]
    beta_den <- coef(fit_den)[z]

    idx1 <- which(colnames(X1) == z)
    idx2 <- k1 + which(colnames(X2) == z)

    # LPR estimate
    lpr <- beta_num / (-beta_den)

    # delta method gradient
    G <- matrix(0, nrow = 1, ncol = k)
    G[1, idx1] <- 1 / (-beta_den)
    G[1, idx2] <- beta_num / (beta_den^2)

    se <- sqrt(G %*% V %*% t(G))

    # 95% CI
    z_score <- qnorm(0.975)

    ci_lb <- lpr - z_score * se
    ci_ub <- lpr + z_score * se

    res <- list(
      lpr = as.numeric(unlist(lpr)),
      se = as.numeric(unlist(se)),
      ci_lb = as.numeric(unlist(ci_lb)),
      ci_ub = as.numeric(unlist(ci_ub)),
      n = n,
      outcome = y,
      treatment = t,
      instrument = z,
      covariates = x,
      model = model
    )

    class(res) <- "lpr4ytz"
    return(res)
  }

  # With covariates and interaction
  else {

    fmla <- paste(x, collapse = " + ")

    get_pred <- function(outcome, val) {

      df_sub <- data[z_vec == val, , drop = FALSE]

      fit <- lm(as.formula(paste(outcome, "~", fmla)), data = df_sub)

      pred <- predict(fit, newdata = data)

      return(pmin(pmax(pred, 0), 1))
    }

    y1 <- get_pred(y, 1)
    y0 <- get_pred(y, 0)

    den1 <- get_pred("den_lpr", 1)
    den0 <- get_pred("den_lpr", 0)

    num <- mean(y1 - y0)
    den <- mean(den0 - den1)

    lpr <- num / den

    res <- list(
      lpr = as.numeric(unlist(lpr)),
      se = NA_real_,
      ci_lb = NA_real_,
      ci_ub = NA_real_,
      n = nrow(data),
      outcome = y,
      treatment = t,
      instrument = z,
      covariates = x,
      model = model,
      note = "Use bootstrap for SE (recommended 1000 reps)"
    )

    class(res) <- "lpr4ytz"
    return(res)
  }

}
