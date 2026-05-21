#' Local Persuasion Rate (LPR)
#'
#' Estimates the Local Persuasion Rate using binary outcome, treatment, and instrument.
#'
#' @param data data.frame
#' @param y outcome variable (binary)
#' @param t treatment variable (binary)
#' @param z instrument variable (binary)
#' @param x covariates (optional character vector)
#' @param model "no_interaction" or "interaction"
#'
#' @return A list of class \code{lpr4ytz}
#' @export
lpr4ytz <- function(data, y, t, z, x = NULL, model = "no_interaction") {

  # 1. input checks
  yv <- data[[y]]
  tv <- data[[t]]
  zv <- data[[z]]

  if (!all(yv %in% c(0,1))) stop(y, " must be binary")
  if (!all(tv %in% c(0,1))) stop(t, " must be binary")
  if (!all(zv %in% c(0,1))) stop(z, " must be binary")

  # helper variable (denominator structure)
  data$den_lpr <- (1 - data[[y]]) * (1 - data[[t]])

  n <- nrow(data)

  # CASE 1: NO COVARIATES OR NO INTERACTION

  if (is.null(x) || model == "no_interaction") {

    rhs <- if (!is.null(x)) {
      paste(c(z, x), collapse = " + ")
    } else {
      z
    }

    # numerator and denominator models
    fit_num <- lm(as.formula(paste(y, "~", rhs)), data = data)
    fit_den <- lm(as.formula(paste("den_lpr ~", rhs)), data = data)

    X1 <- model.matrix(fit_num)
    X2 <- model.matrix(fit_den)

    e1 <- residuals(fit_num)
    e2 <- residuals(fit_den)

    # sandwich components
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
    V <- V * (n / (n - 1))

    # coefficient extraction
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

    ci <- c(
      lwr = lpr - qnorm(0.975) * se,
      upr = lpr + qnorm(0.975) * se
    )

    return(list(
      lpr = as.numeric(lpr),
      se = as.numeric(se),
      ci = ci,
      n = n,
      outcome = y,
      treatment = t,
      instrument = z,
      covariates = x,
      model = model,
      case = "no_interaction"
    ))
  }

  # CASE 2: INTERACTION MODEL
  if (!is.null(x) && model == "interaction") {

    rhs <- paste(x, collapse = " + ")

    get_pred <- function(outcome_var, z_val) {
      df_sub <- data[data[[z]] == z_val, , drop = FALSE]
      fit <- lm(as.formula(paste(outcome_var, "~", rhs)), data = df_sub)
      p <- predict(fit, newdata = data)
      pmin(pmax(p, 0), 1)
    }

    y1 <- get_pred(y, 1)
    y0 <- get_pred(y, 0)

    den1 <- get_pred("den_lpr", 1)
    den0 <- get_pred("den_lpr", 0)

    num <- mean(y1 - y0)
    den <- mean(den0 - den1)

    lpr <- num / den

    return(list(
      lpr = as.numeric(lpr),
      se = NA,
      ci = c(NA, NA),
      n = n,
      outcome = y,
      treatment = t,
      instrument = z,
      covariates = x,
      model = model,
      case = "interaction"
    ))
  }

  stop("Invalid model specification")
}
