#' Upper Bound of Average Persuasion Rate
#'
#' Computes the upper bound of the Average Persuasion Rate (APR) under binary outcome
#' and binary instrument settings, with optional covariates.
#'
#' The estimator follows a two-case structure:
#' \itemize{
#'   \item No covariates / no interaction: closed-form sandwich estimator with delta method SE.
#'   \item Covariates + interaction: plug-in regression-based bounds (no analytic SE).
#' }
#'
#' @param data data.frame containing variables
#' @param Y character. Binary outcome variable name (0/1)
#' @param T character. Binary treatment variable name (0/1)
#' @param Z character. Binary instrument variable name (0/1)
#' @param X optional character vector of covariate names
#' @param model character. Either "no_interaction" or "interaction"
#'
#' @return A list with:
#' \describe{
#'   \item{ub_coef}{Upper bound estimate of APR}
#'   \item{ub_se}{Standard error (NA if covariates are used)}
#'   \item{ci_lb}{Lower bound of 95% CI (NA if covariates used)}
#'   \item{ci_ub}{Upper bound of 95% CI (NA if covariates used)}
#'   \item{outcome}{Outcome variable name}
#'   \item{treatment}{Treatment variable name}
#'   \item{instrument}{Instrument variable name}
#'   \item{covariates}{Covariates used (if any)}
#'   \item{model}{Model specification}
#'   \item{n}{Sample size}
#'   \item{class}{S3 class label ("aprub")}
#' }
#'
#' @details
#' This function implements the upper bound derived in the persuasion rate framework.
#' When covariates are included, the estimator relies on a plug-in regression approach,
#' and standard errors are not currently implemented.
#'
#' @examples
#' data <- data.frame(
#'   y = c(0,1,0,1),
#'   t = c(0,1,0,1),
#'   z = c(0,1,0,1)
#' )
#'
#' aprub(data, "y", "t", "z")
#'
#' @export
aprub <- function(data, y, t, z, x = NULL, model = "no_interaction") {

  model <- match.arg(model, c("no_interaction", "interaction"))

  if (model == "interaction" && is.null(x)) {
    warning("model='interaction' ignored because X is NULL")
    model <- "no_interaction"
  }

  yv <- data[[y]]
  zv <- data[[z]]

  if (!all(yv %in% c(0,1))) stop(y, " must be binary")
  if (!all(zv %in% c(0,1))) stop(z, " must be binary")

  # CASE 1: NO COVARIATES
  if (model == "no_interaction") {

    # A = Y * T + (1 - T)
    # B = Y * (1 - T)
    A <- y
    B <- 1 - y   # simplified reduced form (no need to store columns)

    fA <- lm(as.formula(paste(y, "~", z)), data = data)
    fB <- lm(as.formula(paste("I(1 -", y, ")", "~", z)), data = data)

    alpha_0 <- coef(fA)["(Intercept)"]
    alpha_1 <- coef(fA)[z]
    beta_0  <- coef(fB)["(Intercept)"]

    den <- (1 - beta_0)

    ub_coef <- (alpha_0 + alpha_1 - beta_0) / den

    # sandwich vcov
    n <- nrow(data)
    X1 <- model.matrix(fA)
    X2 <- model.matrix(fB)

    e1 <- residuals(fA)
    e2 <- residuals(fB)

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

    # delta method
    idx1 <- which(colnames(X1) == z)
    idx2 <- k1 + which(colnames(X2) == "(Intercept)")

    G <- matrix(0, 1, k)
    G[1, idx1] <- 1 / den
    G[1, idx2] <- -(alpha_0 + alpha_1 - 1) / (den^2)

    se <- sqrt(as.numeric(G %*% V %*% t(G)))

    ci_lb <- ub_coef - qnorm(0.975) * se
    ci_ub <- ub_coef + qnorm(0.975) * se

    res <- list(
      ub_coef = as.numeric(ub_coef),
      ub_se = as.numeric(se),
      ci_lb = as.numeric(ci_lb),
      ci_ub = as.numeric(ci_ub),
      outcome = y,
      treatment = t,
      instrument = z,
      covariates = x,
      model = model,
      n = n
    )

    class(res) <- "aprub"
    return(res)
  }

  # CASE 2: COVARIATES
  X_formula <- paste(x, collapse = " + ")

  if (model == "interaction") {

    fmla_A <- as.formula(paste(y, "~", z, "+", X_formula))
    fmla_B <- as.formula(paste("I(1 -", y, ") ~", z, "+", X_formula))

    reg_A <- lm(fmla_A, data = data)
    reg_B <- lm(fmla_B, data = data)

    yhat_A <- predict(reg_A)
    yhat_B <- predict(reg_B)

    beta_A <- coef(reg_A)[z]
    beta_B <- coef(reg_B)[z]

    yhat1 <- yhat_A + beta_A - beta_A * z
    yhat0 <- yhat_B - beta_B * z

    yhat1 <- pmin(pmax(yhat1, 0), 1)
    yhat0 <- pmin(pmax(yhat0, 0), 1)

    ub_coef <- mean(yhat1 - yhat0) / mean(1 - yhat0)

    res <- list(
      ub_coef = as.numeric(ub_coef),
      ub_se = NA_real_,
      ci_lb = NA_real_,
      ci_ub = NA_real_,
      outcome = y,
      treatment = t,
      instrument = z,
      covariates = x,
      model = model,
      n = nrow(data)
    )

    class(res) <- "aprub"
    return(res)
  }

}
