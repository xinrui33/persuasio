#' Estimate the upper bound on the average persuasion rate
#'
#' __aprub__ estimates the upper bound on the average persuasion rate (APR).
#' _varlist_ should include _depvar_ _treatrvar_ _instrvar_ _covariates_ in
#' order. Here, _depvar_ is binary outcomes (_y_), _treatrvar_ is binary
#' treatment (_t_), _instrvar_ is binary instruments (_z_), and _covariates_
#' (_x_) are optional.
#'
#' There are two cases: (i) _covariates_ are absent and (ii) _covariates_ are
#' present.
#'
#' With covariates, there are two model specifications: `no_interaction`and
#' `interaction`.
#'
#' @param data data.frame containing variables
#' @param y outcome, outcome variable (binary 0/1)
#' @param t treatment. treatment variable (binary 0/1)
#' @param z instrument, instrument variable (binary 0/1)
#' @param x optional covariates, if they exist
#' @param model model specification: "no_interaction" or "interaction"
#'
#'
#' @return A list with:
#' \itemize{
#'   \item \code{ub_coef}: Upper bound estimate of APR
#'   \item \code{ub_se}: Standard error (NA if covariates are used)
#'   \item \code{ci_lb}: Lower bound of 95\% CI (NA if covariates used)
#'   \item \code{ci_ub}: Upper bound of 95\% CI (NA if covariates used)
#'   \item \code{outcome}: Outcome variable name
#'   \item \code{treatment}: Treatment variable name
#'   \item \code{instrument}: Instrument variable name
#'   \item \code{covariates}: Covariates used (if any)
#'   \item \code{model}: Model specification
#'   \item \code{n}: Sample size
#'   \item \code{class}: S3 class label ("aprub")
#' }
#'
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
