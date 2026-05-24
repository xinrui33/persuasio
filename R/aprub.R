#' Estimate the upper bound of the average persuasion rate
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

  y_var <- data[[y]]
  z_var <- data[[z]]
  t_var <- data[[t]]

  if (!all(y_var %in% c(0,1))) stop(paste(y, " must be binary"))
  if (!all(t_var %in% c(0,1))) stop(paste(t, " must be binary"))
  if (!all(z_var %in% c(0,1))) stop(paste(z, " must be binary"))

  A <- y_var * t_var + (1 - t_Var)
  B <- y_var * (1 - t_var)

  # Case 1: No covariates
  if (is.null(x)) {

    fA <- lm(as.formula(paste(A, "~", z_var)), data = data)
    fB <- lm(as.formula(paste(B, "~", z_var)), data = data)

    alpha_0 <- coef(fA)["(Intercept)"]
    alpha_1 <- coef(fA)[z_var]
    beta_0  <- coef(fB)["(Intercept)"]

    ub_coef <- (alpha_0 + alpha_1 - beta_0) / (1 - beta_0)

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

    # bread
    B <- matrix(0, k, k)
    B[1:k1, 1:k1] <- B1
    B[(k1+1):k, (k1+1):k] <- B2

    # meat
    S1 <- X1 * e1
    S2 <- X2 * e2

    M <- rbind(
      cbind(crossprod(S1), crossprod(S1, S2)),
      cbind(t(crossprod(S1, S2)), crossprod(S2))
    )

    V <- B %*% M %*% B
    V <- V * (n / (n - 1)) # Stata small-sample correction

    # delta methodc(analogous to Stata nlcom)
    idx_a0 <- which(colnames(X1) == "(Intercept)")
    idx_a1 <- which(colnames(X1) == z_var)
    idx_b0 <- k1 + which(colnames(X2) == "(Intercept)")

    G <- matrix(0, nrow = 1, ncol = k)
    G[1, idx1] <- 1 / (1 - beta_0)
    G[1, idx2] <- -(alpha_0 + alpha_1 - 1) / ((1 - beta_0)^2)

    se <- sqrt(as.numeric(G %*% V %*% t(G)))

    z_score <- qnorm(0.975)

    ci_lb <- ub_coef - z_score * se
    ci_ub <- ub_coef + z_score * se

    res <- list(
      ub_coef = as.numeric(ub_coef),
      ub_se = as.numeric(se),
      ci_lb = as.numeric(ci_lb),
      ci_ub = as.numeric(ci_ub),
      outcome = y_var,
      treatment = t_var,
      instrument = z_var,
      covariates = x,
      model = model,
      n = n
    )

    class(res) <- "aprub"
    return(res)
  }

  # Case 2: With covariates
  else {

    x_var <- data[[x]]
    fmla <- paste(x_var, collapse = " + ")

    if (model == "no_interaction") {

      fmla_A <- as.formula(paste("A ~", z, "+", fmla))
      fmla_B <- as.formula(paste("B ~", z, "+", fmla))

      fA <- lm(fmla_A, data = data)
      fB <- lm(fmla_B, data = data)

      yhat_A <- predict(fA)
      yhat_B <- predict(fB)

      beta_A <- coef(fA)[z_var]
      beta_B <- coef(fB)[z_var]

      yhat1 <- yhat_A + beta_A - beta_A * z_var
      yhat0 <- yhat_B - beta_B * z_var
    }

    if (model == "interaction") {

      fmla_A <- as.formula(paste("A ~", fmla))
      fmla_B <- as.formula(paste("B ~", fmla))

      fA <- lm(fmla_A, data = data[z_var == 1, ])
      fA <- lm(fmla_A, data = data[z_var == 0, ])

      yhat1 <- predict(fA, newdata = data)
      yhat0 <- predict(fB, newdata = data)
    }

    yhat1 <- pmin(pmax(yhat1, 0), 1)
    yhat0 <- pmin(pmax(yhat0, 0), 1)

    # upper bound
    ub_num <- mean(yhat1 - yhat0)
    ub_den <- mean(1 - yhat0)

    ub_coef <- ub_num / ub_den

    res <- list(
      ub_coef = as.numeric(ub_coef),
      ub_se = NA_real_,
      ci_lb = NA_real_,
      ci_ub = NA_real_,
      outcome = y_var,
      treatment = t_var,
      instrument = z_var,
      covariates = x_var,
      model = model,
      n = nrow(data)
    )

    class(res) <- "aprub"
    return(res)
  }

}
