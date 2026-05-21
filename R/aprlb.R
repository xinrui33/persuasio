#' Lower Bound of Average Persuasion Rate
#'
#' Computes the lower bound of the Average Persuasion Rate using a linear probability model
#' and sandwich (HC1) variance estimation.
#'
#' Supports two model specifications:
#' \itemize{
#'   \item \code{no_interaction}: pooled regression with covariates
#'   \item \code{interaction}: separate regressions by instrument status
#' }
#'
#' If \code{model = "interaction"} but \code{X = NULL}, the function falls back to
#' \code{"no_interaction"} with a warning.
#'
#' @param data data.frame containing variables
#' @param Y character, outcome variable (binary 0/1)
#' @param Z character, instrument variable (binary 0/1)
#' @param X optional character vector of covariates
#' @param model model specification: "no_interaction" or "interaction"
#' @param quiet logical, suppress messages
#'
#' @return A list containing:
#' \describe{
#'   \item{lb_coef}{lower bound estimate}
#'   \item{lb_se}{standard error (NA if interaction model)}
#'   \item{ci_lb}{95\% CI lower bound}
#'   \item{ci_ub}{95\% CI upper bound}
#'   \item{outcome}{Y}
#'   \item{instrument}{Z}
#'   \item{covariates}{X}
#'   \item{model}{model used}
#'   \item{n}{sample size}
#' }
#'
#' @export
aprlb <- function(data, Y, Z, X = NULL, model = "no_interaction", quiet = FALSE) {

  model <- match.arg(model, c("no_interaction", "interaction"))

  if (model == "interaction" && is.null(X)) {
    warning("model='interaction' ignored because X is NULL")
    model <- "no_interaction"
  }

  n <- nrow(data)

  # CASE 1: NO INTERACTION MODEL
  if (model == "no_interaction") {

    fmla <- as.formula(paste(Y, "~", Z,
                             if (!is.null(X)) paste("+", paste(X, collapse = "+"))))

    fit <- lm(fmla, data = data)

    y <- data[[Y]]
    z <- data[[Z]]

    est_Z <- coef(fit)[Z]
    est_intercept <- coef(fit)["(Intercept)"]

    # lower bound estimate
    lb_coef <- est_Z / (1 - est_intercept)

    # sandwich vcoc (HC1)
    Xmat <- model.matrix(fit)
    e <- residuals(fit)

    XtX_inv <- solve(crossprod(Xmat))
    meat <- crossprod(Xmat * e)
    vcov <- XtX_inv %*% meat %*% XtX_inv
    vcov <- vcov * (n / (n - ncol(Xmat)))

    idx <- c(which(colnames(Xmat) == Z),
             which(colnames(Xmat) == "(Intercept)"))

    V <- vcov[idx, idx]

    g <- matrix(c(
      1 / (1 - est_intercept),
      est_Z / (1 - est_intercept)^2
    ), nrow = 1)

    var <- as.numeric(g %*% V %*% t(g))
    se <- sqrt(var)

    zval <- qnorm(0.975)

    ci_lb <- lb_coef - zval * se
    ci_ub <- lb_coef + zval * se

    res <- list(
      lb_coef = as.numeric(lb_coef),
      lb_se = as.numeric(se),
      ci_lb = as.numeric(ci_lb),
      ci_ub = as.numeric(ci_ub),
      outcome = Y,
      instrument = Z,
      covariates = X,
      model = model,
      n = n
    )

    class(res) <- "aprlb"
    res
  }

  # CASE 2: INTERACTION MODEL

  rhs <- paste(X, collapse = " + ")

  get_pred <- function(outcome_var, z_val) {
    df_sub <- data[data[[Z]] == z_val, , drop = FALSE]
    fit <- lm(as.formula(paste(outcome_var, "~", rhs)), data = df_sub)
    p <- predict(fit, newdata = data)
    pmin(pmax(p, 0), 1)
  }

  y1 <- get_pred(Y, 1)
  y0 <- get_pred(Y, 0)

  lb_coef <- mean(y1 - y0)

  res <- list(
    lb_coef = as.numeric(lb_coef),
    lb_se = NA_real_,
    ci_lb = NA_real_,
    ci_ub = NA_real_,
    outcome = Y,
    instrument = Z,
    covariates = X,
    model = model,
    n = n
  )

  class(res) <- "aprlb"
  return(res)
}
