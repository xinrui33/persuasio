#' Estimate the lower bound on the average persuasion rate
#'
#' __aprlb__ estimates the lower bound on the average persuasion rate (APR).
#' _varlist_ should include _depvar_ _instrvar_ _covariates_ in order. Here,
#' _depvar_ is binary outcomes (_y_), _instrvar_ is binary instruments (_z_),
#' and _covariates_ (_x_) are optional.
#'
#' There are two cases: (i) _covariates_ are absent and (ii) _covariates_ are
#' present.
#'
#' With covariates, there are two model specifications: `no_interaction`and
#' `interaction`.
#'
#'
#' @param data data.frame containing variables
#' @param y outcome, outcome variable (binary 0/1)
#' @param z instrument, instrument variable (binary 0/1)
#' @param x optional covariates, if they exist
#' @param model model specification: "no_interaction" or "interaction"
#'
#'
#' @return A list with:
#' \itemize{
#'   \item \code{lb_coef}: Lower bound estimate of the Average Persuasion Rate
#'   \item \code{lb_se}: Standard error of the estimate (NA under interaction model)
#'   \item \code{ci_lb}: Lower bound of the 95\% confidence interval
#'   \item \code{ci_ub}: Upper bound of the 95\% confidence interval
#'   \item \code{outcome}: Outcome variable name
#'   \item \code{instrument}: Instrument variable name
#'   \item \code{covariates}: Covariates used in estimation
#'   \item \code{model}: Model specification used
#'   \item \code{n}: Sample size
#'   \item \code{class}: S3 class label ("aprub")
#' }
#'
#' @export
aprlb <- function(data, y, z, x = NULL, model = "no_interaction") {

  model <- match.arg(model, c("no_interaction", "interaction"))

  if (model == "interaction" && is.null(x)) {
    warning("model='interaction' ignored because X is NULL")
    model <- "no_interaction"
  }

  yv <- data[[y]]
  zv <- data[[z]]

  if (!all(yv %in% c(0,1))) stop(y, " must be binary")
  if (!all(zv %in% c(0,1))) stop(z, " must be binary")

  # CASE 1: NO INTERACTION MODEL
  if (model == "no_interaction") {

    fmla <- as.formula(paste(y, "~", z,
                             if (!is.null(x)) paste("+", paste(x, collapse = "+"))))

    fit <- lm(fmla, data = data)

    y <- data[[y]]
    z <- data[[z]]

    est_z <- coef(fit)[z]
    est_intercept <- coef(fit)["(Intercept)"]

    # lower bound estimate
    lb_coef <- est_z / (1 - est_intercept)

    # sandwich vcoc
    n <- nrow(data)
    Xmat <- model.matrix(fit)
    e <- residuals(fit)

    XtX_inv <- solve(crossprod(Xmat))
    meat <- crossprod(Xmat * e)
    vcov <- XtX_inv %*% meat %*% XtX_inv
    vcov <- vcov * (n / (n - ncol(Xmat)))

    idx <- c(which(colnames(Xmat) == z),
             which(colnames(Xmat) == "(Intercept)"))

    V <- vcov[idx, idx]

    g <- matrix(c(
      1 / (1 - est_intercept),
      est_z / (1 - est_intercept)^2
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
      outcome = y,
      instrument = z,
      covariates = x,
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

  y1 <- get_pred(y, 1)
  y0 <- get_pred(y, 0)

  lb_coef <- mean(y1 - y0)

  res <- list(
    lb_coef = as.numeric(lb_coef),
    lb_se = NA_real_,
    ci_lb = NA_real_,
    ci_ub = NA_real_,
    outcome = y,
    instrument = z,
    covariates = x,
    model = model,
    n = n
  )

  class(res) <- "aprlb"
  return(res)
}
