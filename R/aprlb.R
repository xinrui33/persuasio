#' Estimate the lower bound of the average persuasion rate
#'
#' __aprlb__ estimates the lower bound of the average persuasion rate (APR).
#' _veclist_ should include _depvar_ _instrvar_ _covariates_ in order. Here,
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
#'   \item \code{class}: S3 class label ("aprlb")
#' }
#'
#' @export
aprlb <- function(data, y, z, x = NULL, model = "no_interaction") {

  model <- match.arg(model, c("no_interaction", "interaction"))

  if (model == "interaction" && is.null(x)) {
    warning("interaction ignored because x is NULL")
    model <- "no_interaction"
  }

  y_vec <- data[[y]]
  z_vec <- data[[z]]

  if (!all(y_vec %in% c(0,1))) stop(paste0(y, "must be binary"))
  if (!all(z_vec %in% c(0,1))) stop(paste0(z, "must be binary"))

  # Case 1: No covariates
  if (is.null(x)) {

    fmla <- as.formula(paste(y, "~", z))

    fit <- lm(fmla, data = data)

    est_z <- coef(fit)[z]
    est_0 <- coef(fit)["(Intercept)"]

    # lower bound estimate
    lb_coef <- est_z / (1 - est_0)

    # sandwich vcov
    n <- nrow(data)
    k <- length(coef(fit))
    Xmat <- model.matrix(fit)
    e <- residuals(fit)

    XtX_inv <- solve(crossprod(Xmat))
    meat <- crossprod(Xmat * e)

    vcov <- XtX_inv %*% meat %*% XtX_inv
    vcov <- vcov * (n / (n - k)) # HC1 correction

    coef_names <- colnames(Xmat)

    idx <- match(c(z, "(Intercept)"), coef_names)

    if (any(is.na(idx))) {

      missing <- c(z, "(Intercept)")[is.na(idx)]

      stop(
        "Required coefficient(s) not found in model matrix: ",
        paste(missing, collapse = ", "),
        "\nThis may be due to: missing intercept, collinearity, or factor coding."
      )
    }

    V <- vcov[idx, idx, drop = FALSE]

    # gradient matrix
    g <- matrix(c(
      1 / (1 - est_0),
      est_z / (1 - est_0)^2
    ), nrow = 1)

    se <- sqrt(as.numeric(g %*% V %*% t(g)))

    z_score <- qnorm(0.975)

    ci_lb <- lb_coef - z_score * se
    ci_ub <- lb_coef + z_score * se

    res <- list(
      lb_coef = as.numeric(lb_coef),
      lb_se = as.numeric(se),
      ci_lb = as.numeric(unlist(ci_lb)),
      ci_ub = as.numeric(unlist(ci_ub)),
      outcome = y,
      instrument = z,
      covariates = x,
      model = model,
      n = n
    )

    class(res) <- "aprlb"
    res
  }

  # Case 2: With covariates

  else {

    x_vec <- data[[x]]

    x_formula <- paste(x, collapse = " + ")

    if (model == "no_interaction") {

      fmla <- as.formula(
        paste(y, "~", z, "+", x_formula)
      )

      fit <- lm(fmla, data = data)

      yhat <- predict(fit)

      beta_z <- coef(fit)[z]

      yhat1 <- yhat + beta_z * (1 - z_vec)
      yhat0 <- yhat - beta_z * z_vec
    }

    if (model == "interaction") {

      fmla <- as.formula(
        paste(y, "~", x_formula)
      )

      fit1 <- lm(fmla, data = data[z_vec == 1, ])
      fit0 <- lm(fmla, data = data[z_vec == 0, ])

      yhat1 <- predict(fit1, newdata = data)
      yhat0 <- predict(fit0, newdata = data)
    }

    # clip fitted values
    yhat1 <- pmin(pmax(yhat1, 0), 1)
    yhat0 <- pmin(pmax(yhat0, 0), 1)

    # lower bound
    lb_num <- mean(yhat1 - yhat0)
    lb_den <- mean(1 - yhat0)

    lb_coef <- lb_num / lb_den

    res <- list(
      lb_coef = as.numeric(lb_coef),
      lb_se = NA_real_,
      ci_lb = NA_real_,
      ci_ub = NA_real_,
      outcome = y,
      instrument = z,
      covariates = x,
      model = model,
      n = nrow(data)
    )

    class(res) <- "aprlb"
    return(res)
  }
}
