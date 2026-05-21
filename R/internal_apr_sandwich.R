#' @keywords internal
.sandwich_vcov <- function(X, e, n) {

  XtX_inv <- solve(crossprod(X))
  meat <- crossprod(X * e)

  vcov <- XtX_inv %*% meat %*% XtX_inv

  # Stata HC1 correction
  vcov * (n / (n - ncol(X)))
}
