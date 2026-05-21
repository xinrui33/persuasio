# minimal toy dataset (covers all required binary cases)
df <- data.frame(
  y = c(0,1,0,1,0,1),
  t = c(0,1,0,1,0,1),
  z = c(0,0,1,1,0,1)
)


# 1. aprlb (needs y, z only)

test_that("aprlb works with required inputs only", {

  res <- aprlb(df, "y", "z")

  expect_s3_class(res, "list")
  expect_true(is.numeric(res$lb_coef))
  expect_true(res$lb_coef >= 0 && res$lb_coef <= 1)

  expect_named(res, c(
    "lb_coef","lb_se","ci_lb","ci_ub",
    "nobs","outcome","instrument","covariates","model"
  ))
})


# 2. aprub (needs y, t, z)

test_that("aprub works with required inputs only", {

  res <- aprub(df, "y", "t", "z")

  expect_s3_class(res, "list")
  expect_true(is.numeric(res$ub_coef))
  expect_true(res$ub_coef >= 0 && res$ub_coef <= 1)

  expect_true(!is.null(res$outcome))
  expect_true(!is.null(res$treatment))
  expect_true(!is.null(res$instrument))
})


# 3. lpr4ytz (needs y, t, z)

test_that("lpr4ytz works with required inputs only", {

  res <- lpr4ytz(df, "y", "t", "z")

  expect_s3_class(res, "list")
  expect_true(is.numeric(res$lpr))
  expect_true(res$lpr >= 0 && res$lpr <= 1)

  expect_true("lpr" %in% names(res))
  expect_true("ci_lb" %in% names(res))
  expect_true("ci_ub" %in% names(res))
})


# 4. calc4persuasio (no dataframe dependency)

test_that("calc4persuasio works independently of data", {

  res <- calc4persuasio(
    y1 = 0.6,
    y0 = 0.3,
    e1 = 0.7,
    e0 = 0.2
  )

  expect_s3_class(res, "calc4persuasio")

  expect_true(all(res$lower <= res$upper))
  expect_true(all(res$parameter %in% c("APR","LPR")))

  expect_equal(nrow(res), 2)
})
