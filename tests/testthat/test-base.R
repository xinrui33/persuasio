# shared toy dataset
df <- data.frame(
  y = c(1,0,1,0,1,0,1,0),
  t = c(1,1,0,0,1,0,1,0),
  z = c(1,1,1,1,0,0,0,0),
  x1 = c(1,2,1,2,1,2,1,2)
)


# 1. aprlb (needs y, z only)

test_that("aprlb works with required inputs only", {

  res <- aprlb(df, "y", "z")

  expect_s3_class(res, "list")
  expect_true(is.numeric(res$lb_coef))
  expect_true(res$lb_coef >= 0 && res$lb_coef <= 1)

  expect_true(!is.null(res$outcome))
  expect_true(!is.null(res$instrument))
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

  expect_true(!is.null(res$outcome))
  expect_true(!is.null(res$treatment))
  expect_true(!is.null(res$instrument))
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

  expect_true(all(res$ci_lb <= res$ci_ub, na.rm = TRUE))

  expect_true(length(res$ci_lb) > 0)
  expect_true(length(res$ci_ub) > 0)

  expect_true(all(is.finite(unlist(res))))
})
