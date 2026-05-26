test_that("interaction model works", {

  df <- data.frame(
    y = c(1, 1, 0, 0, 1, 0, 0, 0, 1, 1, 0, 0),
    t = c(1, 1, 1, 0, 1, 0, 0, 0, 1, 1, 0, 0),
    z = c(1, 1, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0),
    x1 = c(1, 2, 1, 2, 1, 2, 1, 2, 1, 2, 1, 2)
  )

  res1 <- aprlb(df, "y", "z", x = "x1", model = "interaction")
  expect_true(is.numeric(res1$lb_coef))

  res2 <- aprub(df, "y", "t", "z", x = "x1", model = "interaction")
  expect_true(is.numeric(res2$ub_coef))

  res3 <- lpr4ytz(df, "y", "t", "z", x = "x1", model = "interaction")
  expect_true(is.numeric(res3$lpr))
})
