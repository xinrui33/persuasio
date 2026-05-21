test_that("persuasio4ytz returns structured result", {

  df <- data.frame(
    y = c(1,0,1,0,1,0,1,0),
    t = c(1,1,0,0,1,0,1,0),
    z = c(1,1,1,1,0,0,0,0),
    x1 = c(1,2,1,2,1,2,1,2)
  )

  res <- persuasio4ytz(df, "y", "t", "z")

  expect_true(is.list(res))
  expect_true("lb_coef" %in% names(res))
  expect_true("ub_coef" %in% names(res) || is.null(res$ub_coef))
})


test_that("persuasio4yz returns scalar bound", {

  df <- data.frame(
    y = c(1,0,1,0,1,0,1,0),
    t = c(1,1,0,0,1,0,1,0),
    z = c(1,1,1,1,0,0,0,0),
    x1 = c(1,2,1,2,1,2,1,2)
  )

  res <- persuasio4yz(df, "y", "z")

  expect_true(is.numeric(res$lb_coef))
  expect_true(res$lb_coef >= 0 && res$lb_coef <= 1)
})


test_that("persuasio4ytz2lpr returns lpr estimate", {

  df <- data.frame(
    y = c(1,0,1,0,1,0,1,0),
    t = c(1,1,0,0,1,0,1,0),
    z = c(1,1,1,1,0,0,0,0),
    x1 = c(1,2,1,2,1,2,1,2)
  )

  res <- persuasio4ytz2lpr(df, "y", "t", "z")

  expect_true(is.numeric(res$lpr))
  expect_true(res$lpr >= 0 && res$lpr <= 1)
})

test_that("persuasio wrapper routes to apr correctly", {

  df <- data.frame(
    y = c(1,0,1,0,1,0,1,0),
    t = c(1,1,0,0,1,0,1,0),
    z = c(1,1,1,1,0,0,0,0),
    x1 = c(1,2,1,2,1,2,1,2)
  )

  res <- persuasio(
    est = "apr",
    varlist = c("y","t","z"),
    data = df
  )

  expect_true(!is.null(res))
  expect_true(is.list(res))
  expect_true(!is.null(res$lb_coef))
})
