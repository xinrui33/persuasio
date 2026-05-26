# test whether functions work with GKB dataset

# base functions
test_that("aprlb works with GKB dataset", {

  data("GKB")

  res <- aprlb(GKB, "voteddem_all", "post")

  expect_s3_class(res, "aprlb")
  expect_true(is.numeric(res$lb_coef))
  expect_true(res$lb_coef >= 0 && res$lb_coef <= 1)

  expect_equal(res$outcome, "voteddem_all")
  expect_equal(res$instrument, "post")
})


test_that("aprub works with GKB dataset", {

  data("GKB")

  res <- aprub(GKB, "voteddem_all", "readsome", "post")

  expect_s3_class(res, "aprub")
  expect_true(is.numeric(res$ub_coef))
  expect_true(res$ub_coef >= 0 && res$ub_coef <= 1)

  expect_equal(res$outcome, "voteddem_all")
  expect_equal(res$treatment, "readsome")
  expect_equal(res$instrument, "post")
})


test_that("lpr4ytz works with GKB dataset", {

  data("GKB")

  res <- lpr4ytz(GKB, "voteddem_all", "readsome", "post")

  expect_s3_class(res, "lpr4ytz")
  expect_true(is.numeric(res$lpr))
  expect_true(res$lpr >= 0 && res$lpr <= 1)

  expect_equal(res$outcome, "voteddem_all")
  expect_equal(res$treatment, "readsome")
  expect_equal(res$instrument, "post")
})

test_that("calc4persuasio works with GKB summary statistics", {

  data("GKB")

  # R summary statistics
  y1 <- mean(GKB$voteddem_all[GKB$post == 1])
  y0 <- mean(GKB$voteddem_all[GKB$post == 0])

  e1 <- mean(GKB$readsome[GKB$post == 1])
  e0 <- mean(GKB$readsome[GKB$post == 0])

  # with exposure rates
  res <- calc4persuasio(
    y1 = y1,
    y0 = y0,
    e1 = e1,
    e0 = e0
  )

  expect_s3_class(res, "calc4persuasio")
  expect_true(is.list(res))

  expect_equal(res$case, "with exposure rates")

  expect_true(res$apr["lower"] <= res$apr["upper"])
  expect_true(res$lpr["lower"] <= res$lpr["upper"])

  expect_true(all(is.finite(unlist(res$apr))))
  expect_true(all(is.finite(unlist(res$lpr))))

  # no exposure rates
  res_partial <- calc4persuasio(
    y1 = y1,
    y0 = y0
  )

  expect_s3_class(res_partial, "calc4persuasio")
  expect_true(is.list(res_partial))

  expect_equal(res_partial$case, "no exposure rates")

  expect_equal(res_partial$apr[["upper"]], 1)
  expect_equal(res_partial$lpr[["upper"]], 1)

  expect_true(res_partial$apr["lower"] <= res_partial$apr["upper"])
  expect_true(res_partial$lpr["lower"] <= res_partial$lpr["upper"])
})

# wrapper functions

test_that("persuasio4ytz returns structured result with GKB", {

  data("GKB")

  res <- persuasio4ytz(GKB, "voteddem_all", "readsome", "post")

  expect_true(is.list(res))
  expect_true("lb_coef" %in% names(res))
  expect_true("ub_coef" %in% names(res) || is.null(res$ub_coef))
})


test_that("persuasio4yz returns scalar bound with GKB", {

  data("GKB")

  res <- persuasio4yz(GKB, "voteddem_all", "post")

  expect_true(is.numeric(res$lb_coef))
  expect_true(res$lb_coef >= 0 && res$lb_coef <= 1)
})


test_that("persuasio4ytz2lpr returns lpr estimate with GKB", {

  data("GKB")

  res <- persuasio4ytz2lpr(GKB, "voteddem_all", "readsome", "post")

  expect_true(is.numeric(res$lpr))
  expect_true(res$lpr >= 0 && res$lpr <= 1)
})


test_that("persuasio wrapper routes to apr correctly with GKB", {

  data("GKB")

  res <- persuasio(
    est = "apr",
    varlist = c("voteddem_all", "readsome", "post"),
    data = GKB
  )

  expect_true(!is.null(res))
  expect_true(is.list(res))
  expect_true(!is.null(res$lb_coef))
})
