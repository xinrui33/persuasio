# toy dataset
df <- data.frame(
  y = c(1, 1, 0, 0, 1, 0, 0, 0, 1, 1, 0, 0),
  t = c(1, 1, 1, 0, 1, 0, 0, 0, 1, 1, 0, 0),
  z = c(1, 1, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0),
  x1 = c(1, 2, 1, 2, 1, 2, 1, 2, 1, 2, 1, 2)
)

test_that("persuasio4ytz print output works", {

  res <- persuasio4ytz(df, "y", "t", "z")

  out <- capture.output(print(res))

  expect_true(length(out) > 0)

  expect_true(any(grepl("Outcome:", out)))
  expect_true(any(grepl("Treatment:", out)))
  expect_true(any(grepl("Instrument:", out)))
})


test_that("persuasio4yz print output works", {

  res <- persuasio4yz(df, "y", "z")

  out <- capture.output(print(res))

  expect_true(length(out) > 0)

  expect_true(any(grepl("Outcome:", out)))
  expect_true(any(grepl("Treatment:", out)))
  expect_true(any(grepl("Instrument:", out)))
})


test_that("persuasio4ytz2lpr print output works", {

  res <- persuasio4ytz2lpr(df, "y", "t", "z")

  out <- capture.output(print(res))

  expect_true(length(out) > 0)

  expect_true(any(grepl("Outcome:", out)))
  expect_true(any(grepl("Treatment:", out)))
  expect_true(any(grepl("Instrument:", out)))
})


test_that("persuasio wrapper prints without error", {

  res <- persuasio(
    est = "apr",
    varlist = c("y","t","z"),
    data = df
  )

  out <- capture.output(print(res))

  expect_true(length(out) > 0)
})
