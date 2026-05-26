# toy dataset
df <- data.frame(
  y = c(1, 1, 1, 0, 0, 0, 1, 0),
  t = c(1, 1, 0, 0, 1, 0, 1, 0),
  z = c(1, 1, 1, 1, 0, 0, 0, 0),
  x1 = c(1, 2, 1, 2, 1, 2, 1, 2)
)

test_that("persuasio4ytz print output works", {

  res <- persuasio4ytz(df, "y", "t", "z")

  out <- capture.output(print(res))

  expect_true(length(out) > 0)
  expect_true(any(grepl("persuasio4ytz", out, fixed = TRUE)) ||
                any(grepl("Average Persuasion Rate", out)))
})


test_that("persuasio4yz print output works", {

  res <- persuasio4yz(df, "y", "z")

  out <- capture.output(print(res))

  expect_true(length(out) > 0)
  expect_true(any(grepl("persuasio4yz", out, fixed = TRUE)) ||
                any(grepl("Lower Bound", out)))
})


test_that("persuasio4ytz2lpr print output works", {

  res <- persuasio4ytz2lpr(df, "y", "t", "z")

  out <- capture.output(print(res))

  expect_true(length(out) > 0)
  expect_true(any(grepl("Local Persuasion", out)) ||
                any(grepl("lpr", out, ignore.case = TRUE)))
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
