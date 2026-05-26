# toy dataset
df <- data.frame(
  y = c(1, 1, 1, 0, 0, 0, 1, 0),
  t = c(1, 1, 0, 0, 1, 0, 1, 0),
  z = c(1, 1, 1, 1, 0, 0, 0, 0),
  x1 = c(1, 2, 1, 2, 1, 2, 1, 2)
)

test_that("print.aprlb works and returns invisibly", {

  res <- aprlb(df, "y", "z")

  expect_output(print(res), "aprlb")
  expect_invisible(print(res))
})


test_that("print.aprub works and returns invisibly", {

  res <- aprub(df, "y", "t", "z")

  expect_output(print(res), "aprub")
  expect_invisible(print(res))
})


test_that("print.lpr4ytz works and returns invisibly", {

  res <- lpr4ytz(df, "y", "t", "z")

  expect_output(print(res), "lpr")
  expect_invisible(print(res))
})


test_that("print.calc4persuasio works and returns invisibly", {

  res <- calc4persuasio(
    y1 = 0.7,
    y0 = 0.3,
    e1 = 0.6,
    e0 = 0.2
  )

  expect_output(print(res), "calc4persuasio")
  expect_invisible(print(res))
})
