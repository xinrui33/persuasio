# shared toy dataset
df <- data.frame(
  y = c(1, 1, 1, 0, 0, 0, 1, 0),
  t = c(1, 1, 0, 0, 1, 0, 1, 0),
  z = c(1, 1, 1, 1, 0, 0, 0, 0),
  x1 = c(1, 2, 1, 2, 1, 2, 1, 2)
)

# 1. aprlb: non-binary outcome / instrument
test_that("aprlb rejects non-binary inputs", {

  df_bad <- df
  df_bad$y[1] <- 2

  expect_error(
    aprlb(df_bad, "y", "z"),
    "must be binary"
  )

  df_bad2 <- df
  df_bad2$z[2] <- -1

  expect_error(
    aprlb(df_bad2, "y", "z"),
    "must be binary"
  )
})

# 2. aprub: non-binary treatment
test_that("aprub rejects non-binary treatment", {

  df_bad <- df
  df_bad$t[1] <- 3

  expect_error(
    aprub(df_bad, "y", "t", "z"),
    "must be binary"
  )
})

# 3. lpr4ytz: non-binary inputs
test_that("lpr4ytz rejects invalid binary variables", {

  df_bad <- df
  df_bad$z[1] <- 9

  expect_error(
    lpr4ytz(df_bad, "y", "t", "z"),
    "must be binary"
  )
})

# 4. calc4persuasio: bounds validation
test_that("calc4persuasio enforces [0,1] bounds", {

  expect_error(
    calc4persuasio(y1 = 1.2, y0 = 0.3),
    "must be in \\[0,1\\]"
  )

  expect_error(
    calc4persuasio(y1 = 0.5, y0 = -0.1),
    "must be in \\[0,1\\]"
  )
})

# 5. invalid model argument
test_that("aprlb handles invalid model argument", {

  expect_error(
    aprlb(df, "y", "t", "z", model = "not_a_model"),
    "should be one of"
  )
})

test_that("aprub handles invalid model argument", {

  expect_error(
    aprub(df, "y", "t", "z", model = "not_a_model"),
    "should be one of"
  )
})

test_that("lpr4ytz handles invalid model argument", {

  expect_error(
    lpr4ytz(df, "y", "t", "z", model = "not_a_model"),
    "should be one of"
  )
})
