
test_that("print.ridge_model displays the model information", {

  model <- ridge_fit(
    x = matrix(
      c(1, 2, 3, 4, 5, 6),
      ncol = 1
    ),
    y = c(2, 4, 6, 8, 10, 12),
    lambda = 1
  )

  output <- capture.output(print(model))

  expect_true(
    any(grepl("Ridge Regression Model", output, fixed = TRUE))
  )

  expect_true(
    any(grepl("Lambda:", output, fixed = TRUE))
  )

  expect_true(
    any(grepl("Coefficients:", output, fixed = TRUE))
  )

  expect_true(
    any(grepl("(Intercept)", output, fixed = TRUE))
  )

  expect_true(
    any(grepl("x1", output, fixed = TRUE))
  )
})


test_that("print.ridge_model invisibly returns the original model", {

  model <- ridge_fit(
    x = matrix(1:6, ncol = 1),
    y = c(2, 4, 6, 8, 10, 12),
    lambda = 1
  )

  result <- withVisible(print(model))

  expect_identical(result$value, model)
  expect_false(result$visible)
})
