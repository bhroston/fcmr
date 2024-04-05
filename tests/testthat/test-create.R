test_that("grey_number catches errors", {
  expect_error(grey_number(0.5, 0.4))
  expect_error(grey_number("a", 0.5))
})
