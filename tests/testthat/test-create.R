test_that("grey_number catches errors", {
  expect_error(grey_number(0.5, 0.4))
  expect_error(grey_number("a", 0.5))
})

test_that("grey_number works", {
  test_grey_number_1 <- grey_number()
  expect_equal(test_grey_number_1$lower, -Inf)
  expect_equal(test_grey_number_1$upper, Inf)
  expect_equal(methods::is(test_grey_number_1), "grey_number")

  test_grey_number_2 <- grey_number(-1, 1)
  expect_equal(test_grey_number_2$lower, -1)
  expect_equal(test_grey_number_2$upper, 1)
})
