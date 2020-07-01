context("CDR")

n <- 100

test_that("Sample data can be created", {
  dat <- cdr(n)

  expect_equal(nrow(dat), n)
})
