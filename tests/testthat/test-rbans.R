context("RBANS")

n <- 100

test_that("Sample data can be created", {
  dat <- rbans(n)

  expect_equal(nrow(dat), n)
})
