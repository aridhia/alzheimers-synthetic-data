context("APOE")

n <- 100

test_that("Sample data can be created", {
  dat <- apoe(n)

  expect_equal(nrow(dat), n)
})
