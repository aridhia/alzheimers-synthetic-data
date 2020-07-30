context("Family History")

n <- 100

#basic_tests(family_history, n)


test_that("Sample data can be created", {
  dat <- family_history(n)

  expect_gte(nrow(dat), n)
  expect_equal(unique(dat$patient_id), 1:n)
})

