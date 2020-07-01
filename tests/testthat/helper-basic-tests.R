
basic_tests <- function(f, n) {

  test_that("Sample data can be created", {
    dat <- f(n)

    expect_equal(nrow(dat), n)
    expect_equal(dat$patient_id, 1:n)
  })

  test_that("Patient IDs can be provided", {
    dat <- f(26, ids = letters)

    expect_equal(nrow(dat), 26)
    expect_equal(dat$patient_id, letters)
  })


}

visit_id_tests <- function(f) {

  test_that("Visit IDs can be provided", {
    dat <- f(26, visit_ids = letters)

    expect_equal(nrow(dat), 26)
    expect_equal(dat$visit_id, letters)
  })

}
