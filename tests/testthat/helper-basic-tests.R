
basic_tests <- function(f, n) {

  test_that("Sample data can be created", {
    dat <- f(n)

    expect_equal(nrow(dat), n)
  })
}
