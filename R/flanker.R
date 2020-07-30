
#' @title Flanker
#' @description Create sample data for the Flanker cognitive test
#' @param n Integer number of participants to create data for
#' @param visit The name of the visit to create data for as a character string
#' @param missing The proportion of participants for which the test was not completed
#' @param ids Optional vector of participant IDs
#' @param visit_ids Optional vector of visit IDs
#' @return A data.frame
#' @examples
#' flanker(10)
#' @export
flanker <- function(n, visit = "V1", missing = 0.1, ids = NULL, visit_ids = NULL) {
  ids <- handle_ids(n, ids)
  visit_ids <- handle_ids(n, visit_ids)

  assessment_performed <- sample_yes_no(n, prob = c(1 - missing, missing))
  performed <- which(assessment_performed == "Yes")
  not_performed <- which(assessment_performed == "No")

  assessment_date <- random_date(n, which_na = not_performed)
  reason_not_performed <- reason_not_collected(n, which_na = performed)

  flanker_error_diff <- sample(0:5, n, replace = TRUE)

  df <- data.frame(
    patient_id = ids,
    visit_id = visit_ids,
    visit = visit,
    flanker_total_trials = 48,
    flanker_score = runif(n, min = 5, max = 10),
    flanker_error_diff = flanker_error_diff,
    flanker_total_corr = 48 - flanker_error_diff,
    flanker_total_mean = rnorm(n, 1, 0.3),
    flanker_total_median = rnorm(n, 1, 0.3),
    flanker_total_stdev = runif(n, 0.1, 0.3),
    flanker_congr_corr = 24,
    flanker_congr_mean = rnorm(n, 1, 0.3),
    flanker_congr_median = rnorm(n, 1, 0.3),
    flanker_congr_stdev = runif(n, 0.1, 0.3),
    flanker_incongr_corr = 24,
    flanker_incongr_mean = rnorm(n, 1, 0.3),
    flanker_incongr_median = rnorm(n, 1, 0.3),
    flanker_incongr_stdev = runif(n, 0.1, 0.3),
    flanker_left_corr = 24,
    flanker_left_mean = rnorm(n, 1, 0.3),
    flanker_left_median = rnorm(n, 1, 0.3),
    flanker_left_stdev = runif(n, 0.1, 0.3),
    flanker_right_corr = 24,
    flanker_right_mean = rnorm(n, 1, 0.3),
    flanker_right_median = rnorm(n, 1, 0.3),
    flanker_right_stdev = runif(n, 0.1, 0.3),
    flanker_up_corr = 24,
    flanker_up_mean = rnorm(n, 1, 0.3),
    flanker_up_median = rnorm(n, 1, 0.3),
    flanker_up_stdev = runif(n, 0.1, 0.3),
    flanker_down_corr = 24,
    flanker_down_mean = rnorm(n, 1, 0.3),
    flanker_down_median = rnorm(n, 1, 0.3),
    flanker_down_stdev = runif(n, 0.1, 0.3),
    assessment_performed = assessment_performed,
    assessment_date = assessment_date,
    reason_not_performed = reason_not_performed
  )

  return(df)
}
