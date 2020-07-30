
#' @title Dot COunting
#' @description Create sample data for the Dot Counting cognitive test
#' @param n Integer number of participants to create data for
#' @param visit The name of the visit to create data for as a character string
#' @param missing The proportion of participants for which the test was not completed
#' @param ids Optional vector of participant IDs
#' @param visit_ids Optional vector of visit IDs
#' @return A data.frame
#' @examples
#' dot_counting(10)
#' @export
dot_counting <- function(n, visit = "V1", missing = 0.1, ids = NULL, visit_ids = NULL) {
  ids <- handle_ids(n, ids)
  visit_ids <- handle_ids(n, visit_ids)

  assessment_performed <- sample_yes_no(n, prob = c(1 - missing, missing))
  performed <- which(assessment_performed == "Yes")
  not_performed <- which(assessment_performed == "No")

  assessment_date <- random_date(n, which_na = not_performed)
  reason_not_performed <- reason_not_collected(n, which_na = performed)

  t1 <- sample_dotcount(n)
  t2 <- sample_dotcount(n)
  t3 <- sample_dotcount(n)
  t4 <- sample_dotcount(n)
  t5 <- sample_dotcount(n)
  t6 <- sample_dotcount(n)
  total <- t1 + t2 + t3 + t4 + t5 + t6


  df <- data.frame(
    patient_id = ids,
    visit_id = visit_ids,
    visit = visit,
    dotcount_t1 = t1,
    dotcount_t2 = t2,
    dotcount_t3 = t3,
    dotcount_t4 = t4,
    dotcount_t5 = t5,
    dotcount_t6 = t6,
    dotcount_total = total,

    assessment_performed = assessment_performed,
    assessment_date = assessment_date,
    reason_not_performed = reason_not_performed
  )

  return(df)
}

sample_dotcount <- function(n) {
  sample(0:6, n, replace = TRUE)
}

