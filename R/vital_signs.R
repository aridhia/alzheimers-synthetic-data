
#' @title Vital Signs
#' @param n Integer number of participants to create data for
#' @param visit The name of the visit to create data for as a character string
#' @param missing The proportion of participants for which the clinical measurements were not taken completed
#' @param ids Optional vector of participant IDs
#' @param visit_ids Optional vector of visit IDs
#' @return A data.frame
#' @export
vital_signs <- function(n, visit = "V1", missing = 0.1, ids = NULL, visit_ids = NULL) {
  ids <- handle_ids(n, ids)
  visit_ids <- handle_ids(n, visit_ids)

  vital_signs_collected <- sample_yn(n, prob = c(1 - missing, missing))
  collected <- which(vital_signs_collected == "Y")
  not_collected <- which(vital_signs_collected == "N")

  reason_not_collected <- reason_not_collected(n, which_na = collected)
  date_collected <- random_date(n, which_na = not_collected)

  height <- sample_height(n)
  weight <- sample_weight(n)
  hip_circumference <- sample_hip_circumference(n, height, weight)

  df <- data.frame(
    patient_id = ids,
    visit_id = visit_ids,
    visit = visit,
    vital_signs_collected = vital_signs_collected,
    reason_not_collected = reason_not_collected,
    date_collected = date_collected,
    height = height,
    weight = weight,
    hip_circumference = hip_circumference,
    waist_circumference = sample_numeric(n),
    systolic_bp = sample_numeric(n),
    diastolic_bp = sample_numeric(n),
    pulse = sample_numeric(n)
  )

  return(df)
}


sample_height <- function(n) {
  rnorm(n, mean = 170, sd = 10)
}

sample_weight <- function(n) {
  rnorm(n, 80, sd = 5)
}

sample_hip_circumference <- function(n, height = NULL, weight = NULL) {
  if(is.null(height) && is.null(weight)) {
    rnorm(n, mean = 105, sd = 10)
  }

  (115 + rnorm(n, sd = 4.5)) + (-0.36 + rnorm(n , sd = 0.03)) * height + (0.62 + rnorm(n, sd = 0.02)) * weight
}


