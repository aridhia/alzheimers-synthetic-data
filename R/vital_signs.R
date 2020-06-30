
#' @export
vital_signs <- function(n, visit = "V1", ids = NULL, visit_ids = NULL) {
  ids <- handle_ids(n, ids)
  visit_ids <- handle_ids(n, visit_ids)

  vital_signs_collected <- sample_yn(n)
  collected <- which(vital_signs_collected == "Y")
  not_collected <- which(vital_signs_collected == "N")

  reason_not_collected <- reason_not_collected(n, which_na = collected)
  date_collected <- random_date(n, which_na = not_collected)

  df <- data.frame(
    patient_id = ids,
    visit_id = visit_ids,
    visit = visit,
    vital_signs_collected = vital_signs_collected,
    reason_not_collected = reason_not_collected,
    date_collected = date_collected,
    height = sample_numeric(n),
    weight = sample_numeric(n),
    hip_circumference = sample_numeric(n),
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
}


