#' @export
mmse <- function(n, visit = "V1", ids = NULL, visit_ids = NULL) {
  ids <- handle_ids(n, ids)
  visit_ids <- handle_ids(n, visit_ids)

  assessment_performed <- sample_yes_no(n)
  performed <- which(assessment_performed == "Yes")
  not_performed <- which(assessment_performed == "No")

  assessment_date <- random_date(n, which_na = not_performed)
  reason_not_performed <- reason_not_collected(n, which_na = performed)

  df <- data.frame(
    patient_id = ids,
    visit_id = visit_ids,
    visit = visit,
    mmse_total = sample_mmse(n, which_na = not_performed),
    mmse_calculation_1 = sample_mmse(n, which_na = not_performed),
    mmse_calculation_2 = sample_mmse(n, which_na = not_performed),
    mmse_calculation_3 = sample_mmse(n, which_na = not_performed),
    mmse_calculation_4 = sample_mmse(n, which_na = not_performed),
    mmse_calculation_5 = sample_mmse(n, which_na = not_performed),
    mmse_comprehension_1 = sample_mmse(n, which_na = not_performed),
    mmse_comprehension_2 = sample_mmse(n, which_na = not_performed),
    mmse_comprehension_3 = sample_mmse(n, which_na = not_performed),
    mmse_drawing_1 = sample_mmse(n, which_na = not_performed),
    mmse_naming_1 = sample_mmse(n, which_na = not_performed),
    mmse_naming_2 = sample_mmse(n, which_na = not_performed),
    mmse_orientation_1 = sample_mmse(n, which_na = not_performed),
    mmse_orientation_2 = sample_mmse(n, which_na = not_performed),
    mmse_orientation_3 = sample_mmse(n, which_na = not_performed),
    mmse_orientation_4 = sample_mmse(n, which_na = not_performed),
    mmse_orientation_5 = sample_mmse(n, which_na = not_performed),
    mmse_orientation_6 = sample_mmse(n, which_na = not_performed),
    mmse_orientation_7 = sample_mmse(n, which_na = not_performed),
    mmse_orientation_8 = sample_mmse(n, which_na = not_performed),
    mmse_orientation_9 = sample_mmse(n, which_na = not_performed),
    mmse_orientation_10 = sample_mmse(n, which_na = not_performed),
    mmse_reading_1 = sample_mmse(n, which_na = not_performed),
    mmse_recall_1 = sample_mmse(n, which_na = not_performed),
    mmse_recall_2 = sample_mmse(n, which_na = not_performed),
    mmse_recall_3 = sample_mmse(n, which_na = not_performed),
    mmse_registration_1 = sample_mmse(n, which_na = not_performed),
    mmse_registration_2 = sample_mmse(n, which_na = not_performed),
    mmse_registration_3 = sample_mmse(n, which_na = not_performed),
    mmse_registration_4 = sample_mmse(n, which_na = not_performed),
    mmse_registration_5 = sample_mmse(n, which_na = not_performed),
    mmse_registration_6 = sample_mmse(n, which_na = not_performed),
    mmse_registration_7 = sample_mmse(n, which_na = not_performed),
    mmse_registration_8 = sample_mmse(n, which_na = not_performed),
    mmse_registration_9 = sample_mmse(n, which_na = not_performed),
    mmse_registration_10 = sample_mmse(n, which_na = not_performed),
    mmse_registration_11 = sample_mmse(n, which_na = not_performed),
    mmse_registration_12 = sample_mmse(n, which_na = not_performed),
    mmse_registration_13 = sample_mmse(n, which_na = not_performed),
    mmse_registration_14 = sample_mmse(n, which_na = not_performed),
    mmse_registration_15 = sample_mmse(n, which_na = not_performed),
    mmse_repetition_1 = sample_mmse(n, which_na = not_performed),
    mmse_writing_1 = sample_mmse(n, which_na = not_performed),
    assessment_performed = assessment_performed,
    assessment_date = assessment_date,
    reason_not_performed = reason_not_performed
  )

  return(df)
}


sample_mmse <- function(n, which_na = c()) {
  x <- sample(1:30, n, replace = TRUE)
  x <- remove_indices(x, which_na)
  return(x)
}
