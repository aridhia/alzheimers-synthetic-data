
#' @title STAI-40
#' @export
stai_40 <- function(n, visit = "V1", missing = 0.1, ids = NULL, visit_ids = NULL) {
  ids <- handle_ids(n, ids)
  visit_ids <- handle_ids(n, visit_ids)

  assessment_performed <- sample_yes_no(n, prob = c(1 - missing, missing))
  performed <- which(assessment_performed == "Yes")
  not_performed <- which(assessment_performed == "No")

  assessment_date <- random_date(n, which_na = not_performed)
  reason_not_performed <- reason_not_collected(n, which_na = performed)

  form1 <- data.frame(
    stai_40_form1_q1 = sample_stai(n, which_na = not_performed),
    stai_40_form1_q2 = sample_stai(n, which_na = not_performed),
    stai_40_form1_q3 = sample_stai(n, which_na = not_performed),
    stai_40_form1_q4 = sample_stai(n, which_na = not_performed),
    stai_40_form1_q5 = sample_stai(n, which_na = not_performed),
    stai_40_form1_q6 = sample_stai(n, which_na = not_performed),
    stai_40_form1_q7 = sample_stai(n, which_na = not_performed),
    stai_40_form1_q8 = sample_stai(n, which_na = not_performed),
    stai_40_form1_q9 = sample_stai(n, which_na = not_performed),
    stai_40_form1_q10 = sample_stai(n, which_na = not_performed),
    stai_40_form1_q11 = sample_stai(n, which_na = not_performed),
    stai_40_form1_q12 = sample_stai(n, which_na = not_performed),
    stai_40_form1_q13 = sample_stai(n, which_na = not_performed),
    stai_40_form1_q14 = sample_stai(n, which_na = not_performed),
    stai_40_form1_q15 = sample_stai(n, which_na = not_performed),
    stai_40_form1_q16 = sample_stai(n, which_na = not_performed),
    stai_40_form1_q17 = sample_stai(n, which_na = not_performed),
    stai_40_form1_q18 = sample_stai(n, which_na = not_performed),
    stai_40_form1_q19 = sample_stai(n, which_na = not_performed),
    stai_40_form1_q20 = sample_stai(n, which_na = not_performed)
  )

  form2 <- data.frame(
    stai_40_form2_q21 = sample_stai(n, which_na = not_performed),
    stai_40_form2_q22 = sample_stai(n, which_na = not_performed),
    stai_40_form2_q23 = sample_stai(n, which_na = not_performed),
    stai_40_form2_q24 = sample_stai(n, which_na = not_performed),
    stai_40_form2_q25 = sample_stai(n, which_na = not_performed),
    stai_40_form2_q26 = sample_stai(n, which_na = not_performed),
    stai_40_form2_q27 = sample_stai(n, which_na = not_performed),
    stai_40_form2_q28 = sample_stai(n, which_na = not_performed),
    stai_40_form2_q29 = sample_stai(n, which_na = not_performed),
    stai_40_form2_q30 = sample_stai(n, which_na = not_performed),
    stai_40_form2_q31 = sample_stai(n, which_na = not_performed),
    stai_40_form2_q32 = sample_stai(n, which_na = not_performed),
    stai_40_form2_q33 = sample_stai(n, which_na = not_performed),
    stai_40_form2_q34 = sample_stai(n, which_na = not_performed),
    stai_40_form2_q35 = sample_stai(n, which_na = not_performed),
    stai_40_form2_q36 = sample_stai(n, which_na = not_performed),
    stai_40_form2_q37 = sample_stai(n, which_na = not_performed),
    stai_40_form2_q38 = sample_stai(n, which_na = not_performed),
    stai_40_form2_q39 = sample_stai(n, which_na = not_performed),
    stai_40_form2_q40 = sample_stai(n, which_na = not_performed)
  )

  form1_total_score <- rowSums(form1)
  form2_total_score <- rowSums(form2)
  total_score <- form1_total_score + form2_total_score

  df <- data.frame(
    patient_id = ids,
    visit_id = visit_ids,
    visit = visit,
    stai_40_total_score = total_score,
    stai_40_form1_total_score = form1_total_score,
    stai_40_form2_total_score = form2_total_score,
    form1,
    form2,
    assessment_date = assessment_date,
    assessment_performed = assessment_performed,
    reason_not_performed = reason_not_performed
  )
  return(df)
}

sample_stai <- function(n, which_na = c()) {
  x <- sample(1:4, n, replace = TRUE)
  x <- remove_indices(x, which_na)
  return(x)
}
