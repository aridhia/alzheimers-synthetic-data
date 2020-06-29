#' @export
four_mountains <- function(n, visit = "V1", ids = NULL) {
  ids <- handle_ids(n, ids)

  assessment_performed <- sample_yes_no(n)
  performed <- which(assessment_performed == "Yes")
  not_performed <- which(assessment_performed == "No")

  assessment_date <- random_date(n, which_na = not_performed)
  reason_not_performed <- reason_not_collected(n, which_na = performed)


  answers <- sample_fms_choice(15)
  fms_t1_choice <- sample_fms_choice(n, which_na = not_performed)
  fms_t1_score <- ifelse(fms_t1_choice == answers[1], "CORRECT", "INCORRECT")
  fms_t2_choice <- sample_fms_choice(n, which_na = not_performed)
  fms_t2_score <- ifelse(fms_t2_choice == answers[2], "CORRECT", "INCORRECT")
  fms_t3_choice <- sample_fms_choice(n, which_na = not_performed)
  fms_t3_score <- ifelse(fms_t3_choice == answers[3], "CORRECT", "INCORRECT")
  fms_t4_choice <- sample_fms_choice(n, which_na = not_performed)
  fms_t4_score <- ifelse(fms_t4_choice == answers[4], "CORRECT", "INCORRECT")
  fms_t5_choice <- sample_fms_choice(n, which_na = not_performed)
  fms_t5_score <- ifelse(fms_t5_choice == answers[5], "CORRECT", "INCORRECT")
  fms_t6_choice <- sample_fms_choice(n, which_na = not_performed)
  fms_t6_score <- ifelse(fms_t6_choice == answers[6], "CORRECT", "INCORRECT")

  df <- data.frame(
    patient_id = ids,
    visit = visit,
    fms_t1_choice = fms_t1_choice,
    fms_t1_rt = sample_reaction_time(n, which_na = not_performed),
    fms_t1_score = fms_t1_score,
    fms_t2_choice = fms_t2_choice,
    fms_t2_rt = sample_reaction_time(n, which_na = not_performed),
    fms_t2_score = fms_t2_score,
    fms_t3_choice = fms_t3_choice,
    fms_t3_rt = sample_reaction_time(n, which_na = not_performed),
    fms_t3_score = fms_t3_score,
    fms_t4_choice = fms_t4_choice,
    fms_t4_rt = sample_reaction_time(n, which_na = not_performed),
    fms_t4_score = fms_t4_score,
    fms_t5_choice = fms_t5_choice,
    fms_t5_rt = sample_reaction_time(n, which_na = not_performed),
    fms_t5_score = fms_t5_score,
    fms_t6_choice = fms_t6_choice,
    fms_t6_rt = sample_reaction_time(n, which_na = not_performed),
    fms_t6_score = fms_t6_score,
    fms_t7_choice = fms_t7_choice,
    fms_t7_rt = sample_reaction_time(n, which_na = not_performed),
    fms_t7_score = fms_t7_score,
    fms_t8_choice = fms_t8_choice,
    fms_t8_rt = sample_reaction_time(n, which_na = not_performed),
    fms_t8_score = fms_t8_score,
    fms_t9_choice = fms_t9_choice,
    fms_t9_rt = sample_reaction_time(n, which_na = not_performed),
    fms_t9_score = fms_t9_score,
    fms_t10_choice = fms_t10_choice,
    fms_t10_rt = sample_reaction_time(n, which_na = not_performed),
    fms_t10_score = fms_t10_score,
    fms_t11_choice = fms_t11_choice,
    fms_t11_rt = sample_reaction_time(n, which_na = not_performed),
    fms_t11_score = fms_t11_score,
    fms_t12_choice = fms_t12_choice,
    fms_t12_rt = sample_reaction_time(n, which_na = not_performed),
    fms_t12_score = fms_t12_score,
    fms_t13_choice = fms_t13_choice,
    fms_t13_rt = sample_reaction_time(n, which_na = not_performed),
    fms_t13_score = fms_t13_score,
    fms_t14_choice = fms_t14_choice,
    fms_t14_rt = sample_reaction_time(n, which_na = not_performed),
    fms_t14_score = fms_t14_score,
    fms_t15_choice = fms_t15_choice,
    fms_t15_rt = sample_reaction_time(n, which_na = not_performed),
    fms_t15_score = fms_t15_score,

    assessment_performed = assessment_performed,
    assessment_date = assessment_date,
    reason_not_performed = reason_not_performed
  )

  return(df)
}

sample_fms_choice <- function(n, which_na = c()) {
  x <- sample(c("C", "E", "S", "T"), n, replace = TRUE)
  x <- remove_indices(x, which_na)
  return(x)
}

sample_reaction_time <- function(n, which_na = c()) {
  x <- sample(1000:10000, n, replace = TRUE)
  x <- remove_indices(x, which_na)
  return(x)
}

sample_fms_score <- function(n, which_na = c()) {
  x <- sample(c("CORRECT", "INCORRECT"), n, replace = TRUE)
  x <- remove_indices(x, which_na)
  return(x)
}

