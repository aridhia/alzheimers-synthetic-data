
#' @title Four Mountains
#' @param n Integer number of participants to create data for
#' @param visit The name of the visit to create data for as a character string
#' @param missing The proportion of participants for which the test was not completed
#' @param ids Optional vector of participant IDs
#' @param visit_ids Optional vector of visit IDs
#' @return A data.frame
#' @export
four_mountains <- function(n, visit = "V1", missing = 0.1, ids = NULL, visit_ids = NULL) {
  ids <- handle_ids(n, ids)
  visit_ids <- handle_ids(n, visit_ids)

  assessment_performed <- sample_yes_no(n, prob = c(1 - missing, missing))
  performed <- which(assessment_performed == "Yes")
  not_performed <- which(assessment_performed == "No")

  assessment_date <- random_date(n, which_na = not_performed)
  reason_not_performed <- reason_not_collected(n, which_na = performed)

  fms_t1_choice <- sample_fms_choice(n, which_na = not_performed)
  fms_t1_score <- ifelse(fms_t1_choice == "T", "CORRECT", "INCORRECT")
  fms_t2_choice <- sample_fms_choice(n, which_na = not_performed)
  fms_t2_score <- ifelse(fms_t2_choice == "T", "CORRECT", "INCORRECT")
  fms_t3_choice <- sample_fms_choice(n, which_na = not_performed)
  fms_t3_score <- ifelse(fms_t3_choice == "T", "CORRECT", "INCORRECT")
  fms_t4_choice <- sample_fms_choice(n, which_na = not_performed)
  fms_t4_score <- ifelse(fms_t4_choice == "T", "CORRECT", "INCORRECT")
  fms_t5_choice <- sample_fms_choice(n, which_na = not_performed)
  fms_t5_score <- ifelse(fms_t5_choice == "T", "CORRECT", "INCORRECT")
  fms_t6_choice <- sample_fms_choice(n, which_na = not_performed)
  fms_t6_score <- ifelse(fms_t6_choice == "T", "CORRECT", "INCORRECT")
  fms_t7_choice <- sample_fms_choice(n, which_na = not_performed)
  fms_t7_score <- ifelse(fms_t7_choice == "T", "CORRECT", "INCORRECT")
  fms_t8_choice <- sample_fms_choice(n, which_na = not_performed)
  fms_t8_score <- ifelse(fms_t8_choice == "T", "CORRECT", "INCORRECT")
  fms_t9_choice <- sample_fms_choice(n, which_na = not_performed)
  fms_t9_score <- ifelse(fms_t9_choice == "T", "CORRECT", "INCORRECT")
  fms_t10_choice <- sample_fms_choice(n, which_na = not_performed)
  fms_t10_score <- ifelse(fms_t10_choice == "T", "CORRECT", "INCORRECT")
  fms_t11_choice <- sample_fms_choice(n, which_na = not_performed)
  fms_t11_score <- ifelse(fms_t11_choice == "T", "CORRECT", "INCORRECT")
  fms_t12_choice <- sample_fms_choice(n, which_na = not_performed)
  fms_t12_score <- ifelse(fms_t12_choice == "T", "CORRECT", "INCORRECT")
  fms_t13_choice <- sample_fms_choice(n, which_na = not_performed)
  fms_t13_score <- ifelse(fms_t13_choice == "T", "CORRECT", "INCORRECT")
  fms_t14_choice <- sample_fms_choice(n, which_na = not_performed)
  fms_t14_score <- ifelse(fms_t14_choice == "T", "CORRECT", "INCORRECT")
  fms_t15_choice <- sample_fms_choice(n, which_na = not_performed)
  fms_t15_score <- ifelse(fms_t15_choice == "T", "CORRECT", "INCORRECT")

  fms_total_target <- (fms_t1_choice == "T") + (fms_t2_choice == "T") + (fms_t3_choice == "T") +
    (fms_t4_choice == "T") + (fms_t5_choice == "T") + (fms_t6_choice == "T") + (fms_t7_choice == "T") +
    (fms_t8_choice == "T") + (fms_t9_choice == "T") + (fms_t10_choice == "T") + (fms_t11_choice == "T") +
    (fms_t12_choice == "T") + (fms_t13_choice == "T") + (fms_t14_choice == "T") + (fms_t15_choice == "T")

  fms_total_configural <- (fms_t1_choice == "C") + (fms_t2_choice == "C") + (fms_t3_choice == "C") +
    (fms_t4_choice == "C") + (fms_t5_choice == "C") + (fms_t6_choice == "C") + (fms_t7_choice == "C") +
    (fms_t8_choice == "C") + (fms_t9_choice == "C") + (fms_t10_choice == "C") + (fms_t11_choice == "C") +
    (fms_t12_choice == "C") + (fms_t13_choice == "C") + (fms_t14_choice == "C") + (fms_t15_choice == "C")

  fms_total_spatial <- (fms_t1_choice == "S") + (fms_t2_choice == "S") + (fms_t3_choice == "S") +
    (fms_t4_choice == "S") + (fms_t5_choice == "S") + (fms_t6_choice == "S") + (fms_t7_choice == "S") +
    (fms_t8_choice == "S") + (fms_t9_choice == "S") + (fms_t10_choice == "S") + (fms_t11_choice == "S") +
    (fms_t12_choice == "S") + (fms_t13_choice == "S") + (fms_t14_choice == "S") + (fms_t15_choice == "S")

  fms_total_elemental <- (fms_t1_choice == "E") + (fms_t2_choice == "E") + (fms_t3_choice == "E") +
    (fms_t4_choice == "E") + (fms_t5_choice == "E") + (fms_t6_choice == "E") + (fms_t7_choice == "E") +
    (fms_t8_choice == "E") + (fms_t9_choice == "E") + (fms_t10_choice == "E") + (fms_t11_choice == "E") +
    (fms_t12_choice == "E") + (fms_t13_choice == "E") + (fms_t14_choice == "E") + (fms_t15_choice == "E")

  fms_total_noresponse <- rep(0, n)
  fms_total_noresponse <- remove_indices(fms_total_noresponse, not_performed)

  fms_t1_rt = sample_reaction_time(n, which_na = not_performed)
  fms_t2_rt = sample_reaction_time(n, which_na = not_performed)
  fms_t3_rt = sample_reaction_time(n, which_na = not_performed)
  fms_t4_rt = sample_reaction_time(n, which_na = not_performed)
  fms_t5_rt = sample_reaction_time(n, which_na = not_performed)
  fms_t6_rt = sample_reaction_time(n, which_na = not_performed)
  fms_t7_rt = sample_reaction_time(n, which_na = not_performed)
  fms_t8_rt = sample_reaction_time(n, which_na = not_performed)
  fms_t9_rt = sample_reaction_time(n, which_na = not_performed)
  fms_t10_rt = sample_reaction_time(n, which_na = not_performed)
  fms_t11_rt = sample_reaction_time(n, which_na = not_performed)
  fms_t12_rt = sample_reaction_time(n, which_na = not_performed)
  fms_t13_rt = sample_reaction_time(n, which_na = not_performed)
  fms_t14_rt = sample_reaction_time(n, which_na = not_performed)
  fms_t15_rt = sample_reaction_time(n, which_na = not_performed)

  fms_total_avgtr <- rowMeans(cbind(fms_t1_rt, fms_t2_rt, fms_t3_rt, fms_t4_rt, fms_t5_rt,
                                    fms_t6_rt, fms_t7_rt, fms_t8_rt, fms_t9_rt, fms_t10_rt,
                                    fms_t11_rt, fms_t12_rt, fms_t13_rt, fms_t14_rt, fms_t15_rt),
                              na.rm = TRUE)

  df <- data.frame(
    patient_id = ids,
    visit_id = visit_ids,
    visit = visit,
    fms_t1_choice = fms_t1_choice,
    fms_t1_rt = fms_t1_rt,
    fms_t1_score = fms_t1_score,
    fms_t2_choice = fms_t2_choice,
    fms_t2_rt = fms_t2_rt,
    fms_t2_score = fms_t2_score,
    fms_t3_choice = fms_t3_choice,
    fms_t3_rt = fms_t3_rt,
    fms_t3_score = fms_t3_score,
    fms_t4_choice = fms_t4_choice,
    fms_t4_rt = fms_t4_rt,
    fms_t4_score = fms_t4_score,
    fms_t5_choice = fms_t5_choice,
    fms_t5_rt = fms_t5_rt,
    fms_t5_score = fms_t5_score,
    fms_t6_choice = fms_t6_choice,
    fms_t6_rt = fms_t6_rt,
    fms_t6_score = fms_t6_score,
    fms_t7_choice = fms_t7_choice,
    fms_t7_rt = fms_t7_rt,
    fms_t7_score = fms_t7_score,
    fms_t8_choice = fms_t8_choice,
    fms_t8_rt = fms_t8_rt,
    fms_t8_score = fms_t8_score,
    fms_t9_choice = fms_t9_choice,
    fms_t9_rt = fms_t9_rt,
    fms_t9_score = fms_t9_score,
    fms_t10_choice = fms_t10_choice,
    fms_t10_rt = fms_t10_rt,
    fms_t10_score = fms_t10_score,
    fms_t11_choice = fms_t11_choice,
    fms_t11_rt = fms_t11_rt,
    fms_t11_score = fms_t11_score,
    fms_t12_choice = fms_t12_choice,
    fms_t12_rt = fms_t12_rt,
    fms_t12_score = fms_t12_score,
    fms_t13_choice = fms_t13_choice,
    fms_t13_rt = fms_t13_rt,
    fms_t13_score = fms_t13_score,
    fms_t14_choice = fms_t14_choice,
    fms_t14_rt = fms_t14_rt,
    fms_t14_score = fms_t14_score,
    fms_t15_choice = fms_t15_choice,
    fms_t15_rt = fms_t15_rt,
    fms_t15_score = fms_t15_score,

    fms_total_target = fms_total_target,
    fms_total_configural = fms_total_configural,
    fms_total_spatial = fms_total_spatial,
    fms_total_elemental = fms_total_elemental,
    fms_total_noresponse = fms_total_noresponse,
    fms_total_avgtr = fms_total_avgtr,
    fms_total_perccorr = fms_total_target / 15,
    fms_total_score = fms_total_target,

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

