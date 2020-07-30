
#' @title Virtual Reality Supermarket Trolley
#' @param n Integer number of participants to create data for
#' @param visit The name of the visit to create data for as a character string
#' @param missing The proportion of participants for which the test was not completed
#' @param ids Optional vector of participant IDs
#' @param visit_ids Optional vector of visit IDs
#' @return A data.frame
#' @export
vr_supermarket_trolley <- function(n, visit = "V1", missing = 0.1, ids = NULL, visit_ids = NULL) {
  ids <- handle_ids(n, ids)
  visit_ids <- handle_ids(n, visit_ids)

  assessment_performed <- sample_yes_no(n, prob = c(1 - missing, missing))
  performed <- which(assessment_performed == "Yes")
  not_performed <- which(assessment_performed == "No")

  assessment_date <- random_date(n, which_na = not_performed)
  reason_not_performed <- reason_not_collected(n, which_na = performed)

  scores_1_7 <- data.frame(
    smt_1_score = sample_01(n, which_na = not_performed),
    smt_2_score = sample_01(n, which_na = not_performed),
    smt_3_score = sample_01(n, which_na = not_performed),
    smt_4_score = sample_01(n, which_na = not_performed),
    smt_5_score = sample_01(n, which_na = not_performed),
    smt_6_score = sample_01(n, which_na = not_performed),
    smt_7_score = sample_01(n, which_na = not_performed)
  )

  scores_8_14 <- data.frame(
    smt_8_score = sample_01(n, which_na = not_performed),
    smt_9_score = sample_01(n, which_na = not_performed),
    smt_10_score = sample_01(n, which_na = not_performed),
    smt_11_score = sample_01(n, which_na = not_performed),
    smt_12_score = sample_01(n, which_na = not_performed),
    smt_13_score = sample_01(n, which_na = not_performed),
    smt_14_score = sample_01(n, which_na = not_performed)
  )

  rts <- data.frame(
    smt_1_rt = sample_reaction_time(n, which_na = not_performed),
    smt_2_rt = sample_reaction_time(n, which_na = not_performed),
    smt_3_rt = sample_reaction_time(n, which_na = not_performed),
    smt_4_rt = sample_reaction_time(n, which_na = not_performed),
    smt_5_rt = sample_reaction_time(n, which_na = not_performed),
    smt_6_rt = sample_reaction_time(n, which_na = not_performed),
    smt_7_rt = sample_reaction_time(n, which_na = not_performed),
    smt_8_rt = sample_reaction_time(n, which_na = not_performed),
    smt_9_rt = sample_reaction_time(n, which_na = not_performed),
    smt_10_rt = sample_reaction_time(n, which_na = not_performed),
    smt_11_rt = sample_reaction_time(n, which_na = not_performed),
    smt_12_rt = sample_reaction_time(n, which_na = not_performed),
    smt_13_rt = sample_reaction_time(n, which_na = not_performed),
    smt_14_rt = sample_reaction_time(n, which_na = not_performed)
  )

  raw_df <- data.frame(scores_1_7, scores_8_14, rts)
  raw_df <- raw_df[, c("smt_1_rt", "smt_1_score", "smt_2_rt", "smt_2_score", "smt_3_rt", "smt_3_score",
                       "smt_4_rt", "smt_4_score", "smt_5_rt", "smt_5_score", "smt_6_rt", "smt_6_score",
                       "smt_7_rt", "smt_7_score", "smt_8_rt", "smt_8_score", "smt_9_rt", "smt_9_score",
                       "smt_10_rt", "smt_10_score", "smt_11_rt", "smt_11_score", "smt_12_rt", "smt_12_score",
                       "smt_13_rt", "smt_13_score", "smt_14_rt", "smt_14_score")]

  df <- data.frame(
    patient_id = ids,
    visit_id = visit_ids,
    visit = visit,
    raw_df,
    smt_sec1_totalcorr = rowSums(scores_1_7),
    smt_sec2_totalcorr = rowSums(scores_8_14),
    smt_all_totalcorr = rowSums(scores_1_7) + rowSums(scores_8_14),
    smt_all_avgcorr = (rowSums(scores_1_7) + rowSums(scores_8_14)) / 14,
    smt_all_avgrt = rowMeans(rts),
    assessment_date = assessment_date,
    assessment_performed = assessment_performed,
    reason_not_performed = reason_not_performed
  )

  return(df)
}
