
#' @title GDS
#' @param n Integer number of participants to create data for
#' @param visit The name of the visit to create data for as a character string
#' @param missing The proportion of participants for which the questionnaire was not completed
#' @param ids Optional vector of participant IDs
#' @param visit_ids Optional vector of visit IDs
#' @return A data.frame
#' @export
gds <- function(n, visit = "V1", missing = 0.1, ids = NULL, visit_ids = NULL) {
  ids <- handle_ids(n, ids)
  visit_ids <- handle_ids(n, visit_ids)

  assessment_performed <- sample_yes_no(n, prob = c(1 - missing, missing))
  performed <- which(assessment_performed == "Yes")
  not_performed <- which(assessment_performed == "No")

  assessment_date <- random_date(n, which_na = not_performed)
  reason_not_done <- reason_not_collected(n, which_na = performed)

  gds_raw <- data.frame(
    gds_1 = sample_gds(n, which_na = not_performed),
    gds_2 = sample_gds(n, which_na = not_performed),
    gds_3 = sample_gds(n, which_na = not_performed),
    gds_4 = sample_gds(n, which_na = not_performed),
    gds_5 = sample_gds(n, which_na = not_performed),
    gds_6 = sample_gds(n, which_na = not_performed),
    gds_7 = sample_gds(n, which_na = not_performed),
    gds_8 = sample_gds(n, which_na = not_performed),
    gds_9 = sample_gds(n, which_na = not_performed),
    gds_10 = sample_gds(n, which_na = not_performed),
    gds_11 = sample_gds(n, which_na = not_performed),
    gds_12 = sample_gds(n, which_na = not_performed),
    gds_13 = sample_gds(n, which_na = not_performed),
    gds_14 = sample_gds(n, which_na = not_performed),
    gds_15 = sample_gds(n, which_na = not_performed),
    gds_16 = sample_gds(n, which_na = not_performed),
    gds_17 = sample_gds(n, which_na = not_performed),
    gds_18 = sample_gds(n, which_na = not_performed),
    gds_19 = sample_gds(n, which_na = not_performed),
    gds_20 = sample_gds(n, which_na = not_performed),
    gds_21 = sample_gds(n, which_na = not_performed),
    gds_22 = sample_gds(n, which_na = not_performed),
    gds_23 = sample_gds(n, which_na = not_performed),
    gds_24 = sample_gds(n, which_na = not_performed),
    gds_25 = sample_gds(n, which_na = not_performed),
    gds_26 = sample_gds(n, which_na = not_performed),
    gds_27 = sample_gds(n, which_na = not_performed),
    gds_28 = sample_gds(n, which_na = not_performed),
    gds_29 = sample_gds(n, which_na = not_performed),
    gds_30 = sample_gds(n, which_na = not_performed)
  )

  positives <- c(1, 5, 7, 9, 15, 19, 21, 27, 29, 30)
  negatives <- c(2, 3, 4, 6, 8, 10, 11, 12, 13, 14, 16, 17, 18, 20, 22, 23, 24, 25, 26, 28)

  gds_total <- rowSums(gds_raw[, positives] == 997) + rowSums(gds_raw[, negatives] == 998)

  df <- data.frame(
    patient_id = ids,
    visit_id = visit_ids,
    visit = visit,
    gds_total = gds_total,
    gds_raw,
    assessment_date = assessment_date,
    assessment_performed = assessment_performed,
    reason_not_done = reason_not_done
  )
  return(df)
}

sample_gds <- function(n, which_na = c()) {
  x <- sample(c(997, 998), n, replace = TRUE)
  x <- remove_indices(x, which_na)
  return(x)
}

