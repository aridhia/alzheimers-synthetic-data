
#' @title PSQI
#' @param n Integer number of participants to create data for
#' @param visit The name of the visit to create data for as a character string
#' @param missing The proportion of participants for which the questionnaire was not completed
#' @param ids Optional vector of participant IDs
#' @param visit_ids Optional vector of visit IDs
#' @return A data.frame
#' @export
psqi <- function(n, visit = "V1", missing = 0.1, ids = NULL, visit_ids = NULL) {
  ids <- handle_ids(n, ids)
  visit_ids <- handle_ids(n, visit_ids)

  assessment_performed <- sample_yes_no(n, prob = c(1 - missing, missing))
  performed <- which(assessment_performed == "Yes")
  not_performed <- which(assessment_performed == "No")

  assessment_date <- random_date(n, which_na = not_performed)
  reason_not_done <- reason_not_collected(n, which_na = performed)

  df <- data.frame(
    patient_id = ids,
    visit_id = visit_ids,
    visit = visit,
    psqi_total = 0,
    psqi_component_1_score = 0,
    psqi_component_2_score = 0,
    psqi_component_3_score = 0,
    psqi_component_4_score = 0,
    psqi_component_5_score = 0,
    psqi_component_6_score = 0,
    psqi_component_7_score = 0,
    psqi_item1 = sample_time(n, hrs = 20:23, mins = seq(0, 45, by = 15)),
    psqi_item2 = 0,
    psqi_item3 = sample_time(n, hrs = c("05", "06", "07", "08", "09"), mins = seq(0, 45, by = 15)),
    psqi_item4 = 0,
    psqi_item5_a = 0,
    psqi_item5_b = 0,
    psqi_item5_c = 0,
    psqi_item5_d = 0,
    psqi_item5_e = 0,
    psqi_item5_f = 0,
    psqi_item5_g = 0,
    psqi_item5_h = 0,
    psqi_item5_i = 0,
    psqi_item5_j = 0,
    psqi_item5_yesno = 0,
    psqi_item6 = 0,
    psqi_item7 = 0,
    psqi_item8 = 0,
    psqi_item9 = 0,
    psqi_item10 = 0,
    psqi_item10_a = 0,
    psqi_item10_b = 0,
    psqi_item10_c = 0,
    psqi_item10_d = 0,
    psqi_item10_e = 0,
    psqi_item10_yesno = 0,
    assessment_date = assessment_date,
    assessment_performed = assessment_performed,
    reason_not_done = reason_not_done
  )
  return(df)
}

sample_time <- function(n, hrs, mins, which_na = c()) {
  x <- paste(sample(hrs, n, replace = TRUE), sample(mins, n, replace = TRUE), sep = ":")
  x <- remove_indices(x, which_na)
  return(x)
}

sample_psqi_item <- function(n, which_na = c()) {
  x <- sample(0:3, n, replace = TRUE)
  x <- remove_indices(x, which_na)
  return(x)
}
