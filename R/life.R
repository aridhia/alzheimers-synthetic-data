
#' @title Lifestyle
#' @export
life <- function(n, visit = "V1", missing = 0.1, ids = NULL, visit_ids = NULL) {
  ids <- handle_ids(n, ids)
  visit_ids <- handle_ids(n, visit_ids)

  assessment_done <- sample_yes_no(n, prob = c(1 - missing, missing))
  done <- which(assessment_done == "Yes")
  not_done <- which(assessment_done == "No")

  reason_not_completed <- reason_not_collected(n, which_na = done)
  drug_abuse <- sample_smoking(n, which_na = not_done)


  df <- data.frame(
    patient_id = ids,
    visit_id = visit_ids,
    visit = visit,
    lifestyle_assessment_completed = assessment_done,
    reason_not_completed = reason_not_completed,
    current_health = sample_health(n, which_na = not_done),
    physical_activity = sample_phys_activity(n, which_na = not_done),
    physical_fitness = sample_health(n, which_na = not_done),
    smoking = sample_smoking(n, which_na = not_done),
    drug_abuse = drug_abuse,
    drug_name = sample_drug_name(n, which_na = which(drug_abuse == "Never"))
  )
  return(df)
}

sample_phys_activity <- function(n, which_na = c()) {
  x <- sample(c("Not at all", "A few times a year", "2-3 times a month", "Once a week", "2-3 times a week", "Daily"), n, replace = TRUE)
  x <- remove_indices(x, which_na)
  return(x)
}

sample_health <- function(n, which_na = c()) {
  x <- sample(c("Very Good", "Good", "Satisfactory", "Relatively Poor"), n, replace = TRUE)
  x <- remove_indices(x, which_na)
  return(x)
}

sample_smoking <- function(n, which_na = c()) {
  x <- sample(c("Current", "Past", "Never"), n, replace = TRUE)
  x <- remove_indices(x, which_na)
  return(x)
}

sample_drug_name <- function(n, which_na = c()) {
  x <- sample(c("Cannabis", "Cocaine"), n, replace = TRUE)
  x <- remove_indices(x, which_na)
  return(x)
}
