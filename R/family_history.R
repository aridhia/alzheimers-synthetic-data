#' @export
family_history <- function(n, missing = 0.1, ids = NULL) {
  ids <- handle_ids(n, ids)

  n_family_members <- sample(0:4, n, replace = TRUE, prob = c(0.25, 0.30, 0.25, 0.15, 0.05))

  n_records <- ifelse(n_family_members > 0, n_family_members, 1)
  total_records <- sum(n_records)

  ids <- rep(ids, n_records)
  family_dementia_history_list <- lapply(n_family_members, function(x) {
    if(x > 0) {
      return(rep("Yes", x))
    } else {
      return("No")
    }
  })

  family_dementia_history <- unlist(family_dementia_history_list)

  no_history <- which(family_dementia_history == "No")

  family_members_list <- lapply(n_family_members, function(x) {
    if(x == 0) {
      return(NA)
    }
    sample(c("Mother", "Father", "Sister", "Brother"), x, replace = FALSE)
  })

  family_member <- unlist(family_members_list)

  bio_relative <- sample_yes_no(total_records, which_na = no_history, prob = c(1 - missing, missing))

  df <- data.frame(
    patient_id = ids,
    family_dementia_history = family_dementia_history,
    family_member = family_member,
    bio_relative = bio_relative,
    age_at_diagnosis = sample_age_years(total_records, which_na = no_history)
  )

  return(df)
}
