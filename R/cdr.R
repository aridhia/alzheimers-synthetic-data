
#' @export
cdr <- function(n, visit = "V1", ids = NULL, visit_ids = NULL) {
  ids <- handle_ids(n, ids)
  visit_ids <- handle_ids(n, visit_ids)

  assessment_performed <- sample_yes_no(n)
  performed <- which(assessment_performed == "Yes")
  not_performed <- which(assessment_performed == "No")

  cdr_community_affairs <- sample_cdr(n, which_na = not_performed)
  cdr_home_hobbies <- sample_cdr(n, which_na = not_performed)
  cdr_judgement <- sample_cdr(n, which_na = not_performed)
  cdr_memory <- sample_cdr(n, which_na = not_performed)
  cdr_orientation <- sample_cdr(n, which_na = not_performed)
  cdr_personal_care <- sample_cdr(n, which_na = not_performed)

  cdr_sum_of_box <- cdr_community_affairs + cdr_home_hobbies + cdr_judgement +
    cdr_memory + cdr_orientation + cdr_personal_care

  cdr_global_score <- calculate_cdr_global_score(cdr_memory)

  assessment_date <- random_date(n, which_na = not_performed)
  reason_not_performed <- reason_not_collected(n, which_na = performed)

  df <- data.frame(
    patient_id = ids,
    visit_id = visit_ids,
    visit = visit,
    cdr_global_score = cdr_global_score,
    cdr_sum_of_box = cdr_sum_of_box,
    cdr_community_affairs = cdr_community_affairs,
    cdr_home_hobbies = cdr_home_hobbies,
    cdr_judgement = cdr_judgement,
    cdr_memory = cdr_memory,
    cdr_orientation = cdr_orientation,
    cdr_personal_care = cdr_personal_care,
    assessment_performed = assessment_performed,
    assessment_date = assessment_date,
    reason_not_performed = reason_not_performed
  )

  return(df)
}


sample_cdr <- function(n, which_na = c()) {
  x <- sample(c(0, 0.5, 1, 2, 3), n, replace = TRUE)
  x <- remove_indices(x, which_na)
  return(x)
}

#' @export
calculate_cdr_global_score <- function(memory, community_affairs, home_hobbies, judgement, orientation, personal_care) {
  return(memory)
}

