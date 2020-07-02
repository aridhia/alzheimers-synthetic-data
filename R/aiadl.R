
#' @importFrom dplyr bind_cols
#' @export
aiadl <- function(n, visit = "V1", missing = 0.1, ids = NULL, visit_ids = NULL) {
  ids <- handle_ids(n, ids)
  visit_ids <- handle_ids(n, visit_ids)

  assessment_performed <- sample_yes_no(n, prob = c(1 - missing, missing))
  performed <- which(assessment_performed == "Yes")
  not_performed <- which(assessment_performed == "No")

  assessment_date <- random_date(n, which_na = not_performed)
  reason_not_performed <- reason_not_collected(n, which_na = performed)

  aiadl_dfs <- lapply(1:69, function(x) aiadl_q(n, which_na = not_performed, colnames = paste0(c("q", "q", "q"), x, c("", "a", "b"))))
  aiadl_raw <- bind_cols(aiadl_dfs)
  q70 = sample_aiadl(n, which_na = not_performed)
  q70a = sample_aiadl(n, which_na = union(not_performed, which(q70 == 2)))

  total_score <- runif(n, 0, 20)
  sum_of_scores <- rowSums(aiadl_raw, na.rm = TRUE)

  df <- data.frame(
    patient_id = ids,
    visit_id = visit_ids,
    visit = visit,

    total_score = total_score,
    sum_of_scores = sum_of_scores,
    items_scored = 70,
    dont_know_count = 0,

    aiadl_raw,
    q70,
    q70a,

    assessment_performed = assessment_performed,
    assessment_date = assessment_date,
    reason_not_performed = reason_not_performed
  )

  return(df)
}

sample_aiadl <- function(n, which_na = c()) {
  x <- sample(c(1, 2), n, replace = TRUE)
  x <- remove_indices(x, which_na)
  return(x)
}

sample_aiadl_a <- function(n, which_na = c()) {
  x <- sample(c(0, 1, 2), n, replace = TRUE)
  x <- remove_indices(x, which_na)
  return(x)
}

sample_aiadl_b <- function(n, which_na = c()) {
  x <- sample(c(1, 2), n, replace = TRUE)
  x <- remove_indices(x, which_na)
  return(x)
}

aiadl_q <- function(n, which_na = c(), colnames = c("q", "qa", "qb")) {
  q <- sample_aiadl(n, which_na)
  qa_na <- which(q == 2)
  qb_na <- which(q != 2)
  qa <- sample_aiadl_a(n, which_na = union(which_na, qa_na))
  qb <- sample_aiadl_b(n, which_na = union(which_na, qb_na))


  df <- data.frame(
    q,
    qa,
    qb
  )
  names(df) <- colnames

  return(df)
}


