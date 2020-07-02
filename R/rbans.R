
#' @export
rbans <- function(n, visit = "V1", missing = 0.1, ids = NULL, visit_ids = NULL) {
  ids <- handle_ids(n, ids)
  visit_ids <- handle_ids(n, visit_ids)

  assessment_performed <- sample_yes_no(n, prob = c(1 - missing, missing))
  performed <- which(assessment_performed == "Yes")
  not_performed <- which(assessment_performed == "No")

  assessment_date <- random_date(n, which_na = not_performed)
  reason_not_performed <- reason_not_collected(n, which_na = performed)

  raw_scores <- data.frame(
    rbans_coding = sample_rbans(n, which_na = not_performed),
    rbans_digit_span = sample_rbans(n, which_na = not_performed),
    rbans_figure_copy = sample_rbans(n, which_na = not_performed),
    rbans_figure_recall = sample_rbans(n, which_na = not_performed),
    rbans_list_learning = sample_rbans(n, which_na = not_performed),
    rbans_line_orientation = sample_rbans(n, which_na = not_performed),
    rbans_list_recall = sample_rbans(n, which_na = not_performed),
    rbans_list_recognition = sample_rbans(n, which_na = not_performed),
    rbans_picture_naming = sample_rbans(n, which_na = not_performed),
    rbans_semantic_fluency = sample_rbans(n, which_na = not_performed),
    rbans_story_memory = sample_rbans(n, which_na = not_performed),
    rbans_story_recall = sample_rbans(n, which_na = not_performed)
  )

  index_scores <- data.frame(
    rbans_attention_index = sample_rbans(n, which_na = not_performed),
    rbans_delayed_memory_index = sample_rbans(n, which_na = not_performed),
    rbans_immediate_memory_index = sample_rbans(n, which_na = not_performed),
    rbans_language_index = sample_rbans(n, which_na = not_performed),
    rbans_visio_constuctional_index = sample_rbans(n, which_na = not_performed)
  )

  df <- data.frame(
    patient_id = ids,
    visit_id = visit_ids,
    visit = visit,
    rbans_total_scale = rowMeans(index_scores),
    rbans_sum_of_index = rowSums(index_scores),
    index_scores,
    raw_scores,
    assessment_performed = assessment_performed,
    assessment_date = assessment_date,
    reason_not_performed = reason_not_performed
  )

  return(df)
}

sample_rbans <- function(n, which_na = c()) {
  x <- rnorm(n, mean = 100, sd = 10)
  x <- remove_indices(x, which_na)
  return(x)
}
