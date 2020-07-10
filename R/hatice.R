#' @export
hatice <- function(n, visit = "V1", missing = 0.1, ids = NULL, visit_ids = NULL) {
  ids <- handle_ids(n, ids)
  visit_ids <- handle_ids(n, visit_ids)

  df <- data.frame(
    patient_id = ids,
    visit_id = visit_ids,
    visit = visit
  )
  return(df)
}
