
#' @title HATICE
#' @param n Integer number of participants to create data for
#' @param visit The name of the visit to create data for as a character string
#' @param missing The proportion of participants for which the questionnaire was not completed
#' @param ids Optional vector of participant IDs
#' @param visit_ids Optional vector of visit IDs
#' @return A data.frame
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
