
patient_id <- function(n) {
  1:n
}

handle_ids <- function(n, ids) {
  if(is.null(ids)) {
    return(patient_id(n))
  }
}
