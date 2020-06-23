
#' @export
apoe <- function(n, visit = "V1", ids = NULL) {
  ids <- handle_ids(n, ids)

  apoe_sample_collected <- sample_yn(n, prob = c(0.9, 0.1))
  collected <- which(apoe_sample_collected == "Y")
  not_collected <- which(apoe_sample_collected == "N")

  apoe_result <- apoe_result(n, which_na = not_collected)
  apoe_sample_date <- random_date(n, which_na = not_collected)
  apoe_reason_not_collected <- reason_not_collected(n, which_na = collected)
  apoe_blood_sample_id <- apoe_sample_id(n, which_na = not_collected)

  df <- data.frame(
    patient_id = ids,
    visit = visit,
    apoe_result = apoe_result,
    apoe_sample_collected = apoe_sample_collected,
    apoe_sample_date = apoe_sample_date,
    apoe_reason_not_collected = apoe_reason_not_collected,
    apoe_blood_sample_id = apoe_blood_sample_id
  )

  return(df)
}

apoe_result <- function(n, prob = c(0.05, 0.1, 0.05, 0.35, 0.25, 0.2), which_na = c(), ...) {
  x <- sample(c("e2/e2", "e2/e3", "e2/e4", "e3/e3", "e3/e4", "e4/e4"), n, replace = TRUE, prob = prob, ...)
  remove_indices(x, which_na)
}



#' @importFrom stringr str_pad
#' @export
apoe_sample_id <- function(n, which_na = c()) {
  n_strings <- str_pad(as.character(1:n), width = 6, side = "left", pad = "0")
  x <- paste0("APOE-", n_strings)
  remove_indices(x, which_na)
}

