

#' @export
csf <- function(n, visit = "V1", missing = 0.1, ids = NULL, visit_ids = NULL) {
  ids <- handle_ids(n, ids)
  visit_ids <- handle_ids(n, visit_ids)

  csf_sample_collected <- sample_yn(n, prob = c(1 - missing, missing))
  collected <- which(csf_sample_collected == "Y")
  not_collected <- which(csf_sample_collected == "N")

  csf_sample_id <- csf_sample_id(n, which_na = not_collected)
  ptau_result <- ptau(n, which_na = not_collected)
  ttau_result <- ttau(n, which_na = not_collected)
  abeta_1_42_result <- abeta_1_42(n, which_na = not_collected)
  csf_reason_not_collected <- reason_not_collected(n, which_na = collected)
  csf_sample_date <- random_date(n, which_na = not_collected)


  df <- data.frame(
    patient_id = ids,
    visit_id = visit_ids,
    visit = visit,
    ptau_result = ptau_result,
    ttau_result = ttau_result,
    abeta_1_42_result = abeta_1_42_result,
    abeta_1_42_comments = NA,
    csf_sample_collected = csf_sample_collected,
    csf_sample_date = csf_sample_date,
    csf_reason_not_collected = csf_reason_not_collected,
    ptau_reason_not_analysed = NA,
    ttau_reason_not_analysed = NA,
    abeta_1_42_reason_not_analysed = NA,
    csf_sample_id = csf_sample_id
  )

  return(df)
}

#' @importFrom stringr str_pad
#' @export
csf_sample_id <- function(n, which_na = c()) {
  n_strings <- str_pad(as.character(1:n), width = 6, side = "left", pad = "0")
  x <- paste0("CSF-", n_strings)
  remove_indices(x, which_na = which_na)
}

#' @export
ptau <- function(n, ttau = NULL, which_na = c()) {
  if(!is.null(ttau)) {
    x <- ptau_from_ttau(ttau)
  } else {
    x <- rnorm(n, mean = 20, sd = 10)
  }
  # Set values below 8 to exactly 8
  x[x < 8] <- 8
  x <- remove_indices(x, which_na)
  return(x)
}

#' @export
abeta_1_42 <- function(n, apoe = NULL, which_na = c()) {
  if(is.null(apoe)) {
    x <- rnorm(n, mean = 1150, sd = 400)
  } else {
    x <- abeta_from_apoe(apoe)
  }

  x[x < 270] <- 270
  x[x > 1700] <- 1700
  x <- remove_indices(x, which_na)
  return(x)
}

#' @export
ttau <- function(n, ptau = NULL, which_na = c()) {
  if(!is.null(ptau)) {
    x <- ttau_from_ptau(ptau)
    x <- x + rnorm(n)
  } else {
    x <- rnorm(n, mean = 200, sd = 90)
  }
  # Set values below 80 to exactly 80
  x[x < 80] <- 80
  x <- remove_indices(x, which_na)
  return(x)
}


# In the mockup:
## Ptau and Ttau will depend on eachother
## Abeta will depend on apoe
##
ttau_from_ptau <- function(ptau) {
  n <- length(ptau)
  ptau * 9 + 45 + rnorm(n, sd = ptau / 2)
}

ptau_from_ttau <- function(ttau) {
  n <- length(ttau)
  ttau * 0.1 - 3.6 + rnorm(n, sd = ttau / 20)
}


#' @importFrom dplyr if_else
abeta_from_apoe <- function(apoe) {
  n <- length(apoe)
  out <- numeric(n)

  apoe_e44 <- which(apoe == "e4/e4")
  apoe_e4 <- which(apoe %in% c("e2/e4", "e3/e4"))
  apoe_other <- which(!(apoe %in% c("e4/e4", "e2/e4", "e3/e4")))

  out[apoe_e44] <- rnorm(length(apoe_e44), mean = 600, sd = 300)
  out[apoe_e4] <- rnorm(length(apoe_e4), mean = 1000, sd = 400)
  out[apoe_other] <- rnorm(length(apoe_other), mean = 1250, sd = 300)

  return(out)
}
