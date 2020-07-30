
#' @title Socio-Demographics
#' @param n Integer number of participants to create data for
#' @param ids Optional vector of participant IDs
#' @return A data.frame
#' @export
socio_demographics <- function(n, ids = NULL) {
  ids <- handle_ids(n, ids)

  site_name = sample_site(n)
  site_id <- site_ids(site_name)

  df <- data.frame(
    patient_id = ids,
    site_name = site_name,
    site_id = site_id,
    sex = sample_sex(n),
    age_years = sample_age_years(n),
    age_months = sample_age_months(n),
    handedness = sample_handedness(n),
    years_education = sample_years_education(n),
    marital_status = sample_marital_status(n),
    ethnicity = sample_ethnicity(n)
  )

  return(df)
}

sample_site <- function(n) {
  sample(c("UEDIN", "BBRC", "VUMC", "CHUT"), n, replace = TRUE)
}

site_ids <- function(sites) {
  x <- factor(sites, levels = c("UEDIN", "BBRC", "VUMC", "CHUT"), labels = c("010", "020", "030", "040"))
  x <- as.character(x)
  return(x)
}

sample_sex <- function(n, prob = c(0.45, 0.45, 0.05, 0.05)) {
  x <- sample(c("m", "f", "u", "un"), n, replace = TRUE, prob = prob)
  return(x)
}

sample_handedness <- function(n, prob = c(0.85, 0.15)) {
  sample(c("Right Hand", "Left Hand"), n, replace = TRUE)
}

sample_ethnicity <- function(n) {
  sample(c("Caucasian/white",
           "Asian",
           "Black",
           "Combination of previous groups",
           "Other"),
         n, replace = TRUE)
}

sample_marital_status <- function(n) {
  sample(c("Married or cohabiting",
           "Widowed",
           "Divorced",
           "Single"),
         n, replace = TRUE)
}

sample_age_months <- function(n) {
  sample(c(1:12), n, replace = TRUE)
}

sample_age_years <- function(n, min_age = 50, max_age = 90, which_na = c()) {
  x <- sample(min_age:max_age, n, replace = TRUE)
  x <- remove_indices(x, which_na)
  return(x)
}

sample_years_education <- function(n, min_years = 10, max_years = 20) {
  sample(min_years:max_years, n, replace = TRUE)
}
