
sample_numeric <- function(n, ...) {
  rnorm(n, ...)
}

sample_integer <- function(n, ...) {

}

random_date <- function(n, start = "2017-01-01", end = "2017-12-31", which_na = c()) {
  s <- seq(from = as.Date(start), to = as.Date(end), by = 1)
  x <- sample(s, n, replace = TRUE)
  remove_indices(x, which_na)
}

sample_yn <- function(n, prob = c(0.5, 0.5)) {
  sample(c("Y", "N"), size = n, replace = TRUE, prob = prob)
}

sample_yes_no <- function(n, prob = c(0.5, 0.5), which_na = c()) {
  x <- sample(c("Yes", "No"), size = n, replace = TRUE, prob = prob)
  x <- remove_indices(x, which_na)
  return(x)
}

reason_not_collected <- function(n, prob = rep(0.25, 4), which_na = c()) {
  x <- sample(c("Physically unwell", "Mentally unwell", "Reasons external to participant", "Other reasons"), size = n, replace = TRUE, prob = prob)
  remove_indices(x, which_na)
}

remove_indices <- function(x, which_na = c()) {
  x[which_na] <- NA
  return(x)
}
