
#' @export
favourites <- function(n, visit = "V1", missing = 0.1, ids = NULL, visit_ids = NULL) {
  ids <- handle_ids(n, ids)
  visit_ids <- handle_ids(n, visit_ids)

  assessment_performed <- sample_yes_no(n, prob = c(1 - missing, missing))
  performed <- which(assessment_performed == "Yes")
  not_performed <- which(assessment_performed == "No")

  assessment_date <- random_date(n, which_na = not_performed)
  reason_not_performed <- reason_not_collected(n, which_na = performed)

  animals <- sample_fav_animal(4, dk = FALSE)
  foods <- sample_fav_food(4, dk = FALSE)

  order1 <- sample(4)
  order2 <- sample(4)
  order_delay <- sample(4)

  animals1 <- animals[order1]
  foods1 <- foods[order1]
  animals2 <- animals[order2]
  foods2 <- foods[order2]
  animals_delay <- animals[order_delay]
  foods_delay <- foods[order_delay]

  fav_learn_r1_food_1 <- sample_fav_animal(n, which_na = not_performed)
  fav_learn_r1_food_1_score <- ifelse(fav_learn_r1_food_1 == animals1[1], "Correct", ifelse(fav_learn_r1_food_1 %in% animals1, "SME", "INT"))


  df <- data.frame(
    patient_id = ids,
    visit_id = visit_ids,
    visit = visit,
    fav_learn_r1_food_1 = fav_learn_r1_food_1,
    fav_learn_r1_food_1_score = fav_learn_r1_food_1_score,

    assessment_performed = assessment_performed,
    assessment_date = assessment_date,
    reason_not_performed = reason_not_performed
  )

  return(df)
}

sample_fav_animal <- function(n, which_na = c(), dk = TRUE) {
  choices <- c("shark", "turtle", "wolf", "cow", "rabbit", "frog", "penguin", "pig", "monkey", "goat")
  if(dk) {
    choices <- c(choices, "DK")
  }
  x <- sample(choices, n)
  x <- remove_indices(x, which_na)
  return(x)
}

sample_fav_food <- function(n, which_na = c(), dk = TRUE) {
  choices <- c("lettuce", "coconut", "cherry", "grapes", "peas", "carrot", "tomato", "lemon", "plum", "mushroom")
  if(dk) {
    choices <- c(choices, "DK")
  }
  x <- sample(choices, n)
  x <- remove_indices(x, which_na)
  return(x)
}
