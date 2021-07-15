#' Generates a random regret message
#'
#' @export
random_regret <- function() {
  sample(regrets, size=1)
}

regrets <- c(
  "Sorry!",
  "Not quite.",
  "Not this time.",
  "No.",
  "Wrong",
  "Unh-unh.",
  "False.",
  "Nope"
)

#' @export
random_success <- function() {
  sample(success, size=1)
}

success <- c(
  "Right!",
  "Excellent!",
  "Good.",
  "Correct.",
  "Right-o!"

)
