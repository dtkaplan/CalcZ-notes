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
