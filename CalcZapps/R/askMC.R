#' Formatting multiple-choice questions for the book
#' @export
askMC <- function (prompt = "The question prompt", ..., id = NULL, right_one = NULL,
                   inline = FALSE, random_answer_order = FALSE, allow_retry = TRUE,
                   correct = "Right!", incorrect = "Sorry.", message = NULL,
                   post_message = NULL, submit_button = "Check answer", try_again_button = "Try again",
                   allow_multiple_correct = FALSE, show_feedback=TRUE)
{
  out <- paste(prompt, "\n\n")
  raw_labels <- c("i", "ii", "iii", "iv", "v", "vi", "vii", "viii", "ix", "x")
  answer_labels <- c(raw_labels,
                     paste0("x", raw_labels),
                     paste0("xx", raw_labels),
                     paste0("xl", raw_labels),
                     paste0("l", raw_labels),
                     letters, LETTERS, paste0(letters, "2"))
  answer_table <- etude2:::dots_to_answers(..., right_one = right_one,
                                           allow_multiple_correct = allow_multiple_correct)
  place_inline <- inline || (sum(nchar(answer_table$item)) < 40)
  if (place_inline) {
    answer_labels <- paste0(rep("    ", nrow(answer_table)))
    newline <- "   "
  } else {
    answer_labels <- paste0(answer_labels, ". ")
    newline <- "     \n"

  }

  if (show_feedback) {
    feedback <- paste("<span class='mcanswer'>",
                      ifelse(answer_table$correct, "✔︎", "︎✘"),
                      answer_table$feedback, "</span>")
  } else {
    feedback <- ""
  }
  answers <- paste0(answer_labels[1:nrow(answer_table)],
                    answer_table$item,
                    feedback,
                    collapse = newline)

  knitr::asis_output(paste0(
    "**Question ", MC_counter$get(), "**  ",
    out, answers))
}
