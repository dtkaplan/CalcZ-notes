#' Formatting multiple-choice questions for the book
#' @export
askMC <- function (prompt = "The question prompt", ..., id = NULL, right_one = NULL,
                   inline = FALSE, random_answer_order = FALSE, allow_retry = TRUE,
                   correct = "Right!", incorrect = "Sorry.", message = NULL,
                   post_message = NULL, submit_button = "Check answer", try_again_button = "Try again",
                   allow_multiple_correct = FALSE, show_feedback=TRUE, out_format=c("Markdown", "GradeScope")) {
  out <- paste(prompt, "\n\n")
  out_format <- match.arg(out_format)
  raw_labels <- c("i", "ii", "iii", "iv", "v", "vi", "vii", "viii", "ix", "x")
  answer_labels <- c(raw_labels,
                     paste0("x", raw_labels),
                     paste0("xx", raw_labels),
                     paste0("xl", raw_labels),
                     paste0("l", raw_labels),
                     letters, LETTERS, paste0(letters, "2"))
  answer_table <- etude2:::dots_to_answers(..., right_one = right_one,
                                           allow_multiple_correct = allow_multiple_correct)


  ## GradeScope output module
  if (out_format == "GradeScope") {

    answers <- paste0("(", ifelse(answer_table$correct, "x", " "), ")  ",
                      fix_dollar_signs(answer_table$item), collapse="\n")

    feedback_for_correct <- answer_table$feedback[answer_table$correct]
    if (nchar(gsub(" *", "", feedback_for_correct)) == 0)
      feedback_for_correct <- random_success()

    feedback <- paste0("[[",
                       paste(fix_dollar_signs(feedback_for_correct),
                             collapse = "\n"),
                       "]]\n")

    total <- paste(out, answers, feedback, sep="\n\n")

    Res <- knitr::asis_output(paste0("<pre>",  total, "\n</pre>\n"))

    return(Res)
  }
  ## End of GradeScope module


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
                      ifelse(answer_table$correct, random_success(), "︎✘"),
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


# For Gradescope output
#' @export
askGS <- function(...) {
  CalcZapps::askMC(..., out_format = "GradeScope")
}
# fix the dollar signs for GradeScope
fix_dollar_signs <- function(str) {
  str <- gsub("\\${1}", "\\$\\$", str)
  str <- gsub("\\${4}", "\\$\\$\\$", str)
  str
}
