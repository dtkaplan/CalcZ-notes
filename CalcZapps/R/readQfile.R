#' Read question files
#'
#' @export
readQfiles <- function(file_names) {
  Q <- NULL
  C <- NULL
  for (k in 1:length(file_names)) {
    this <- CalcZapps::readQfile(file_names[k])
    Q <- dplyr::bind_rows(Q, this$Q)
    C <- dplyr::bind_rows(C, this$C)
  }

  return(list(Q=Q, C=C))
}
#'
#' @export
readQfile <- function(fname) {
  content <- readLines(fname)
  start_lines <- grepl("^\\*\\*", content)
  Headers <- content[start_lines]
  qname <- gsub("\\*+([a-zA-Z0-9\\.-_]*)\\*+.*$", "\\1", Headers)
  prompt <- gsub("\\*+[a-zA-Z0-9\\.-_\\?,!\\$\\|\\\\'\"]*\\*+\\s*(.*)\\s*\\[(.*)\\]\\w*$", "\\1", Headers)
  topic <- gsub("\\*+[a-zA-Z0-9\\.-_]*\\*+\\s*(.*)\\s*\\[(.*)\\]\\w*$", "\\2", Headers)

  start_line_num <- c(which(start_lines), length(content) + 1)

  Questions <- tibble::tibble(
    qname = qname, prompt=prompt, topic = topic,
    unique = stringi::stri_rand_strings(length(qname), 14)
  )

  Choices <- list()
  for (k in 1:sum(start_lines)) {
    these_line_nums <- (start_line_num[k]+1):(start_line_num[k+1] - 1)
    this_question <- content[these_line_nums]
    valid_lines <- grepl("^\\s?\\[", this_question)
    invalid_lines <- which(!valid_lines)
    wrong_start <- !grepl("^ {0,}\\#", this_question[!valid_lines]) & nchar(this_question[!valid_lines]) > 0

    if (any(wrong_start)) {
      wrong_line_nums <- these_line_nums[!valid_lines][wrong_start]
      stop("Lines ",
           paste0(wrong_line_nums, collapse=", "),
           " in file ", fname,
           " are invalid. Non-MC item lines must be blank or comment (#)."
      )
    }
    lines <- this_question[valid_lines]
    Choices[[k]] <- parse_choices(lines)
    Choices[[k]]$question <- Questions$unique[k]
  }

  list(Q = Questions, C = dplyr::bind_rows(Choices))
}


#' Accepted syntax
#' Line starts with [a] or [+] means correct, with [a] meaning "keep order" and "+"
#' meaning "randomize order" [] or [ ] or [-] means incorrect
#' Next characters until open bracket are the choice text itself
#' Optionally [feedback] {param=whatever, param2=whatever}
#' @export
parse_choices <- function(lines) {
  right_wrong_pattern <- "^\\s?\\[\\s?(.?)\\s?\\].*"
  mark <- gsub(right_wrong_pattern, "\\1", lines)
  # Strip off leading and trailing [] blocks
  content <- gsub("\\[.?\\]\\s?", "", lines)
  content <- gsub("\\[.*$", "", content)
  content <- gsub("\\s+$", "", content)
  feedback_pattern <- ".*\\[(.*)\\]\\s?$"
  feedback <- gsub(feedback_pattern, "\\1", lines)
  has_feedback <- grepl("\\]\\s?$", lines)
  feedback[!has_feedback] <- " "

  tibble::tibble(choice_text = content,
                 feedback = feedback,
                 mark = mark,
                 correct =  mark %in% c("+", "a")
  )
}

