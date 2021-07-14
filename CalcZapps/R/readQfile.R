#' Read a question file
#'
#' @export
readQfile <- function(fname) {
  content <- readLines(fname)
  start_lines <- grepl("^\\*\\*", content)
  Headers <- content[start_lines]
  qname <- gsub("\\*+([a-zA-Z0-9\\.]*)\\*+.*$", "\\1", Headers)
  prompt <- gsub("\\*+[a-zA-Z0-9\\.]*\\*+\\s*(.*)\\s*\\[(.*)\\]\\w*$", "\\1", Headers)
  topic <- gsub("\\*+[a-zA-Z0-9\\.]*\\*+\\s*(.*)\\s*\\[(.*)\\]\\w*$", "\\2", Headers)

  start_line_num <- c(which(start_lines), length(content) + 1)

  Questions <- tibble::tibble(
    qname = qname, prompt=prompt, topic = topic,
    unique = stringi::stri_rand_strings(length(qname), 14)
  )

  Choices <- list()
  for (k in 1:sum(start_lines)) {
    these_line_nums <- (start_line_num[k]+1):(start_line_num[k+1] - 1)
    this_question <- content[these_line_nums]
    valid_lines <- grepl("^-\\s*", this_question)
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
    lines <- strsplit(this_question[valid_lines], "[", fixed=TRUE)
    Choices[[k]] <- tibble::tibble(
      prompt = sapply(lines, function(x) gsub("^-\\s*", "", x[1])),
      correct = sapply(lines, function(x) gsub("\\]\\s*$", "", x[2])),
      feedback = sapply(lines, function(x) gsub("\\]$\\s*", "", x[3])),
      question = Questions$unique[k]
    )
  }

  list(Q = Questions, C = dplyr::bind_rows(Choices))
}
