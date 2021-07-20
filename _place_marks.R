# Insert marks in a chapter file

remark_chapter <- function(fname, start, skip=5) {
  text <- readLines(fname)
  long_lines <- nchar(text) > 200
  blank_lines <- !grepl("[a-zA-Z0-9`]", text)
  chunk_lines <- grepl("^[```|:::]", text)


  long_lines <- long_lines & ! chunk_lines
  long_lines <- long_lines[-length(long_lines)]
  blank_lines <- blank_lines[-1]

  # Long lines followed by a blank line
  candidate_lines <- which(long_lines & blank_lines)

  for (k in 1:length(candidate_lines)) {
    mark_text <- glue::glue("`r mark({start + skip*(k-1)})`")
    text[candidate_lines[k]] <- paste(text[candidate_lines[k]], mark_text)
  }

  writeLines(text, con=fname)
  return(start + skip*length(candidate_lines))
}
<<<<<<< HEAD
=======


>>>>>>> shared
