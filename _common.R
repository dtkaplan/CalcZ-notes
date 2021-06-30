library(mosaic)
library(mosaicCalc)
library(glue)
library(math141Z) # REPLACE THIS WHEN PACKAGES ARE RE-ALIGNED
library(here) # for file locations

knitr::opts_chunk$set(out.width="90%", fig.align="center")

# Link to the CalcZ sandbox
sandbox <- function() {
  "[sandbox](https://maa-statprep.shinyapps.io/CalcZ-Sandbox/){target=\"_blank\"}"
}

exercise_file <- function(day,  file_name, course="141Z") {
    path=glue::glue("Exercises/DD-{course}-{day}/{file_name}")
    here(path)
}

if (!exists("objective_list"))
  objective_list <- list()

add_objective <- function(ID, text) {
  objective_list[[length(objective_list) + 1]] <<-
                      list(ID = ID, text = text)
}

state_objective <- function(ID, text) {
  add_objective(ID, text)
  format_objective(list(ID=ID, text=text))
}

format_objective <- function(obj) {
  knitr::asis_output(glue::glue("#. **[{obj$ID}]** *{obj$text}*\n\n"))
}

show_objectives <- function() {
  Tmp <- lapply(objective_list, tibble::as_tibble) %>%
    bind_rows()
  readr::write_csv(Tmp, file="objective-list.csv")

  lapply(objective_list, format_objective) %>%
    unlist() %>%
    paste(collapse="\n") %>%
    knitr::asis_output()
}

# A blank image for spacing
BlankImage <- gf_blank(hp ~ wt, data=mtcars) %>% gf_theme(theme_void())

letter_counter <- function() {
  counter <- 0
  names <- c(LETTERS, paste0(LETTERS, 1), paste0(LETTERS, 2),
             paste0(LETTERS, 3), paste0(LETTERS, 4), paste0(LETTERS, 5))
  res <- list()
  res$reset <- function(s = 0) {
    counter <<- s
  }
  res$get   <- function() {
    counter <<- counter+1
    names[counter %% length(names)] # never run out
  }
  res
}
MC_counter <- letter_counter()


# Formatting multiple-choice questions for the book
askMC <- function (prompt = "The question prompt", ..., id = NULL, right_one = NULL,
          inline = FALSE, random_answer_order = FALSE, allow_retry = TRUE,
          correct = "Right!", incorrect = "Sorry.", message = NULL,
          post_message = NULL, submit_button = "Check answer", try_again_button = "Try again",
          allow_multiple_correct = FALSE)
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

  feedback <- paste("<span class='mcanswer'>",
                    ifelse(answer_table$correct, "✓", "☹︎"),
                     answer_table$feedback, "</span>")
  answers <- paste0(answer_labels[1:nrow(answer_table)],
                    answer_table$item,
                    feedback,
                    collapse = newline)

  knitr::asis_output(paste0(
                "**Question ", MC_counter$get(), "**  ",
                out, answers))
}

sandbox_link <- function() {
  "[SANDBOX](https://maa-statprep.shinyapps.io/CalcZ-Sandbox/)"
}

segments <- function(tilde, domain, h=NULL, nsegs=20) {
  f <- makeFun(tilde)
  df <- D(tilde)
  vname <- all.vars(tilde[[3]])
  if (is.null(h)) h <- base::diff(domain[[1]])/nsegs
  start <- seq(domain[[1]][1] + h/2, domain[[1]][2], by=h )
  slopes <- df(start)
  offsets <- f(start)

  res <- tibble(x = start - h/2,
                xend = x + h,
                start = start,
                y = -slopes*h/2.2,
                yend = slopes*h/2.2,
                slope = slopes,
                offset = offsets,
                yf = y + offset,
                yfend = yend + offset)

  res
}

mark <- function(id) {
  id <- as.character(substitute(id))
  glue::glue('<span style="float: right; padding-left: 50px;"><a name="{id}" href="#{id}"><img src="./images/_icons8/icons8-signpost.png" title="Location: {id}" width="12px"/></a><span style="color: red; font-size: 6pt;">{id}</red></span>')
}
