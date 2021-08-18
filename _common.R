library(mosaic)
library(mosaicCalc)
library(glue)
library(math141Z) # REPLACE THIS WHEN PACKAGES ARE RE-ALIGNED
library(CalcZapps)
library(here) # for file locations
library(thematic)
library(gridExtra) # for arranging plots.

# theme the plots to match the document
thematic_rmd()
ggplot2::theme_set(theme_bw(base_size = 16))


knitr::opts_chunk$set(out.width="90%", fig.align="center")

# Link to the CalcZ sandbox
sandbox <- function() {
  "[sandbox](https://maa-statprep.shinyapps.io/CalcZ-Sandbox/){target=\"_blank\"}"
}

exercise_file <- function(day,  file_name, course="141Z") {
    path=glue::glue("Exercises/DD-{course}-{day}/{file_name}")
    here(path)
}

# Resolve the exercise number assigned to a permanent name like "3KEgLM"
# or "chicken-sit-table".
# See script in <_make_exercise_index.R>
if (file.exists("_exercise_cross_reference.csv")) {
  exercise_cross_reference <- readr::read_csv("_exercise_cross_reference.csv")
}

ref_ex <- function(perm_name) {
  # This is not yet implemented but will be based on a program that searches through all the
  # insertion commands in the chapters and assembles a table with exercise number, perm name, word-name. This
  # function will read that table and replace <perm_name> with the exercise number.
  res <- if (nchar(perm_name) < 10) { # It's a hash
      exercise_cross_reference %>% filter(hash==!!perm_name) %>% .$number
  } else {
    exercise_cross_reference %>% filter(wordname == !!perm_name) %>% .$number
  }

  if (is.null(res) || nchar(res) == 0 ) return("**MISSING EXERCISE NUMBER**")
  else return(res)
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

MC_counter <- CalcZapps:::letter_counter()



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
  glue::glue('<span style="float: right; padding-left: 50px;"><a name="{id}" href="#{id}"><img src="www/icons8-signpost.png" title="Location: {id}" width="12px"/></a><span style="color: red; font-size: 6pt;">{id}</red></span>')
}

# This has been replaced by CalcZapps::exercise_navpoint() for new exercises

ex.mark <- function(num, perm_id, fname="no file specified") {
  perm_id <- as.character(substitute(perm_id))
  glue::glue('**Exercise {num}**: <span><a name="File: {fname}" href="#{perm_id}"><img src="www/icons8-signpost.png" title="Location: {fname}" width="12px"/></a><span style="color: red; font-size: 9pt;">{perm_id}</red></span>')
}

# For gradescope output
askMC <- CalcZapps::askGS

