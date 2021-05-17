library(mosaic)
library(mosaicCalc)
library(math141Z) # REPLACE THIS WHEN PACKAGES ARE RE-ALIGNED

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
