# Create index information

Chapters <- yaml::read_yaml("_bookdown.yml")$rmd_files

toss_out <- grepl("part-marker|index|outline", Chapters)
Chapters <- Chapters[!toss_out]

#Chapters <- Chapters[1]


Res <- NULL
for (k in 1:length(Chapters)) {
  text <- readLines(Chapters[k])
  definition_lines <- grepl("\\*{3}", text)
  line_nums <- which(definition_lines)
  matches <- stringr::str_match_all(text, "\\*{3}([^\\*]*)\\*{3}")
  make_entry <- function(x, y) {
    if (nrow(x) == 0) NULL
    else tibble(phrase = x[,2], line=y, chapter=Chapters[k], chap_num=k)
  }
  This_file <- mapply(make_entry, matches, 1:length(matches)) %>% bind_rows()
  Res <- bind_rows(Res, This_file)
}

  #rmarkdown::yaml_front_matter("_bookdown.yml")
