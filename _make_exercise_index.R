# make the index of exercises

make_exercise_index <- function(fnames) {

  Res <- list()
  for (file in fnames) {
    content <- readLines(file)
    firstline <- "number, hash, sourcefile"
    # Pull out all lines where an exercise is inserted into text
    exline_nums <- grep("r insert_calcZ_exercise", content)
    if (length(exline_nums) == 0 ) next;
    exlines <- content[exline_nums]
    exlines <- gsub(".r insert\\_calcZ\\_exercise\\(|\\).", "", exlines)
    cat("Filename ", file, "\n")
    this_file_res <- read.csv(textConnection(c(firstline,exlines)),
                              stringsAsFactors = FALSE) |> as.data.frame()
    this_file_res <- this_file_res %>%
      mutate(
        number = as.character(number),
        sourcefile = gsub("^ *", "", sourcefile)
      )
    this_file_res$chapter_file <- file
    this_file_res$line_no <- exline_nums
    this_file_res$wordname <- gsub(" *[^/]*/[^/]*/([^/]*).Rmd *",
                                   "\\1",
                                   this_file_res$sourcefile)

    Res[[length(Res) + 1]] <- this_file_res
  }

  dplyr::bind_rows(Res)
}


Ex_table <- make_exercise_index(dir(pattern = "*.Rmd"))
readr::write_csv(Ex_table, file="_exercise_cross_reference.csv")
