library(shiny)
library(dplyr)
library(shinyjs)
library(CalcZapps)
library(digest)

# Success policy: 17 out of 20
nright <- 8
ntotal <- 10

kill_empty_lines <- function(str) {
  str[nchar(str) > 0]
}

source_files <-
  readLines("https://raw.githubusercontent.com/dtkaplan/Zdrill/main/Roster.Rmd") %>%
  gsub("#.*$", "", .) %>% # get rid of comments
  kill_empty_lines() %>%
  paste0("https://raw.githubusercontent.com/dtkaplan/Zdrill/main/inst/", .)

# Add in the testing questions straight from the Zdrill sources
draft_questions <- "/Users/kaplan/KaplanFiles/USAFA/Zdrill/testing.Rmd"
dq_size <- file.info(draft_questions)$size
if (is.na(dq_size) || dq_size < 10) draft_questions <- NULL

# test_file <- system.file("Zdrill/www/text.Rmd", package="CalcZapps")
source_files <- c(
    source_files,
    draft_questions
)



Qbank <- readQfiles(source_files)
Qbank_topics <- unique(Qbank$Q$topic)

# an experiment to see if I can get markdown to render as HTML.
md2html <- function(s) {
    backtick <- "`([^`]*)`"
    doubledollar   <- "\\${2}([^\\$]+)\\${2}"
    dollar   <- "\\${1}([^\\$]+)\\${1}"
    bold     <- "\\*{2}([^\\*]*)\\*{2}"
    italics  <- "\\*{1}([^\\*]*)\\*{1}"
    s %>%
        gsub(backtick, "<code>\\1</code>", ., perl=TRUE) %>%
        gsub(doubledollar, "QQQQQ\\1QQQQQ", ., perl=TRUE) %>% # re-encode the $$
        gsub(dollar, "\\\\\\(\\1\\\\)", ., perl=TRUE) %>%
        gsub(bold, "<strong>\\1</strong>", ., perl=TRUE) %>%
        gsub(italics, "<em>\\1</em>", ., perl=TRUE) %>%
        gsub("QQQQQ", "\\$\\$", ., perl=TRUE) # put back the $$
}

ui <- fluidPage(
  # make sure the answer elements are wide enough
  tags$head(
    tags$style(HTML("
      @import url('https://fonts.googleapis.com/css2?family=Yusei+Magic&display=swap');
      .MCchoice {
        width: 50em;
      }"))),

  titlePanel("CalcZ Quick Response"),

    sidebarLayout(
        sidebarPanel(
            useShinyjs(),
            selectInput("topic_choice", "Topic:", sort(Qbank_topics) ),
            actionButton("startover", "Start Over"),
            tags$hr(),
            textOutput("question_name"),
            tags$hr(),
            textInput("passcode", "access code"),
            selectInput("instructor_choice", "Choose question:", choices=1:3),
            textOutput("show_success_code")
        ),

        # Show a plot of the generated distribution
        mainPanel(
            div(uiOutput("Qprompt")),
            uiOutput("Choices"),
            tags$hr(),
            uiOutput("Feedback"),
            actionButton("check_answer", "Check answer"),
            tags$hr(),
            actionButton("nextQ", "Next question"),
            tags$hr(),
            textOutput("score"),
            tags$hr(),
            textOutput("success_key")

        ),
        position = "right"
    )
)

server <- function(input, output, session) {
    State <- reactiveValues()
    this_question <- reactiveValues(valid = FALSE)
    feedback <- reactiveValues(message="")

    observeEvent(input$check_answer, {
        if (is.null(input$answers)) feedback$message <- ""
        else feedback$message <- this_question$feedback[as.integer(input$answers)]
    })

    shinyjs::hide("nextQ")

    observeEvent(c(input$topic_choice, input$startover), {
        # Get the unique IDs, in random order of the questions matching the topic
        Questions <- Qbank$Q %>% dplyr::filter(topic == input$topic_choice)
        State$next_q <- 0
        State$q_ids <- sample(Questions$unique)
        State$q_names <- sort(Questions$qname)
        State$n_correct <- 0
        State$n_answered <- 0
        State$success_code <- digest::digest(input$topic_choice, "md5", serialize = FALSE)
        next_question()
    })


    observeEvent(input$the_choices, {
      cat("Choice made: ", input$the_choices, "\n")
    })
    observeEvent(input$check_answer, {
      if (instructor_chooses()) return()
      req(input$answers)

      State$n_answered <- State$n_answered + 1

      was_correct <- this_question$correct == as.numeric(input$answers)
      if (was_correct) {
        State$n_correct <- State$n_correct + 1
      } else {
        # Insert the question back into the queue
        this_id <- State$q_ids[State$next_q]
        n_qs <- length(State$q_ids)
        if (n_qs - State$next_q <= 3 ) {
          return(NULL) # do nothing
        } else {
          index <- sample((State$next_q+3):n_qs, size=1)
        }

        if (index <= length(State$q_ids)) {
          tmp <- State$q_ids[index]
          State$q_ids[index] <- this_id
          State$q_ids[State$next_q] <- tmp
        }
      }
    })

    observeEvent(c(input$instructor_choice, input$nextQ), {
        shinyjs::show("check_answer")
        shinyjs::hide("nextQ")
        isolate(feedback$message <- "")
        next_question()
    })

    output$question_name <- renderText({
        glue::glue("Question ID: {this_question$name}")
    })
    output$success_key <- renderText({
      return(NULL) # Taking out the GradeScope hash connection
        if (State$n_correct < nright ) {
            glue::glue("Target: {nright} out of {ntotal}")
        } else if (State$n_answered <= ntotal ) {
            glue::glue("Success. Upload your token: {State$success_code}")
        } else {
            State$n_correct <- 0
            State$n_answered <- 0
            "Sorry. Resetting to zero. Try again!"
        }
    })
    output$show_success_code <- renderText({
      paste("Success token is", {State$success_code})
    })

    output$score <- renderText({
        paste(input$topic_choice, ": ", State$n_correct, "correct out of", State$n_answered, "attempts.")
    })

    # The guts of the server
    next_question <- reactive({
        input$nextQ # for the dependency
        input$topic_choice # ditto
        # Get the next item
        if (instructor_chooses()) {
          selected_question <- Qbank$Q %>%
            filter(topic == input$topic_choice, qname == input$instructor_choice) %>% .$unique
          req(selected_question)
        } else {
          # shuffle as we go around the loop
          isolate({
            State$next_q <- State$next_q + 1
            if (State$next_q > length(State$q_ids)) {
              State$next_q <- 1
              State$q_ids <- sample(State$q_ids)
            }
          })
          k <- State$next_q

          # Option for an instructor to set the question.
          selected_question <- State$q_ids[k]
        }
        # Fill in the next question
        this_question$valid <- TRUE
        prompt_info <- Qbank$Q %>% filter(unique == selected_question)
        this_question$name   <- prompt_info$qname
        this_question$prompt <- prompt_info$prompt %>% # handle backquotes
            md2html()
        choices <- Qbank$C %>% filter(question == selected_question)
        this_question$choices <- choices$choice_text %>% md2html()
        this_question$correct <- which(choices$correct)
        this_question$feedback <- choices$feedback %>%
            md2html()
        this_question$random_order <- !any(choices$mark == "a")
        list(unique = prompt_info$unique, name = prompt_info$qname)

    })
    output$Qprompt <- renderUI({
        if (!this_question$valid) return(NULL)
        withMathJax(HTML(glue::glue("<div style=\"color: green; font-size: 1.3em;\">{this_question$prompt}</div>")))
    })
    output$Choices <- renderUI({
        if (!this_question$valid) return(NULL)
        prompts <- this_question$choices
        inds <- 1:length(prompts)
        prompts <- lapply(prompts, HTML)
        if (this_question$random_order) {
            #randomize order of choices
            inds <- sample(1:length(prompts))
            #choice_set <- choice_set[inds]
            prompts <- prompts[inds]
        }

        contents <- bigRadioButtons("answers", "", prompts, inds)
        withMathJax(HTML(contents))
    })


    output$Feedback <- renderUI({
        if (nchar(feedback$message)==0) return(" ") # for the dependency
        if (instructor_chooses()) input$answers # for the dependency when instructor is browsing
        isolate({
            if (!is.null(input$answers)) message <- feedback$message
            else return(NULL)
            correct_sign <- ifelse(this_question$correct == input$answers, random_success(), random_regret())
            if (!instructor_chooses()) {
              shinyjs::hide("check_answer")
              shinyjs::show("nextQ")
            }
        })
        withMathJax(HTML(paste(correct_sign, message, sep=" ")))
    })

    instructor_chooses <- reactive({
      input$passcode %in% c("mouse-eats-corn")
    })

    observe({
        if (instructor_chooses()) {
          shinyjs::show("instructor_choice")
          shinyjs::show("show_success_code")
        } else {
          shinyjs::hide("instructor_choice")
          shinyjs::hide("show_success_code")
        }
    })

    observeEvent(input$topic_choice, {
        the_choices <- Qbank$Q %>%
          filter(topic == input$topic_choice) %>% .$qname %>% sort()

        updateSelectInput(session, "instructor_choice", choices = the_choices)
    })
}


# Run the application
shinyApp(ui = ui, server = server)
