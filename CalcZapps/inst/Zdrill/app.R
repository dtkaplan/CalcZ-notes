library(shiny)
library(shinydashboard)
library(dplyr)
library(shinyjs)

library(CalcZapps)

question_file <- system.file("Zdrill/www/text.Rmd", package="CalcZapps")
Qbank <- CalcZapps::readQfile(question_file)
Qbank_topics <- unique(Qbank$Q$topic)

menu1 <- dropdownMenu(type = "messages", badgeStatus = "success",
                      messageItem("Support Team",
                                  "This is the content of a message.",
                                  time = "5 mins"
                      ),
                      messageItem("Support Team",
                                  "This is the content of another message.",
                                  time = "2 hours"
                      ),
                      messageItem("New User",
                                  "Can I get some help?",
                                  time = "Today"
                      )
)

ui <- dashboardPage(
    dashboardHeader(menu1, title="CalcZ Practice"),
    dashboardSidebar(
        p("Hello"),
        disable = TRUE,
        useShinyjs()
    ),
    dashboardBody(
        fluidRow(
            column(width=8,
                   uiOutput("Qprompt"),
                   uiOutput("Choices"),
                   tags$hr(),
                   uiOutput("Feedback"),
                   actionButton("check_answer", "Check answer"),
                   actionButton("nextQ", "Next question"),
                   tags$hr(),
                   textOutput("score")
                   ),
            column(width=2,
                   selectInput("topic_choice", "Topic:", Qbank_topics ),
                   actionButton("startover", "Start Over")
                   )
        )
    )

)

server <- function(input, output) {
    State <- reactiveValues()
    this_question <- reactiveValues(valid = FALSE)
    feedback <- reactiveValues(message="")

    observeEvent(input$check_answer, {
      cat("Selected answer is", input$answers, "\n")
      if (is.null(input$answers)) feedback$message <- ""
      else feedback$message <- this_question$feedback[as.integer(input$answers)]
    })

    shinyjs::hide("nextQ")
    observeEvent(c(input$topic_choice, input$startover), {
      # Get the unique IDs, in random order of the questions matching the topic
      State$next_q <- 0
      State$q_ids <- sample(Qbank$Q %>% dplyr::filter(topic == input$topic_choice) %>% .$unique)
      State$n_correct <- 0
      State$n_answered <- 0
      next_question()
    })

    observeEvent(input$check_answer, {
      req(input$answers)
      State$n_answered <- State$n_answered + 1

      was_correct <- this_question$correct == as.numeric(input$answers)
      if (was_correct) {
        State$n_correct <- State$n_correct + 1
      } else {
        # Insert the question back into the queue
        this_id <- State$q_ids[State$next_q]
        n_qs <- length(State$q_ids)
        index <-  if (State$next_q == n_qs) {
            State$next_q
          } else {
          sample((State$next_q+1):n_qs, size=1)
          }
        cat("Current position : ", State$next_q, "Insertion point: ", index, "\n")
        if (index <= length(State$q_ids)) {
          tmp <- State$q_ids[index]
          State$q_ids[index] <- this_id
          State$q_ids[State$next_q] <- tmp
          State$next_q <- State$next_q - 1
        }
      }
    })

    observeEvent(input$nextQ, {
      shinyjs::show("check_answer")
      shinyjs::hide("nextQ")
      isolate(feedback$message <- "")
      next_question()
    })

    output$score <- renderText({
      paste(input$topic_choice, ": ", State$n_correct, "correct out of", State$n_answered, "attempts.")
    })

    next_question <- reactive({
        cat("Getting next question\n")
        input$nextQ # for the dependency
        input$topic_choice # ditto
        # Get the next item
        # shuffle as we go around the loop
        isolate({
          State$next_q <- State$next_q + 1
          if (State$next_q > length(State$q_ids)) {
            State$next_q <- 1
            State$q_ids <- sample(State$q_ids)
          }
        })
        k <- State$next_q

        # Fill in the next question
        this_question$valid <- TRUE
        prompt_info <- Qbank$Q %>% filter(unique == State$q_ids[k])
        this_question$prompt <- prompt_info$prompt
        choices <- Qbank$C %>% filter(question == State$q_ids[k])
        this_question$choices <- choices$prompt
        this_question$correct <- which(choices$correct == "+")
        this_question$feedback <- choices$feedback
        list(unique = prompt_info$unique, name = prompt_info$qname)

    })
  output$Qprompt <- renderUI({
    if (!this_question$valid) return(NULL)
    withMathJax(HTML(this_question$prompt))
    })
  output$Choices <- renderUI({
    if (!this_question$valid) return(NULL)
    prompts <- this_question$choices
    choice_set <- as.list(1:length(prompts))
    names(choice_set) <- prompts
    withMathJax(
      radioButtons("answers", "Choose one", choice_set, selected=character(0))
    )
  })
  output$Feedback <- renderUI({
    cat("Printing feedback\n")
    if (nchar(feedback$message)==0) return(" ") # for the dependency
    isolate({
      if (!is.null(input$answers)) message <- feedback$message
      else return(NULL)
      correct_sign <- ifelse(this_question$correct == input$answers, "+", random_regret())
      shinyjs::hide("check_answer")
      shinyjs::show("nextQ")
    })
    withMathJax(p(paste(correct_sign, message, sep=" ")))
  })
}

shinyApp(ui, server)
