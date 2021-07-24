library(shiny)
library(dplyr)
library(shinyjs)
library(CalcZapps)

# Success policy: 17 out of 20
nright <- 8
ntotal <- 10

test_file <- system.file("Zdrill/www/text.Rmd", package="CalcZapps")
source_files <- c(
    # "https://raw.githubusercontent.com/dtkaplan/Zdrill/main/Block_1.Rmd",
    test_file
)
Qbank <- CalcZapps::readQfiles(source_files)
Qbank_topics <- unique(Qbank$Q$topic)

# an experiment to see if I can get markdown to render as HTML.
md2html <- function(s) {
    backtick <- "`([^`]*)`"
    dollar   <- "\\${1}([^\\$]*)\\${1}"
    bold     <- "\\*{2}([^\\*]*)\\*{2}"
    italics  <- "\\*{1}([^\\*]*)\\*{1}"
    s %>%
        gsub(backtick, "<code>\\1</code>", .) %>%
        gsub(dollar, "\\(\\\\\\1\\\\)", .) %>%
        gsub(bold, "<strong>\\1</strong>", .) %>%
        gsub(italics, "<em>\\1</em>", .)
}

ui <- fluidPage(

    # Application title
    titlePanel("Can I make a radiobutton table?"),

    # Sidebar with a slider input for number of bins
    sidebarLayout(
        sidebarPanel(
            selectInput("topic_choice", "Topic:", Qbank_topics ),
            actionButton("startover", "Start Over"),
            tags$hr(),
            textOutput("question_name")
        ),

        # Show a plot of the generated distribution
        mainPanel(
            div(uiOutput("Qprompt")),
            uiOutput("choiceHolder"),
            tags$hr(),
            uiOutput("Feedback"),
            actionButton("check_answer", "Check answer"),
            actionButton("nextQ", "Next question"),
            tags$hr(),
            textOutput("score"),
            tags$hr(),
            textOutput("success_key")

        ),
        position = "right"
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  # output$choice1 <- renderUI({
  #     req(input$choices)
  #     withMathJax(p( HTML(glue::glue("Choice is {input$choices}.
  #                Will this work with plain text?<br><br>I hope so!<br>\\(x^{input$choices} + 4 x\\)"))
  #     ))
  # })
    output$choiceHolder <- renderUI({
        choices <- paste("Choice", 1:4, "\\(x^2 + \\sqrt{\\strut y}\\)")
        contents <- bigRadioButtons("the_choices", "Pick one of these, please.", choices)
        withMathJax(HTML(contents))
    })
    observeEvent(input$the_choices, {
        cat("Choice made: ", input$the_choices, "\n")
    })

}

# Run the application
shinyApp(ui = ui, server = server)
