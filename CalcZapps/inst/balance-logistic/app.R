library(shiny)
library(mosaic)
library(mosaicCalc)
library(math141Z)
library(dplyr)

omega_slider <- sliderInput("p1", "angular frequency 𝜔", min=0, max=2, step=0.01, value=1)
omega2_slider <- sliderInput("p2", "angular frequency 𝜔", min=0.3, max=2, step=0.01, value=1)
k_slider <- sliderInput("p1", "exponential parameter k", min=-1, max=1.2, step=0.01, value=0.25)
n_slider <- sliderInput("p1", "Power-law exponent n", min=-2, max=2, step=0.1, value=0.5)
center_slider <- sliderInput("p1", "Sigmoid center", min = 0, max = 10, step=0.1, value=5)
width_slider <- sliderInput("p2", "Sigmoid width", min = 0.1, max=8, step=0.1, value=2)

sigmoid <- function(t, center, width) {
    pnorm(t, mean=center, sd = width)
}

# set up the list of candidate ansatze
ansatz_candidates <- tibble::tribble(
    ~ fun, ~ control1, ~ control2,
    function(t, 𝜔, p2) sin(𝜔 * t), omega_slider, NA, # replace p1 and p2 with the actual names
    function(t, k, p2) exp(k * t), k_slider, NA,
    function(t, k, 𝜔) exp(k * t) * sin(𝜔 * t), k_slider, omega2_slider,
    function(t, center, width) sigmoid(t, center, width), center_slider, width_slider,
    function(t, n, p2) (t^n), n_slider, NA,
    function(t, n, 𝜔) (t^n) * sin(𝜔 * t), n_slider, omega2_slider,
) %>% mutate(display = unlist(sapply(fun, function(x) deparse(body(x)))),
             n = row_number())
ansatz_choices <- as.list(ansatz_candidates$n)
names(ansatz_choices) <- ansatz_candidates$display

ui <- fluidPage(

    # Application title
    titlePanel("Balancing limited growth functions"),

    # Sidebar with a slider input for number of bins
    sidebarLayout(
        sidebarPanel(
          "Solve",
          p(HTML("<span style='font-size: 30px;'><span style='color: red'>ẋ</span> = <span style='color: green'>r x (1-x)</span></span>")),
          p("where"),
          splitLayout(
              numericInput("alpha_val", "r = ",
                       min=-1, max=1, step=0.25, value=-0.25,
                       width="85px") #,
              # numericInput("b_val", "b = ",
              #              min=-1, max=1, step=0.25, value=-0.25,
              #              width="85px")
          ),
          tags$hr(),
          selectInput("ansatz", "Ansatz form", ansatz_choices),
          htmlOutput("param1"),
          htmlOutput("param2")
        ),

        # Show a plot of the generated distribution
        mainPanel(
          p(HTML("<span style='color: blue; font-size: 30px;'>Ansatz function: x(t)</span>")),
          plotOutput("ansatz_plot", height="200px"),
          p(HTML("Functions to balance: <span style='color: red; font-size: 30px;'>ẋ</span> and <span style='color: green; font-size: 30px;'>r x(t) (1-x(t))</span>")),
          plotOutput("fun_plot", height="200px"),
          htmlOutput("status")
        )
    )
)

server <- function(input, output, server) {
    success <- reactiveVal(FALSE)
    output$param1 <- renderUI({
        req(input$ansatz)
        control <- ansatz_candidates$control1[[as.numeric(input$ansatz)]]

        if (!inherits(control, "shiny.tag")) NULL
        control
        })
    output$param2 <- renderUI({
        req(input$ansatz)
        control <- ansatz_candidates$control2[[as.numeric(input$ansatz)]]

        if (!inherits(control, "shiny.tag")) NULL
        else control
        })

    get_ansatz <- reactive({
        ansatz_candidates$fun[[as.numeric(input$ansatz)]]
    })
    parameterized_ansatz <- reactive({
        fun <- get_ansatz()
        function(t) fun(t, p1(), p2())
    })
    observe({
        f <- get_ansatz()
    })
    ## parameters
    p1 <- reactive({
        req(input$p1)
        input$p1
    })
    p2 <- reactive({
        req(input$p2)
        input$p2
    })


    points_to_plot <- reactive({
        req(input$ansatz)
        req(input$p1)
        fun <- parameterized_ansatz()
        dfun <- D(fun(t) ~ t)
        Res <- tibble::tibble(
            t = seq(0, 10, length=200),
            x = fun(t),
            x_scaled = input$alpha_val * x * (1- x),
            dx = dfun(t)
        )
        # are the functions balanced
        success((max(abs(Res$dx - Res$x_scaled)) < (abs(input$alpha_val)*0.02)))
        Res
    })
    output$fun_plot <- renderPlot({
        Pts <- points_to_plot()
        gf_path(x_scaled ~ t, data = Pts,
                color="green", size=2, alpha = 0.5) %>%
            gf_path(dx ~ t, color = "red") %>%
            gf_labs(y = "Red function & green function")
    })
    output$ansatz_plot <- renderPlot({
        Pts <- points_to_plot()
        gf_path(x ~ t, data = Pts, color="blue")
    })
    output$status <- renderText({
        message <-
          if (success())
            "<span style='color: green; font-size: 30px;'>Good enough!</span>"
          else
            "<span style='color: red; font-size: 20px;'>Functions are not balanced.</span>"

    })

}

# Run the application
shinyApp(ui = ui, server = server)
