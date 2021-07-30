
library(shiny)
library(mosaic)
library(mosaicCalc)
library(dplyr)
library(tibble)

# set up the list of functions
function_set <- tibble::tribble(
    ~ fun, ~ min, ~ max,
    function(x) -0.5*x, -10, 10,
    function(x) +0.5*x, -10, 10,
    function(x) -0.4*(x - 5), -2, 10,
    function(x) 2*x*(1-x/8), -2, 10,

) %>% mutate(display = unlist(sapply(fun, function(x) deparse(body(x)))),
             n = row_number())

fun_choices <- as.list(function_set$n)
names(fun_choices) <- paste("∂𝗍 x =", function_set$display)

ui <- fluidPage(

    # Application title
    titlePanel("Numerical solution to differential equations."),
    tags$p("Select one of the basic differential equation models provided.
    Pick an initial condition x_0.
    Then press 'step' to take a Euler step with dt=h.
    With many Euler steps, you have constructed the solution x(t)."),
    tags$p("The table shows the initial condition in the first row followed by the value of
           x at each successive time step."),
    tags$p("The top graph shows the function dynamics(x) versus x. The lower graph
           shows x(t) versus t."),
    tags$p("The blue line in the top graph shows the value of the instantaneous rate of change
           of x at the last step in the table. Read the value off the vertical axis."),
    tags$p("The corresponding blue line in the bottom graph shows that same rate of change.
           But since that graph is x(t) versus t, the rate of change corresponds to the slope
           of the blue line. The next Euler step will fall on that blue line. If the next step
           brings us to a different value of x, blue line will change in accordance with the
           dynamical function."),
    sidebarLayout(
        sidebarPanel(
            selectInput("dynamics_fun", "Dynamics",
                        choices = fun_choices
                        ),
            sliderInput("hstep",
                        "Size of h:",
                        min = 0.1,
                        max = 2,
                        step = 0.1,
                        value = 0.5),
            sliderInput("x0",
                        "Initial condition on x:",
                        min = -10, max=10, step=0.1, value=1),
            splitLayout(actionButton("take_step", "Step"),
                       actionButton("undo", "Undo"),
                       actionButton("clear", "Clear")),
            tableOutput("steps")
        ),


        mainPanel(
           plotOutput("dynamics"),
           plotOutput("solution")
        )
    )
)


server <- function(input, output) {
    nsteps <- reactiveVal(0)
    the_fun <- reactive({
        function_set[[as.numeric(input$dynamics_fun), "fun"]][[1]]
    })
    minx <- reactive({
        function_set[[as.numeric(input$dynamics_fun), "min"]]
    })
    maxx <- reactive({
        function_set[[as.numeric(input$dynamics_fun), "max"]]
    })
    observeEvent(input$take_step, {
        nsteps(nsteps() + 1)
    })
    observeEvent(input$undo, {
        nsteps(nsteps() - 1)
    })
    observeEvent(c(input$clear, input$dynamics_fun), {
        nsteps(0)
    })
    the_steps <- reactive({
        Res <- tibble::tibble(
            t = (0:nsteps()) * input$hstep,
            dt_x = 0*t,
            x = 0*t,
            step = 0*t)
        Res$x[1] <- input$x0
        Res$dt_x[1] <- the_fun()(Res$x[1])
        Res$step[1] <- Res$dt_x[1] * input$hstep
        if (nsteps() > 0) {
            for (k in 1:nsteps()) {
                Res$step[k+1] = Res$dt_x[k] * input$hstep
                Res$x[k+1] = Res$x[k] + Res$step[k+1]
                Res$dt_x[k+1] = the_fun()(Res$x[k+1])
            }
        }

        Res$step[1] <-  NA

        Res
    })
    output$steps <- renderTable({
        the_steps() %>% tail(10)
    })
    output$dynamics <- renderPlot({
        Traj <- the_steps()
       # dom <- range(Traj$x) + c(-1, 1)
        Dyn <- tibble::tibble(
            x = seq(minx(), maxx(), length = 300),
            dt_x = the_fun()(x)
        )
        Points <- Traj["x"] %>%
            mutate(dx = the_fun()(x),
                   n = row_number(),
                   alpha = .3 + n/length(n))
        current_slope = Points$dx[nrow(Points)]
        gf_path(dt_x ~ x, data = Dyn) %>%
            gf_hline(yintercept = 0, color="orange3") %>%
            gf_point(dx ~ x, data = Points, alpha = ~ alpha) %>%
            gf_hline(yintercept = current_slope, color="dodgerblue")

        })
    output$solution <- renderPlot({
        Traj <- the_steps()
        max_time <- 10
        Slope <- tibble::tibble(
            t = seq(Traj$t[nrow(Traj)], max_time),
            x = Traj$x[nrow(Traj)] +
                Traj$dt_x[nrow(Traj)]*(t - Traj$t[nrow(Traj)])
        )
        gf_point(x ~ t, data = Traj) %>%
            gf_path(x ~ t) %>%
            gf_path(x ~ t, data = Slope, color="dodgerblue") %>%
            gf_hline(yintercept = 0, color="orange3") %>%
            gf_lims(y = c(minx(), maxx()), x = c(0,max_time))
    })

}

# Run the application
shinyApp(ui = ui, server = server)
