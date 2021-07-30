library(shiny)
library(mosaic)
library(mosaicCalc)
library(math141Z)

ui <- fluidPage(
  h2("Flows, eigenvalues, and eigenvectors"),
  p("Click in the left graph to set a & b in the [ab10] matrix.
  The eigenvalues are shown below the graph."),
  p("Click in the right graph to set the initial conditions for the trajectory."),
  p("Each [ab10] value corresponds to one shape of flow, shown in the
    right graph. The black glyphs  follow streamlines of the flow over
    the same time span. The red glyph is also a streamline, but you can click
    to start it wherever you want and you can adjust the time span."),
  p("The thin green line runs from (0,0) to the starting point of the red
    streamline. When the green line aligns exactly with the red streamline,
    you have found an eigenvector."),
    splitLayout(
      cellWidths = c("45%", "55%"),
      div(
        math141Z::abcdUI("num1")
      ),
      div(
        plotOutput("flowplot", click="where"),
        uiOutput("orientation"),
        span(
          radioButtons("traj_steps", "time steps", choices = c(5, 10, 25, 100, 250, 1000), selected = 10, inline = TRUE,
                       width = NULL),
          style="color: red;")
           )
    )
)

server <- function(input, output, session) {
  the_matrix <- abcdServer("num1")
  clickx <- reactiveVal()
  clicky <- reactiveVal()
  clickx(1)
  clicky(0)
  observeEvent(input$where, {
    # If the clicked point is near the eigenvector put it exactly on
    # the eigenvector
    # Get the eigenvalues, vector will be (lambda, 1)
    isolate(clicky(input$where$y))
    eigvalues <- eigen_stuff()$values
    ratio <- input$where$x / input$where$y
    if (abs(ratio - eigvalues[1]) < 0.03) {
      isolate(clickx(eigvalues[1] * input$where$y))
    } else if (abs(ratio - eigvalues[2]) < 0.03) {
      isolate(clickx(eigvalues[2] * input$where$y))
    } else {
      isolate(clickx(input$where$x))
    }
  })


  eigen_stuff <- reactive({
      eigen(the_matrix())
  })
  traj <- reactive({
      req(the_matrix())
      x <- y <- numeric(input$traj_steps)
      x[1] <- clickx()
      y[1] <- clicky()
      for (k in 2:length(x)) {
          dx = 0.05*dx()(x = x[k-1], y[k-1])
          dy = 0.05*dy()(x = x[k-1], y[k-1])
          x[k] <- x[k-1] + dx
          y[k] <- y[k-1] + dy
      }

      tibble(x=x, y=y,
             thickness = (1:length(x)/(0.2*length(x))),
             alpha = 0.2 + thickness, size = 0.6*thickness,
             lineend = "round"
             ) %>%
        mutate(size = ifelse(size < 1, 1, size))

  })

  dx <- reactive({
      mosaic::makeFun(a*x + b*y ~ x + y, a=the_matrix()[1,1], b = the_matrix()[1,2])
  })
  dy <- reactive({
      mosaic::makeFun(c*x + d*y ~ x + y, c=the_matrix()[2,1], d = the_matrix()[2,2])
  })

  output$orientation <- renderUI({
    ang <- traj_ang_start()
    vec_string <- paste0("$$\\left(\\begin{array}{c}",
                         signif(1/tan(ang),4), "\\\\",
                         1, "\\end{array}\\right)$$")
    withMathJax(
      HTML(
        glue::glue("<div style='color: green; width: 100%; text-align: center;'>
                   Vector along green line: {vec_string}Orientation
                   {round(180*ang/pi, 1)}°</div>")

    ))
  })
  output$flowplot <- renderPlot({
      P <- make_flowplot()

      P %>%
         gf_path(y ~ x, data = traj(), inherit = FALSE, color="orange3",
                 alpha = ~ alpha, size = ~ size) %>%
        gf_abline(intercept=0,
                  slope = tan(traj_ang_start()),
                  color="green") %>%
        gf_point(0 ~ 0, color="green") %>%
        gf_refine(coord_fixed(xlim=c(-2,2), ylim=c(-2,2), expand=FALSE, clip="off")) %>%
        gf_theme(theme_void())

  })
  traj_ang_start <- reactive({
    atan2(clicky(), clickx())
  })
  make_flowplot <- eventReactive(list(input$nsteps, the_matrix()),{
      E <- eigen_stuff()
      if (!is.complex(E$values)) {
          # Construct the eigenvectors
          one <- E$vectors[,1]
          two <- E$vectors[,2]

          # make them unit length x 3
          one <- 3*one / sqrt(sum(one^2))
          two <- 3*two / sqrt(sum(two^2))
          Vecs <- tibble::tibble(
              xend = c(one[1], two[1]),
              yend = c(one[2], two[2]),
              x = -xend, y = -yend
          )
      } else {
          Vecs <- tibble(x=c(0,0), y=0, xend=0, yend=0)
      }
      math141Z::streamlines(list(dx(), dy()), nsteps = 10,
                            domain(x = c(-2.5, 2.5), y = c(-2.5, 2.5)),
                            n = 10) %>%
          # gf_segment(y + yend ~ x + xend, data = Vecs, inherit=FALSE,
          #            color = c("green", "dodgerblue"), alpha = 0.4, size = 2) %>%
          gf_refine(coord_fixed())


  })
}
# Run the application
shinyApp(ui = ui, server = server)
