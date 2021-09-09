library(shiny)
library(mosaic)
library(mosaicCalc)

makePlot <- function(knots, jitters, controls) {

}

# Define UI for application that draws a histogram
ui <- fluidPage(
    titlePanel("Interpolation explorer"),

    # Sidebar with a slider input for number of bins
    sidebarLayout(
        sidebarPanel(
            actionButton("newpoints", "Start again"),
            tags$hr(),
            selectInput("npoints", "# of knot points",
                        c(3, 5, 10, 15, 20), selected=5),
            tags$hr(),
            #numericInput("npoints", "# of knot points", value=5, min=3, max=20, step=1,),
            tags$hr(),
            tags$p("Interpolating function type"),
                 checkboxInput("linear", "linear interpolator", TRUE),
            checkboxInput("cubic", "cubic spline", FALSE),
            checkboxInput("global", "global polynomial", FALSE),
            actionButton("jitter", "Jitter"),
            radioButtons("exclude", "Exclude endpoints",
                         choices=0:6, inline=TRUE)
        ),

        # Show a plot of the generated distribution
        mainPanel(
           plotOutput("fplot", height = "200px"),
           plotOutput("dfplot", height = "200px"),
           plotOutput("ddfplot", height = "200px" )
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    jitterList <- reactiveValues(jitters=list())
    orig_funs <- reactiveValues()
    npoints <- reactive({
        as.numeric(input$npoints)
    })

    xrange <- reactive({
        x <- knot_locations()$x
        toss <- as.numeric(input$exclude)
        if (toss < 1) return(range(x))

        toss <- pmin(floor((length(x)/2))-1, toss)
        keepers <- (toss+1):(length(x) -toss)
        range(x[keepers])
    })

    knot_locations <- reactive({
        input$newpoints
        diffs <- pmax(0.5, abs(rexp(npoints(), rate=3)))
        tibble::tibble(
            x = cumsum(diffs),
            y = runif(length(diffs))
        )
    })

    observeEvent(input$newpoints, {
        # Eliminate the old jittered plots
        jitterList$jitters <- list()
    })
    new_jitter <- eventReactive(input$jitter, {
        Res <- knot_locations()
        set.seed(NULL)
        Res$y <- Res$y + rnorm(nrow(Res), 0, .05)
        Res
    })
    make_funs <- reactive({
        K <- knot_locations()
        orig_funs$linear <- mosaic::connector(y ~ x, data = K)
        orig_funs$cubic <- mosaic::spliner(y ~ x, data = K)
        orig_funs$global <-
            makeFun(lm(y ~ poly(x, npoints() - 1), data = K))
        orig_funs
    })
    make_jitter_funs <- observeEvent(input$jitter, {
        req(input$jitter > 0)
        J <- isolate(new_jitter())
        Previous <- reactiveValuesToList(isolate(jitterList))
        n <- length(Previous$jitters)
        res <- list()
        res$linear <- mosaic::connector(y ~ x, data = J)
        res$cubic <- mosaic::spliner(y ~ x, data = J)
        res$global <-
            makeFun(lm(y ~ poly(x, npoints() - 1), data = J))
        isolate(jitterList$jitters[[n+1]] <- res)
    })
    plot_points <- reactive({
        funs <- make_funs()
        K <- knot_locations()
        dom <- domain(x=extendrange(xrange(), f=0.005))
        P <- gf_point(y ~ x, data = K, size=3, alpha=0.75, color="green")
        if (input$linear) {
            P <- P %>%
                mosaicCalc::slice_plot(funs$linear(x) ~ x,
                                       npts = 500,
                                       size = 2, alpha = 0.5,
                                       color="gray", domain=dom)
        }
        if (input$cubic) {
            P <- P %>%
                mosaicCalc::slice_plot(funs$cubic(x) ~ x,
                                       npts = 500,
                                       size = 2, alpha = 0.5,
                                       color="dodgerblue", domain=dom)
        }
        if (input$global) {
            P <- P %>%
                mosaicCalc::slice_plot(funs$global(x) ~ x,
                                       npts = 500,
                                       size = 2, alpha = 0.5,
                                       color="orange", domain=dom)
        }

        P %>% gf_lims(x = xrange())
    })
    output$fplot <- renderPlot({
        P <- plot_points()
        Jitters <- jitterList$jitters
        if (length(Jitters) == 0) return(P)
        dom <- domain(x=extendrange(xrange(), f=0.02))
        for (k in 1:length(Jitters)) {
            if (input$linear) {
                P <- P %>%
                    mosaicCalc::slice_plot(Jitters[[k]]$linear(x) ~ x,
                                           npts=500,
                                           color="gray", alpha=.5, domain=dom)
            }
            if (input$cubic) {
                P <- P %>%
                    mosaicCalc::slice_plot(Jitters[[k]]$cubic(x) ~ x,
                                           npts=500,
                                           color="dodgerblue", alpha=.5, domain=dom)
            }
            if (input$global) {
                P <- P %>%
                    mosaicCalc::slice_plot(Jitters[[k]]$global(x) ~ x,
                                           npts=500,
                                           color="orange", alpha=.5, domain=dom)
            }
        }
        P %>% gf_lims(x = xrange()) %>%
            gf_vline(xintercept=~x, data = knot_locations(), alpha=0.4, color="green", size=2)

    })

    output$dfplot <- renderPlot({
        funs <- make_funs()
        K <- knot_locations()
        dom <- domain(x=extendrange(xrange(), f=0.02))
        dfuns <- list()
        dfuns$linear <- D(funs$linear(x) ~ x)
        dfuns$cubic <-  D(funs$cubic(x)  ~ x)
        dfuns$global <- D(funs$global(x) ~ x)
        P <- ggplot()
        if (input$linear) {
            P <- P %>%
                mosaicCalc::slice_plot(dfuns$linear(x) ~ x,
                                       size = 2, alpha = 0.5,
                                       color="gray", domain=dom, npts=500)
        }
        if (input$cubic) {
            P <- P %>%
                mosaicCalc::slice_plot(dfuns$cubic(x) ~ x,
                                       size = 2, alpha = 0.5,
                                       color="dodgerblue", domain=dom, npts=500)
        }
        if (input$global) {
            P <- P %>%
                mosaicCalc::slice_plot(dfuns$global(x) ~ x,
                                       size = 2, alpha = 0.5,
                                       color="orange", domain=dom, npts=500)
        }
        P %>%
            gf_lims(x = xrange()) %>%
            gf_vline(xintercept=~x, data = K,
                     alpha=0.4, size=2, color="green") %>%
            gf_labs(y="1st deriv. w.r.t. x")
    })

    output$ddfplot <- renderPlot({
        if (!(input$cubic || input$global)) return(NULL)
        funs <- make_funs()
        K <- knot_locations()
        dom <- domain(x=extendrange(xrange(), f=0.02))
        ddfuns <- list()
        ddfuns$linear <- D(funs$linear(x) ~ x & x)
        ddfuns$cubic <-  D(funs$cubic(x)  ~ x & x)
        ddfuns$global <- D(funs$global(x) ~ x & x)
        P <- ggplot()
        # if (input$linear) {
        #     P <- P %>%
        #         mosaicCalc::slice_plot(ddfuns$linear(x) ~ x,
        #                                color="gray", domain=dom, npts=500)
        # }
        if (input$cubic) {
            P <- P %>%
                mosaicCalc::slice_plot(ddfuns$cubic(x) ~ x,
                                       size = 2, alpha = 0.5,
                                       color="dodgerblue", domain=dom, npts=500)
        }
        if (input$global) {
            P <- P %>%
                mosaicCalc::slice_plot(ddfuns$global(x) ~ x,
                                       size = 2, alpha = 0.5,
                                       color="orange", domain=dom, npts=500)
        }
        P %>%
            gf_lims(x = xrange()) %>%
            gf_vline(xintercept=~x, data = K,
                     alpha=0.4, size=2, color="green") %>%
            gf_labs(y="2nd deriv. w.r.t. x")

    })
}

# Run the application
shinyApp(ui = ui, server = server)
