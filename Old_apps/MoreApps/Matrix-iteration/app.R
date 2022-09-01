library(shiny)
library(mosaic)
library(math141Z)
library(dplyr)

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Matrix iteration"),

    # Sidebar with a slider input for number of bins
    sidebarLayout(
        sidebarPanel(
            tags$table(
                tags$th("The Matrix"),
                tags$tr(
                   tags$td(numericInput("avalue", label="", value=0.9,min=-2, max=2, step=0.01, width="85px")),
                   tags$td(numericInput("bvalue", label="", value=.05,min=-2, max=2, step=0.01, width="85px")),
                ),
                tags$tr(
                    tags$td(numericInput("cvalue", label="", value=.05,min=-2, max=2, step=0.01, width="85px")),
                    tags$td(numericInput("dvalue", label="", value=1.01,min=-2, max=2, step=0.01, width="85px")),
                )
            ),
            textOutput("eigenvalues"),
            tags$hr(),
            splitLayout(actionButton("clear", "Clear graph"),
                        actionButton("clear_last", "Undo")),
            p("  "),
            numericInput("nsteps", label="number of iterations:", value=30, min=2, max=100),
            p("Click in the top graph to start a trajectory from that (x,y) point. The trajectory
            will be plotted in that graph, while y versus n will be in the bottom graph. The colors
              mean nothing in terms of dynamics. They are there to help you associate each trajectory segment to it's
              y vs. n solution.")
        ),

        # Show a plot of the generated distribution
        mainPanel(
            plotOutput("space", click="initial"),
            plotOutput("timey", height="200px"),
            plotOutput("timex", height="200px")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    current_start <- reactiveVal(c(0,0))
    step_n <- reactive({
        if (is.null(input$nsteps) || is.na(input$nsteps)) 5
        else input$nsteps
    })
    observe({
        req(input$initial)
        where <- c(input$initial$x, input$initial$y)

        current_start(where)
    })
    observe({
        A() # for the dependency
        current_start(c(NA, NA))
        trajectory_store$L = list()
    })

    A <- reactive({
        res <- matrix(c(input$avalue, input$bvalue,
                        input$cvalue, input$dvalue), nrow=2, byrow=TRUE)
        res[is.na(res)] <- 0
        res
    })

    colors <- c("black", "blue", "green", "red", "orange",
                topo.colors(10), heat.colors(10), terrain.colors(10), rainbow(10))
    current_trajectory <- reactive({
        current_start()
        trajectory <- matrix(rep(0,2*(step_n()+1)), nrow=2)
        trajectory[, 1] <- current_start()
        for(k in 1:step_n()){
            trajectory[, k+1] <- A() %*% trajectory[, k]
        }

        Res <- data.frame(t(trajectory)) %>%
            rename(x = "X1", y = "X2") %>%
            mutate(n=row_number()-1)
        Res
    })
    trajectory_store <- reactiveValues(L = list())
    observeEvent(input$clear, {
        trajectory_store$L <- list()
    })
    observeEvent(input$clear_last, {
        n <- length(trajectory_store$L)
        if (n > 0) trajectory_store$L[n] <- NULL
    })
    observeEvent(current_trajectory(), { # update the store
        req(current_trajectory())
        n <- length(trajectory_store$L) + 1
        shade <- colors[n]
        trajectory_store$L[[n]] <-
            current_trajectory() %>%
            dplyr::mutate(path = rnorm(1), hue = shade)
    })

    time_series_plot_y <- reactive({
        if (length(trajectory_store$L) == 0) {
            Dat <- data.frame(n=1:20, y=-2:2)
            return(ggplot(Dat, aes(x=n, y=y)) + geom_blank())
        }
        suppressWarnings(
            gf_point(y ~ n, data=bind_rows(trajectory_store$L),
                     color= ~ hue, group=~path, alpha = ~ 1.1 - sqrt(1/n)) %>%
                gf_path(y ~ n, group=~path) %>%
                gf_refine(scale_color_identity()) %>%
                gf_theme(theme(legend.position = "none")) %>%
                gf_labs(title = "y versus n")
        )
    })

    time_series_plot_x <- reactive({
        if (length(trajectory_store$L) == 0) {
            Dat <- data.frame(n=1:20, y=-2:2)
            return(ggplot(Dat, aes(x=n, y=x)) + geom_blank())
        }
        suppressWarnings(
            gf_point(x ~ n, data=bind_rows(trajectory_store$L),
                     color= ~ hue, group=~path, alpha = ~ 1.1 - sqrt(1/n)) %>%
                gf_path(x ~ n, group=~path) %>%
                gf_refine(scale_color_identity()) %>%
                gf_theme(theme(legend.position = "none")) %>%
                gf_labs(title = "x versus n")
        )
    })

    trajectory_plot <- reactive({
        if (length(trajectory_store$L) == 0) {
            Dat <- data.frame(x=-2:2, y=-2:2)
            return(ggplot(Dat, aes(x=x, y=y)) + geom_blank() + coord_fixed())
        }

        suppressWarnings(
            gf_point(y ~ x, data=bind_rows(trajectory_store$L),
                     color= ~ hue, group=~path, alpha = ~ 1.1 - sqrt(1/n)) %>%
                #gf_text(y ~ x, label=~n) %>%
                gf_path(y ~ x, group=~path) %>%
                gf_refine(scale_color_identity(),
                          coord_fixed(xlim=c(-2, 2), ylim=c(-2, 2), clip="on")) %>%
                gf_theme(theme(legend.position = "none"))
        )
    })

    observe({

    })
    output$space <- renderPlot({
        trajectory_plot()
    })

    output$timey <- renderPlot({
        time_series_plot_y()
    })

    output$timex <- renderPlot({
        time_series_plot_x()
    })

    output$eigenvalues <- renderText({
        paste(paste(c("𝝺₁ =","𝝺₂ ="),
                    round(eigen(A())$values, 2)
                    ), collapse=",     ")
    })
}

# Run the application
shinyApp(ui = ui, server = server)
