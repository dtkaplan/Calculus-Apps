library(shiny)
library(mosaic)
library(mosaicCalc)

ui <- fluidPage(

    # Application title
    titlePanel("Differential Eq. in 1 variable"),


    sidebarLayout(
        sidebarPanel(
            withMathJax(
                textInput("dynamics_eq", "$$\\partial_t x(t) \\equiv f(x(t))$$",
                       placeholder = "f(x) formula goes here")
                ),
            checkboxInput("flow", "Show flow", FALSE)
        ),

        mainPanel(
            plotOutput("DE", click = "state")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    dynamics <- reactive({
        if (input$dynamics_eq == "")
            return(mosaic::makeFun(2*x*(1-x) ~ x))

        # if it's a number, return some random dynamics
        if (grepl("^[0-9]*$", input$dynamics_eq)) {
            fraw <- rfun(~ x, seed=as.numeric(input$dynamics_eq))
            vals <- range(fraw(seq(-5, 5, by=.01)))
            f <- function(x) {
                (fraw(6*(x-.5)) - mean(vals))/diff(vals)
            }
            return(f)
        }

        isolate(traj_x(numeric(0)))
        form <- as.formula(paste(input$dynamics_eq, "~ yyzzyy"))
        vars <- all.vars(form)
        if (length(vars) > 2) stop("Use only 1 dynamical variable")
        if (length(vars) <= 1) stop("Must use a dynamical variable")
        dyvar <- setdiff(vars, "yyzzyy")
        form <- as.formula(paste(input$dynamics_eq, "~", dyvar))
        mosaic::makeFun(form)

    })


    where <- reactiveVal(.25)

    observe({
        if (!is.null(input$state$x))
            where(input$state$x)
    })

    output$DE <- renderPlot({
        suppressWarnings(make_plot())
    })

    make_plot <- reactive({
        state <- where()
        f <- dynamics()
        endarrow <- state + f(state)*0.5
        Flow <- tibble::tibble(
            x = seq(-.12, 1.2, by=0.1),
            speed = f(x),
            xend = x + 0.5*speed,
            asize = abs(speed),
            arrow_size = 3*asize/max(asize),
            y=0)


        redarrowsize = 3*abs(f(state))/max(abs(Flow$speed))
        P <- slice_plot(f(x) ~ x, domain(x = c(-0.25, 1.25))) %>%
            gf_hline(yintercept = 0, color = "blue") %>%
            gf_segment(0 + 0 ~ state + endarrow, inherit = FALSE, color = "red",
                       size=3, alpha = 0.5, arrow=grid::arrow()) %>%
            gf_point(0 ~ state, color="blue", inherit=FALSE, size=3, fill=NULL) %>%
            gf_labs(x = "State x", y = "Change of state dx/dt",
                    title="\"Logistic\" Dynamics. State shown in blue.")

        if (input$flow) {
            P %>% gf_segment(y + y ~ x + xend, data = Flow, inherit=FALSE,
                             color="blue", size = ~arrow_size, alpha = 0.3,
                             arrow = arrow(length = unit(Flow$arrow_size, "mm"))) %>% gf_refine(scale_size_identity())
        } else {
            P
        }

    })
}

# Run the application
shinyApp(ui = ui, server = server)
