
library(shiny)
library(mosaic)
library(mosaicCalc)
library(math141Z)

ui <- fluidPage(

    # Application title
    titlePanel("Linear continuous dynamics"),

    sidebarLayout(
        sidebarPanel(
            sliderInput("aparam",
                        "Parameter a",
                        min = -1,
                        max = 1,
                        value = 0.5, step=0.01),
            sliderInput("bparam",
                        "Parameter b",
                        min = -1,
                        max = 1,
                        value = 0.68, step=0.01),
            sliderInput("cparam",
                        "Parameter c",
                        min = -1,
                        max = 1,
                        value = 0.24, step=0.01),
            sliderInput("dparam",
                        "Parameter d",
                        min = -1,
                        max = 1,
                        value = 0.5, step=0.01),
            p("Eigenvalues:"),
            textOutput("eigen1"),
            textOutput("eigen2"),
            sliderInput("nsteps",
                        "# of Euler steps for flowfield",
                        min = 5, max=50, step=1, value = 10),
            sliderInput("traj_steps", "Duration of trajectory", min=10, max=2000,step=25, value=100)
        ),

        # Show a plot of the generated distribution
        mainPanel(
           plotOutput("flowplot", click = "where")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {
    clickx <- reactiveVal()
    clicky <- reactiveVal()
    clickx(1)
    clicky(0)

    dx <- reactive({
        mosaic::makeFun(a*x + b*y ~ x + y, a=input$aparam, b = input$bparam)
    })
    dy <- reactive({
        mosaic::makeFun(c*x + d*y ~ x + y, c=input$cparam, d = input$dparam)
    })
    observeEvent(input$where, {
        isolate(clickx(input$where$x))
        isolate(clicky(input$where$y))
    })
    output$flowplot <- renderPlot({
        P <- make_flowplot()

        P %>%
         gf_path(y ~ x, data = traj(), inherit = FALSE, color="red")

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
        math141Z::streamlines(list(dx(), dy()), nsteps = input$nsteps,
                             domain(x = c(-3, 3), y = c(-3, 3)),
                             n = 10) %>%
            gf_segment(y + yend ~ x + xend, data = Vecs, inherit=FALSE,
                       color = c("green", "blue"), alpha = 0.4, size = 2) %>%
            gf_refine(coord_fixed())


    })
    the_matrix <- reactive({
        req(input$aparam)
        req(input$bparam)
        req(input$cparam)
        req(input$dparam)
        matrix(c(input$aparam, input$bparam,
                 input$cparam, input$dparam),
               nrow = 2, byrow = TRUE)
    })
    eigen_stuff <- reactive({
        eigen(the_matrix())
    })
    output$eigen1 <- renderText({
        round(eigen_stuff()$values[1], 2)
    })
    output$eigen2 <- renderText({
        round(eigen_stuff()$values[2], 2)
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

        tibble(x=x, y=y)

    })
}

# Run the application
shinyApp(ui = ui, server = server)
