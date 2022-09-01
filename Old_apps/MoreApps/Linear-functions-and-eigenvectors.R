library(shiny)
library(ggformula)

ui <- fluidPage(

    # Application title
    titlePanel("Linear functions and eigenvectors"),

    sidebarLayout(
        sidebarPanel(
            sliderInput("aparam",
                        "Parameter a",
                        min = -1,
                        max = 1,
                        value = 0, step=0.01),
            sliderInput("bparam",
                        "Parameter b",
                        min = -1,
                        max = 1,
                        value = 0, step=0.01),
            sliderInput("cparam",
                        "Parameter c",
                        min = -1,
                        max = 1,
                        value = 0, step=0.01),
            sliderInput("dparam",
                        "Parameter d",
                        min = -1,
                        max = 1,
                        value = 0, step=0.01),
            tags$hr(),
            checkboxInput("zoom", "Zoom in", value=FALSE),
            sliderInput("spread", "Angle spread",
                        min = 0,
                        max = 180,
                        step = 5,
                        value = 15),
            p("Eigenvalues:"),
            textOutput("eigen1"),
            textOutput("eigen2")

        ),

        # Show a plot of the generated distribution
        mainPanel(
            plotOutput("eigenplot", click = "where")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {
    clickx <- reactiveVal()
    clicky <- reactiveVal()
    clickx(1)
    clicky(0)

    spread_angle <- reactive({
        pmax(0.02, input$spread *pi / 180)
    })

    xout <- reactive({
        mosaic::makeFun(a*x + b*y ~ x + y, a=input$aparam, b = input$bparam)
    })
    yout <- reactive({
        mosaic::makeFun(c*x + d*y ~ x + y, c=input$cparam, d = input$dparam)
    })
    observeEvent(input$where, {
        isolate(clickx(input$where$x))
        isolate(clicky(input$where$y))
    })
    output$eigenplot <- renderPlot({
        P <- make_eigenplot()
        center_angle <- atan2(clicky(), clickx())
        angles <- center_angle + seq(-spread_angle(), spread_angle(), length=50)
        Dots <- tibble::tibble(
            x = cos(angles),
            y = sin(angles),
            x2 = xout()(x, y),
            y2 = yout()(x, y),
            color = hcl.colors(length(x), palette = "Dark2")
        )
        # typical_dist <- mean(sqrt(Dots$x2^2 + Dots$y2^2))
        # Dots$x3 <- Dots$x2 / typical_dist
        # Dots$y3 <- Dots$y2 / typical_dist



        P <- gf_point(y ~ x, data = Dots, alpha = .6,
                      color = ~color, shape="o", size=5, inherit=FALSE) %>%
          gf_point(y2 ~ x2, data = Dots, alpha = .6, color = ~color, inherit=FALSE) %>%
          gf_segment(y + y2 ~ x + x2, data = Dots, alpha = 0.25, color=~color) %>%
            # gf_point(y3 ~ x3, data = Dots, color = ~color, inherit=FALSE) %>%
          gf_refine(coord_fixed(), scale_color_identity())

        E <- eigen_stuff()
        if (!is.complex(E$values)) {
            # Construct the eigenvectors
            one <- E$vectors[,1]
            two <- E$vectors[,2]
            slope_one <- one[2] / one[1]
            slope_two <- two[2] / two[1]
            cat("Slopes are", slope_one, "and", slope_two, "\n")
            if (abs(slope_one) != Inf) {
                P <- P %>%
                    gf_abline(intercept = 0, slope = slope_one,
                              alpha = 0.2, size=2)
            } else {
                P <- P %>%
                    gf_vline(xintercept =0,alpha = 0.2, size=2 )
            }
            if (abs(slope_two) != Inf) {
                P <- P %>%
                    gf_abline(intercept = 0, slope = slope_two, alpha = 0.2, size=2)
            } else {
                P <- P %>%
                    gf_vline(xintercept =0,alpha = 0.2, size=2 )
            }
        }
        Circle <- tibble::tibble(
            angle = seq(0, 2*pi, length = 360),
            x = cos(angle),
            y = sin(angle)
        )

        if (input$zoom) {
          P %>%
            gf_theme(theme_minimal())
        } else {
          P %>%
            gf_path(y ~ x, data = Circle, alpha = 0.3, inherit = FALSE) %>%
                gf_theme(theme_minimal())
        }
    })

    make_eigenplot <- eventReactive(the_matrix(),{
        E <- eigen_stuff()
        if (!is.complex(E$values)) {
            # Construct the eigenvectors
            one <- E$vectors[,1]
            two <- E$vectors[,2]

            # make them unit length
            one <- one / sqrt(sum(one^2))
            two <- two / sqrt(sum(two^2))
            Vecs <- tibble::tibble(
                xend = c(one[1], two[1]),
                yend = c(one[2], two[2]),
                x = -xend, y = -yend
            )
        } else {
            Vecs <- tibble(x=c(0,0), y=0, xend=0, yend=0)
        }
        gf_segment(y + yend ~ x + xend, data = Vecs, inherit=FALSE,
                   color = c("green", "blue"), alpha = 0.2, size = 2)


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
}

# Run the application
shinyApp(ui = ui, server = server)
