
library(shiny)
library(mosaic)
library(mosaicCalc)
library(ggplot2)
Quadratic_approx <- function(g, x_ref, y_ref) {
    dx <- D(g(x,y) ~ x)
    dy <- D(g(x,y) ~ y)
    dxy <- D(dx(x,y) ~ y)
    dxx <- D(g(x,y) ~ x + x)
    dyy <- D(g(x,y) ~ y + y)
    x0  <- x_ref
    y0  <- y_ref
    function(x,y) {
        g(x0, y0) +
            dx(x=x0,y=y0)*(x-x0) +
            dy(x=x0, y=y0)*(y-y0) +
            dxy(x=x0, y=y0)*(x-x0)*(y-y0) +
            dxx(x=x0, y=y0)*(x-x0)^2 +
            dyy(x=x0, y=y0)*(y-y0)^2
    }
}

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Optimization of functions of two variables"),

    # Sidebar with a slider input for number of bins
    sidebarLayout(
        sidebarPanel(
            actionButton("restart", "Start again with a new function"),
            checkboxInput("show_fun", "Use training wheels."),
            radioButtons("hint_level", "Method:",
                         choices = c(
                             "1) Function value" = 1,
                             "2) Follow gradient"  = 2,
                             "3) Zeros of partial x" = 3,
                             "4) Zeros of partial y" = 4,
                             "5) Zeros of partial x and partial y" = 5,
                             "6) Local quadratic" = 6,
                             "7) Local quad + zeros of partials" = 7
                         )
            )

        ),

        mainPanel(
            plotOutput("graphics", click=clickOpts("xstar"), height="600px")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {
    our_fun <- reactiveVal(rfun(~ x + y))
    guesses_x <- reactiveVal(numeric(0))
    guesses_y <- reactiveVal(numeric(0))
    dx_fun  <- reactiveVal(NA)
    dy_fun <- reactiveVal(NA)


    observeEvent(input$xstar, {
        guesses_x(c(guesses_x(), input$xstar$x))
        guesses_y(c(guesses_y(), input$xstar$y))
    })

    observeEvent(
        input$restart,
        {
            fun <- rfun(~ x + y)
            our_fun(fun)
            dx_fun(D(our_fun()(x,y) ~ x))
            dy_fun(D(our_fun()(x,y) ~ y))
            guesses_x(numeric(0))
            guesses_y(numeric(0))
        },
        ignoreNULL = FALSE,
        ignoreInit = FALSE)

    output$graphics <- renderPlot({
        Guesses <- data.frame(x = guesses_x(), y = guesses_y())
        Guesses$val <- with(Guesses, our_fun()(x=x, y=y))
        Guesses$label <- as.character(round(Guesses$val,2))

        P <- ggplot(data = Guesses, mapping = aes(x=x, y=y)) +
            lims(x=c(-5,5), y=c(-5,5))

        if (input$show_fun) {
            P <- contour_plot(our_fun()(x=x, y=y) ~ x + y,
                              domain(x=c(-5,5), y=c(-5,5)),
                              filled = TRUE)
        } else {
            P <- P + geom_blank()
        }


        if (input$hint_level %in% c(3,5,7)) {
            P <- P %>% contour_plot(dy_fun()(x=x, y=y) ~ x + y,
                                    domain(x=c(-5,5), y=c(-5,5)),
                                    contours_at =0, filled = FALSE,
                                    contour_color = "red")
        }
        if (input$hint_level %in% c(4,5,7)) {
             P <- P %>%  contour_plot(dx_fun()(x=x, y=y) ~ x + y,
                             domain(x=c(-5,5), y=c(-5,5)),
                             contours_at =0, filled = FALSE,
                             contour_color = "blue")
        }


        if (nrow(Guesses) > 0) {
            P <- P + geom_text(data = Guesses, aes(x=x,y=y,label=label), hjust=1, vjust=1) +
                geom_point(aes(x=x, y=y), data = Guesses)
        }

        if (length(guesses_x()) > 0) {
            xx <- guesses_x()[length(guesses_x())]
            yy <- guesses_y()[length(guesses_x())]
            angle <- atan2(dy_fun()(x=xx,y=yy), dx_fun()(x=xx, y=yy))

            if (input$hint_level >= 2) {
                P <- P + geom_segment(aes(x=xx, y=yy, xend = xx+cos(angle), yend=yy+sin(angle)), arrow = arrow())
            }

            if (input$hint_level %in% c(6,7)) {
                quad_approx <- Quadratic_approx(our_fun(), xx, yy)
                P <- P %>% contour_plot(quad_approx(x=x, y=y) ~ x + y,
                                        domain(x=c(-5,5), y=c(-5,5)),
                                        filled = FALSE,
                                        contour_color = "orange")
            }
        }

        P + coord_fixed()
    })


}

# Run the application
shinyApp(ui = ui, server = server)
