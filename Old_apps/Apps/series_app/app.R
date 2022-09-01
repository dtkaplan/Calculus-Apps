library(shiny)
library(ggplot2)

# Define UI for application that draws a histogram
ui <- fluidPage(
    titlePanel("Taylor Series"),

    # Sidebar with a slider input for number of bins
    sidebarLayout(
        sidebarPanel(
            radioButtons("f", label="Choose function", inline=TRUE,
                         choices = c("exp(x)", "sin(x)", "cos(x)", "log(1+x)")),
            sliderInput("order", "Take series to n=", min=0, max=15, step=1, value=2)
        ),
        # Show a plot of the generated distribution
        mainPanel(
            plotOutput("graphic")
        )
    )
)


server <- function(input, output, server) {
    xaxis <- seq(-3, 3, length=201)
    plotData <- tibble::tibble(x = xaxis,
                               `sin(x)` = sin(x),
                               `cos(x)` = cos(x),
                               `exp(x)` = exp(x),
                               `log(1+x)` = log(1+x)
    )
    funData <- reactive({
        A <- plotData[c("x", input$f)]
        names(A)[2] = "y"
        A
    })

    vandermonde <- reactive({
        outer(xaxis, 0:15, "^")
    })
    coefficients <- reactive({


        if (input$f == "log(1+x)") {
            res <- c(0, 1/(1:15)*rep_len(c(1,-1), length.out=15 ))
        } else {
            factorials <- cumprod(c(1, 1:15))
            res <- cbind(c(1/factorials))
            if (input$f == "sin(x)") {
                res <- res * c(rep_len(c(0, 1, 0, -1), length.out=16))
            } else if (input$f == "cos(x)") {
                res <- res * rep_len(c(1,0,-1,0), length.out=16)
            }
        }
        cbind(res)
    })
    power_series <- reactive({
        cat("input order", input$order)
        coef <- coefficients()[1:(input$order+1), ,drop=FALSE]
        M <- vandermonde()[, 1:(input$order+1), drop=FALSE]
        tibble::tibble(x = xaxis, y=c(M%*%coef))
    })

    output$graphic <- renderPlot({
        input$order # for dependencies
        input$f
        suppressWarnings(
            ggplot(funData(), aes(x=x, y=y)) + geom_line(alpha=0.3, size=2) +
                geom_line(data=power_series(), aes(x=x,y=y)) +
                ylim(-2, 5)
        )
    })

}

# Run the application
shinyApp(ui = ui, server = server)
