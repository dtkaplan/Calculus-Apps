
library(shiny)
library(ggplot2)


# Define UI for application that draws a histogram
ui <- fluidPage(
    withMathJax(),
    # Application title
    titlePanel("High-order polynomial model"),

    # Sidebar with a slider input for number of bins
    sidebarLayout(
        sidebarPanel(
            sliderInput("one", "$$\\mbox{Coefficient on}\\ x$$", min = 0, max = 20, value = 19.6, step=0.1),
            sliderInput("two", "$$\\mbox{Coefficient on}\\ x^2$$", min = 0, max = 75, value = 51.9, step=0.1),
            sliderInput("three", "$$\\mbox{Coefficient on}\\ x^3$$", min = -100, max = 0, value = -76.8, step=0.1),
            sliderInput("four", "Coefficient on x^4", min = -80, max = 0, value = -58.7, step=0.1),
            sliderInput("five", "Coefficient on x5", min = 0, max = 400, value = 280.5, step=0.1),
        ),

        mainPanel(
           plotOutput("energy_plot")
        )
    )
)

server <- function(input, output) {

    output$energy_plot <- renderPlot({
        x <-seq(-0.5, 0.5, length = 101)
        y <- 2.5 + input$one*x + input$two*x^2 + input$three*x^3 + input$four*x^4 + input$five*x^5
        ggplot(data = tibble(x, y), aes(x = x, y = y)) + geom_line() +
            geom_hline(yintercept = 2.5, color = "red")
    })
}

# Run the application
shinyApp(ui = ui, server = server)
