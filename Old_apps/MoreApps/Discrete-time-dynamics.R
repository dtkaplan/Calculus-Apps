
library(shiny)
library(ggformula)
library(ggplot2)

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Stepping through discrete-time dynamics"),

    # Sidebar with a slider input for number of bins
    sidebarLayout(
        sidebarPanel(
                selectInput("mu","𝜇 parameter",
                                    choices=c(1, 2, 3, 3.5, 3.7,4.0)),
                htmlOutput("stats")
        ),


        # Show a plot of the generated distribution
        mainPanel(
           plotOutput("dynamics_plot",
                      brush=brushOpts("outline", resetOnNew=TRUE, fill=NA, stroke="red"))
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  mu <- reactive({as.numeric(input$mu)})
  draw <- reactive({
      Data <- tibble::tibble(
        x = seq(-.1, 1.1, length=121),
        y = mu()*x*(1-x)
      )
      gf_line(y ~ x, data = Data) %>%
          gf_abline(slope=1, intercept=0, color="blue") %>%
          gf_refine(coord_fixed(ratio = 1)) %>%
          gf_labs(x = "x[i]", y = "x[i+1]") %>%
          gf_lims(y = c(-.1, 1.1))


  })

  output$dynamics_plot <- renderPlot({
      draw() %>%
          gf_refine(
              scale_x_continuous(breaks = seq(-0.1,1.1,by=0.1),
                                 minor_breaks = seq(-0.1,1.1, by=0.05),
                                 limits=c(-.1, 1.1)),
              scale_y_continuous(breaks = seq(-0.1,1.1,by=0.1),
                                 minor_breaks = seq(-0.1,1.1, by=0.05),
                                 limits=c(-.1, 1.1))
          )
  })
  output$stats <- renderText({
      if (!is.null(input$outline))
        glue::glue("Left: {round(input$outline$xmin,2)}<br>
                   Right: {round(input$outline$xmax, 2)}<br>
                   Top: {round(input$outline$ymax,2)}<br>
                   Bottom: {round(input$outline$ymin,2)}")
  })

}

# Run the application
shinyApp(ui = ui, server = server)
