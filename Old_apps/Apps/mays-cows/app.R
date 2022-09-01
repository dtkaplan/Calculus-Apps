library(shiny)
library(mosaicCalc)

ui <- fluidPage(

    # Application title
    titlePanel("Grazing Cows"),

    # Sidebar with a slider input for number of bins
    sidebarLayout(
        sidebarPanel(
            sliderInput("ncows",
                        "Number of cows:",
                        min = 0,
                        max = 50,
                        step = 1,
                        value = 0),
            checkboxInput("growth", "Show grass growth dynamics", value=FALSE),
            checkboxInput("consumption", "Show grass consumption", value=FALSE),
            checkboxInput("net", "Show net grass dynamics", value=TRUE)
        ),

        # Show a plot of the generated distribution
        mainPanel(
           plotOutput("grassPlot")
        )
    )
)


server <- function(input, output) {
    base_consumption <- makeFun(beta*v^2/(v_0^2 + v^2) ~ v, beta=0.1, v_0=3)
    grass_growth <- makeFun(r*v*(1-v/k) ~ v, k=25, r=1/3)
    consumption <- reactive({
        function(v) input$ncows * base_consumption(v)
    })
    net_growth <- reactive({
        function(v) grass_growth(v) - consumption()(v)
    })
    output$grassPlot <- renderPlot({
        P <- gf_hline(yintercept=0, color="red")
        dom <- domain(v=c(0, 25))
        if (input$growth) P <- P %>% slice_plot(grass_growth(v) ~ v, dom,
                                                color="green", size=2, alpha=0.4)
        if (input$consumption) P <- P %>% slice_plot(consumption()(v) ~ v, dom,
                                                     color="brown", size=2, alpha=0.4)
        if (input$net) P <- P %>% slice_plot(net_growth()(v) ~ v, dom,
                                             color="blue")

        P %>% gf_labs(x = "Biomass of grass", y = "Biomass growth/day")
    })
}

# Run the application
shinyApp(ui = ui, server = server)
