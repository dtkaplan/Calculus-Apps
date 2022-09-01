library(shiny)

# source("~/Downloads/antiDplot.R")
library(mosaic)
library(ggformula)
library(mosaicCalc)

draw_accum <- function(Fun, n=11, alpha = 1, hshift=0, 
                       accumgraph = TRUE, Fgraph=TRUE, C=0) {
  fun <- D(Fun(x) ~ x)
  xrange <- c(-3, 3)
  xwidth <- diff(xrange)/ (2*(n+1))
  f_data <- tibble::tibble(
    xpts = seq(xrange[1], xrange[2], length=n+2)[2:(n+1)] + 
      hshift * xwidth,
    xstart = xpts - xwidth,
    xend = xpts + xwidth,
    slope = fun(xpts),
    scaled_slope = (slope - min(slope)) / diff(range(slope)), # on [0,1] scale
    ystart = -slope*xwidth,
    yend = slope*xwidth,
    move_vertical = c(0,cumsum(yend-ystart)[-length(yend)]),
    hue = hsv(.1 + 4*scaled_slope/5, 1, 1),
    ystart2 = alpha*move_vertical + C,
    yend2 = alpha*move_vertical + 2*xwidth*slope + C,
    midpoint = (ystart2 + yend2)/2
  )
  dom <- domain(x=xrange)
  
  if (accumgraph) {
    P <- NULL
    slice_plot(Fun(x) ~ x, dom, color="gray", alpha=Fgraph) %>% 
      gf_segment(data = f_data, ystart2 + yend2 ~ xstart + xend, color = ~ hue) %>%
      #gf_point(data = f_data, 0 ~ xpts, color = ~ hue) %>%
      #gf_segment(data = f_data, ystart + yend ~ xstart + xend, color = ~ hue) %>%
      gf_point(data = f_data, midpoint ~ xpts, color = ~ hue) %>%
      gf_refine(scale_colour_identity()) %>%
      gf_labs(y = "F(x) and accumulated slopes")
  } else {
    slice_plot(fun(x) ~ x, dom) %>%
      gf_point(data = f_data, slope ~ xpts, color = ~ hue, size=2) %>%
      gf_refine(scale_colour_identity()) %>%
      gf_labs(y="f(x) -- the slope of F(x)")
  } 

}


ui <- fluidPage(

    # Application title
    titlePanel("From f(x) to F(x)"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            sliderInput("alpha",
                        "Align segments:",
                        min = 0,
                        max = 1,
                        value = 0,
                        step = 0.05),
            sliderInput("n", "Number of segments:",
                        min = 3, max = 51, step=2, value = 5),
            sliderInput("C", "Constant of integration C",
                        min = -50, max = 50, value=0, step = 0.5),
            checkboxInput("showF", "Show original F()", value=FALSE),
            actionButton("newfun", "Make new function f(x)")
        ),

        # Show a plot of the generated distribution
        mainPanel(
           plotOutput("Fun"),
           plotOutput("deriv", height="200px")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    Fun <- reactive({
      input$newfun
      rfun(~ x)
    })
    output$Fun <- renderPlot({
       draw_accum(Fun(), n = input$n, 
                  alpha = input$alpha, 
                  hshift = 0,
                  Fgraph = input$showF,
                  accumgraph = TRUE,
                  input$C)
    })
    output$deriv <- renderPlot({
      draw_accum(Fun(), n = input$n, 
                 alpha = 1, 
                 hshift = 0,
                 accumgraph = FALSE)
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
