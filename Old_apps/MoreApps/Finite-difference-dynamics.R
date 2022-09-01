
library(mosaic)
library(mosaicCalc)
library(shiny)

ui <- fluidPage(

    # Application title
    titlePanel("Finite-difference dynamics"),

    # Sidebar with a slider input for number of bins
    sidebarLayout(
        sidebarPanel(
            textInput("dynamics", "Dynamics", value=0),
            sliderInput("nstart",
                        "# Starting points",
                        min = 0,
                        max = 50,
                        value = 0),
            checkboxInput("show_bounce", "Show bounce plot", TRUE),
            checkboxInput("show_cobweb", "Show cobweb plot", FALSE)
        ),

        # Show a plot of the generated distribution
        mainPanel(
           plotOutput("dynamics_plot", click="startx")
        )
    )
)

makeLoops <- function(Pairs) {
  Loops <- list()
  for (k in 1:nrow(Pairs)) {
    a = Pairs[[k, "origin"]]
    b = Pairs[[k, "dest"]]
    fun <- function(x) {- (x-a)*(x-b)*4/abs(b-a)^(3/2)}
    Loops[[k]] <- tibble::tibble(
      x = seq(a, b, length=30),
      y = fun(x),
      color = Pairs[[k, "color"]],
      group = k
    )
  }
  bind_rows(Loops)
}

# Define server logic required to draw a histogram
server <- function(input, output) {
    xstart <- reactiveVal(0)
    observe({
      xstart(req(input$startx)$x)
    })
    xrange <- reactive({
        c(-5, 5)
    })
    dynfun <- reactive({
      if (is.null(input$dynamics) || input$dynamics == 0) {
        mu = 5*runif(1)
        return(function(s) {mu - (s/5)^2})
      }
      f <-  rfun( ~ s, seed = as.numeric(input$dynamics))
      xpts <- seq(-5, 5, length=1000)
      ypts <- f(xpts)
      function(s) {
          5*(f(s) - mean(ypts))/diff(range(ypts))
      }
    })
    Iters <- reactive({
      fun <- dynfun()
      x0 <- xstart()
      Traj <- math141Z::Iterate(fun, x0, 5)
      n <- nrow(Traj) - 1
      Pairs <- tibble::tibble(
        origin = Traj[[2]][-(n+1)],
        dest = Traj[[2]][-1],
        group = 1:n,
        color = "black"
      )
      Pairs
    })
    output$dynamics_plot <- renderPlot({
      if (! input$show_bounce) return(NULL)
      if (input$nstart > 0 ){
         Loops <- makeLoops(flow_loops())
         P <- gf_line(y ~ x, data = Loops,  group=~group, color = ~ color) %>%
           gf_refine(scale_color_identity())
      } else {
      P <- NULL
      }
      Path <- Iters()
        TrajLoops <- makeLoops(Path)
        Path$step <- 1:nrow(Path)
        P %>% gf_path(y ~ x, data=TrajLoops) %>%
          gf_text(-0.1 ~ origin, data = Path, label=~step) %>%
          gf_refine(coord_fixed(ratio = 1, xlim=xrange()))  %>%
          gf_theme(
            axis.ticks.y = element_blank(),
            axis.text.y = element_blank())

    })

    flow_loops <- reactive({
      if (0 == input$nstart) return(NULL)
      from <- min(xrange())
      to <- max(xrange())
      Pairs <- tibble::tibble(
        origin = seq(from, to, length=input$nstart),
        dest = dynfun()(origin),
        color = hcl.colors(length(origin), palette = "Dark2")
      )

    })
}

# Run the application
shinyApp(ui = ui, server = server)
