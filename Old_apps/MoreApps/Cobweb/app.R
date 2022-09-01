

library(shiny)

ui <- fluidPage(

    # Application title
    titlePanel("Discrete-time dynamics"),

    # Sidebar with a slider input for number of bins
    sidebarLayout(
        withMathJax(
            sidebarPanel(
            (textInput("dynamics_eq", "$$x_{t+1} \\equiv f(x_t)$$",
                       placeholder = "f(x) formula goes here")),
            actionButton("take_step", "Take more step(s)"),
            numericInput("n_steps", "Multi-step", min=1, max=10, value=1, step=1),
            sliderInput("xrange", "x-limits", min = -3, max = 3,
                        value = c(0,1), step=0.25),
            tags$p("Double click to set initial condition.")

        )
        ),

        # Show a plot of the generated distribution
        mainPanel(
           plotOutput("time_series", height = "180px"),
           plotOutput("dynamics", dblclick="start_x")
           )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {
    # stores the time series
  traj_x <- reactiveVal(numeric(0))

  observe({
    # reset trajectory when dynamics change
    dynamics()
    traj_x(numeric(0))
  })
  dynamics <- reactive({
      if (input$dynamics_eq == "")
          return(mosaic::makeFun(2*x*(1-x) ~ x))

      # if it's a number, return some random dynamics
      if (grepl("^[0-9]*$", input$dynamics_eq)) {
        fraw <- rfun(~ x, seed=as.numeric(input$dynamics_eq))
        vals <- range(fraw(seq(-5, 5, by=.01)))
        f <- function(x) {
          (fraw(6*(x-.5)) - vals[1])/diff(vals)
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
  observeEvent(input$take_step, {
      req(length(traj_x()) > 0)

      x <- numeric(input$n_steps + 1)
      x[1] <- isolate(traj_x()[length(traj_x())])
      for (k in 1:input$n_steps) {
          x[k+1] = dynamics()(x[k])
      }

      if (length(traj_x()) == 0 ) isolate(traj_x(x))
      isolate(traj_x(c(traj_x(), x[-1] )))

  })
  output$dynamics <- renderPlot({
      dynamics_plot()
  })

  # Trigger the trajectory by double clicking
  observeEvent(input$start_x, {
      if (! is.null(input$start_x$x)) {
          x <- numeric(length = input$n_steps)
          x[1] <- input$start_x$x
          for (k in 1:input$n_steps) x[k+1] <- dynamics()(x[k])
          isolate(traj_x(x))
      }
  })

  dynamics_plot <- reactive({
      Points <- tibble::tibble(
          domain =seq(input$xrange[1], input$xrange[2], length = 100),
          y = dynamics()(domain)
      )
      suppressWarnings({
      P <- gf_line(y ~ domain, data = Points) %>%
          gf_abline(intercept = 0, slope = 1, color="blue") %>%
          gf_labs(x = expression(x[t]), y = expression(x[t+1]),
                  title = "Dynamics") %>%
          gf_refine(coord_fixed())
      })
      Cobweb <- cobweb()
      if (!is.null(Cobweb) && nrow(Cobweb) > 1) P <- P %>%
          gf_point(y ~ x, data = Cobweb, inherit=FALSE) %>%
          gf_path(y ~ x, data = Cobweb, alpha = 0.3, color = "red") %>%
          gf_text(y ~ x, label = ~ label, data = Cobweb, size = 4,
                  hjust=1, vjust=0)

      P
  })

  cobweb <- reactive({
      if (length(traj_x()) < 2) return(NULL)

      Points <- tibble(
          x = traj_x()[-length(traj_x())],
          y = traj_x()[-1],
          step = 0:(length(y)-1) * 2
      )
      Diags <- tibble(
          x = traj_x(),
          y = x,
          step = (1:length(y))*2 - 3
      )

      bind_rows(Points, Diags) %>% arrange(step) %>%
          mutate(label = ifelse((step %% 2) == 1 , (step+1)/2, ""))
  })

  output$time_series <- renderPlot({
      tmax <- pmax(10, length(traj_x()))
      if (length(traj_x())==0) return(NULL)
      Points <- tibble(
          t = 0:(length(traj_x()) -1),
          y = traj_x()
      )
      gf_point(y ~ t, data = Points) %>%
          gf_lims(x = c(0, tmax)) %>%
          gf_labs(y = expression(x[t]), x = "t") %>%
          gf_line(y ~ t, data = Points, alpha = 0.3, color="red")
  })

}

# Run the application
shinyApp(ui = ui, server = server)
