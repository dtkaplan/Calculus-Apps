library(mosaic)
library(mosaicCalc)
library(ggformula)
library(shiny)

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Average and marginal change"),

    # Sidebar with a slider input for number of bins
    sidebarLayout(
        sidebarPanel(
          textInput("tilde", "Tilde expression for function:", "x/exp(x) ~ x"),
          wellPanel(textOutput("ave_slope")),
          wellPanel(textOutput("secant_slope")),
          wellPanel(textOutput("tangent_slope"))
        ),
        mainPanel(
           plotOutput("Function", brush = brushOpts("mark", direction="x"))
        )
    )
)

make_fun_from_tilde <- function(tilde) {
    ## This is copied from the chain of derivatives function,
    ## except for this next line
    default_fun <- makeFun(0*x ~ x)

    # Is <tilde> even a valid expression
    check_tilde <- try(parse(text = tilde),  silent = TRUE)
    if (inherits(check_tilde, "try-error")) return(default_fun)
    fun_formula <- try(as.formula(tilde))
    if (inherits(fun_formula,  "try_error"))
        return(default_fun)
    if (length(fun_formula) != 3)
        return(default_fun)
    if (length(setdiff(all.vars(fun_formula), "pi")) != 1)
        return(default_fun)
    if (length(all.vars(fun_formula[[3]])) !=  1)
        return(default_fun)
    if (length(all.vars(fun_formula[[2]])) ==  0) {
        # it's a constant
        fun_formula = as.formula(paste(paste(fun_formula[[2]], "+0*x"),  "~ x"))
    }

    # test that the function works
    vals <- -3:3
    f <- try(makeFun(fun_formula))
    if (inherits(f, "try-error")) return(default_fun)


    y <- try(f(vals))
    if (inherits(f, "try-error")) return(default_fun)
    if (!is.numeric(y)) return(default_fun)
    if (length(y) != length(vals) ) return(default_fun)

    f
}

standard_domain <- list(x = c(0, 10))

server <- function(input, output) {

    xmin <- reactiveVal(5)
    xmax <- reactiveVal(5.1)

    get_function <- reactive({
        make_fun_from_tilde(input$tilde)
    })
    get_derivative <- reactive({
        D(get_function()(x) ~ x)
    })

    # update endpoints
    observe( {
        if (!is.null(input$mark)) {
           xmin(input$mark$xmin)
           xmax(input$mark$xmax)
        }
    })

    average_slope <- reactive({
        (get_function()(xmin()) - get_function()(0))/
            xmin()
    })
    secant_slope <- reactive({
        (get_function()(xmax()) - get_function()(xmin())) /
            (xmax() - xmin())
    })
    derivative_slope <- reactive({
        get_derivative()(xmin())
    })

    output$Function <- renderPlot({
      Pt <- tibble(x = xmin(), xmax = xmax())
      half_length <- 2
      if (!is.null(Pt)) {
          Pt$y = get_function()(Pt$x)
          Pt$ymax = get_function()(Pt$xmax)
          Pt$xleft = pmax(0, Pt$x - half_length)
          Pt$xright = pmin(10, Pt$x + half_length)
          slope = get_derivative()(Pt$x)
          Pt$yleft = Pt$y + slope*(Pt$xleft - Pt$x)
          Pt$yright = Pt$y + slope*(Pt$xright - Pt$x)

          tangent_slope <- (Pt$ymax - Pt$y) / (Pt$xmax - Pt$x)
          Pt$tan_yleft = Pt$y + tangent_slope*(Pt$xleft - Pt$x)
          Pt$tan_yright = Pt$y + tangent_slope*(Pt$xright - Pt$x)
      }
      P <- slice_plot(get_function()(x) ~ x, standard_domain)

      if (!is.null(Pt)){
       P <- P %>%
           gf_segment(tan_yleft + tan_yright ~ xleft + xright,
                      data = Pt, color = "red") %>%
           gf_segment(y + ymax ~ x + xmax, data = Pt, color = "red",
                      linetype = "dotted") %>%
           gf_segment(0 + y ~ 0 + x, data = Pt, color = "green") %>%
           gf_segment(yleft + yright ~ xleft + xright, data = Pt, color = "blue")

      }

      P
    })

    output$ave_slope <- renderText({
        paste("Average slope:", signif(average_slope(), 3))
    })

    output$secant_slope <- renderText({
        paste("Finite diff. slope:", signif(secant_slope(), 3))
    })

    output$tangent_slope <- renderText({
        paste("Infinitesimal slope:", signif(derivative_slope(), 3))
    })
}

# Run the application
shinyApp(ui = ui, server = server)
