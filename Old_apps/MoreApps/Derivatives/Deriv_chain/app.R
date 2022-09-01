#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(mosaic)
library(mosaicCalc)
library(ggformula)

is_function_valid  <- function(tilde, default_val = 1) {

    default_tilde <- as.formula(paste(default_val, "+0*x  ~  x"))
    default_fun <- makeFun(default_tilde)

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

get_function_body <- function(f, label = "df/dx =") {
    content <- capture.output(body(f))
    res  <-
        if (nchar(content) < 70) paste(label,  content)
        else paste(label, "numerical function call")

    gsub("\\[1\\]", "",  gsub(" ", "", res))
}

get_function_as_tilde  <- function(f, degree = 1) {
    b <- body(f)
    var <- setdiff(all.vars(b), "pi")
    if (length(var) == 0 ) var <- "x"
    f <- 1 ~ 1
    f[[3]] <- as.name(var)
    if (degree == 2) f[[3]]  <- parse(text=paste(var, "+",  var))
    f[[2]] <- b

    f
}


# Define UI for application that draws a histogram
ui <- fillPage(
    h3("   Up and down the chain of derivatives"),
    fillCol(
          fillRow(
              wellPanel(
                  h5("Antiderivatives of f( ) and g( )" ),
                  textOutput("F_label"),
                  textOutput("G_label")
              ),
              plotOutput("Accum", height="160px"),
              flex = c(3, 5)
              ),
           fillRow(
               wellPanel(
                 textInput("tilde_f", "Tilde expression for f( )",
                           value = "sin(2*pi*x/2.5) ~ x"),
                 textInput("tilde_g", "Tilde expression for g( )",
                           value = "0 ~ x")
               ),
                   plotOutput("Fun", height = "160px"),
               flex = c(3, 5)
               ),
           fillRow(
               wellPanel(
                   h5("Derivatives of f( ) and g( )"),
                   textOutput("df_label"),
                   textOutput("dg_label")
               ),
               plotOutput("D1", height = "160px"),
               flex = c(3, 5)
               ),
           fillRow(
               wellPanel(
                   h5("2nd derivatives of f( ) and g( )"),
                   textOutput("ddf_label"),
                   textOutput("ddg_label")
               ),
               plotOutput("D2", height = "160px"),
               flex = c(3, 5)
           )
    )
)

fixed_domain <- list(x = c(-5,  5))
kill_x <- theme(axis.title.x=element_blank(),
                axis.text.x=element_blank(),
                axis.ticks.x=element_blank())

# Define server logic required to draw a histogram
server <- function(input, output) {
    f <- reactiveVal()
    F <- reactiveVal()
    df <- reactiveVal()
    ddf <- reactiveVal()
    g <- reactiveVal()
    G <- reactiveVal()
    dg <- reactiveVal()
    ddg <- reactiveVal()

    get_f_function <- reactive({
        is_function_valid(input$tilde_f)
    })
    get_g_function <- reactive({
        is_function_valid(input$tilde_g, default_val = 0)
    })

    observe({ # assign functions to f, F,  df, ddf
        f(get_f_function())
        tilde <- get_function_as_tilde(f())
        F(antiD(tilde))
        df(D(tilde))
        ddf(D(get_function_as_tilde(f(), 2)))
    })

    observe({ # assign functions to g, G,  dg, ddg
        g(get_g_function())
        tilde <- get_function_as_tilde(g())
        G(antiD(tilde))
        dg(D(tilde))
        ddg(D(get_function_as_tilde(g(), 2)))
    })

    output$Accum <- renderPlot({
        slice_plot(F()(x) ~ x, fixed_domain, size=1.5) %>%
            slice_plot(G()(x) ~ x,  color = "orange",  size = 1.2) %>%
            gf_labs(y  = "") +
            kill_x
    })
    output$Fun <- renderPlot({
        slice_plot(f()(x) ~ x, fixed_domain, size=1.5) %>%
            slice_plot(g()(x) ~ x, color = "orange",  size = 1.2) %>%
            gf_labs(y  = "")  +
            kill_x
    })
    output$D1 <- renderPlot({
        slice_plot(df()(x) ~ x, fixed_domain, size=1.5) %>%
            slice_plot(dg()(x) ~  x, color = "orange",  size = 1.2) %>%
            gf_labs(y  = "")  +
            kill_x
    })
    output$D2 <- renderPlot({
        slice_plot(ddf()(x) ~ x, fixed_domain, size=1.5)%>%
            slice_plot(ddg()(x)  ~ x,   color  = "orange",  size = 1.2) %>%
            gf_labs(y  = "")
    })


    output$df_label <-  renderText({
        get_function_body(df(),  "f'(x) =")
    })
    output$ddf_label  <- renderText({
        get_function_body(ddf(), "f''(x) = ")
    })
    output$F_label  <- renderText({
        get_function_body(F(), "F(x) = ")
    })
    output$dg_label  <- renderText({
        get_function_body(dg(), "g'(x) = ")
    })
    output$ddg_label  <- renderText({
        get_function_body(ddg(), "g''(x) = ")
    })
    output$G_label  <- renderText({
        get_function_body(G(), "G(x) = ")
    })
}

# Run the application
shinyApp(ui = ui, server = server)
