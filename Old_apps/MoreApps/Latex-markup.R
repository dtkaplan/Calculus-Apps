library(shiny)

answers <- c("\\int_a^b x^2 dx", "\\int^b_a x^2 dx")



basicUI <- HTML('<div style="font-size: 25px;">
  <div id="prompt" class="shiny-html-output"></div>
</div>
<hr/>
<div style="font-size: 25px;">
  <div class="form-group shiny-input-container">
    <label class="control-label" for="latex">Type latex math here.</label>
    <textarea id="latex" class="form-control" rows="5" cols="50" style="font-size: 25px"></textarea>
  </div>
</div>
<div id="rendered" class="shiny-html-output" style="font-size:25px;"></div>
<div id="feedback" class="shiny-html-output" style="color: green; font-size:30px;"></div>')

makeLatexServer <- function(answers) {
  server <- function(input, output, session) {
    squash_str <- "\\{|\\}| |\t|\n"
    compare <- gsub(squash_str, "", answers)
    output$rendered <- renderUI({
      withMathJax(paste0("$$", input$latex, "$$"))
    })
    output$prompt <- renderUI({
      withMathJax(paste0("Typeset this: $$",answers[1],"$$"))
    })
    output$feedback <- renderUI({
      if (gsub(squash_str, "", input$latex) %in% compare) "Success!"
      else ""
    })
  }
  
  server
}


# Run the application 
shinyApp(#ui = makeLatexUI(),
  ui = basicUI,
         server = makeLatexServer(answers))
