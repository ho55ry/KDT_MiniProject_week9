library(shiny)

ui <- fluidPage(
  selectInput(inputId = "indep_var", label = "Independent variable:", 
              choices = NULL, selected = NULL),
  selectInput(inputId = "dep_var", label = "Dependent variable:", 
              choices = NULL, selected = NULL),
  plotOutput(outputId = "plot")
)

server <- function(input, output, session) {
  df <- read.csv('D:/EXAM_python/EX_SHINY/APP_SECOND/가구.csv')
  
  observe({
    updateSelectInput(session, "indep_var", 
                      label = "Independent variable:", 
                      choices = names(df), selected = "가구원수")
    updateSelectInput(session, "dep_var", 
                      label = "Dependent variable:", 
                      choices = names(df), selected = "월세평가금액")
  })
  
  output$plot <- renderPlot({
    x <- df[[input$indep_var]]
    y <- df[[input$dep_var]]
    plot(x, y, col = "blue", xlab = input$indep_var, ylab = input$dep_var)
    abline(lm(y ~ x), col = "red")
  })
}

shinyApp(ui = ui, server = server)