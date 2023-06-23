library('shiny')

ui <- fluidPage(
    titlePanel("Hello, Shiny!"),
    sidebarLayout(
        sidebarPanel(
            radioButtons('type','BLOOD TYPE:',c('A','B','AB','O')),
            sliderInput('age','AGE:',min=1,max=100,value=25)
        ),
        mainPanel(
            textOutput('blood'),
            plotOutput('agePlot')
        )
    )
)

server <- function(input, output, session) {
    output$blood <- renderText(paste('당신의 혈액형은 ',input$type,'형입니다.'))
    output$agePlot <- renderPlot(({plot(0:input$age)}))
}

shinyApp(ui=ui, server=server)