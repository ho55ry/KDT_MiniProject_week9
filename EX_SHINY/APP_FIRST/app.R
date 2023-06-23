### shiny 패키지 설치 --------------------------------------
library('shiny')


### 웹앱 프로그램 구조 -------------------------------------
# Front-End (UI) + Back-End (Server)
# - 앱 실행 코드
# ---------------------------------------------------------


### Front-End (UI) 처리 함수 구현 --------------------------
# 웹페이지 구성 => 입출력 요소
# ---------------------------------------------------------
ui <- fluidPage(
    # 화면에 글자 보여주는 위젯
    textOutput("txt"), # outputid : txt
    textOutput('value'),
    verbatimTextOutput('code'),
    imageOutput('img1',width = '50px'),
    plotOutput('plot1',width='400px'),
    tableOutput('stastic'),
    dataTableOutput('dynamic'),
    textInput('comment','COMMENT','input your message'),
    verbatimTextOutput('msg'),
    numericInput('obs','Observations:',10,min=1,max=100),
    verbatimTextOutput('value2')
)


### Back-End (Server) 기능 함수 구현 -----------------------
# 사용자의 요청에 해당하는 페이지 생성
# 웹 페이지 출력 내용 생성
# - input : Front-End에서 입력한 데이터 저장 객체
# - output : Front-End로 보낼 데이터 저장 객체
# ---------------------------------------------------------
server <- function(input, output, session)
{
output$txt=renderText("hi") # outputid : txt
output$value=renderText(('1235125'))
output$code=renderPrint(summary(1:6)) # {} 한줄짜리는 안넣어도 돌아가긴함
filename <- normalizePath(file.path('images','output.png'))
# APP_FIRST 폴더안에 imgs폴더 -> './imgs','image.png'
output$img1 = renderImage({list(src=filename,contentType='image/png',alt='text')},deleteFile = FALSE)
output$plot1 = renderPlot({
    cars2<- cars+rnorm(nrow(cars))
    plot(cars2,col='blue',pch=20)})
output$stastic <- renderTable(head(mtcars))
output$dynamic <- renderDataTable(mtcars,options=list(pageLength=5))
output$msg<- renderText({input$comment})
output$value2 <- renderText({input$obs})
}


### Shiny Web App 실행 ------------------------------------

# ---------------------------------------------------------
shinyApp(ui = ui, server = server)