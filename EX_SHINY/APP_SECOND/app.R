library(shiny)
library(ggplot2)
library(dplyr)
library(car)

ui <- fluidPage(
  titlePanel("5조 가계동향조사 데이터 분석 및 시각화"),
  
  tabsetPanel(
    tabPanel("데이터 소개",
      verbatimTextOutput(outputId = "s_introduce")
    ),
    
    tabPanel("데이터 시각화",
    selectInput(inputId = "l_var", label = "variable:", choices = NULL, selected = NULL),
    plotOutput(outputId = "l_hist"),
    plotOutput("l_pie1"),
    plotOutput("l_pie2"),
    plotOutput("l_pie3"),
    plotOutput("l_pie4"),
    plotOutput("l_pie5"),
    plotOutput("l_pie6"),
    plotOutput("l_pie7"),
    plotOutput("l_pie8"),
    plotOutput("l_pie9"),
    plotOutput("l_pie10")
    ),

    
    tabPanel("가설과 검정",
      textOutput('one'),
      textOutput('two'),
      textOutput('three'),
      textOutput('four'),
      textOutput('five'),
      plotOutput("k_plot1"),
      verbatimTextOutput("k_cor1"),
      plotOutput("k_plot2"),
      verbatimTextOutput("k_cor2"),
      plotOutput("k_plot3"),
      verbatimTextOutput("k_cor3"),
      plotOutput("k_plot4"),
      verbatimTextOutput("k_cor4"),
      plotOutput("k_plot5"),
      verbatimTextOutput("k_cor5")
    ),
    
    tabPanel("단순선형회귀분석",
      selectInput(inputId = "s_indep_var", label = "Independent variable:", choices = NULL, selected = NULL),
      selectInput(inputId = "s_dep_var", label = "Dependent variable:", choices = NULL, selected = NULL),
      plotOutput(outputId = "s_plot"),
      verbatimTextOutput(outputId = "s_summary")
    ),
    
    tabPanel("다중회귀분석",
      h4("독립 변수 선택"),
      checkboxGroupInput("indep_vars", "독립 변수",
                        choices = c("연령", "월세", "면적", "비소비지출"),
                        selected = c("연령", "월세", "면적", "비소비지출")),
      
      h4("선형 회귀 분석 결과"),
      verbatimTextOutput("regression_result"),
      
      plotOutput(outputId = "h_plot"),
      h4("등분산성 검정 결과"),
      verbatimTextOutput("h_ncv_test"),
      verbatimTextOutput("h_spreadresult"),
      h4("등분산성 시각화"),
      plotOutput("h_spread_level_plot"),
      h4("최종 모형 결과"),
      verbatimTextOutput("h_lm_result"),
      plotOutput("h_lm_plot"),
      h4("등분산성 검정 결과"),
      verbatimTextOutput("h_lm_ncv_test"),
      h4("이상점, 영향점"),
      verbatimTextOutput("h_outlier"),
      plotOutput("h_influence_plot"),
      plotOutput("h_influence")
    ),
    
    tabPanel("회귀분석을 이용한 소비지출 예측",
    h4("회귀 모형 식"),
               verbatimTextOutput("regression_formula"),
               
               h4("예측 결과"),
               fluidRow(
                 column(4,
                        numericInput("age", "연령", value = 30, min = 18, max = 100),
                        numericInput("rent", "월세", value = 50, min = 20, max = 9000),
                        numericInput("size", "면적", value = 20, min = 10, max = 320),
                        numericInput("non_consumption", "비소비지출", value = 10000, min = 0, max = 23000000)
                 ),
                 column(4,
                        verbatimTextOutput("predicted_result"),
                 
                 )
               )
    )
  )
)

### 서버 ------------------------------------------------------------------


server <- function(input, output,session) {
    # 사용할 데이터 --------------------------------------------------
    df <- read.csv('C:/EXAM_python/MONTH_02/DAY_0228_9주차_팀플/EX_SHINY/APP_SECOND/가구동향.csv')
    h_df <- select(df, 가구주_연령, 월세평가금액,주거용전용면적,가계지출_소비지출금액, 가계지출_비소비지출금액)
    names(h_df) <- c('연령','월세','면적','소비지출','비소비지출')

    # 1번 탭 ---------------------------------------------------------
    output$s_introduce <- renderPrint({
      print('# 데이터 행,열 수 ---------------------')
      print(dim(df))
      print('                                       ')
      print('# 열 종류 -----------------------------')
      str(df[,1:14])
      print('                                       ')
      str(df[,15:133])})


    # 2번 탭 ---------------------------------------------------------
    output$l_hist <- renderPlot({hist(df[,input$l_var])})

    output$l_pie1 <- renderPlot({
  df_subset <- df[, c(15,34,37,44,50,61,69,79,83,104,113,116,126)]
  names(df_subset) <- gsub('소비지출 비목별 구성비', '', names(df_subset))
  df_subset_sum <- colSums(df_subset)
  df_pie <- data.frame(category = names(df_subset_sum), value = df_subset_sum)
  colors <- c("#ffeda0","#ffD700","#FFEFD5","#feb24c","#FF8C00","#f03b20","#FA8072","#ffB6c1", "#bd0026","#8b0000", "#6a0e00", "#fff7bc", "#f7f7f7")
  ggplot(df_pie, aes(x = "", y = value, fill = category)) +
    geom_bar(stat = "identity", width = 1, color = "white") +
    coord_polar("y", start = 0) +
    scale_fill_manual(values = colors,name = "소비지출 구성") +
    theme_void() +
    theme(legend.position = "right") +
    geom_text(aes(label = paste0(round(value/sum(df_subset_sum)*100, 1), "%")), position = position_stack(vjust = 0.5))})




output$l_pie2 <- renderPlot({
  df_grouped <- aggregate(df[,1], by=list(df[,1]), FUN=sum)
  df <- data.frame(category=df_grouped$Group.1, value=df_grouped$x)
  df$category <- as.factor(df$category)
  ggplot(df, aes(x="", y=value/sum(value), fill=category)) +
    geom_col(width=1) +
    coord_polar("y", start=0) +
    theme_void() +
    scale_fill_manual(name = "가구 구성",values = c("#F9FBE7", "#E6EE9C", "#AED581", "#AFB42B", "#59a14f","#33691E","#F0f2f1","#FFF9C4"), 
                      labels = c("1인가구", "2인가구", "3인가구", "4인가구", "5인가구", "6인가구", "7인가구", "8인가구")) +
    theme(legend.position="right") +
    ggtitle("전체 가구원수 비율") +
    geom_text(aes(y = value/sum(value), label = scales::percent(value/sum(value))), 
              position = position_stack(vjust = 0.5), size=4)
  
})




output$l_pie3 <- renderPlot({
  one_data <- df[df$가구원수 == 1, ]
  freq <- colSums(one_data[, c(15,34,37,44,50,61,69,79,83,104,113,116,126)])
  colors <- c("#E1F5Fe", "#B2EBF2", "#81D4FA", "#40C4ff", "#619CFF", "#0277BD", "#00B8D4", "#80DEEa", "#0288D1", "#E0F7FA", "#0072B2", "#01579B","#B3E5FC")
  pie_data <- data.frame(labels = names(freq), values = freq)
  pie_data$percent <- pie_data$values / sum(pie_data$values) * 100
  ggplot(pie_data, aes(x = "", y = values, fill = labels)) +
    geom_bar(width = 0.5, stat = "identity") +
    coord_polar(theta = "y") +
    ggtitle("1인가구 소비지출 비목별 구성비") +
    theme_void() +
    scale_fill_manual(values = colors,name = "소비지출 구성") +
    geom_text(aes(label = paste0(round(percent, 1), "%")), position = position_stack(vjust = 0.5))
  
})


output$l_pie4 <- renderPlot({
  
  one_data <- df[df$가구원수 == 2, ]
  freq <- colSums(one_data[, c(15,34,37,44,50,61,69,79,83,104,113,116,126)])
  colors <- c("#E1F5Fe", "#B2EBF2", "#81D4FA", "#40C4ff", "#619CFF", "#0277BD", "#00B8D4", "#80DEEa", "#0288D1", "#E0F7FA", "#0072B2", "#01579B","#B3E5FC")
  pie_data <- data.frame(labels = names(freq), values = freq)
  pie_data$percent <- pie_data$values / sum(pie_data$values) * 100

  ggplot(pie_data, aes(x = "", y = values, fill = labels)) +
    geom_bar(width = 0.5, stat = "identity") +
    coord_polar(theta = "y") +
    ggtitle("2인가구 소비지출 비목별 구성비") +
    theme_void() +
    scale_fill_manual(values = colors,name = "소비지출 구성") +
    geom_text(aes(label = paste0(round(percent, 1), "%")), position = position_stack(vjust = 0.5))
})


output$l_pie5 <- renderPlot({one_data <- df[df$가구원수 == 3, ]
freq <- colSums(one_data[, c(15,34,37,44,50,61,69,79,83,104,113,116,126)])
colors <- c("#E1F5Fe", "#B2EBF2", "#81D4FA", "#40C4ff", "#619CFF", "#0277BD", "#00B8D4", "#80DEEa", "#0288D1", "#E0F7FA", "#0072B2", "#01579B","#B3E5FC")
pie_data <- data.frame(labels = names(freq), values = freq)
pie_data$percent <- pie_data$values / sum(pie_data$values) * 100

ggplot(pie_data, aes(x = "", y = values, fill = labels)) +
  geom_bar(width = 0.5, stat = "identity") +
  coord_polar(theta = "y") +
  ggtitle("3인가구 소비지출 비목별 구성비") +
  theme_void() +
  scale_fill_manual(values = colors,name = "소비지출 구성") +
  geom_text(aes(label = paste0(round(percent, 1), "%")), position = position_stack(vjust = 0.5))

})




output$l_pie6 <- renderPlot({one_data <- df[df$가구원수 == 4,]
freq <- colSums(one_data[, c(15,34,37,44,50,61,69,79,83,104,113,116,126)])
colors <- c("#E1F5Fe", "#B2EBF2", "#81D4FA", "#40C4ff", "#619CFF", "#0277BD", "#00B8D4", "#80DEEa", "#0288D1", "#E0F7FA", "#0072B2", "#01579B","#B3E5FC")
pie_data <- data.frame(labels = names(freq), values = freq)
pie_data$percent <- pie_data$values / sum(pie_data$values) * 100

ggplot(pie_data, aes(x = "", y = values, fill = labels)) +
  geom_bar(width = 0.5, stat = "identity") +
  coord_polar(theta = "y") +
  ggtitle("4인가구 소비지출 비목별 구성비") +
  theme_void() +
  scale_fill_manual(values = colors,name = "소비지출 구성") +
  geom_text(aes(label = paste0(round(percent, 1), "%")), position = position_stack(vjust = 0.5))
})


output$l_pie7 <- renderPlot({one_data <- df[df$가구원수 == 5, ]
freq <- colSums(one_data[, c(15,34,37,44,50,61,69,79,83,104,113,116,126)])
colors <- c("#E1F5Fe", "#B2EBF2", "#81D4FA", "#40C4ff", "#619CFF", "#0277BD", "#00B8D4", "#80DEEa", "#0288D1", "#E0F7FA", "#0072B2", "#01579B","#B3E5FC")
pie_data <- data.frame(labels = names(freq), values = freq)
pie_data$percent <- pie_data$values / sum(pie_data$values) * 100

ggplot(pie_data, aes(x = "", y = values, fill = labels)) +
  geom_bar(width = 0.5, stat = "identity") +
  coord_polar(theta = "y") +
  ggtitle("5인가구 소비지출 비목별 구성비") +
  theme_void() +
  scale_fill_manual(values = colors,name = "소비지출 구성") +
  geom_text(aes(label = paste0(round(percent, 1), "%")), position = position_stack(vjust = 0.5))
})


output$l_pie8 <- renderPlot({one_data <- df[df$가구원수 == 6, ]
freq <- colSums(one_data[, c(15,34,37,44,50,61,69,79,83,104,113,116,126)])
colors <- c("#E1F5Fe", "#B2EBF2", "#81D4FA", "#40C4ff", "#619CFF", "#0277BD", "#00B8D4", "#80DEEa", "#0288D1", "#E0F7FA", "#0072B2", "#01579B","#B3E5FC")
pie_data <- data.frame(labels = names(freq), values = freq)
pie_data$percent <- pie_data$values / sum(pie_data$values) * 100

ggplot(pie_data, aes(x = "", y = values, fill = labels)) +
  geom_bar(width = 0.5, stat = "identity") +
  coord_polar(theta = "y") +
  ggtitle("6인가구 소비지출 비목별 구성비") +
  theme_void() +
  scale_fill_manual(values = colors,name = "소비지출 구성") +
  geom_text(aes(label = paste0(round(percent, 1), "%")), position = position_stack(vjust = 0.5))

})



output$l_pie9 <- renderPlot({one_data <- df[df$가구원수 == 7, ]
freq <- colSums(one_data[, c(15,34,37,44,50,61,69,79,83,104,113,116,126)])
colors <- c("#E1F5Fe", "#B2EBF2", "#81D4FA", "#40C4ff", "#619CFF", "#0277BD", "#00B8D4", "#80DEEa", "#0288D1", "#E0F7FA", "#0072B2", "#01579B","#B3E5FC")
pie_data <- data.frame(labels = names(freq), values = freq)
pie_data$percent <- pie_data$values / sum(pie_data$values) * 100


ggplot(pie_data, aes(x = "", y = values, fill = labels)) +
  geom_bar(width = 0.5, stat = "identity") +
  coord_polar(theta = "y") +
  ggtitle("7인가구 소비지출 비목별 구성비") +
  theme_void() +
  scale_fill_manual(values = colors,name = "소비지출 구성") +
  geom_text(aes(label = paste0(round(percent, 1), "%")), position = position_stack(vjust = 0.5))

})



output$l_pie10 <- renderPlot({one_data <- df[df$가구원수 == 8, ]
freq <- colSums(one_data[, c(15,34,37,44,50,61,69,79,83,104,113,116,126)])
colors <- c("#E1F5Fe", "#B2EBF2", "#81D4FA", "#40C4ff", "#619CFF", "#0277BD", "#00B8D4", "#80DEEa", "#0288D1", "#E0F7FA", "#0072B2", "#01579B","#B3E5FC")
pie_data <- data.frame(labels = names(freq), values = freq)
pie_data$percent <- pie_data$values / sum(pie_data$values) * 100

ggplot(pie_data, aes(x = "", y = values, fill = labels)) +
  geom_bar(width = 0.5, stat = "identity") +
  coord_polar(theta = "y") +
  ggtitle("8인가구 소비지출 비목별 구성비") +
  theme_void() +
  scale_fill_manual(values = colors,name = "소비지출 구성") +
  geom_text(aes(label = paste0(round(percent, 1), "%")), position = position_stack(vjust = 0.5))
})


    # 3번 탭 ---------------------------------------------------------
    output$k_cor1 <- renderPrint({cor.test(df$가구원수,df$주거용전용면적,method = c('pearson', 'kendall', 'spearman'))})
    output$k_cor2 <- renderPrint({cor.test(df$가구원수,df$가계지출_소비지출금액,method = c('pearson', 'kendall', 'spearman'))})
    output$k_cor3 <- renderPrint({cor.test(df$가계지출_비소비지출금액,df$가계지출_소비지출_교육비,method = c('pearson', 'kendall', 'spearman'))})
    output$k_cor4 <- renderPrint({cor.test(df$가계지출_비소비지출금액,df$가계지출_소비지출_오락문화_서적구입비,method = c('pearson', 'kendall', 'spearman'))})
    output$k_cor5 <- renderPrint({cor.test(df$가계지출_비소비지출금액,df$가계지출_소비지출_보건_의약품구입비,method = c('pearson', 'kendall', 'spearman'))})
    output$k_plot1 <- renderPlot({boxplot(주거용전용면적 ~ 가구원수, data = df,col = heat.colors(8),ylab = "주거용 전용 면적",main = "가구원 수 & 주거용 전용 면적")})
    output$k_plot2 <- renderPlot({boxplot(가계지출_소비지출금액 ~ 가구원수,data=df,col = heat.colors(8),ylab = '소비 지출 금액',xlab = '가구원 수',main = '가구원 수 & 소비 지출 금액')})
    output$k_plot3 <- renderPlot({plot(가계지출_소비지출_교육비 ~ 가계지출_비소비지출금액,data=df,col = '#6B8068',pch = 18,xlab = '비소비 지출 금액',ylab = '교육비')})
    output$k_plot4 <- renderPlot({plot(가계지출_소비지출_오락문화_서적구입비 ~ 가계지출_비소비지출금액,data=df,col = '#6B8068',pch = 18,xlab = '비소비 지출 금액',ylab = '서적 구입비')})
    output$k_plot5 <- renderPlot({plot(df$가계지출_소비지출_보건_의약품구입비 ~ df$가계지출_비소비지출금액,col = '#6B8068',pch = 18,xlab = '비소비 지출 금액',ylab = '의약품 구입비')})
    output$one <- renderText('0.0~0.2 : very weak correlation (or negligible)')
    output$two <- renderText('0.2~0.4 : weak correlation')
    output$three <- renderText('0.4~0.6 : moderate correlation')
    output$four <- renderText('0.6~0.8 : strong correlation')
    output$five <- renderText('0.8~1.0 : very strong correlation')

    # 4번 탭 ---------------------------------------------------------
    s_fit <- reactive({lm(df[[input$s_dep_var]] ~ df[[input$s_indep_var]])})

    observe({
    updateSelectInput(session, "s_indep_var", label = "Independent variable:", choices = names(df)[1:14], selected = "가구원수")
    updateSelectInput(session, "s_dep_var", label = "Dependent variable:", choices = names(df)[1:14], selected = "월세평가금액")
    updateSelectInput(session, "l_var", label = "variable:", choices = names(df), selected = "가구원수")
    })

    output$s_plot <- renderPlot({
      s_x <- df[[input$s_indep_var]]
      s_y <- df[[input$s_dep_var]]
      ggplot(df, aes(x = s_x, y = s_y)) +
      geom_point() +
      geom_smooth(method = lm, se = FALSE, color = "red") +
      xlab(input$s_indep_var) +
      ylab(input$s_dep_var)})

    output$s_summary <- renderPrint({
      summary(s_fit())})


    # 5번 탭 -----------------------------------------------------------
     h_df=mutate(h_df, l소비지출 = log(소비지출), l비소비지출= log(ifelse(비소비지출==0,0.1,비소비지출)))
    h_fit <- lm(l소비지출 ~ 연령 + 월세 + 면적 + l비소비지출 , data=h_df)
    h_lm<-lm(formula(l소비지출^2 ~ 연령 + 월세 + 면적 + l비소비지출) , data=h_df)
    
    output$regression_result <- renderPrint({
      fit3 <- lm(l소비지출 ~ ., data = h_df[, c("l소비지출", input$indep_vars)])
      summary(fit3)
      })
    
    output$h_plot <- renderPlot({par(mfrow=c(2,2))
      plot(h_fit, col='plum',pch=19)})
    
    output$h_ncv_test <- renderPrint({
      ncv_test <- ncvTest(h_fit)
      ncv_test
    })
    
    output$h_spreadresult <- renderPrint({
      spreadLevelPlot(h_fit)
    })
    
    
    output$h_spread_level_plot <- renderPlot({
      spread_level_plot <- spreadLevelPlot(h_fit)
      spread_level_plot
    })
    
    output$h_lm_result <- renderPrint({
      summary(h_lm)
    })
    
    output$h_lm_plot <- renderPlot({par(mfrow=c(2,2))
      plot(h_lm, col='plum',pch=19)})
    
    
    output$h_lm_ncv_test <- renderPrint({
      ncvTest(h_lm)
    })
    
    output$h_outlier <- renderPrint({
      outlierTest(h_lm)
    })
    
    
    output$h_influence_plot <- renderPlot({
      influencePlot(h_lm)

    })
    
    output$h_influence <- renderPlot({
      plot(h_lm, which=4)
      
    })

    # 6번 탭 -----------------------------------------------------------
    output$regression_formula <- renderPrint({
    
    formula(h_lm)
  })
  
  # 예측 결과 출력
  output$predicted_result <- renderPrint({
    age <- input$age
    rent <- input$rent
    size <- input$size
    non_consumption <- input$non_consumption
    new_df <- data.frame(연령 = age, 월세 = rent, 면적 = size, l비소비지출 = non_consumption)
    predicted_value <- predict(h_lm, newdata = new_df)
    predicted_value
  })
    
}

shinyApp(ui = ui, server = server)