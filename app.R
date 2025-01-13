library(shiny)
library(ggplot2)
library(survminer)
library(DT)
library(survival)

# Функция для анализа выживаемости
analyze_survival <- function(data) {
  data$Group <- as.factor(data$Group)
  survival_object <- Surv(data$Time, data$Status)
  logrank_test <- survdiff(survival_object ~ data$Group)
  p_value <- 1 - pchisq(logrank_test$chisq, df = length(unique(data$Group)) - 1)
  km_fit <- survfit(survival_object ~ data$Group, data = data)
  km_plot <- ggsurvplot(
    km_fit,
    data = data,
    pval = TRUE,
    xlab = "Время (годы)",
    ylab = "Вероятность выживания",
    title = "Каплан-Майер"
  )
  return(list(p_value = p_value, plot = km_plot))
}

# UI
ui <- fluidPage(
  titlePanel("Анализ выживаемости"),
  sidebarLayout(
    sidebarPanel(
      fileInput("file", "Загрузите файл CSV",
                accept = c(".csv")),
      actionButton("analyze", "Проанализировать данные"),
      hr(),
      downloadButton("download_results", "Скачать результаты")
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("График выживаемости", plotOutput("survival_plot")),
        tabPanel("Результаты Лог-ранг теста", DTOutput("logrank_table"))
      )
    )
  )
)

# Server
server <- function(input, output, session) {
  # Данные, загруженные пользователем
  data <- reactiveVal(NULL)
  
  # Результаты анализа
  results <- reactiveVal(list())
  
  observeEvent(input$file, {
    req(input$file)
    df <- read.csv(input$file$datapath, sep = ";")
    data(df)
  })
  
  observeEvent(input$analyze, {
    req(data())
    analysis <- analyze_survival(data())
    results(analysis)
  })
  
  output$survival_plot <- renderPlot({
    req(results())
    results()$plot$plot
  })
  
  output$logrank_table <- renderDT({
    req(results())
    data.frame(
      Group = levels(data()$Group),
      P_Value = results()$p_value
    )
  })
  
  output$download_results <- downloadHandler(
    filename = function() {
      paste0("logrank_results_", Sys.Date(), ".csv")
    },
    content = function(file) {
      write.csv(data.frame(
        Group = levels(data()$Group),
        P_Value = results()$p_value
      ), file, row.names = FALSE)
    }
  )
}

# Запуск приложения
shinyApp(ui = ui, server = server)

