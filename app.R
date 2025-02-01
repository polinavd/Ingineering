# Подключаем библиотеки
library(shiny)
library(ggplot2)
library(survminer)
library(DT)
library(survival)

cat("Загрузка UI...\n")

# UI
ui <- fluidPage(
  titlePanel("Анализ выживаемости"),
  sidebarLayout(
    sidebarPanel(
      fileInput("file", "Загрузите файл CSV", accept = c(".csv")),
      textInput("plot_title", "Название графика", "График Каплана-Майера"),
      checkboxInput("show_grid", "Показывать сетку", value = TRUE),
      checkboxInput("show_median", "Показывать медиану", value = FALSE),
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

cat("UI загружен успешно!\n")

# Функция для анализа выживаемости
analyze_survival <- function(data, plot_title) {
  cat("Начинается анализ выживаемости...\n")
  
  # Преобразование данных
  data$Time <- as.numeric(gsub(",", ".", as.character(data$Time)))
  data$Status <- as.integer(data$Status)
  data$Group <- as.factor(data$Group)
  
  # Проверка данных
  if (any(is.na(data$Time)) || any(is.na(data$Status)) || any(is.na(data$Group))) {
    stop("Ошибка: в данных есть пропущенные значения.")
  }
  
  if (length(unique(data$Group)) < 2) {
    stop("Ошибка: недостаточно групп для анализа. Требуется минимум 2.")
  }
  
  # Лог-ранг тест и создание модели survfit, используя данные напрямую
  logrank_test <- survdiff(Surv(Time, Status) ~ Group, data = data)
  p_value <- 1 - pchisq(logrank_test$chisq, df = length(unique(data$Group)) - 1)
  
  km_fit <- survfit(Surv(Time, Status) ~ Group, data = data)
  
  # Построение графика с помощью ggsurvplot
  km_plot <- ggsurvplot(
    fit = km_fit,
    data = data,
    pval = TRUE,
    risk.table = TRUE,
    title = plot_title,
    legend.title = "Группы",
    xlab = "Время (годы)",
    ylab = "Вероятность выживания"
  )
  
  return(list(p_value = p_value, plot = km_plot, data = data, km_fit = km_fit))
}

cat("Функция анализа выживаемости загружена!\n")

# Server
server <- function(input, output, session) {
  cat("Запуск сервера...\n")
  
  data <- reactiveVal(NULL)
  results <- reactiveVal(list())
  
  observeEvent(input$file, {
    cat("Файл загружен пользователем\n")
    req(input$file)
    tryCatch({
      df <- read.csv(input$file$datapath, sep = ";")
      cat("Файл успешно загружен и считан.\n")
      
      if (!all(c("Time", "Status", "Group") %in% colnames(df))) {
        stop("Ошибка: Файл должен содержать колонки 'Time', 'Status', 'Group'.")
      }
      
      df$Time <- as.numeric(gsub(",", ".", as.character(df$Time)))
      df <- df[!is.na(df$Time) & !is.na(df$Status), ]
      
      data(df)
      cat("Файл загружен в data().\n")
    }, error = function(e) {
      cat("Ошибка при чтении файла: ", e$message, "\n")
    })
  })
  
  observeEvent(input$analyze, {
    cat("Запущен анализ данных...\n")
    req(data())
    
    withProgress(message = "Анализ данных...", value = 0, {
      incProgress(0.3, detail = "Подготовка данных")
      Sys.sleep(0.5)
      
      tryCatch({
        analysis <- analyze_survival(data(), input$plot_title)
        results(analysis)
        incProgress(0.7, detail = "Анализ завершён")
        cat("Анализ успешно завершён!\n")
      }, error = function(e) {
        cat("Ошибка при анализе данных: ", e$message, "\n")
      })
    })
  })
  
  output$survival_plot <- renderPlot({
    req(results())
    # Извлекаем ggplot объект из результата ggsurvplot
    base_plot <- results()$plot$plot
    
    if (!input$show_grid) {
      base_plot <- base_plot + theme(panel.grid = element_blank())
    }
    
    if (input$show_median) {
      overall_median <- median(results()$data$Time, na.rm = TRUE)
      base_plot <- base_plot +
        geom_hline(yintercept = 0.5, linetype = "dashed", color = "blue") +
        geom_vline(xintercept = overall_median, linetype = "dotted", color = "red") +
        annotate("text", x = overall_median, y = 0.55,
                 label = paste("Me =", round(overall_median, 2)),
                 color = "red", hjust = -0.1)
    }
    
    return(base_plot)
  })
  
  output$logrank_table <- renderDT({
    req(results())
    data.frame(
      Metric = "Log-rank p-value",
      Value = results()$p_value
    )
  })
  
  output$download_results <- downloadHandler(
    filename = function() {
      paste0("logrank_results_", Sys.Date(), ".csv")
    },
    content = function(file) {
      req(results())
      write.csv(data.frame(
        Metric = "Log-rank p-value",
        Value = results()$p_value
      ), file, row.names = FALSE)
    }
  )
}

cat("Сервер загружен, запускаем приложение...\n")

# Запуск приложения Shiny
shinyApp(ui = ui, server = server)


