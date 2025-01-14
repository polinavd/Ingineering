# Подключаем библиотеки
library(shiny)
library(ggplot2)
library(survminer)
library(DT)
library(survival)

# Функция для анализа выживаемости
analyze_survival <- function(data) {
  cat("Начинается анализ выживаемости...\n")
  
  # Преобразование данных
  data$Time <- as.numeric(gsub(",", ".", as.character(data$Time)))
  data$Status <- as.integer(data$Status)
  data$Group <- as.factor(data$Group)
  
  cat("Данные после преобразования:\n")
  print(head(data))
  
  # Проверка данных
  if (any(is.na(data$Time)) || any(is.na(data$Status)) || any(is.na(data$Group))) {
    stop("Данные содержат пропуски. Проверьте файл.")
  }
  
  # Создаём объект выживаемости
  survival_object <- Surv(data$Time, data$Status)
  cat("Объект survival_object создан:\n")
  print(survival_object)
  
  # Проверка: survival_object существует
  if (is.null(survival_object)) {
    stop("Объект survival_object не был создан.")
  }

  # Лог-ранг тест
  logrank_test <- survdiff(survival_object ~ Group, data = data)
  cat("Лог-ранг тест выполнен успешно.\n")
  
  p_value <- 1 - pchisq(logrank_test$chisq, df = length(unique(data$Group)) - 1)
  cat("Log-rank p-value:", p_value, "\n")

  # Проверка survfit
  km_fit <- tryCatch({
    survfit(survival_object ~ Group, data = data)
  }, error = function(e) {
    cat("Ошибка при создании модели survfit: ", e$message, "\n")
    stop("Проверьте данные и код survfit.")
  })
  cat("Модель survfit успешно создана:\n")
  print(summary(km_fit))

  # Подготовка данных вручную для ggplot
  plot_data <- data.frame(
    time = km_fit$time,
    surv = km_fit$surv,
    strata = rep(names(km_fit$strata), km_fit$strata)
  )

  # Построение графика вручную
  km_plot <- ggplot(plot_data, aes(x = time, y = surv, color = strata)) +
    geom_step() +
    labs(
      title = "График Каплана-Майера",
      x = "Время (годы)",
      y = "Вероятность выживания"
    ) +
    theme_minimal()

  cat("График Каплана-Майера успешно построен.\n")

  return(list(p_value = p_value, plot = km_plot))
}

# UI
ui <- fluidPage(
  titlePanel("Анализ выживаемости"),
  sidebarLayout(
    sidebarPanel(
      fileInput("file", "Загрузите файл CSV", accept = c(".csv")),
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
    tryCatch({
      df <- read.csv(input$file$datapath, sep = ";")
      cat("Файл успешно загружен и считан.\n")
      
      # Проверка структуры данных
      if (!all(c("Time", "Status", "Group") %in% colnames(df))) {
        stop("Файл должен содержать колонки 'Time', 'Status', 'Group'.")
      }
      
      data(df)  # Сохраняем данные в реактивной переменной
    }, error = function(e) {
      cat("Ошибка при чтении файла: ", e$message, "\n")
      showNotification("Ошибка при чтении файла. Проверьте формат данных.", type = "error")
    })
  })
  
  observeEvent(input$analyze, {
    req(data())
    withProgress(message = "Анализ данных...", value = 0, {
      incProgress(0.3, detail = "Подготовка данных")
      Sys.sleep(0.5)  # Искусственная задержка для демонстрации
      
      tryCatch({
        analysis <- analyze_survival(data())
        results(analysis)  # Сохраняем результаты в реактивной переменной
        incProgress(0.7, detail = "Анализ завершён")
      }, error = function(e) {
        cat("Ошибка при анализе данных: ", e$message, "\n")
        showNotification("Ошибка при анализе данных. Проверьте формат данных.", type = "error")
      })
    })
  })
  
  output$survival_plot <- renderPlot({
    req(results())
    results()$plot
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

# Запуск приложения
shinyApp(ui = ui, server = server)
