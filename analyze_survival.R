# Подключаем библиотеки
library(survival)
library(survminer)

# Функция для анализа выживаемости
analyze_survival <- function(data, output_dir = "results/") {
  # Проверяем входные данные
  cat("Проверка данных:\n")
  print(head(data))
  print(str(data))
  
  # Преобразуем Group в фактор
  data$Group <- as.factor(data$Group)
  cat("Уникальные группы:\n")
  print(unique(data$Group))
  
  # Проверяем, что групп больше одной
  if (length(unique(data$Group)) < 2) {
    stop("Должно быть как минимум две группы для анализа выживаемости.")
  }
  
  # Создаём объект выживаемости
  survival_object <- Surv(data$Time, data$Status)
  cat("Объект survival_object:\n")
  print(survival_object)
  
  # Лог-ранг тест
  logrank_test <- survdiff(survival_object ~ data$Group)
  p_value <- 1 - pchisq(logrank_test$chisq, df = length(unique(data$Group)) - 1)
  cat("Log-rank p-value:", p_value, "\n")
  
  # Построение графика Каплана-Майера
  km_fit <- survfit(Surv(Time, Status) ~ Group, data = data)
  km_plot <- ggsurvplot(
    km_fit,
    data = data,
    pval = TRUE,
    xlab = "Время (годы)",
    ylab = "Вероятность выживания",
    title = "Каплан-Майер"
  )
  
  # Сохраняем график
  dir.create(output_dir, showWarnings = FALSE)
  ggsave(filename = paste0(output_dir, "kaplan_meier_plot_", Sys.Date(), ".png"), plot = km_plot$plot)
  
  return(list(p_value = p_value, plot = km_plot))
}

print("Загружаю функцию run_survival_pipeline...")

# Функция для запуска пайплайна анализа выживаемости
run_survival_pipeline <- function(file_paths, output_dir = "results/") {
  results <- list()
  
  # Создаём таблицу для сохранения p-value
  logrank_results <- data.frame(File = character(), P_Value = numeric(), stringsAsFactors = FALSE)
  
  for (file in file_paths) {
    cat("Обработка файла:", file, "\n")
    
    # Считываем и подготавливаем данные
    data <- read_and_prepare_data(file)
    cat("Данные успешно считаны:\n")
    print(head(data))
    
    # Анализируем выживаемость
    analysis <- analyze_survival(data, output_dir)
    
    # Сохраняем результаты
    results[[file]] <- list(
      p_value = analysis$p_value,
      plot = analysis$plot
    )
    
    # Добавляем результаты Лог-ранг теста в таблицу
    logrank_results <- rbind(logrank_results, data.frame(File = file, P_Value = analysis$p_value))
  }
  
  # Сохраняем таблицу Лог-ранг тестов в CSV
  logrank_file <- file.path(output_dir, paste0("logrank_results_", Sys.Date(), ".csv"))
  write.csv(logrank_results, logrank_file, row.names = FALSE)
  cat("Результаты Лог-ранг теста сохранены в файл:", logrank_file, "\n")
  
  return(results)
}
