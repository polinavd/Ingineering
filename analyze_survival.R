# Подключаем библиотеки
library(survival)
library(survminer)

analyze_survival <- function(data) {
  cat("Начинается анализ выживаемости...\n")
  
  # Преобразование данных
  data$Time <- as.numeric(gsub(",", ".", as.character(data$Time)))
  data$Status <- as.integer(data$Status)
  data$Group <- as.factor(data$Group)
  
  cat("Данные после преобразования:\n")
  print(head(data))
  
  # Создаём объект выживаемости
  survival_object <- Surv(data$Time, data$Status)
  cat("Объект survival_object создан:\n")
  print(survival_object)
  
  # Проверка: survival_object существует
  if (is.null(survival_object)) {
    stop("Объект survival_object не был создан.")
  }
  
  # Лог-ранг тест
  logrank_test <- survdiff(survival_object ~ data$Group)
  cat("Лог-ранг тест выполнен успешно.\n")
  
  p_value <- 1 - pchisq(logrank_test$chisq, df = length(unique(data$Group)) - 1)
  cat("Log-rank p-value:", p_value, "\n")
  
  # Построение графика Каплана-Майера
  km_fit <- survfit(survival_object ~ data$Group, data = data)
  cat("Построение модели survfit завершено.\n")
  
  km_plot <- ggsurvplot(
    km_fit,
    data = data,
    pval = TRUE,
    xlab = "Время (годы)",
    ylab = "Вероятность выживания",
    title = "График Каплана-Майера"
  )
  cat("График Каплана-Майера построен.\n")
  
  return(list(p_value = p_value, plot = km_plot))
}
