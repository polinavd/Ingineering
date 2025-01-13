# Установить рабочую директорию (опционально, если требуется)
setwd("/mnt/c/Users/levch/Documents/GitHub/Ingeneering")

# Загружаем функции из файлов
cat("Загружаю функции из read_and_prepare.R\n")
source("read_and_prepare.R")
cat("Функция read_and_prepare_data успешно загружена\n")

cat("Загружаю функции из analyze_survival.R\n")
source("analyze_survival.R")
cat("Функция analyze_survival успешно загружена\n")

# Указываем путь к папке с файлами данных
file_paths <- list.files(path = "data/", pattern = "*.csv", full.names = TRUE)

# Запуск пайплайна
results <- run_survival_pipeline(file_paths, output_dir = "results/")

# Выводим результаты
for (file in names(results)) {
  cat("Результаты для файла:", file, "\n")
  cat("Log-rank p-value:", results[[file]]$p_value, "\n")
}

test_data <- read_and_prepare_data("data/survival_data.csv")
print(head(test_data))
# Загружаем функции из других файлов
source("read_and_prepare.R")
source("analyze_survival.R")

# Указываем путь к папке с файлами данных
file_paths <- list.files(path = "data/", pattern = "*.csv", full.names = TRUE)

# Запуск пайплайна
results <- run_survival_pipeline(file_paths, output_dir = "results/")

# Выводим результаты
for (file in names(results)) {
  cat("Результаты для файла:", file, "\n")
  cat("Log-rank p-value:", results[[file]]$p_value, "\n")
}
