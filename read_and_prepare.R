read_and_prepare_data <- function(file_path) {
  # Считываем данные
  data <- read.csv(file_path, header = TRUE, sep = ";")
  
  # Преобразуем Time (замена запятых на точки и преобразование в число)
  data$Time <- as.numeric(gsub(",", ".", as.character(data$Time)))
  
  # Проверяем структуру
  if (!all(c("Time", "Status", "Group") %in% colnames(data))) {
    stop("Файл должен содержать колонки 'Time', 'Status', 'Group'")
  }
  
  return(data)
}
