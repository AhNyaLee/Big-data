# Подключение библиотек
library(ggplot2)
library(car)
library(stats)

# Чтение данных
data <- read.csv("~/Documents/6 semester/Big data/6laba/data.csv", header = TRUE, sep = ",")

# Фильтрация данных для греко-римской борьбы
wrestling <- data[data$Event == "Wrestling Men's Welterweight, Greco-Roman", ]
wrestling <- wrestling[, !names(wrestling) %in% c("Season", "Sport", "Event", "Sex")]

# Функция для расчета статистик
calculate_stats <- function(x) {
  mean_val <- mean(x, na.rm = TRUE)
  median_val <- median(x, na.rm = TRUE)
  mode_val <- as.numeric(names(sort(table(x), decreasing = TRUE)[1]))
  var_val <- var(x, na.rm = TRUE)
  sd_val <- sd(x, na.rm = TRUE)
  min_val <- min(x, na.rm = TRUE)
  max_val <- max(x, na.rm = TRUE)
  
  data.frame(
    Среднее = mean_val,
    Медиана = median_val,
    Мода = mode_val,
    Дисперсия = var_val,
    Стандартное_отклонение = sd_val,
    Минимум = min_val,
    Максимум = max_val
  )
}

# Выбор числовых столбцов (исключая ID)
numeric_cols <- wrestling[, sapply(wrestling, is.numeric) & !names(wrestling) %in% "ID"]

# Расчет статистик для числовых столбцов
stats_list <- lapply(numeric_cols, calculate_stats)
stats_table <- do.call(rbind, stats_list)
stats_table <- cbind(Переменная = names(numeric_cols), stats_table)
rownames(stats_table) <- NULL

# Вывод таблицы статистик
cat("\nТаблица статистических показателей:\n")
print(stats_table)

# Функция для построения графиков
plot_analysis <- function(df) {
  numeric_cols <- sapply(df, is.numeric)
  numeric_names <- names(df)[numeric_cols & !names(df) %in% c("ID", "Year")]
  categorical_names <- names(df)[!numeric_cols & !names(df) %in% c("Name")]
  
  for (col in numeric_names) {
    p1 <- ggplot(df, aes(y = !!sym(col))) +
      geom_boxplot(fill = "lightblue", width = 0.3) +
      ggtitle(paste("Ящик с усами для", col)) +
      theme_minimal() +
      theme(
        axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank()
      ) +
      scale_y_continuous(name = col)
    
    p2 <- ggplot(df, aes(x = !!sym(col))) +
      geom_histogram(fill = "orange", bins = 30, color = "black") +
      ggtitle(paste("Гистограмма для", col)) +
      xlab(col) +
      ylab("Частота") +
      theme_minimal()
    
    print(p1)
    print(p2)
  }
  for (col in categorical_names) {
    p <- ggplot(df, aes(x = !!sym(col))) +
      geom_bar(fill = "orange", color = "black") +
      ggtitle(paste("Столбчатая диаграмма для", col)) +
      xlab(col) +
      ylab("Частота") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
    
    print(p)
  }
}

# Построение графиков для греко-римской борьбы
cat("\nПостроение графиков для греко-римской борьбы...\n")
plot_analysis(wrestling)

# Проверка нормальности
normality_results <- lapply(names(numeric_cols), function(col) {
  shapiro_result <- shapiro.test(numeric_cols[[col]])
  
  p <- ggplot(wrestling, aes(sample = !!sym(col))) +
    stat_qq() +
    stat_qq_line(color = "red") +
    ggtitle(paste("Q-Q график для", col)) +
    xlab("Теоретические квантили") +
    ylab("Выборочные квантили") +
    theme_minimal()
  print(p)
  data.frame(
    Переменная = col,
    W = shapiro_result$statistic,
    p_значение = shapiro_result$p.value
  )
})

# Вывод результатов теста Шапиро-Уилка
normality_table <- do.call(rbind, normality_results)
cat("\nРезультаты теста Шапиро-Уилка на нормальность:\n")
print(normality_table)

# Расчет дисперсии
variance_results <- lapply(names(numeric_cols), function(col) {
  var_val <- var(numeric_cols[[col]], na.rm = TRUE)
  data.frame(
    Переменная = col,
    Дисперсия = var_val
  )
})

# Вывод таблицы дисперсий
variance_table <- do.call(rbind, variance_results)
cat("\nДисперсия для числовых переменных:\n")
print(variance_table)

# Тест Левена (если есть несколько категорий в Medal)
if (length(unique(wrestling$Medal)) > 1) {
  levene_results <- lapply(names(numeric_cols), function(col) {
    test <- leveneTest(wrestling[[col]] ~ wrestling$Medal, data = wrestling)
    data.frame(
      Переменная = col,
      F_значение = test$`F value`[1],
      p_значение = test$`Pr(>F)`[1]
    )
  })
  
  levene_table <- do.call(rbind, levene_results)
  cat("\nТест Левена на гомогенность дисперсий (по медалям):\n")
  print(levene_table)
} else {
  cat("\nТест Левена пропущен: в столбце Medal только одна категория или данные отсутствуют.\n")
}

# Расчет энтропии для категориальных переменных
calculate_entropy <- function(x) {
  x <- x[!is.na(x)]
  if (length(x) == 0) return(NA)
  probs <- table(x) / length(x)
  entropy <- -sum(probs * log2(probs), na.rm = TRUE)
  return(entropy)
}

# Выбор категориальных столбцов
categorical_cols <- wrestling[, !sapply(wrestling, is.numeric)]

# Расчет энтропии
entropy_results <- lapply(names(categorical_cols), function(col) {
  entropy_val <- calculate_entropy(categorical_cols[[col]])
  unique_count <- length(unique(categorical_cols[[col]][!is.na(categorical_cols[[col]])]))
  data.frame(
    Переменная = col,
    Шенноновская_энтропия = entropy_val,
    Уникальные_значения = unique_count
  )
})

# Вывод таблицы энтропии
entropy_table <- do.call(rbind, entropy_results)
cat("\nШенноновская энтропия и количество уникальных значений для категориальных переменных:\n")
print(entropy_table)

# Тест Уилкоксона для веса (греко-римская борьба)
wilcox_test <- wilcox.test(wrestling$Weight, mu = 75, alternative = "two.sided", conf.int = TRUE)
cat("\nТест Уилкоксона для веса (гипотеза: среднее = 75 кг):\n")
print(wilcox_test)

# Описательные статистики для веса
weight_summary <- c(
  Среднее = mean(wrestling$Weight, na.rm = TRUE),
  Стандартное_отклонение = sd(wrestling$Weight, na.rm = TRUE),
  N = length(na.omit(wrestling$Weight))
)
cat("\nОписательные статистики для веса:\n")
print(weight_summary)

# Гистограмма веса
p1 <- ggplot(wrestling, aes(x = Weight)) +
  geom_histogram(binwidth = 5, fill = "orange", color = "black") +
  geom_vline(xintercept = mean(wrestling$Weight, na.rm = TRUE), color = "blue", linetype = "solid", size = 1) +
  ggtitle("Гистограмма веса с указанием среднего и гипотетического значения (75 кг)") +
  xlab("Вес (кг)") +
  ylab("Частота") +
  theme_minimal()
print(p1)

# Фильтрация данных для вольной борьбы (исправлено)
wrestling_F <- data[data$Event == "Wrestling Men's Welterweight, Freestyle", ]
wrestling_F <- wrestling_F[, !names(wrestling_F) %in% c("Season", "Sport", "Event", "Sex")]

# Проверка нормальности для веса
cat("\nТест Шапиро-Уилка для веса (греко-римская борьба):\n")
print(shapiro.test(wrestling$Weight))

cat("\nТест Шапиро-Уилка для веса (вольная борьба):\n")
print(shapiro.test(wrestling_F$Weight))

# Проверка равенства дисперсий
cat("\nF-тест для проверки равенства дисперсий веса:\n")
print(var.test(wrestling$Weight, wrestling_F$Weight))

# t-тест (если нормальность и равенство дисперсий подтверждены)
cat("\nT-тест для сравнения среднего веса (предполагая равные дисперсии):\n")
print(t.test(wrestling$Weight, wrestling_F$Weight, var.equal = TRUE))

# t-тест Уэлча (если дисперсии не равны)
cat("\nT-тест Уэлча для сравнения среднего веса (без предположения о равенстве дисперсий):\n")
print(t.test(wrestling$Weight, wrestling_F$Weight, var.equal = FALSE))

# U-тест Манна-Уитни (если данные не нормальны)
cat("\nU-тест Манна-Уитни для сравнения веса:\n")
print(wilcox.test(wrestling$Weight, wrestling_F$Weight))