library(cluster)
library(ggplot2)  
library(BBmisc)    
library(NbClust)   
library(scatterplot3d)
# 1. Загрузка данных
data <- read.csv("~/Documents/6 semester/Big data/5laba/data.csv", header = TRUE, sep = ",")

# 2. Дескриптивный анализ
numeric_cols <- data[, sapply(data, is.numeric)]
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
    СтандартноеОтклонение = sd_val,  
    Минимум = min_val,
    Максимум = max_val
  )
}

stats_list <- lapply(numeric_cols, calculate_stats)
stats_table <- do.call(rbind, stats_list)
stats_table <- cbind(Column = names(numeric_cols), stats_table)
rownames(stats_table) <- NULL

print(stats_table)

# Визуализация данных
plot_analysis <- function(df) {
  numeric_cols <- sapply(df, is.numeric)
  numeric_names <- names(df)[numeric_cols]
  categorical_names <- names(df)[!numeric_cols]
  
  for (col in numeric_names) {
    p1 <- ggplot(df, aes(y = !!sym(col))) +
      geom_boxplot(fill = "lightblue", width = 0.3) +
      ggtitle(paste(col)) +
      theme_minimal() +
      theme(
        axis.title.x = element_blank(),  
        axis.text.x = element_blank(),  
        axis.ticks.x = element_blank()   
      ) +
      scale_y_continuous(name = col)   
    p2 <- ggplot(df, aes(x = !!sym(col))) +
      geom_histogram(fill = "orange", bins = 30, color = "black") +
      ggtitle(paste(col)) +
      theme_minimal()
    
    print(p1)
    print(p2)
  }
  
  for (col in categorical_names) {
    p <- ggplot(df, aes(x = !!sym(col))) +
      geom_bar(fill = "orange", color = "black") +
      ggtitle(paste(col)) +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
    
    print(p)
  }
}

plot_analysis(data)

# 3. Оценка оптимального числа кластеров
numeric_cols <- na.omit(numeric_cols)  # Удаление NA
df_scaled <- scale(numeric_cols)       # Стандартизация
df_sc_norm <- normalize(df_scaled, method = "range", range = c(0, 1))  # Нормализация

# Метод силуэта
silhouette_score <- function(k, data) {
  km <- kmeans(data, centers = k, nstart = 25)
  ss <- silhouette(km$cluster, dist(data))
  mean(ss[, 3])
}

k_values <- 2:10
sil_scores <- sapply(k_values, silhouette_score, data = df_sc_norm)
sil_df <- data.frame(k = k_values, Silhouette = sil_scores)

p_sil <- ggplot(sil_df, aes(x = k, y = Silhouette)) +
  geom_line() +
  geom_point() +
  labs(title = "Метод силуэта", x = "Число кластеров", y = "Средний силуэтный коэффициент") +
  theme_minimal()
print(p_sil)

# Метод локтя
wss_score <- function(k, data) {
  km <- kmeans(data, centers = k, nstart = 25)
  km$tot.withinss
}

wss_scores <- sapply(k_values, wss_score, data = df_sc_norm)
wss_df <- data.frame(k = k_values, WSS = wss_scores)

p_wss <- ggplot(wss_df, aes(x = k, y = WSS)) +
  geom_line() +
  geom_point() +
  labs(title = "Метод локтя", x = "Число кластеров", y = "Сумма квадратов внутри кластеров (WSS)") +
  theme_minimal()
print(p_wss)

# Статистика разрыва
gap_stat <- clusGap(df_sc_norm, FUN = kmeans, nstart = 25, K.max = 10, B = 50)
gap_df <- data.frame(k = 1:10, Gap = gap_stat$Tab[, "gap"], SE = gap_stat$Tab[, "SE.sim"])

p_gap <- ggplot(gap_df, aes(x = k, y = Gap)) +
  geom_line() +
  geom_point() +
  geom_errorbar(aes(ymin = Gap - SE, ymax = Gap + SE), width = 0.2) +
  labs(title = "Статистика разрыва", x = "Число кластеров", y = "Значение разрыва") +
  theme_minimal()
print(p_gap)

# Алгоритм консенсуса
n_clust <- NbClust(df_sc_norm, distance = "euclidean", min.nc = 2, max.nc = 10, 
                   method = "kmeans", index = "all")

nbclust_df <- data.frame(k = n_clust$Best.nc["Number_clusters", ])
p_nbclust <- ggplot(nbclust_df, aes(x = k)) +
  geom_histogram(binwidth = 1, fill = "orange", color = "black") +
  labs(title = "Алгоритм консенсуса (NbClust)", x = "Число кластеров", y = "Частота") +
  theme_minimal()
print(p_nbclust)

# 4. Выполнить иерархическую кластеризацию вашего набора данных, построив дендрограмму
dist.datas=dist(df_sc_norm)
clust.datas=hclust(dist.datas,'ward.D')
plot(clust.datas,cex=0.2, ann = FALSE)
title(main = "Дендрограмма кластеризации (3 группы)")
rect.hclust(clust.datas,k=3,border="red")

cluster_cut3 <- cutree(clust.datas, k = 3)
group1_indices <- which(cluster_cut3 == 1)
group1_data_norm <- df_sc_norm[group1_indices, ]
print(group1_data_norm)

# 5. Построить диаграмму со столбчатыми диаграммами и боксплотами групп.
plot_cluster_profiles <- function(clust_obj, df_norm, k) {
  while (dev.cur() > 1) dev.off()
  dev.new()
  cluster_cut <- cutree(clust_obj, k = k)
  cluster_means <- t(sapply(1:k, function(i) {
    colMeans(df_norm[cluster_cut == i, , drop = FALSE])
  }))
  param_colors <- rainbow(ncol(df_norm))
  layout_matrix <- matrix(c(1:k, rep(k+1, 2)), nrow = 1)
  layout(layout_matrix, widths = c(rep(4, k), 3)) 
  par(oma = c(0, 0, 2, 0), mar = c(5, 4, 2, 1))
  for (i in 1:k) {
    barplot(cluster_means[i, ],
            col = param_colors,
            ylim = c(0, 1),
            border = NA,
            main = paste("Кластер", i),
            names.arg = rep("", ncol(df_norm))) 
  }
  par(mar = c(0, 0, 0, 0))
  plot.new()
  legend("left",
         legend = colnames(df_norm),
         fill = param_colors,
         xpd = NA,
         bty = "n",
         cex = 0.8)
  
  layout(1)
}

plot_cluster_boxplots <- function(clust_obj, df_norm, k) {
  while (dev.cur() > 1) dev.off() 
  dev.new(width = 12, height = 8) 
  cluster_cut <- cutree(clust_obj, k = k)
  n_cols <- ncol(df_norm)
  n_rows <- ceiling(n_cols / 3) 
  par(mfrow = c(n_rows, 3), mar = c(3, 2, 2, 1), oma = c(0, 0, 1, 0)) 
  for (i in 1:n_cols) {
    boxplot(df_norm[, i] ~ cluster_cut,
            main = colnames(df_norm)[i],
            ylab = "Нормированное значение",
            xlab = "Кластеры")
  }
  mtext(paste("Распределение показателей по", k, "кластерам"), outer = TRUE, cex = 1, font = 2)
  par(mfrow = c(1, 1)) 
}
plot_cluster_profiles(clust.datas, df_sc_norm, k = 3)
plot_cluster_boxplots(clust.datas, df_sc_norm, k = 3)


# 6. Выполнить кластеризацию датасета по k-means
library(ggplot2)
library(stats)
set.seed(123)
kmeans_3 <- kmeans(df_sc_norm, centers = 3, nstart = 25)
pca_result <- prcomp(df_sc_norm, scale. = TRUE)
df_pca <- data.frame(pca_result$x[, 1:2], Cluster = factor(kmeans_3$cluster))
p <- ggplot(df_pca, aes(x = PC1, y = PC2, color = Cluster)) +
  geom_point(size = 2) +
  stat_ellipse(aes(fill = Cluster), geom = "polygon", alpha = 0.2, show.legend = FALSE) +  # Эллипсы плотности
  scale_color_brewer(palette = "Set2") +
  scale_fill_brewer(palette = "Set2") +
  ggtitle("K-means кластеризация (k = 3)") +
  xlab(NULL) + 
  ylab(NULL) + 
  theme_minimal() +
  guides(color = guide_legend(title = "cluster"))
print(p)
split(row.names(df_sc_norm), kmeans_3$cluster)

# 7. Выполнить построение scatterplot с помощью функций plot или pairs.
plot_cluster_pairs <- function(kmeans, k) {
  cluster_cut <- kmeans$cluster
  pairs(df_sc_norm,
        col = cluster_cut,
        pch = 19,
        main = paste("Диаграммы рассеяния с цветами кластеров (k =", k, ")"))
}
plot_cluster_pairs(kmeans_3, 3)

# 8.	Построить трехмерную кластеризацию по scatterplot3d.
plot_3d_clusters <- function(df_norm, kmeans_result, k) {
  x <- df_norm[, 1]
  y <- df_norm[, 2]
  z <- df_norm[, 3]
  cluster_palette <- rainbow(k)
  point_colors <- cluster_palette[kmeans_result$cluster]
  scatterplot3d(x, y, z,
                color = point_colors,
                pch = 19,
                main = paste("3D кластеризация (k =", k, ")"),
                xlab = colnames(df_norm)[1],
                ylab = colnames(df_norm)[2],
                zlab = colnames(df_norm)[3])
  legend("topright",
         legend = paste("Кластер", 1:k),
         col = cluster_palette,
         pch = 19,
         cex = 0.7,
         inset = -0.09, 
         xpd = TRUE,
         box.lty = 0)
}

plot_3d_clusters(df_sc_norm, kmeans_3, k = 3)