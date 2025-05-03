#5.2

# разделить его на две части (обучающую и тестовую)
data_2_new <- data.frame(cbind(df_sc_norm, factor(kmeans_3$cluster)))
colnames(data_2_new)[ncol(data_2_new)] <- "groups"
data_2_new$groups <- factor(data_2_new$groups)
set.seed(123)
ind <- sample(2, nrow(data_2_new), replace = TRUE, prob = c(0.7, 0.3))
trainData <- data_2_new[ind == 1, ] 
testData <- data_2_new[ind == 2, ] 


# 2. Решить задачу с помощью наивного Байесовского классификатора
library(klaR)
naive_model <- NaiveBayes(groups ~ ., data = trainData)
naive_model$tables
summary(trainData)
par(mfrow = c(2, 3), mar = c(3,3,2,1), oma = c(0,0,0,0))

# Автоматическое построение графиков без легенд
plot(naive_model, legendplot = FALSE)

# Добавление общей легенды
legend("bottomleft", 
       legend = levels(trainData$groups),
       col = 1:length(levels(trainData$groups)),
       lty = 1,
       cex = 0.8,
       bty = "n")

pred <- predict(naive_model, testData)$class
conf_matrix <- table(Факт = testData$groups, Прогноз = pred)
print(conf_matrix)


# 3.Применить метод деревьев решений для задачи классификации
library(party)
tree_ctree <- ctree(groups ~ ., data = trainData)
plot(tree_ctree, main = "Дерево решений для кластеров")

# Предсказания на тестовых данных
ctree_pred <- predict(tree_ctree, newdata = testData)

# Матрица ошибок
ctree_conf_matrix <- table(Факт = testData$groups, Прогноз = ctree_pred)
print(ctree_conf_matrix)

# Точность классификации
ctree_accuracy <- sum(diag(ctree_conf_matrix)) / sum(ctree_conf_matrix)
cat("Точность дерева решений (ctree):", round(ctree_accuracy, 3), "\n")

# Сравнение с наивным байесом
nb_accuracy <- sum(diag(conf_matrix)) / sum(conf_matrix)
cat("Точность наивного байеса:", round(nb_accuracy, 3), "\n")



# 4. Выполнить классификацию с помощью случайного леса
library(randomForest)
rf_model <- randomForest(groups ~ ., data = trainData, ntree = 100, importance = TRUE)
print(rf_model)

# Предсказание
rf_pred <- predict(rf_model, testData)

# Матрица ошибок и точность
conf_matrix <- table(Predicted = rf_pred, Actual = testData$groups)
print(conf_matrix)
accuracy <- sum(diag(conf_matrix)) / sum(conf_matrix)
cat("Точность модели случайного леса:", round(accuracy, 3), "\n")

# Важность признаков
print(importance(rf_model))
varImpPlot(rf_model, main = "Важность признаков (Random Forest)")
