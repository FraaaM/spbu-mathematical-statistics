#install.packages(c("dplyr", "caret", "MASS", "lmtest", "pROC", "e1071", "aod"))

library(dplyr)             
library(caret)             
library(MASS)             
library(lmtest)            
library(pROC) 
library(aod)              

df <- gold_binary_regression
colnames(df) <- c("date", "silver_open", "silver_close", "oil_open", "oil_close", 
                  "nasdaq_open", "nasdaq_close", "sp500_open", "sp500_close", 
                  "gold_trend")

df$silver_open <- as.numeric(df$silver_open)
df$silver_close <- as.numeric(df$silver_close)
df$oil_open <- as.numeric(df$oil_open)
df$oil_close <- as.numeric(df$oil_close)
df$nasdaq_open <- as.numeric(df$nasdaq_open)
df$nasdaq_close <- as.numeric(df$nasdaq_close)
df$sp500_open <- as.numeric(df$sp500_open)
df$sp500_close <- as.numeric(df$sp500_close)
df$gold_trend <- as.numeric(df$gold_trend)


df <- na.omit(df[, c("silver_open", "silver_close", "oil_open", "oil_close", 
                       "nasdaq_open", "nasdaq_close", "sp500_open", "sp500_close", 
                       "gold_trend")])
str(df)

# Тренировочная выборка (75% данных) 
set.seed(123) 
train_data <- df %>% sample_frac(0.75)
test_data <- anti_join(df, train_data, by = names(df)) 

cat("Размер тренировочной выборки:", nrow(train_data), "\n")
cat("Размер тестовой выборки:", nrow(test_data), "\n")

# ==============================================================================
# 1) Описательная статистика
cat("\n--- 1. Описательная статистика ---\n")
summary(train_data)


# 2) Базовая модель логистической регрессии (Logit)
cat("\n--- 2. Базовая модель (Logit) ---\n")
# family = binomial(link = "logit") указывает на логистическую регрессию
logit_model <- glm(gold_trend ~ ., data = train_data, family = binomial(link = "logit"))
print(summary(logit_model))


# 3) Уравнение бинарной регрессии
cat("\n--- 3. Уравнение регрессии ---\n")
coefs <- coef(logit_model)
cat("P(gold_trend=1) = 1 / (1 + exp(-z)), где z = \n")
cat(round(coefs[1], 3), 
    "+", round(coefs[2], 3), "* silver_trend",
    "+", round(coefs[3], 3), "* oil_trend",
    "+", round(coefs[4], 3), "* nasdaq_trend",
    "+", round(coefs[5], 3), "* sp500_trend",
    "+", round(coefs[6], 3), "* palladium_trend\n")


# 4) Протестировать значимость коэффициентов регрессии в отдельности
cat("\n--- 4. Значимость отдельных коэффициентов (z-test) ---\n")
# Это выводится в summary, столбец Pr(>|z|)
print(summary(logit_model)$coefficients)
cat("\nЕсли Pr(>|z|) < 0.05, предиктор статистически значим.\n")


# 5) Проверить значимость регрессии в целом (Wald & Likelihood Ratio)
cat("\n--- 5. Значимость модели в целом ---\n")

# A. Тест отношения правдоподобия (Likelihood Ratio Test - LRT)
# Сравниваем нашу модель с "Null model" (модель только с интерсептом)
null_model <- glm(gold_trend ~ 1, data = train_data, family = binomial)
lrt_res <- lrtest(logit_model, null_model)
print(lrt_res)
cat("LRT p-value:", lrt_res$`Pr(>Chisq)`[2], "\n")

# B. Критерий Вальда (глобальный)
# Проверяем гипотезу, что все коэффициенты (кроме Intercept) равны 0
wald_res <- wald.test(b = coef(logit_model), Sigma = vcov(logit_model), Terms = 2:length(coefs))
print(wald_res)


# 6) Доверительные интервалы для коэффициентов
cat("\n--- 6. Доверительные интервалы ---\n")
cat("На основе профиля правдоподобия (confint):\n")
print(confint(logit_model))

cat("\nНа основе стандартных ошибок (confint.default):\n")
print(confint.default(logit_model))


# 7) Сравнительный анализ логит и пробит моделей
cat("\n--- 7. Логит vs Пробит ---\n")
probit_model <- glm(gold_trend ~ ., data = train_data, family = binomial(link = "probit"))

cat("AIC Logit:", AIC(logit_model), "\n")
cat("AIC Probit:", AIC(probit_model), "\n")
if(AIC(logit_model) < AIC(probit_model)) {
  cat("Вывод: Логит-модель лучше (меньше AIC).\n")
} else {
  cat("Вывод: Пробит-модель лучше (меньше AIC).\n")
}


# 8) Таблица сопряженности (Cutoff = 0.5)
cat("\n--- 8. Таблица сопряженности (Cutoff = 0.5) ---\n")
# Предсказание вероятностей на ТЕСТОВОЙ выборке
pred_probs <- predict(logit_model, newdata = test_data, type = "response")

# Превращаем вероятности в классы 0 и 1 с порогом 0.5
pred_class_05 <- ifelse(pred_probs > 0.5, 1, 0)

# Для caret нужны факторы
confusion_05 <- confusionMatrix(data = as.factor(pred_class_05), 
                                reference = as.factor(test_data$gold_trend), 
                                positive = "1")
print(confusion_05$table)


# 9) Специфичность и Чувствительность (из confusionMatrix)
cat("\n--- 9. Метрики (Cutoff = 0.5) ---\n")
# Sensitivity (Чувствительность): способность верно предсказать 1 (рост)
# Specificity (Специфичность): способность верно предсказать 0 (падение)
print(confusion_05$byClass[c("Sensitivity", "Specificity")])


# 10) Оптимальное пороговое значение 
cat("\n--- 10. Оптимальный порог ---\n")
roc_obj <- roc(test_data$gold_trend, pred_probs)

# 2. Находим оптимальный порог
# Метод "youden" ищет точку, где сумма (Чувствительность + Специфичность) максимальна
# ret="threshold" означает, что нам нужно вернуть именно значение порога
opt_cutoff_val <- coords(roc_obj, "best", best.method = "youden", ret = "threshold")
opt_cutoff <- as.numeric(opt_cutoff_val[1])
cat("Optimal Cutoff:", opt_cutoff, "\n")

# Строим матрицу ошибок с новым порогом
pred_class_opt <- ifelse(pred_probs > opt_cutoff, 1, 0)

confusion_opt <- confusionMatrix(as.factor(pred_class_opt), 
                                 as.factor(test_data$gold_trend), 
                                 positive = "1")

print(confusion_opt$table)
cat("\nМетрики для оптимального порога:\n")
print(confusion_opt$byClass[c("Sensitivity", "Specificity")])


# --- 11. ROC кривая (через пакет pROC) ---
cat("\n--- 11. ROC кривая ---\n")
roc_obj <- roc(test_data$gold_trend, pred_probs)

# print.auc=TRUE выведет значение площади под кривой прямо на графике
plot(roc_obj, 
     main = "ROC Curve", 
     col = "blue", 
     lwd = 2, 
     print.auc = TRUE, 
     legacy.axes = TRUE,     # Чтобы ось X шла от 0 до 1 (1-Specificty)
     xlab = "False Positive Rate (1 - Specificity)",
     ylab = "True Positive Rate (Sensitivity)")

cat("График построен.\n")
cat("AUC (Area Under Curve):", auc(roc_obj), "\n")


# 12) Улучшение модели (StepAIC)
cat("\n--- 12. Улучшение модели (StepAIC) ---\n")
# Используем stepAIC для отбора переменных (назад/вперед)
best_model <- stepAIC(logit_model, direction = "both", trace = 0) # trace=0 чтобы не спамить в консоль
print(summary(best_model))

cat("AIC начальной модели:", AIC(logit_model), "\n")
cat("AIC лучшей модели:", AIC(best_model), "\n")