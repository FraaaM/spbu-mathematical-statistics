if(!require(glmnet)) install.packages("glmnet")
if(!require(car)) install.packages("car")
if(!require(quantreg)) install.packages("quantreg")

library(glmnet)
library(car)
library(quantreg)

df <- gold_price_regression
colnames(df) <- c("X", "date", "eur_usd", "silver_high", "silver_volume",
                  "oil_high", "palladium_high", "gold_high")

df$eur_usd <- as.numeric(df$eur_usd)
df$silver_high <- as.numeric(df$silver_high)
df$silver_volume <- as.numeric(df$silver_volume)
df$oil_high <- as.numeric(df$oil_high)
df$palladium_high <- as.numeric(df$palladium_high)
df$gold_high <- as.numeric(df$gold_high)

# Создание чистого датасета
data <- na.omit(df[, c("eur_usd", "silver_high", "silver_volume", 
                       "oil_high", "palladium_high", "gold_high")])

# ==============================================================================

#1. РИДЖ РЕГРЕССИЯ (Ridge Regression)
cat("\n--- 1. Построение Ридж регрессии ---\n")

x_matrix <- as.matrix(data[, -which(names(data) == "gold_high")]) # Предикторы
y_vector <- data$gold_high # Таргет

# Построение модели (alpha = 0 для Ridge, alpha = 1 для Lasso)
ridge_model <- glmnet(x_matrix, y_vector, alpha = 0)

# A. Поиск оптимального параметра k (lambda) с помощью кросс-валидации (CV plot)
cv_ridge <- cv.glmnet(x_matrix, y_vector, alpha = 0)

# График зависимости ошибки от логарифма лямбды (аналог bias/variance trade-off)
par(mfrow = c(1, 2))
plot(ridge_model, xvar = "lambda", label = TRUE, main = "Ridge Trace (Coeff vs Lambda)")
plot(cv_ridge, main = "CV Plot (MSE vs Lambda)")
par(mfrow = c(1, 1))

best_lambda <- cv_ridge$lambda.min
cat("Оптимальное значение Lambda (k):", best_lambda, "\n")

# 2. ПРОВЕРКА МУЛЬТИКОЛЛИНЕАРНОСТИ (VIF)
cat("\n--- 2. Проверка мультиколлинеарности (VIF) ---\n")
# VIF считается на обычной линейной модели (OLS)
ols_model <- lm(gold_high ~ ., data = data)
vif_values <- vif(ols_model)
print(vif_values)

if(any(vif_values > 10)) {
  cat("Вывод: Присутствует сильная мультиколлинеарность (VIF > 10).\n")
  cat("Ридж регрессия здесь обоснована, так как она борется с мультиколлинеарностью.\n")
} else {
  cat("Вывод: Мультиколлинеарность не критична.\n")
}

# 3. ЗНАЧИМОСТЬ УРАВНЕНИЯ РЕГРЕССИИ (OLS)
cat("\n--- 3. Значимость уравнения регрессии ---\n")
# Для Ридж-регрессии классических p-values нет, проверяем значимость базовой OLS модели
summary_ols <- summary(ols_model)
f_stat <- summary_ols$fstatistic
p_val_f <- pf(f_stat[1], f_stat[2], f_stat[3], lower.tail = FALSE)

cat("F-statistic:", f_stat[1], "\n")
cat("p-value:", p_val_f, "\n")
if(p_val_f < 0.05) cat("Модель в целом значима.\n")

# 4. СРАВНИТЕЛЬНЫЙ АНАЛИЗ (OLS vs RIDGE)
cat("\n--- 4. Сравнение OLS и Ridge ---\n")

# Коэффициенты OLS
coef_ols <- coef(ols_model)

# Коэффициенты Ridge (при оптимальной lambda)
coef_ridge <- coef(cv_ridge, s = "lambda.min")

# Сводим в таблицу
comparison_table <- data.frame(
  Variable = rownames(coef_ridge),
  OLS_Coef = as.numeric(coef_ols),
  Ridge_Coef = as.numeric(coef_ridge)
)
print(comparison_table)

cat("\nИнтерпретация: Коэффициенты Ridge обычно меньше по модулю (стянуты к нулю) для уменьшения дисперсии оценки.\n")

# 5. КВАНТИЛЬНАЯ РЕГРЕССИЯ
cat("\n--- 5. Квантильная регрессия (taus = 0.1, 0.25, 0.5, 0.75, 0.9) ---\n")
taus <- c(0.1, 0.25, 0.5, 0.75, 0.9)
rq_models <- rq(gold_high ~ ., data = data, tau = taus)

print(summary(rq_models))

# 6. ГРАФИКИ РЕЗУЛЬТАТОВ КВАНТИЛЬНОЙ РЕГРЕССИИ
cat("\n--- 6. Графики изменения коэффициентов от квантиля ---\n")
# Этот график показывает, как меняется влияние предикторов для дешевого и дорогого золота
plot(summary(rq_models), parm = "silver_high", main = "Effect of Silver on Gold across Quantiles")
# Можно построить для всех переменных сразу:
plot(summary(rq_models))

# 7. СРАВНЕНИЕ ЛИНЕЙНОЙ (OLS) И МЕДИАННОЙ РЕГРЕССИИ
cat("\n--- 7. Сравнение OLS (Mean) и Median Regression (Tau=0.5) ---\n")
# Медианная регрессия (Robust, устойчива к выбросам)
rq_median <- rq(gold_high ~ ., data = data, tau = 0.5)

cat("=== OLS (Mean) Model ===\n")
print(coef(ols_model))

cat("\n=== Median (Quantile 0.5) Model ===\n")
print(coef(rq_median))

# Оценка качества (например, MAE - средняя абсолютная ошибка)
mae_ols <- mean(abs(residuals(ols_model)))
mae_med <- mean(abs(residuals(rq_median))) # Для rq residuals извлекаются чуть иначе
mae_med_calc <- mean(abs(data$gold_high - predict(rq_median, data)))

cat("\nMAE OLS:", mae_ols, "\n")
cat("MAE Median:", mae_med_calc, "\n")

if(mae_med_calc < mae_ols) {
  cat("Медианная регрессия дает меньшую ошибку по модулю (лучше работает с выбросами).\n")
}