library(ggplot2)
library(MASS)      
library(car)       
library(lmtest)    
library(corrplot)  

df <- gold_price_regression
colnames(df) <- c("X", "date", "eur_usd", "silver_high", "silver_volume",
                  "oil_high", "palladium_high", "gold_high")

df$eur_usd <- as.numeric(df$eur_usd)
df$silver_high <- as.numeric(df$silver_high)
df$silver_volume <- as.numeric(df$silver_volume)
df$oil_high <- as.numeric(df$oil_high)
df$palladium_high <- as.numeric(df$palladium_high)
df$gold_high <- as.numeric(df$gold_high)

data <- na.omit(df[, c("eur_usd", "silver_high", "silver_volume", 
                           "oil_high", "palladium_high", "gold_high")])

# ==============================================================================
# a. Провести корреляционный анализ имеющихся данных
cat("\n--- a. Корреляционный анализ ---\n")
cor_matrix <- cor(data)
print(round(cor_matrix, 2))

# Визуализация
corrplot(cor_matrix, method = "number", type = "upper", tl.col = "black")
plot(data)


# b. Построить базовую модель линейной регрессии
cat("\n--- b. Базовая модель линейной регрессии ---\n")
# Зависимая переменная: gold_high, Предикторы: все остальные (.)
fit <- lm(gold_high ~ ., data = data)


# c. Вывести результаты анализа базовой модели
cat("\n--- c. Результаты анализа базовой модели (Summary) ---\n")
model_summary <- summary(fit)
print(model_summary)


# d. Записать уравнение линейной регрессии
cat("\n--- d. Уравнение регрессии ---\n")
coefs <- coef(fit)
cat("Gold_High =", round(coefs[1], 2), 
    "+", round(coefs[2], 2), "* eur_usd", 
    "+", round(coefs[3], 2), "* silver_high", 
    "+", round(coefs[4], 5), "* silver_volume", 
    "+", round(coefs[5], 2), "* oil_high", 
    "+", round(coefs[6], 2), "* palladium_high\n")


# e. Проверить значимость каждого отдельного коэффициента с помощью T-test
# Если P < 0.05, мы отвергаем нулевую гипотезу (гипотезу о том, что переменная не влияет)
cat("\n--- e. Значимость коэффициентов (T-test) ---\n")
print(model_summary$coefficients[, "Pr(>|t|)"])
cat("Если Pr(>|t|) < 0.05, коэффициент значим.\n")


# f. Проверить значимость уравнения регрессии с помощью F-test
cat("\n--- f. Значимость уравнения (F-test) ---\n")
f_stat <- model_summary$fstatistic
p_value_f <- pf(f_stat[1], f_stat[2], f_stat[3], lower.tail = FALSE)
cat("F-statistic:", f_stat[1], "\n")
cat("p-value:", p_value_f, "\n")
if(p_value_f < 0.05) cat("Модель значима в целом.\n") else cat("Модель не значима.\n")


# g. Построить график рассеяния и уравнения регрессии
# Т.к. регрессия множественная, строим график "Реальные vs Предсказанные значения"
cat("\n--- g. График Реальные vs Предсказанные значения ---\n")
plot(data$gold_high, fitted(fit),
     main = "Actual vs Fitted (Predicted)",
     xlab = "Actual Gold Price", ylab = "Predicted Gold Price",
     col = "lightblue", pch = 16)
abline(0, 1, col = "black", lwd = 2) # Линия идеального предсказания


# h. Построить доверительные интервалы для коэффициентов
cat("\n--- h. 95% Доверительные интервалы для коэффициентов ---\n")
print(confint(fit))


# i. Проверка важных наблюдений (influential observations) - Расстояние Кука
cat("\n--- i. Проверка влиятельных наблюдений (Cook's Distance) ---\n")
cooksd <- cooks.distance(fit)
# График расстояния Кука
plot(cooksd, pch="*", cex=2, main="Influential Obs by Cooks distance")
abline(h = 4/nrow(data), col="red")  # Пороговое значение
#text(x=1:length(cooksd)+1, y=cooksd, labels=ifelse(cooksd>4/nrow(data), names(cooksd),""), col="blue")

# Вывод индексов влиятельных наблюдений
influential <- as.numeric(names(cooksd)[(cooksd > 4/nrow(data))])
cat("Количество влиятельных наблюдений:", length(influential), "( Превосходят", 4/nrow(data), ')', "\n")


# j. Улучшение модели с помощью StepAIC
cat("\n--- j. Улучшение модели (StepAIC) ---\n")
fit_step <- stepAIC(fit, direction = "both", trace = FALSE)
print(summary(fit_step))


# k. Повторный анализ (если модель изменилась)
if(!identical(formula(fit), formula(fit_step))) {
  cat("\n--- k. Модель изменилась. Результаты новой модели ---\n")
  print(summary(fit_step))
  fit_final <- fit_step
} else {
  cat("\n--- k. StepAIC оставил модель без изменений. Используем базовую. ---\n")
  fit_final <- fit
}


# l. Построить диагностические графики и дать интерпретацию
cat("\n--- l. Диагностические графики ---\n")
par(mfrow = c(2, 2))
plot(fit_final)
par(mfrow = c(1, 1))
cat("ИНТЕРПРЕТАЦИЯ ГРАФИКОВ:")
cat("1. Residuals vs Fitted: Проверка линейности. Линия должна быть горизонтальной. Если есть изгиб - нужна нелинейная модель.")
cat("2. Normal Q-Q: Проверка нормальности остатков. Точки должны лежать на диагонали.")
cat("3. Scale-Location: Проверка гомоскедастичности (постоянства дисперсии). Линия должна быть горизонтальной.")
cat("4. Residuals vs Leverage: Поиск влиятельных выбросов.\n")


# m. Проверить модель на наличие выбросов (тест Бонферрони)
cat("\n--- m. Тест на выбросы (outlierTest) ---\n")
outlier_test <- outlierTest(fit_final)
print(outlier_test)


# n. Проверить модель на гетероскедастичность (Breusch-Pa gan test)
cat("\n--- n. Тест Бреуша-Пагана на гетероскедастичность ---\n")
bp_test <- bptest(fit_final)
print(bp_test)
if(bp_test$p.value < 0.05) cat("H0 отвергается: Присутствует гетероскедастичность (дисперсия остатков меняется).\n") else cat("Гетероскедастичности нет.\n")


# o. Проверить остатки на автокорреляцию (Durbin-Watson test)
cat("\n--- o. Тест Дарбина-Уотсона на автокорреляцию ---\n")
dw_test <- dwtest(fit_final)
print(dw_test)
cat("DW статистика близка к 2 = нет автокорреляции. Ближе к 0 = положительная, к 4 = отрицательная.\n")


# p. Проверить остатки на нормальность (Shapiro-Wilk)
cat("\n--- p. Тест Шапиро-Уилка на нормальность остатков ---\n")
residuals_final <- residuals(fit_final)
print(shapiro.test(residuals_final))
cat("Если p < 0.05, остатки распределены НЕ нормально.\n")


# q. Проверить модель на мультиколлинеарность (VIF)
cat("\n--- q. Проверка мультиколлинеарности (VIF) ---\n")
vif_vals <- vif(fit_final)
print(vif_vals)
cat("VIF = 1: Идеал. Переменная абсолютно уникальна и вообще не зависит от других.")
cat("VIF > 5 или 10 указывает на сильную мультиколлинеарность.\n")


# r. Трансформация Box-Cox
cat("\n--- r. Трансформация Box-Cox ---\n")
# Находим оптимальную лямбду
bc <- boxcox(fit_final, plotit = TRUE)
lambda <- bc$x[which.max(bc$y)]
cat("Оптимальная Lambda:", lambda, "\n")

# Создаем трансформированную переменную
# Формула трансформации: (y^lambda - 1) / lambda (если lambda != 0)
# Для простоты часто используют y^lambda
data$gold_boxcox <- (data$gold_high^lambda - 1) / lambda


# s. Анализ новой модели (с Box-Cox)
cat("\n--- s. Новая модель с трансформацией Box-Cox ---\n")
# Обновляем формулу, заменяя gold_high на gold_boxcox
new_formula <- as.formula(paste("gold_boxcox ~", paste(names(coef(fit_final))[-1], collapse = " + ")))

fit_bc <- lm(new_formula, data = data)
print(summary(fit_bc))

# Краткая диагностика новой модели
cat("--- Сравнение R-squared ---\n")
cat("Original Adjusted R2:", summary(fit_final)$adj.r.squared, "\n")
cat("Box-Cox Adjusted R2: ", summary(fit_bc)$adj.r.squared, "\n")

cat("--- Проверка нормальности остатков новой модели ---\n")
if(length(residuals(fit_bc)) > 5000) {
  print(shapiro.test(sample(residuals(fit_bc), 5000)))
} else {
  print(shapiro.test(residuals(fit_bc)))
}