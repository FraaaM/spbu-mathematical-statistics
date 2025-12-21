# Анализ переменных (по гистограмме и ящику с усами)
# eur_usd: двухгоровый с небольшим скосом в право, нет выбросов
# silver_high: скошено вправо, выбросы только после Q3+1,5×IQR (верхний ус)
# silver_volume: сильно скошено вправо (mean > median), выбросы только сверху
# oil_high: Трехгорбый тип, выбросы только сверху (после Q3+1,5×IQR)
# palladium_high: скошено вправо, выбросы только сверху
# gold_high: Двухгорбый тип со скосом в право, выбросы только сверху

# Загрузка данных
df <- gold_price_regression
colnames(df) <- c("X", "date", "eur_usd", "silver_high", "silver_volume",
                  "oil_high", "palladium_high", "gold_high")

# Выбор числовых переменных для анализа
numeric_vars <- c("eur_usd", "silver_high", "silver_volume", 
                  "oil_high", "palladium_high", "gold_high")
str(df)
head(df)

# Гистограммы и ящик с усами для каждой переменной
for(var in numeric_vars) {
  x <- as.numeric(df[[var]])  
  x <- x[!is.na(x)]           
  
  par(mfrow = c(1, 2))
  hist(x, 
       main = paste("Гистограмма:", var),
       xlab = var,
       col = "lightblue",
       border = "darkblue",
       probability = FALSE)

  boxplot(x,
          main = paste("Boxplot:", var),
          ylab = var,
          col = "lightblue",
          border = "darkblue",
          outcol = "black")
}

# Расчет статистик
if(!require(moments)) install.packages("moments")
library(moments)

calc_stats <- function(x) {
  x <- as.numeric(x)
  x <- na.omit(x)
  
  c(
    N = length(x),
    Mean = mean(x),
    Median = median(x),
    Max = max(x),
    Min = min(x),
    Q1 = as.numeric(quantile(x, 0.25)),
    Q2 = as.numeric(quantile(x, 0.50)),
    Q3 = as.numeric(quantile(x, 0.75)),
    IQR = IQR(x),
    Variance = var(x),
    SD = sd(x),
    Skewness = skewness(x),
    Kurtosis = kurtosis(x)
  )
}

stats_table <- sapply(df[, numeric_vars], calc_stats)
stats_table <- round(t(stats_table), 4)
cat('\n === Описательная статистика === \n')
print(stats_table)

# Матрица корреляций
df_numeric <- data.frame(lapply(df[, numeric_vars], as.numeric))
cor_matrix <- cor(df_numeric, use = "complete.obs")
cat("\n=== Матрица попарных корреляций ===\n")
print(round(cor_matrix, 4))

# Визуализация корреляций
if(!require(corrplot)) install.packages("corrplot")
library(corrplot)

par(mfrow = c(1, 1))
corrplot(cor_matrix, 
         method = "color",
         type = "upper",
         addCoef.col = "black",
         tl.col = "black",
         number.cex = 0.7,
         title = "Матрица корреляций",
         mar = c(0, 0, 2, 0))