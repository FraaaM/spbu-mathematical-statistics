# --- 1. ПОДГОТОВКА ДАННЫХ ---
df <- gold_price_regression
colnames(df) <- c("X", "date", "eur_usd", "silver_high", "silver_volume",
                  "oil_high", "palladium_high", "gold_high")

# Преобразование числовых переменных
df$eur_usd <- as.numeric(df$eur_usd)
df$silver_high <- as.numeric(df$silver_high)
df$silver_volume <- as.numeric(df$silver_volume)
df$oil_high <- as.numeric(df$oil_high)
df$palladium_high <- as.numeric(df$palladium_high)
df$gold_high <- as.numeric(df$gold_high)
df$oil_high <- as.numeric(df$oil_high)

df$date <- as.Date(df$date, format = "%Y-%m-%d") 
df$month_num <- as.numeric(format(df$date, "%m"))

# Выбираем основную переменную для сезонного анализа 
target_var <- df$gold_high 
target_name <- "Gold High Price"

# --- 2. ГРУППИРОВКА ПО СЕЗОНАМ ---
# Индексы (используем номер месяца)
all_idx <- !is.na(target_var)

# Лето: Июнь(6), Июль(7), Август(8)
summer_idx <- df$month_num %in% c(6, 7, 8) & !is.na(target_var)

# Осень: Сентябрь(9), Октябрь(10), Ноябрь(11)
autumn_idx <- df$month_num %in% c(9, 10, 11) & !is.na(target_var)

# Зима: Декабрь(12), Январь(1), Февраль(2)
winter_idx <- df$month_num %in% c(12, 1, 2) & !is.na(target_var)

# Весна: Март(3), Апрель(4), Май(5)
spring_idx <- df$month_num %in% c(3, 4, 5) & !is.na(target_var)

#print(length(df$gold_high))
#no_nan <- df$gold_high[!is.na(df$gold_high)]
#print(length(no_nan))

# --- 3. ФУНКЦИЯ АНАЛИЗА ---
check_normal <- function(x, name) {
  x <- x[!is.na(x)]
  n <- length(x)
  m <- mean(x)
  s <- sd(x)
  
  cat("\n\n====", name, "====\n")
  cat("N =", n, "\n")
  cat("mean =", round(m, 6), " sd =", round(s, 6), "\n")
  
  # Shapiro-Wilk тест (проверяет нормальность)
  sh <- shapiro.test(x)
  cat("Shapiro: W =", round(sh$statistic,6), " p =", signif(sh$p.value,6), "\n")
  
  par(mfrow = c(1, 2))
  
  # Гистограмма и QQ-plot
  hist(x, main = paste("Hist:", name), xlab = "Цена", col = "lightblue", prob = TRUE)
  lines(density(x), col = "black", lwd = 2)
  
  qqnorm(x, main = paste("QQ-plot:", name))
  qqline(x, col = "red", lwd = 2)
  
  # Уровень значимости
  alpha <- 0.05
  
  # Доверительный интервал для среднего (μ)
  t_val <- qt(1 - alpha/2, df = n - 1)
  ci_mean <- c(m - t_val * s/sqrt(n), m + t_val * s/sqrt(n))
  
  # Доверительный интервал для стандартного отклонения (σ)
  chi_low <- qchisq(1 - alpha/2, df = n - 1)
  chi_high <- qchisq(alpha/2, df = n - 1)
  ci_sd <- c(sqrt((n - 1)*s^2 / chi_low), sqrt((n - 1)*s^2 / chi_high))
  
  cat("95% CI for mean:", round(ci_mean, 4), "\n")
  cat("95% CI for sd:", round(ci_sd, 4), "\n")
  
  par(mfrow = c(1, 1)) 
}

# --- 4. ЗАПУСК ПО ГРУППАМ ---
# Запускаем анализ для золота (Gold High)
check_normal(target_var[all_idx], paste(target_name, "(All)"))
check_normal(target_var[summer_idx], paste(target_name, "(Summer)"))
check_normal(target_var[autumn_idx], paste(target_name, "(Autumn)"))
check_normal(target_var[winter_idx], paste(target_name, "(Winter)"))
check_normal(target_var[spring_idx], paste(target_name, "(Spring)"))

# Проверка на нормальность silver_volume и oil_high
check_normal(df$silver_volume, "silver_volume")
check_normal(df$oil_high, "oil_high")

# --- 5. ПРОВЕРКА НА ЭКСПОНЕНЦИАЛЬНОЕ РАСПРЕДЕЛЕНИЕ ---
# берем silver_volume (объемы часто асимметричны)

vol_val <- df$silver_volume[!is.na(df$silver_volume)]
n_vol <- length(vol_val)
mean_vol <- mean(vol_val)
lambda_vol <- 1 / mean_vol  

# Добавление небольшого шума (jitter) для устранения совпадающих значений
#set.seed(123)
#vol_val_jitter <- vol_val + runif(n_vol, -0.1, 0.1)
vol_val_jitter <- vol_val

# Kolmogorov-Smirnov тест
# Проверяем гипотезу H0, что объем торгов распределен экспоненциально
ks_vol <- ks.test(vol_val_jitter, "pexp", rate = lambda_vol)

cat("\n\n==== Проверка Silver Volume на экспоненциальное распределение ====\n")
cat("N =", n_vol, "\n")
cat("mean =", round(mean_vol, 6), " lambda =", round(lambda_vol, 6), "\n")
cat("KS: D =", round(ks_vol$statistic, 6), " p =", signif(ks_vol$p.value, 6), "\n")

# Если p-value < 0.05, гипотеза об экспоненциальном распределении отвергается - H1.

# Визуализация проверки
par(mfrow = c(1, 2))

# 1. Гистограмма с наложенной теоретической экспоненциальной кривой
hist(vol_val_jitter, 
     main = "Hist: Silver Volume", 
     prob = TRUE, 
     xlab = "Volume", 
     col = "lightblue",
     breaks = 30)
curve(dexp(x, rate = lambda_vol), add = TRUE, col = "black", lwd = 2)

# 2. QQ-plot (Экспоненциальный)
qqplot(qexp(ppoints(vol_val_jitter), ratez = lambda_vol), vol_val_jitter, 
       main = "QQ-plot: Silver Volume (Exp)", 
       xlab = "Theoretical Exp Quantiles", 
       ylab = "Sample Quantiles")
abline(0, 1, col = "blue")

par(mfrow = c(1, 1)) # Сброс настроек графики