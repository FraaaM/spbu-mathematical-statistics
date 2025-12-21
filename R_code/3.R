# 254 аудитория 3-4 пара
# --- 1. Подготовка данных ---
df <- gold_price_regression
colnames(df) <- c("X", "date", "eur_usd", "silver_high", "silver_volume",
                  "oil_high", "palladium_high", "gold_high")

# Преобразование типов
df$gold_high <- as.numeric(df$gold_high)
df$silver_volume <- as.numeric(df$silver_volume)
df$oil_high <- as.numeric(df$oil_high)

df$date_parsed <- as.Date(df$date)
df$month_num <- as.numeric(format(df$date_parsed, "%m"))

# --- Определение периодов ---
first_half_idx <- df$month_num %in% 1:6
second_half_idx <- df$month_num %in% 7:12

summer_months <- c(6, 7, 8)    
winter_months <- c(12, 1, 2)   
spring_months <- c(3, 4, 5)    
autumn_months <- c(9, 10, 11) 

# --- ГИПОТЕЗА 1: Сравнение средних цен Золота ---
gold_first <- df$gold_high[first_half_idx]
gold_second <- df$gold_high[second_half_idx]

cat("\n=== Проверка нормальности (Gold High) ===\n")
print(shapiro.test(df$gold_high))

# Так как распределение не нормальное, используем тест Уилкоксона (Манна-Уитни)
w_test_gold <- wilcox.test(gold_first, gold_second)

cat("\n=== Гипотеза 1. H0: Распределения цен Gold в 1-й и 2-й половине года идентичны ===\n")
print(w_test_gold)

if (w_test_gold$p.value > 0.05) {
  cat("ВЫВОД: Не отвергаем H0. Значимой разницы нет.\n")
} else {
  cat("ВЫВОД: Отвергаем H0. Цена золота значимо различается в разных половинах года.\n")
}

# --- ГИПОТЕЗА 2: Сравнение среднего Объема торгов Серебром ---
vol_first <- df$silver_volume[first_half_idx]
vol_second <- df$silver_volume[second_half_idx]

cat("\n\n=== Проверка нормальности (Silver Volume) ===\n")
print(shapiro.test(df$silver_volume))

# Используем тест Уилкоксона
w_test_vol <- wilcox.test(vol_first, vol_second)

cat("\n=== Гипотеза 2. H0: Объем торгов Silver в 1-й и 2-й половине года идентичен ===\n")
print(w_test_vol)

if (w_test_vol$p.value > 0.05) {
  cat("ВЫВОД: Не отвергаем H0. Активность торгов одинакова.\n")
} else {
  cat("ВЫВОД: Отвергаем H0. Активность торгов значимо меняется.\n")
}


# --- ГИПОТЕЗА 3: Сравнение дисперсии Золота Летом и Зимой ---
gold_summer <- df$gold_high[df$month_num %in% summer_months]
gold_winter <- df$gold_high[df$month_num %in% winter_months]

# Используем тест Флигнера-Киллина (аналог F-теста для ненормальных данных)
fligner_gold <- fligner.test(list(gold_summer, gold_winter))

cat("\n\n=== Гипотеза 3. H0: Дисперсия цены Gold летом = зимой ===\n")
print(fligner_gold)

if (fligner_gold$p.value > 0.05) {
  cat("ВЫВОД: Не отвергаем H0. Волатильность одинакова.\n")
} else {
  cat("ВЫВОД: Отвергаем H0. Волатильность значимо отличается.\n")
}


# --- ГИПОТЕЗА 4: Сравнение дисперсии Нефти Весной и Осенью ---
oil_spring <- df$oil_high[df$month_num %in% spring_months]
oil_autumn <- df$oil_high[df$month_num %in% autumn_months]

cat("\n\n=== Проверка нормальности (Oil High) ===\n")
print(shapiro.test(df$oil_high))

# Используем тест Флигнера-Киллина
fligner_oil <- fligner.test(list(oil_spring, oil_autumn))

cat("\n=== Гипотеза 4. H0: Дисперсия цены Oil весной = осенью ===\n")
print(fligner_oil)

if (fligner_oil$p.value > 0.05) {
  cat("ВЫВОД: Не отвергаем H0. Дисперсии не отличаются.\n")
} else {
  cat("ВЫВОД: Отвергаем H0. Дисперсии значимо отличаются.\n")
}