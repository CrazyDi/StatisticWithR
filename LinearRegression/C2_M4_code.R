# Множественная линейная регрессия
# В.М. Хайтов, к.б.н.
# Биологический факультет, СПбГУ

# чтобы избежать "scientific notation"
options(scipen = 6, digits = 3)



## Мир сложнее, чем простая линейная регрессия   ----------------------

## Пример --- маркер рака простаты   ----------------------

## PSA --- маркер рака простаты
# Возможно несколько вариантов формулировки вопроса исследования:
#
# Можно ли судить о концентрации
# простат-специфичного антигена по клиническим
# параметрам?
# Коррелирует ли концентрация простат-специфичного
# антигена с клиническими параметрами?
#
# Данные: Stamey, et al. 1989
#
# Исследовано 97 пациентов, перенесших простатотомию.
# __Зависимая перменная__:
# `lpsa` --- логарифм концентрации простат-специфичного антигена
#
# __Предикторы__
# lcavol --- логарифм объема опухоли
# lweight --- логарифм веса
# age --- возраст пациента
# lbph --- логарифм степени доброкачественной гиперплазии
# svi --- поражение семенных пузырьков
# lcp --- логарифм меры поражения капсулы
# gleason --- оценка по шкале Глисона
# pgg45 --- доля оценок 4 и 5 по шкале Глисона




## Разведочный анализ в R ------------

## Читаем данные
library(ElemStatLearn)
data("prostate")
prost <- prostate[, -10]
head(prost)

## Смотрим на структуру данных
str(prost)

## Есть ли пропущенные значения?
colSums(is.na(prost))

## Ищем выбросы: данные для боксплотов
library(tidyr) # пакет для tidy данных
prost_long <- gather(prost)  # переводим в длинный формат
head(prost_long)

# То же самое при помощи конвейерного оператора `%>%`
prost_long <- prost %>% gather()

# Боксплоты
library(ggplot2)
theme_set(theme_bw())

ggplot(prost_long, aes(x = key, y = value)) +
  geom_boxplot() +
  labs(x = 'Переменные', y = 'Значения')

## Боксплоты после стандартизации

library(dplyr) # пакет для предобработки данных
prost_long <- prost %>% gather() %>% group_by(key) %>% mutate (scale_value = scale(value))

ggplot(prost_long, aes(x = key, y = scale_value)) +
  geom_boxplot() +
  labs(x = 'Переменные', y = 'Стандартизованные \nзначения')

## Ищем выбросы: точечные диаграммы Кливленда
ggplot(prost, aes(y = 1:nrow(prost), x = lcavol)) + geom_point() +
  labs(y = 'Порядковый номер \nв датасете', x = 'Значения переменной (lcavol)')

## Как выглядит выброс на диаграмме Кливленда?
set.seed(3289473)
prost2 <- prost
random_index <- sample(1:nrow(prost2), 1)
prost2$lcavol[random_index]  <- prost$lcavol[random_index] * 10 # Внесем случайное изменение

ggplot(prost2, aes(y = 1:nrow(prost), x = lcavol)) + geom_point() +
  labs(y = 'Порядковый номер \nв датасете', x = 'Значение переменной (lcavol)')

## Ищем выбросы: диаграммы Кливленда для всех переменных
gg_dot <- ggplot(prost, aes(y = 1:nrow(prost))) + geom_point() + ylab('index')
Pl1 <- gg_dot + aes(x = lpsa)
Pl2 <- gg_dot + aes(x = lweight)
Pl3 <- gg_dot + aes(x = age)
Pl4 <- gg_dot + aes(x = lbph)
Pl5 <- gg_dot + aes(x = svi)
Pl6 <- gg_dot + aes(x = lcp)
Pl7 <- gg_dot + aes(x = gleason)
Pl8 <- gg_dot + aes(x = pgg45)
Pl9 <- gg_dot + aes(x = lcavol)

library(cowplot) # пакет для группировки графиков
theme_set(theme_bw())

plot_grid(Pl1, Pl2, Pl3, Pl4, Pl5, Pl6,
          Pl7, Pl8, Pl9, ncol = 3, nrow = 3)


## 'Странные' предикторы
plot_grid(Pl5, Pl7, ncol = 2)

## Характер связи между зависимой переменной и предикторами
gg_cor <- ggplot(prost, aes(y = lpsa)) + geom_point() + geom_smooth(se = FALSE)
Pl1 <- gg_cor + aes(x = lcavol)
Pl2 <- gg_cor + aes(x = lweight)
Pl3 <- gg_cor + aes(x = age)
Pl4 <- gg_cor + aes(x = lbph)
Pl6 <- gg_cor + aes(x = lcp)
Pl8 <- gg_cor + aes(x =  pgg45)

plot_grid(Pl1, Pl2, Pl3, Pl4, Pl6, Pl8, ncol = 3)


## Модель множественной линейной регрессии и ее интерпретация   ----------------------

# Матрица корреляций
round(cor(prost[, -c(5, 7)]), 2) # без дискретных переменных svi и gleason
## Так нельзя!
## Нам нужно построить множественную регрессионную модель

## Множественная линейная модель в R
M_prost_1 <- lm(lpsa ~ lcavol + lweight + age + lbph +
                  lcp + pgg45, data = prost)

## Теперь мы можем записать уравнение множественной регрессии
coef(M_prost_1)

## Модельная матрица в множественной регрессии
X <- model.matrix(M_prost_1)
head(X)



## Мультиколлинеарность и другие условия применимости   ----------------------

## Проверяем на мультиколлинеарность построенную модель
library(car)
vif(M_prost_1)

## Удалим из модели избыточный предиктор
M_prost_2 <- update(M_prost_1, ~ . - lcp)
vif(M_prost_2)

## Данные для анализа остатков
M_prost_2_diag <- fortify(M_prost_2)
head(M_prost_2_diag)

## График расстояния Кука
ggplot(M_prost_2_diag, aes(x = 1:nrow(M_prost_2_diag), y = .cooksd)) +
  geom_bar(stat = 'identity') + coord_cartesian(ylim = c(0, 2)) +
  geom_hline(yintercept = 1, linetype = 2)

## График остатков от предсказанных значений
gg_resid <- ggplot(data = M_prost_2_diag, aes(x = .fitted, y = .stdresid)) +
  geom_point() + geom_hline(yintercept = 0) + geom_smooth()
gg_resid

## Графики зависимости остатков от предикторов в модели
res_1 <- gg_resid + aes(x = lcavol)
res_2 <- gg_resid + aes(x = lweight)
res_3 <- gg_resid + aes(x = age)
res_4 <- gg_resid + aes(x = lbph)
res_5 <- gg_resid + aes(x = pgg45)

plot_grid(res_1, res_2, res_3, res_4,
          res_5, ncol = 3)

## Графики остатков от переменных, не включенных в модель
res_6 <- gg_resid + aes(x = prost$lcp) + labs(x = 'lcp')
res_7 <- ggplot(M_prost_2_diag, aes(y = .stdresid)) +
  geom_boxplot(aes(x = factor(prost$svi))) + labs(x = 'svi')
res_8 <- ggplot(M_prost_2_diag, aes(y = .stdresid)) +
  geom_boxplot(aes(x = factor(prost$gleason))) + labs(x = 'gleason')

plot_grid(res_6, res_7, res_8, nrow = 1)



## Квантильный график остатков
qqPlot(M_prost_2) # qqPlot из пакета car
## Сравнение влияния отдельных предикторов   ----------------------
summary(M_prost_2)

## Какой из предикторов оказывает наиболее сильное влияние?
M_prost_2_scaled <- lm(lpsa ~ scale(lcavol) + scale(lweight) + scale(age) +
                         scale(lbph) + scale(pgg45), data = prost)
summary(M_prost_2_scaled)

coef(M_prost_2_scaled)



## Качество подгонки модели множественной линейной регрессии   ----------------------

## Визуализация модели   ----------------------

## Влияние одного предиктора: данные для визуализации
# заготовка возможных значений предикторов
new_data <- data.frame(lcavol = seq(min(prost$lcavol), max(prost$lcavol), length.out = 50),
                       lweight = mean(prost$lweight),
                       age = mean(prost$age),
                       lbph = mean(prost$lbph),
                       pgg45 = mean(prost$pgg45))

new_data$predicted <- predict(M_prost_2, newdata = new_data) # предсказанные значения
head(new_data, 3)

## Визуализация влияния одного предиктора
ggplot(data = new_data, aes(x = lcavol, y = predicted)) +
  geom_line(color = 'blue', size = 1) +
  geom_point(data = prost, aes(x = lcavol, y = lpsa)) +
  labs(y = 'lpsa', x = 'lcavol')

## Рассчитаем границы доверительной области
# стандартные ошибки при помощи predict()
new_data$SE <- predict(M_prost_2, newdata = new_data, se.fit = TRUE)$se.fit # стандартные ошибки

# стандартные ошибки вручную
X <- model.matrix(~ lcavol + lweight + age + lbph + pgg45 , data = new_data) # модельная матрица
new_data$SE <- sqrt(diag(X %*% vcov(M_prost_2) %*% t(X)))                    # стандартные ошибки

t_crit <- qt(p = 0.975, df = nrow(prost) - length(coef(M_prost_2))) # критическое значение t
new_data$upper <- new_data$predicted + t_crit * new_data$SE # верхняя граница доверительной зоны
new_data$lower <- new_data$predicted - t_crit * new_data$SE # нижняя граница доверительной зоны

## Визуализация влияния одного предиктора
ggplot(data = new_data, aes(x = lcavol, y = predicted)) +
  geom_ribbon(data = new_data, aes(ymin = lower, ymax = upper), alpha = 0.3) +
  geom_line(color = 'blue', size = 1) +
  geom_point(data = prost, aes(x = lcavol, y = lpsa)) +
  labs(y = 'lpsa', x = 'lcavol')


## Влияние двух предикторов: данные для визуализации
new_data <- expand.grid(lcavol = seq(min(prost$lcavol), max(prost$lcavol), length.out = 50),
                        lweight = seq(min(prost$lweight), max(prost$lweight), length.out = 50),
                        age = mean(prost$age),
                        lbph = mean(prost$lbph),
                        lcp = mean(prost$lcp),
                        pgg45 = mean(prost$pgg45))

new_data$predicted <- predict(M_prost_2, newdata = new_data)
head(new_data, 3)

ggplot(new_data, aes(x = lcavol, y = predicted, group = lweight)) +
  geom_line(aes(color = lweight)) +
  geom_point(data = prost, aes(x = lcavol, y = lpsa)) +
  scale_color_continuous(high = 'red', low = 'yellow') +
  ylab('lpsa')

