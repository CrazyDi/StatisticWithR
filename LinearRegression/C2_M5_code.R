# Сравнение линейных моделей
# М.А. Варфоломеева, PhD
# Биологический факультет, СПбГУ

# чтобы избежать "scientific notation"
options(scipen = 6, digits = 3)

## Зачем нужно сравнивать модели ----------------------
## Вспомним пример из прошлого модуля
# От каких переменных зависит концентрация простат-специфичного антигена?
# Исследовано 97 пациентов, перенесших простатотомию.
# Зависимая переменная `lpsa` --- это логарифм концентрации простат-специфичного антигена.
# Источник данных: Stamey, et al. 1989
## Модель, на которой мы остановились в прошлый раз
library(ElemStatLearn)
data("prostate")
prost <- prostate[, -10]
M_prost_2 <- lm(lpsa ~ lcavol + lweight + age + lbph + pgg45, data = prost)
# Предикторы, которые остались в модели после проверки условий применимости:
# - `lcavol` --- логарифм объема опухоли
# - `lweight` --- логарифм веса
# - `age` --- возраст пациента
# - `lbph` --- логарифм степени доброкачественной гиперплазии
# - `pgg45` --- доля оценок 4 и 5 по шкале Глисона


## Полные, уменьшенные и вложенные модели ----------------------


## Частный F-критерий  ----------------------

## Частный F-критерий в R `anova(модель_1, модель_2)`
M_prost_lcavol <- update(M_prost_2, . ~ . - lcavol)
anova(M_prost_lcavol, M_prost_2)

## Можем убедиться в эквивалентности F- и t-тестов
F_val <- anova(M_prost_2, M_prost_lcavol)[2, 'F']
F_val
t_val <- coef(summary(M_prost_2))['lcavol', 't value']
t_val^2


## I и II типы сумм квадратов  ----------------------

## От состава модели зависит характер влияния фактора
# Если возраст --- единственный фактор в модели, $F = 2.81$
M_prost_age1 <- lm(lpsa ~ age, data = prost)
M_prost_null <- lm(lpsa ~ 1, data = prost)
anova(M_prost_null, M_prost_age1)

# Когда мы учли влияние остальных факторов, $F =  2.65$
M_prost_age2 <- update(M_prost_2, . ~ . - age)
anova(M_prost_age2, M_prost_2)


## Порядок проведения сравнений при F-тестах (типы сумм квадратов)

## Последовательное тестирование `anova(модель)`
anova(M_prost_2)

# Если мы поставим `lcavol` последним, то изменится SS для других предикторов
M_prost_2_reord <- lm(lpsa ~ lweight + age + lbph + pgg45 + lcavol, data = prost)
anova(M_prost_2_reord)


## Поочередное тестирование `Anova(модель)` или `drop1()`
library(car)
Anova(M_prost_2)
# drop1(M_prost_2, test = 'F')

## При поочередном тестировании результат НЕ зависит от порядка предикторов
Anova(M_prost_2_reord)


## Зачем бывает нужно упрощать модели ---------------


## Упрощение модели при помощи частных F-тестов ----------------------

# Обратный пошаговый алгоритм (backward selection)

## Некоторые предикторы незначимы. Можно упростить модель
summary(M_prost_2)

## Частный F-критерий `drop1()`
drop1(M_prost_2, test = 'F')

## Тестируем предикторы (шаг 2)
# убрали lbph
M_prost_3 <- update(M_prost_2, . ~ . - lbph)
drop1(M_prost_3, test = 'F')

## Тестируем предикторы (шаг 3)
# убрали age
M_prost_4 <- update(M_prost_3, . ~ . - age )
drop1(M_prost_4, test = 'F')

## Тестируем предикторы (шаг 4)
# Убрали pgg45
M_prost_5 <- update(M_prost_4, . ~ . - pgg45)
drop1(M_prost_5, test = 'F')

## Итоговая модель
summary(M_prost_5)


## Диагностика финальной модели ----------------------

## Данные для анализа остатков
library(cowplot)
library(ggplot2)
theme_set(theme_bw())

M_prost_5_diag <- data.frame(
  fortify(M_prost_5),
  prost[, c('age', 'lbph', 'pgg45', 'lcp', 'svi', 'gleason')])
head(M_prost_5_diag, 3)

## График остатков от предсказанных значений
gg_resid <- ggplot(data = M_prost_5_diag, aes(x = .fitted, y = .stdresid)) +
  geom_point() + geom_hline(yintercept = 0) + geom_smooth()
gg_resid

## Графики зависимости остатков от предикторов в модели
res_1 <- gg_resid + aes(x = lcavol)
res_2 <- gg_resid + aes(x = lweight)
plot_grid(res_1, res_2, nrow = 1)

## Графики остатков от переменных, не включенных в модель
res_3 <- gg_resid + aes(x = age)
res_4 <- gg_resid + aes(x = lbph)
res_5 <- gg_resid + aes(x = pgg45)
res_6 <- gg_resid + aes(x = lcp)
res_7 <- ggplot(M_prost_5_diag, aes(y = .stdresid)) +
  geom_boxplot(aes(x = factor(svi)))
res_8 <- ggplot(M_prost_5_diag, aes(y = .stdresid)) +
  geom_boxplot(aes(x = factor(gleason)))
plot_grid(res_3, res_4, res_5, res_6, res_7, res_8, nrow = 2)


## Квантильный график остатков
qqPlot(M_prost_5) # qqPlot из пакета car


## Визуализация финальной модели ----------------------

## Теперь можно интерпретировать результаты
summary(M_prost_5)

## Данные для визуализации
new_data <- data.frame(lcavol = seq(min(prost$lcavol), max(prost$lcavol), length.out = 50),
                       lweight = mean(prost$lweight)) # заготовка возможных значений предикторов
new_data$predicted <- predict(M_prost_5, newdata = new_data) # предсказанные значения
# стандартные ошибки при помощи predict()
new_data$SE <- predict(M_prost_5, newdata = new_data, se.fit = TRUE)$se.fit # стандартные ошибки
t_crit <- qt(p = 0.975, df = nrow(prost) - length(coef(M_prost_5))) # критическое значение t
new_data$upper <- new_data$predicted + t_crit * new_data$SE # верхняя граница доверительной зоны
new_data$lower <- new_data$predicted - t_crit * new_data$SE # нижняя граница доверительной зоны
head(new_data)

## Визуализация модели
ggplot(data = new_data, aes(x = lcavol, y = predicted)) +
  geom_line(color = 'blue', size = 1) +
  geom_point(data = prost, aes(x = lcavol, y = lpsa)) +
  labs(y = 'lpsa', x = 'lcavol') +
  geom_ribbon(data = new_data, aes(ymin = lower, ymax = upper),
              fill = 'gray', alpha = 0.5)

