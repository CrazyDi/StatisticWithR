# Моделирование структуры дисперсии в смешанных моделях
# В.М. Хайтов, к.б.н.
# Биологический факультет, СПбГУ

# ## "Эволюция" линейных моделей #####


# ## Пример -- сексуальная активность мух #####
# ## Продолжительность жизни самцов и их сексуальная активность
# Размножение -- это процесс, требующий больших
# вложений энергии. Часто платой за такие вложения
# оказывается сокращение продолжительности жизни.
# Долго считалось, что если производство гамет --
# единственный вклад самцов в размножение, то их
# общие энергозатраты относительно незначительны,
# но так ли это?
# Зависит ли продолжительность жизни самцов мух от их сексуальной активности?
# Данные: Partridge, Farquhar, 1981
# Источник: James, Stanley, 1994

# Зависимая переменная:
# - `longevity`	-- продолжительность жизни самца (количество дней).
# Предиктор в фокусе исследования:
# - `activity`--- дискретный фактор, характеризующий условия активности самцов
# Ковариата:
# - `thorax` --- длина груди, непрерывная величина (мм).

# ## Читаем данные и знакомимся с ними
data(fruitfly, package = 'faraway')
fly <- fruitfly # Переименуем датасет для краткости
str(fly)
colSums(is.na(fly)) # Есть ли пропущенные значения?
table(fly$activity) # Сколько измерений в экспериментальных группах?

# ## Нет ли выбросов? Cтроим диаграммы Кливленда
library(ggplot2); library(cowplot); theme_set(theme_bw())

gg_dot <- ggplot(fly, aes(y = 1:nrow(fly))) + geom_point()

plot_grid(gg_dot + aes(x = longevity),
          gg_dot + aes(x = thorax))

# ## Нет ли коллинеарности дискретных и непрерывных предикторов?
ggplot(fly, aes(x = activity, y = thorax)) + geom_boxplot()


# ## Гипотеза и модель
# Гипотеза:
# Продолжительность жизни самцов мух зависит от их половой активности.
# ## Код для подгонки модели
mod1 <- lm(longevity ~ thorax * activity, data = fruitfly)


# ## Диагностика модели
mod1_diag <- fortify(mod1)
ggplot(mod1_diag, aes(x = .fitted, y = .stdresid)) +
  geom_point() +
  geom_hline(yintercept = 0)
# Мы не можем доверять результатам оценки, так как присутствуют явные признаки гетероскедастичности.


# ## Моделирование дисперсии #####



# ## GLS модель для данных
library(nlme)
mod1_gls <- gls(longevity ~ thorax * activity, data = fruitfly)
Pl_resid_mod1_gls <- qplot(x = fitted(mod1_gls), y = residuals(mod1_gls, type = 'pearson')) +
  geom_hline(yintercept = 0)
Pl_resid_mod1_gls


# ## Дисперсия может зависеть от непрерывной ковариаты #####

# ##  Фиксированная структура дисперсии. varFixed()
mod2_gls <- update(mod1_gls, weights = varFixed( ~ thorax))

# ## Можем сравнить две модели при помощи AIC
AIC(mod1_gls, mod2_gls)


# ## Степенная зависимость дисперсии от ковариаты: varPower()
mod3_gls <- update(mod1_gls, weights = varPower(form = ~ thorax))
mod3_gls$modelStruct

# ## Степенная зависимость дисперсии от ковариаты для разных уровней дискретного фактора
mod4_gls <- update(mod1_gls, weights = varPower(form = ~ thorax|activity))
mod4_gls$modelStruct

# ## Усложненная степенная зависимость дисперсии от ковариаты:`varConstPower()`
mod5_gls <- update(mod1_gls, weights = varConstPower(form = ~ thorax))
mod6_gls <- update(mod1_gls, weights = varConstPower(form = ~ thorax|activity))
mod5_gls$modelStruct
mod6_gls$modelStruct

# ## Экспоненциальная зависимость дисперсии от ковариаты: `varExp()`
mod7_gls <- update(mod1_gls, weights = varExp(form = ~ thorax))
mod8_gls <- update(mod1_gls, weights = varExp(form = ~ thorax|activity))
mod7_gls$modelStruct
mod8_gls$modelStruct

# ## Дисперсия может зависить от дискретного фактора #####

# ## Разные дисперсии для разных уровней дискретного фактора:  `varIdent()`
mod9_gls <- update(mod1_gls, weights = varIdent(form = ~1|activity))
summary(mod9_gls)

# ## Комбинированная структура дисперсии: `varComb()`
mod10_gls <- update(mod1_gls, weights = varComb(varIdent(form = ~ 1|activity),
                                                varFixed(~ thorax)))

mod11_gls <- update(mod1_gls, weights = varComb(varIdent(form = ~ 1|activity),
                                                varPower(form = ~ thorax)))

mod12_gls <- update(mod1_gls, weights = varComb(varIdent(form = ~1| activity),
                                                varExp(form = ~ thorax)))

mod13_gls <- update(mod1_gls, weights = varComb(varIdent(form = ~ 1|activity),
                                                varConstPower(form = ~ thorax)))


# ## Моделирование гетерогенности дисперсии -- финальная модель #####

# ## Находим финальную модель
AICs <- AIC(mod1_gls,  mod2_gls,  mod3_gls, mod4_gls, mod5_gls,
            mod6_gls,  mod7_gls,  mod8_gls, mod9_gls, mod10_gls,
            mod11_gls, mod12_gls, mod13_gls)
AICs

# ## Финальная модель
AICs[which.min(AICs$AIC), ]

summary(mod10_gls)$call


# ## Диагностика финальной модели
Pl_resid_mod1_gls <- Pl_resid_mod1_gls  + ggtitle('Было') + labs(x = '.fitted', y = 'Pearson resid.')
Pl_resid_mod10_gls <-  qplot(x = fitted(mod10_gls), y = residuals(mod10_gls, type = 'pearson')) +
  geom_hline(yintercept = 0) + ggtitle('Стало')+ labs(x = '.fitted', y = 'Pearson resid.')
plot_grid(Pl_resid_mod1_gls, Pl_resid_mod10_gls)

# ## Упрощение модели
# Для упрощения фиксированной части модели надо переподобрать модель с помощью ML
mod10_gls_ML <- update(mod10_gls, method = 'ML')
drop1(mod10_gls_ML, test = 'Chi')

# ## Больше ничего упростить нельзя
mod10_gls_ML2 <- update(mod10_gls_ML, .~.-thorax:activity)
drop1(mod10_gls_ML2, test = 'Chi' )

# ## Финальная модель
mod10_final <- update(mod10_gls_ML2, method = 'REML')

# ## Что за модель мы построили?
summary(mod10_final)

# ## Подготовка визуализации предсказаний модели
library(dplyr)
new_data <- fly %>% group_by(activity) %>%
  do(data.frame(thorax = seq(min(.$thorax), max(.$thorax), length.out = 100)))

# Предсказанные значения при помощи матриц
X <- model.matrix(~ thorax + activity, data = new_data)
b <- coef(mod10_final)
new_data$fit <- X%*%b

# Cтандартные ошибки
new_data$se <- sqrt(diag(X %*% vcov(mod10_final) %*% t(X)))
new_data$lwr <- new_data$fit - 2 * new_data$se
new_data$upr <- new_data$fit + 2 * new_data$se

# ## Визуализация предсказаний финальной модели
ggplot(new_data, aes(x = thorax, y = fit, group = activity)) + geom_line() +
  geom_ribbon(aes(ymin = lwr, ymax = upr, fill = activity), alpha = 0.5) +
  geom_point(data = fly, aes(x = thorax, y = longevity, colour = activity)) +
  labs(y = 'longevity')



# ## Моделирование структуры дисперсии при наличии случайных факторов  #####
# ## Пример --- вес крысят
# Были изучены  27 помётов крысят (`Litter`). В
# каждом помете было разное количество особей
# (`Lsize`, от 2 до 18) разного пола (переменная
# `sex`).
# Пометы разделили на 3 группы и подвергли
# некоторому экспериментальному воздействию
# (фактор `Treatment` с градациями `Control`,
# `Low` и `High`).
# Затем, по прошествии определенного времени, крысят взвесили.
# Задача --- оценить влияние экспериментального воздействия на вес крысят.
# Источник: Pinheiro and Bates, 2000

# ## Особенности дизайна эксперимента
data(RatPupWeight)
rat <- as.data.frame(RatPupWeight)
head(rat, 14)

# Нас интересует влияние экспериментального
# воздействия (`Treatment`) на вес крысят
# (`weight`).
# Однако на вес могут повлиять ковариаты `sex`
# (пол) и `Lsize` (величина выводка), кроме того,
# возможны взаимодействия между предикторами.
# Кроме того, крысята из одного помета могут быть
# похожи друг на друга --- нужно учесть
# группирующий фактор `Litter`

# ## Нет ли пропущенных данных
colSums(is.na(rat))


# ## Нет ли выбросов?
gg_dot <- ggplot(rat, aes(x = weight, y = 1:nrow(rat))) + geom_point()
plot_grid(gg_dot, gg_dot + aes(x = Lsize))

# ## Нет ли коллинеарности дискретных и непрерывных предикторов?
gg_box <- ggplot(rat, aes(x = Treatment, y = Lsize)) + geom_boxplot()
plot_grid(gg_box, gg_box + aes(x = sex))


# ## Модель со случайным фактором #####

# ## Начальные модели

# Модель со случайным отрезком
mod1 <- lme(weight ~ Treatment*sex*Lsize, random = ~1|Litter, data = rat)

# Модель со случайным отрезком и случайным угловым коэффициентом
mod2 <- lme(weight ~ Treatment*sex*Lsize, random = ~1 + Lsize|Litter, data = rat)

# Модель `mod1` лучше.
AIC(mod1, mod2)

# ## Диагностика модели
mod1_diag <- data.frame(.fitted = fitted(mod1), .resid = residuals(mod1, type = 'pearson'), rat)
Pl_d1 <- ggplot(mod1_diag, aes(y = .resid)) + geom_point(aes(x = .fitted))
plot_grid(Pl_d1, Pl_d1 + geom_point(aes(x = Lsize)),
          ggplot(mod1_diag, aes(y = .resid)) + geom_boxplot(aes(x = sex)),
          ggplot(mod1_diag, aes(y = .resid)) + geom_boxplot(aes(x = Treatment)),
          nrow = 1, rel_widths = c(0.3, 0.3, 0.15, 0.25))


# ## Если не вводить коррекцию дисперсии, модель можно упростить
mod1_ML <- update(mod1, method = 'ML')
drop1(mod1_ML, test = 'Chi')

# ## Упрощаем модель без корректировки дисперсии: Шаг 1
mod1_ML2 <- update(mod1_ML, .~.-Treatment:sex:Lsize)
drop1(mod1_ML2, test = 'Chi')

# ## Упрощаем модель без корректировки дисперсии: Шаг 2
mod1_ML3 <- update(mod1_ML2, .~.-Treatment:sex)
drop1(mod1_ML3, test = 'Chi')

# ## Упрощаем модель без корректировки дисперсии: Шаг 3
mod1_ML4 <- update(mod1_ML3, .~.-sex:Lsize)
drop1(mod1_ML4, test = 'Chi')

# ## Упрощаем модель без корректировки дисперсии: Шаг 4
mod1_ML5 <- update(mod1_ML4, .~.-Treatment:Lsize)
drop1(mod1_ML5, test = 'Chi')


# ## Моделируем структуру дисперсии #####

# ## Ковариата -- дискретный фактор
mod1_1 <- update(mod1, weights = varIdent(form = ~ 1|Treatment))
mod1_2 <- update(mod1, weights = varIdent(form = ~ 1 |sex))

# ## Ковариата -- непрерывная величина
mod1_3 <- update(mod1, weights = varPower(form = ~ Lsize))
mod1_4 <- update(mod1, weights = varPower(form = ~ Lsize|Treatment))
mod1_5 <- update(mod1, weights = varPower(form = ~ Lsize|sex))

mod1_6 <- update(mod1, weights = varConstPower(form = ~ Lsize))
mod1_7 <- update(mod1, weights = varConstPower(form = ~ Lsize|Treatment))
mod1_8 <- update(mod1, weights = varConstPower(form = ~ Lsize|sex))

mod1_9  <- update(mod1, weights = varExp(form = ~ Lsize))
mod1_10 <- update(mod1, weights = varExp(form = ~ Lsize|Treatment))
mod1_11 <- update(mod1, weights = varExp(form = ~ Lsize|sex))

# ## Комбинированное влияние непрерывной и дискрентной ковариаты
mod1_12 <- update(mod1, weights = varComb(varExp(form = ~ Lsize),
                                     varIdent(form = ~1|Treatment)))
mod1_13 <- update(mod1, weights = varComb(varExp(form = ~ Lsize),
                                     varIdent(form = ~1|sex)))
mod1_14 <- update(mod1, weights = varComb(varPower(form = ~ Lsize),
                                     varIdent(form = ~1|Treatment)))
mod1_15 <- update(mod1, weights = varComb(varPower(form = ~ Lsize),
                                     varIdent(form = ~1|sex)))

# ## Выбираем лучшую модель
AICs <- as.data.frame(AIC(mod1_1, mod1_2, mod1_3, mod1_4,
                          mod1_5, mod1_6, mod1_7, mod1_8,
                          mod1_9, mod1_10, mod1_11, mod1_12,
                          mod1_13, mod1_14, mod1_15))
AICs[which.min(AICs$AIC), ]
summary(mod1_12)$call

# ## Диагностика модели после корректировки дисперсии
mod1_12_diag <- data.frame(.fitted = fitted(mod1_12),
                           .resid = residuals(mod1_12, type = 'pearson'))
Pl_d2 <- ggplot(mod1_12_diag, aes(x = .fitted, y = .resid) ) + geom_point()
grid.arrange(Pl_d1 + ggtitle('Было'), Pl_d2 + ggtitle('Стало'), nrow = 1)


# ## Можем ли мы ответить на вопрос, о влиянии `Treatment`, как самостоятельного фактора?

# С учетом корректировки дисперсии:
drop1(update(mod1_12, method='ML'), test = 'Chi')

summary(mod1_12)


# ## Код для визуализации предсказаний модели
new_data <- rat %>% group_by(Treatment, sex) %>%
  do(data.frame(Lsize = seq(min(.$Lsize), max(.$Lsize))))

# Предсказанные значения при помощи матриц
X <- model.matrix(~ Treatment*sex*Lsize, data = new_data)
b <- fixed.effects(mod1_12)
new_data$fit <- X%*%b

# Cтандартные ошибки
new_data$se <- sqrt( diag(X %*% vcov(mod1_12) %*% t(X)) )
new_data$lwr <- new_data$fit - 2 * new_data$se
new_data$upr <- new_data$fit + 2 * new_data$se

# ## Визуализация предсказаний модели
ggplot(new_data, aes(x = Lsize, y = fit))  +
  geom_ribbon(aes(ymax = upr, ymin = lwr, fill = Treatment), alpha = 0.5) +
  geom_point(data = rat, aes(y = weight, colour = Treatment)) +
  geom_line(aes(group = Treatment)) + facet_wrap(~ sex) +
  labs(y = 'weight')

