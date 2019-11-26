# Модели со взаимодействием дискретных и непрерывных предикторов
# В.М. Хайтов, к.б.н.
# Биологический факультет, СПбГУ

# чтобы избежать scientific notation
options(scipen = 6, digits = 3)


# Подбор модели со взаимодействием дискретного и непрерывного предикторов  --------------------------
# Вес новорожденного у курящих и некурящих матерей
# Зависимая переменная:
# - `bwt` --- вес ребенка при рождении.
# Предикторы:
# - `age` --- возраст матери (непрерывный предиктор) .
# - `smoke` --- курила ли мать во время беременности (дискретный предиктор).
# Данные: Hosmer, Lemeshow, 1989

# Чтение данных
library(MASS)
baby <- birthwt # Переименуем данные для удобства
str(baby)

# Преобразуем значения перменных
baby$smoke[baby$smoke == 1] <- 'Smoker'
baby$smoke[baby$smoke == 0] <- 'Non smoker'
baby$smoke <- factor(baby$smoke)

# Исследуем данные
library(ggplot2)
theme_set(theme_bw())
gg_dot <- ggplot(baby, aes(y = 1:nrow(baby))) + geom_point()
gg_dot + aes(x = age)

# Исследуем данные
gg_dot + aes(x = bwt)

# Исследуем данные
table(baby$smoke)
table(baby$age)

# Строим полную модель
Model <- lm(bwt ~ age * smoke, data = baby)

# Диагностика модели --------------------------
# Проверка на коллинеарность
# Построим упрощенную модель для того, чтобы воспользоваться функцией `vif()` из пакета `car`
Mod_vif <- lm(bwt ~ age + smoke, data = baby)
library(car)
vif(Mod_vif)
# Коллинеарности нет.

# Анализ остатков
# Данные для графиков остатков
Model_diag <- fortify(Model)
head(Model_diag)

# График расстояния Кука
ggplot(Model_diag, aes(x = 1:nrow(Model_diag), y = .cooksd)) +
  geom_bar(stat = 'identity')

# График остатков в зависимости от предсказанных значений
gg_resid <- ggplot(data = Model_diag, aes(x = .fitted, y = .stdresid)) +
  geom_point() + geom_hline(yintercept = 0) + geom_smooth()
gg_resid

# Графики остатков в зависимости от предикторов, включенных в модель
# Непрерывный предиктор
gg_resid + aes(x = age)
# Дискретный фактор
ggplot(Model_diag, aes(x = smoke, y = .stdresid)) +
  geom_boxplot() + geom_hline(yintercept = 0)

# Графики остатков в зависимости от предикторов, не включеных в модель
Model_diag$lwt <- baby$lwt
ggplot(data = Model_diag, aes(x = lwt, y = .stdresid)) +
  geom_point() + geom_hline(yintercept = 0) + geom_smooth()

# Квантильный график остатков
library(car)
qqPlot(Model, id = FALSE)


# Анатомируем модель  --------------------------

# Результаты подбора модели
summary(Model)

# Можем ли мы избавиться от взаимодействия?
drop1(Model, test = 'F')
# Если удалить взаимодействия, то модель значимо изменится.

# Модельная матрица при наличии взаимодействий дискретного и непрерывного предиктора
X <- model.matrix(Model)
head(X, 10)

# Вектор коэффициентов
options(digits = 6)
b <- coefficients(Model)
b

# Предсказанные значения в матричном виде
as.numeric(fitted(Model)[1])
(X %*% b) [1]

# Предсказанное значение для объекта, относящегося к базовому уровню
X[1,] # Первая строка в модельной матрице

# Что происходит при матричном умножении
1 * 2406.0580 + 19.0 * 27.7314  + 0 * 798.1749 + 0 * -46.5719


# Предсказанное значение для объекта, не относящегося к базовому уровню
as.numeric(fitted(Model)[3])
X[3,] # Третья строка в модельной матрице

# Что происходит при матричном умножении
1 * 2406.0580 + 20 * 27.7314  + 1 * 798.1749 + 20 * -46.5719



# Визуализация модели со взаимодействием дискретного и непрерывного предикторов  --------------------------

# Диапазон возрастов разный для курящих и некурящих, поэтому...
library(dplyr)
new_data <- baby %>% group_by(smoke)%>%
  do(data.frame(age = seq(min(.$age), max(.$age), length.out = 100)))

# Предсказанные значения
Predictions <- predict(Model, newdata = new_data, se.fit = TRUE)
new_data$fit <- Predictions$fit

# Стандартные ошибки
new_data$se <- Predictions$se.fit
# Критические значения t
t_crit <- qt(0.975, df = nrow(baby) - length(coef(Model)))
# Доверительный интервал
new_data$upr <- new_data$fit + t_crit * new_data$se
new_data$lwr <- new_data$fit - t_crit * new_data$se

# Рисуем график предсказаний
Pl_smoke <- ggplot(new_data, aes(x = age, y = fit)) +
  geom_ribbon(alpha = 0.2, aes(ymin = lwr, ymax = upr, group = smoke)) +
  geom_line(aes(colour = smoke))
Pl_smoke

# На графике предсказаний можно показать исходные значения
Pl_final <- Pl_smoke +
  geom_point(data = baby, aes(x = age, y = bwt, colour = smoke)) +
  scale_colour_discrete('Курила ли мать', labels = c('Не курила', 'Курила')) +
  labs(x = 'Возраст матери', y = 'Вес новорожденного')
Pl_final



# Модели с несколькими уровнями дискретного предиктора и взаимодействием --------------------------


# От чего зависит  обилие птиц во фрагментированных ландшафтах Австралии

bird <- read.csv('data/loyn.csv')

# Предположим, что в фокусе нашего исследования влияние на птиц выпаса скота, но вкупе с другими предикторами, которые также могут влиять на обилие птиц.
# Данные: Loyn, 1987
# Источник: Quinn,Keugh,2002

# Зависимая перменная
# `ABUND` --- обилие птиц на стандартном маршруте
# Предикторов очень много
#
# - `AREA` --- площадь лесного массива (Га)
# - `YRISOL` --- год, в котором произошла изоляция лесного массива
# - `DIST` --- расстояние до ближайшего другого лесного массива (км)
# - `LDIST` --- расстояние до ближайшего более крупного массива (км)
# - `GRAZE` --- качественная оценка уровня выпаса скота (1 - низкий уровень, 5 - высокий уровень)
# - `ALT` --- высота над уровнем моря (м)

# Нет ли выбросов в зависимой переменной?
gg_dot <- ggplot(bird, aes(y = 1:nrow(bird))) + geom_point()
gg_dot + aes(x = ABUND)

# Нет ли выбросов в переменной `AREA`?
gg_dot + aes(x = AREA)

# Нет ли выбросов в переменной `YRISOL`?
gg_dot + aes(x = YRISOL)

# Нет ли выбросов в переменной `DIST`?
gg_dot + aes(x = DIST)

# Нет ли выбросов в переменной `LDIST`?
gg_dot + aes(x = LDIST)

# Нет ли выбросов в переменной `GRAZE`?
gg_dot + aes(x = GRAZE)
# На самом деле это дискретная переменная.
# Ее нужно сделать фактором.

# Нет ли выбросов в переменной `ALT`?
gg_dot + aes(x = ALT)


# Связи между переменными
pairs(bird)

# Удаляем отскакивающие значения.
include <- bird$AREA < 500 & bird$DIST < 1000 & bird$ALT < 250
bird_1 <- bird[include, ]
# Дальше будем работать с датафреймом `bird_1`

# Делаем фактором дискретную переменную `GRAZE`
bird_1$GRAZE_factor <- factor(bird_1$GRAZE)


# Проверка на мультиколлинеарность
# Для того чтобы воспользоваться функцией `vif()`
# из пакета `car`, построим модель без
# взаимодействия предикторов.
Model_vif <- lm(ABUND ~  AREA + YRISOL + DIST + LDIST + GRAZE_factor + ALT, data = bird_1)
library(car)
vif(Model_vif)
# `GRAZE_factor` --- имеет максимальную величину
# vif, но нас интересует, как на обилие птиц
# влияет именно этот фактор. Поэтому мы вместо
# `GRAZE_factor` удалим `YRISOL`.

# Проверка на мультиколлинеарность
Model_vif2 <- update(Model_vif, ~ . - YRISOL)
vif(Model_vif2)
# Теперь мультиколлинеарности нет!

# Подбор модели, описывающей обилие птиц  --------------------------

# Строим модель
Model_bird <- lm(ABUND ~ AREA + DIST + LDIST + ALT  + GRAZE_factor +
                   AREA:GRAZE_factor +
                   DIST:GRAZE_factor +
                   LDIST:GRAZE_factor +
                   ALT:GRAZE_factor,
                 data = bird_1)

# Упрощаем модель. Шаг 1.
drop1(Model_bird, test = 'F')

# Упрощаем модель. Шаг 2.
Model_bird_1 <- update(Model_bird, . ~ . - ALT:GRAZE_factor)
drop1(Model_bird_1, test = 'F')

# Упрощаем модель. Шаг 3.
Model_bird_2 <- update(Model_bird_1, . ~ . - ALT )
drop1(Model_bird_2, test = 'F')

# Упрощаем модель. Шаг 4.
Model_bird_3 <- update(Model_bird_2, . ~ . - LDIST:GRAZE_factor )
drop1(Model_bird_3, test = 'F')

# Упрощаем модель. Шаг 5.
Model_bird_4 <- update(Model_bird_3, . ~ . - LDIST)
drop1(Model_bird_4, test = 'F')

# Упрощаем модель. Шаг 6.
Model_bird_5 <- update(Model_bird_4, . ~ . - DIST:GRAZE_factor )
drop1(Model_bird_5, test = 'F')

# Упрощаем модель. Шаг 7.
Model_bird_6 <- update(Model_bird_5, . ~ . - DIST)
drop1(Model_bird_6, test = 'F')

# Ничего больше сократить нельзя.
# `Model_bird_6` --- это финальная модель.

# Диагностика финальной модели --------------------------

# Есть ли влиятельные наблюдения?
Model_bird_diag <- fortify(Model_bird_6)

ggplot(Model_bird_diag, aes(x = 1:nrow(Model_bird_diag), y = .cooksd)) +
  geom_bar(stat = 'identity')

# График зависимости остатков от предсказанных значений
gg_resid <- ggplot(data = Model_bird_diag, aes(x = .fitted, y = .stdresid)) +
  geom_point() + geom_hline(yintercept = 0) + geom_smooth()
gg_resid

# График зависимости остатков от предикторов, включенных в модель
gg_resid + aes(x = AREA)
ggplot(data = Model_bird_diag, aes(x = GRAZE_factor, y = .stdresid)) +
  geom_boxplot() + geom_hline(yintercept = 0)

# График зависимости остатков от предикторов, не включенных в модель
ggplot(data = Model_bird_diag, aes(x = bird_1$YRISOL, y = .stdresid)) +
  geom_point() + geom_hline(yintercept = 0) + geom_smooth()

ggplot(data = Model_bird_diag, aes(x = bird_1$DIST, y = .stdresid)) +
  geom_point() + geom_hline(yintercept = 0) + geom_smooth()

ggplot(data = Model_bird_diag, aes(x = bird_1$LDIST, y = .stdresid)) +
  geom_point() + geom_hline(yintercept = 0) + geom_smooth()

ggplot(data = Model_bird_diag, aes(x = bird_1$ALT, y = .stdresid)) +
  geom_point() + geom_hline(yintercept = 0) + geom_smooth()


# Представляем результаты подбора модели  --------------------------

# Влияние факторов
Anova(Model_bird_6)

# Для трактовки результатов нужна визуализация модели.
# Формируем данные для графика
new_data <- bird_1 %>%
  group_by (GRAZE_factor) %>%
  do(data.frame(AREA = seq(min(.$AREA), max(.$AREA), length.out = 100)))

Predictions <- predict(Model_bird_6, newdata = new_data, se.fit = TRUE)
# Предсказанные значения
new_data$fit <- Predictions$fit
# Стандартные ошибки
new_data$se <- Predictions$se.fit
# Критические значения t для 95% доверительного интервала
t_crit <- qt(0.975, df = nrow(bird_1) - length(coef(Model_bird_6)))
# Доверительный интервал
new_data$upr <- new_data$fit + t_crit * new_data$se
new_data$lwr <- new_data$fit - t_crit * new_data$se


# Строим график
Plot_birds <- ggplot(new_data, aes(x = AREA, y = fit)) +
  geom_ribbon(alpha = 0.2, aes(ymin = lwr, ymax = upr, group = GRAZE_factor)) +
  geom_line(aes(colour = GRAZE_factor), size = 1) +
  geom_point(data = bird_1, aes(x = AREA, y = ABUND, colour = GRAZE_factor))
Plot_birds

# Последние штрихи к графику
Plot_final <- Plot_birds +
  labs(x = 'Площадь лесного массива',
       y = 'Обилие птиц',
       color = 'Интенсивность \nвыпаса скота')
Plot_final
# О чем говорит рисунок?
