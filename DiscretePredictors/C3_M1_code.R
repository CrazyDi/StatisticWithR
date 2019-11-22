# Модели с дискретными и непрерывными предикторами
# В.М. Хайтов, к.б.н.
# Биологический факультет, СПбГУ

# чтобы избежать scientific notation
options(scipen = 6, digits = 3)


# Пример --- козы, глисты и линейные модели ----------------
# Данные: Alan Pearson, Animal Health Laboratory, Lincoln, New Zealand.
# Источник: Saville, Wood, 1991

# - `Treatment` --- обработка от глистов (стандартная, интенсивная)
# - `Weightgain` --- привес за время эксперимента, кг
# - `Initial.wt` --- начальный, стартовый вес, который имели козы до начала эксперимента, кг


# Читаем данные
library(readxl)
goat <- read_excel('data/goats.xlsx', sheet = 1)
head(goat)

# Подготавливаем данные
colnames(goat)

# Переименуем переменные для краткости.
colnames(goat) <- c('Treatment', 'Wt', 'Stw')

# Сделаем тип обработки фактором.
goat$Treatment <- factor(goat$Treatment)

# Знакомимся с данными
# Нет ли пропущенных значений?
colSums(is.na(goat))

# Объемы выборок:
table(goat$Treatment)

# Есть ли выбросы?
library(ggplot2)
library(cowplot)
theme_set(theme_bw())
gg_dot <- ggplot(goat, aes(y = 1:nrow(goat))) + geom_point()
dot1 <- gg_dot + aes(x = Wt)
dot2 <- gg_dot + aes(x = Stw)
plot_grid(dot1, dot2, ncol = 2)

# Подбор модели ------------------------------------------------
# Строим полную модель
Mod_goat_full <- lm(Wt ~ Stw + Treatment + Stw:Treatment, data = goat)
# Аналогичная, но более короткая запись
Mod_goat_full <- lm(Wt ~ Stw * Treatment, data = goat)

# Можно ли не учитывать взаимодействие предикторов?
drop1(Mod_goat_full, test = 'F')
# Значимого взаимодействия не наблюдается!

# Строим сокращенную модель
# Можно просто переписать формулу.
Mod_goat_reduced <- lm(Wt ~ Stw + Treatment, data = goat)
# Но есть более удобный способ.
Mod_goat_reduced <- update(Mod_goat_full, . ~ . - Stw:Treatment)


# Диагностика модели ---------------------------------------------

# Нет ли коллинеарности между начальным весом и типом обработки?
library(car)
vif(Mod_goat_reduced)

# Нет ли коллинеарности между начальным весом и типом обработки?
ggplot(goat, aes(x = Treatment, y = Stw)) +
  geom_boxplot()

# Строим диагностические графики
MG_diag <- fortify(Mod_goat_reduced)
Pl_diag1 <- ggplot(MG_diag, aes(x = 1:nrow(MG_diag), y = .cooksd)) +
  geom_bar(stat = 'identity')

Pl_diag2 <- ggplot(MG_diag, aes(x = .fitted, y = .stdresid)) +
  geom_point() + geom_hline(yintercept = 0)

Pl_diag3 <- ggplot(MG_diag, aes(x = Stw, y = .stdresid)) +
  geom_point() + geom_hline(yintercept = 0)

Pl_diag4 <- ggplot(MG_diag, aes(x = Treatment, y = .stdresid)) +
  geom_boxplot()

library(cowplot)
plot_grid(Pl_diag1, Pl_diag2, Pl_diag3, Pl_diag4, nrow = 2)


# Проверка на нормальность распределения остатков
qqPlot(Mod_goat_reduced, id = FALSE) # функция из пакета car

# Трактовка регрессионной модели, включающей один дискретный и один непрерывный предиктор -----

# График модели
gg_goat <- ggplot(data = goat, aes(y = Wt, x = Stw, colour = Treatment)) +
  geom_point()  +
  labs(x = 'Начальный вес, кг', y = 'Привес, кг') +
  scale_colour_discrete('Способ обработки',
                  breaks = c('intensive', 'standard'),
                  labels = c('Интенсивный', 'Стандартный'))
gg_goat

# Добавляем линии регрессии
library(dplyr)
new_data <- goat %>% group_by(Treatment)%>%
  do(data.frame(Stw = seq(min(.$Stw), max(.$Stw), length.out = 10)))
new_data$fit = predict(Mod_goat_reduced, newdata = new_data)

gg_goat + geom_line(data = new_data, aes(x = Stw, y = fit, colour = Treatment))


# Находим границы доверительной области

# Находим вариационно-ковариационную матрицу для модели Mod_goat_reduced
VC <- vcov(Mod_goat_reduced)
# Модельная матрица
X <- model.matrix(~ Stw + Treatment, data = new_data)
# Стандартные ошибки
new_data$se <- sqrt(diag(X %*% VC %*% t(X)))
# t для расчета 95% доверительного интервала
t_crit <- qt(0.975, df = nrow(goat) - length(coef(Mod_goat_reduced)))
# Границы доверительных интервалов
new_data$upr <- new_data$fit + t_crit * new_data$se
new_data$lwr <- new_data$fit - t_crit * new_data$se

# Наносим на график границы доверительной области
gg_goat + geom_line(data = new_data, aes(x = Stw, y = fit, colour = Treatment)) +
  geom_ribbon(data = new_data,
              aes(x = Stw, y = fit, ymin = lwr, ymax = upr,
                  fill = Treatment), alpha = 0.3, size = 0) +
  scale_fill_discrete('Способ обработки',
                      breaks = c('intensive', 'standard'),
                      labels = c('Интенсивный', 'Стандартный'))


# О чем говорят числа в `summary()`?
summary(Mod_goat_reduced)


# Изменение базового уровня фактора  -----------------------

# Меняем базовый уровень
goat$Treatment <- relevel(goat$Treatment, ref = 'standard')
levels(goat$Treatment)

# Строим новую модель
Mod_goat_reduced_2 <- lm(Wt ~ Stw + Treatment, data = goat)

# Результаты
# Было:
coef(Mod_goat_reduced)

# Стало:
coef(Mod_goat_reduced_2)
# Некоторые коэффициенты стали другими, но по смыслу ничего не изменилось.
# Изменение базового уровня --- это чисто
# формальная процедура, от которой ничего не
# изменяется по сути, но это иногда необходимо для
# более удобной визуализации.


# Общие линейные модели -------------------------------------------


# Analysis of covariance (ANCOVA) --- частный случай общих линейных моделей ------------

# Емкость легких у разных возрастных групп
# Различается ли объем легких  разных возрастных групп пациентов, которых готовят к операции?
# Данные: Altman, 1991
# Источник: пакет ISwR

# Читаем данные
tlc <- read.table('data/tlc.csv', sep = ';', header = TRUE)
str(tlc)

# Переменные:
# - `age` -- возраст.
# - `sex` -- пол (1 - женский; 2 - мужской)
# - `height` -- рост (см)
# - `tlc`-- объем легких (л)


# Немного преобразуем исходный датасет
# Создаем переменную, кодирующую возрастную группу
tlc$age_group[tlc$age < 20] <- 'teenagers'
tlc$age_group[tlc$age < 30 & tlc$age >= 20] <- 'young'
tlc$age_group[tlc$age >= 30] <- 'adult'

# Создаем фактор с удобным порядком градаций
tlc$age_group <- factor(tlc$age_group, levels = c('teenagers', 'young', 'adult'))

# Как распределены измерения между группами
table(tlc$age_group, tlc$sex)



# Модель с дискретным предиктором
Mod <- lm(tlc ~ age_group, data = tlc)
summary(Mod)


# Кодирование дискретных предикторов в R  ----------------

# Модельная матрица для моделей с дискретными факторами

X <- model.matrix(Mod)
# Фрагмент модельной матрицы
X[c(1:5, 10:15), ]


# Матричное умножение
# Вектор коэффициентов
coef(Mod)

# Для объекта, относящегося к базовому уровню
X[2,]

# Для объекта, относящегося к `young`
X[12,]


# Для объекта, относящегося к `adult`
X[1, ]


# Геометрическая интерпретация линейной модели с дискретным предиктором -----

# Данные для графика предсказаний модели

# Это будет график средних значений зависимой переменной для каждой градации дискретного фактора.

# Создаем искусственный датасет со всеми возможными значениями предиктора
new_data <- data.frame(age_group = factor(levels(tlc$age_group),
                                          levels = levels(tlc$age_group)))
# Предсказанные моделью средние значения зависимой перменной
new_data$fit <- predict(Mod, newdata = new_data, se.fit = TRUE)$fit
# Стандартные ошибки
new_data$se <- predict(Mod, newdata = new_data, se.fit = TRUE)$se.fit
# Критические значения t для расчета доверительных интервалов
t_crit <- qt(0.975, df = nrow(tlc) - length(coef(Mod)))
# Границы доверительных интервалов
new_data$upr <- new_data$fit + t_crit * new_data$se
new_data$lwr <- new_data$fit - t_crit * new_data$se

# График предсказаний модели
library(ggplot2)
ggplot(new_data, aes(x = age_group, y = fit)) +
  geom_bar(stat = 'identity', aes(fill = age_group)) +
  geom_errorbar(aes(ymin = lwr, ymax = upr), width = 0.2) +
  scale_x_discrete(labels = c('Подростки', 'Молодые', 'Взрослые')) +
  guides(fill = 'none') +
  labs(x = 'Возрастная группа', y = 'Объем легких')

# Можно ли доверять полученным результатам?
Mod_diag <- fortify(Mod) # Создаем датафрейм с диагностическими данными
Mod_diag$height <- tlc$height # Переменная, не вошедшая в модель

ggplot(Mod_diag, aes(x = height, y = .stdresid)) +
  geom_point() + geom_hline(yintercept = 0) +
  geom_smooth(method = 'lm')


# Виден очевидный паттерн в остатках!

# Анализ с учетом ковариаты  -------------------------------------

# В модели необходимо учесть ковариату
Mod_cov <- lm(tlc ~ age_group * height, data = tlc)

# Нужен тест на наличие взаимодействия.
drop1(Mod_cov, test = 'F')
# Значимого взаимодействия нет!
# Можно делать ANCOVA.

# ANCOVA: модель с учетом ковариаты
Mod_cov_2 <- lm(tlc ~ age_group + height, data = tlc)

# Модельная матрица в ANCOVA
X <- model.matrix(Mod_cov_2)
head(X)


# Матричные умножения в ANCOVA

# Вектор коэффициентов
coef(Mod_cov_2)

# Для объекта, относящегося к базовому уровню
X[2, ]

# Для объекта, относящегося к `young`
X[13, ]

# Для объекта, относящегося к `adult`
X[1, ]


# Диагностика модели в ANCOVA  ------------------------------------

# Нет ли коллинеарности
library(car)
vif(Mod_cov_2)

# График остатков
# Датафрейм с диагностическими данными
Mod_cov_2_diag <- fortify(Mod_cov_2)

ggplot(Mod_cov_2_diag, aes(x = .fitted, y = .stdresid)) +
  geom_point() + geom_hline(yintercept = 0) +
  geom_smooth(method = 'lm')


# Остатки в зависимости от непрерывного предиктора
ggplot(Mod_cov_2_diag, aes(x = height, y = .stdresid)) +
  geom_point() +
  geom_hline(yintercept = 0) +
  geom_smooth(method = 'lm')

# Остатки в зависимости от дискретного предиктора
ggplot(Mod_cov_2_diag, aes(x = age_group, y = .stdresid)) +
  geom_boxplot() +
  geom_hline(yintercept = 0)


# Результаты ANCOVA и визуализация модели --------------------------

summary(Mod_cov_2)

# Можем воспользоваться частными F-тестами, чтобы протестировать значимость предикторов в целом.
Anova(Mod_cov_2)

# Сложности визуализации
# Необходимо вместо обычных средних использовать **скорректированные средние** (**Adjusted means**).
# Создаем искусственный датасет со всеми возможными значениями дискретного предиктора
# и средним значением ковариаты
new_data <- data.frame(age_group = factor(levels(tlc$age_group),
                                          levels = levels(tlc$age_group)),
                       height = mean(tlc$height))
# Предсказанные моделью скорретированные средние значения зависимой переменной
new_data$fit <- predict(Mod_cov_2, newdata = new_data, se.fit = TRUE)$fit

# Стандартные ошибки
new_data$se <- predict(Mod_cov_2, newdata = new_data, se.fit = TRUE)$se.fit
# t для расчета доверительного интервала
t_crit <- qt(0.975, df = nrow(tlc) - length(coef(Mod_cov_2)))
# Границы доверительных интервалов
new_data$upr <- new_data$fit + t_crit * new_data$se
new_data$lwr <- new_data$fit - t_crit * new_data$se

# Визуализация модели
ggplot(new_data, aes(x = age_group, y = fit)) +
  geom_bar(stat = 'identity', aes(fill = age_group)) +
  geom_errorbar(aes(ymin = lwr, ymax = upr), width = 0.2) +
  scale_x_discrete(labels = c('Подростки', 'Молодые', 'Взрослые')) +
  guides(fill = 'none') +
  labs(x = 'Возрастная группа', y = 'Объем легких')
