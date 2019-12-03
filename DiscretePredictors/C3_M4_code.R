# Многофакторный дисперсионный анализ
# М.А. Варфоломеева, PhD
# Биологический факультет, СПбГУ


# Пример --- игра "Диктатор"  --------------------


# Альтруистическое поведение в сплоченных группах
# Как влияет на степень альтруизма возможность узнавания партнера?
# Будет ли разной степень альтруизма по отношению к "своим" и "чужим"?
# Участников эксперимента разделили на группы. Каждая группа выполнила совместное творческое задание --- чтобы сильнее сплотиться.
# Эксперимент:
# "Диктатор" получил 11 монет и конверт, в котором была описана одна из игровых ситуаций, и должен был решить, сколько денег готов отдать партнеру.
# Данные: Ritov, Kogut, 2017


# Открываем данные
library(foreign)
dat <- read.spss('data/journal.pone.0187903.s002.sav', to.data.frame = TRUE)
colSums(is.na(dat))


# Нас интересуют только такие игроки:
# - Диктаторы, распоряжавшиеся деньгами (`dat$player == 1`)
# - Отобранные для исследования (`dat$filter == 1`)
# - Закончившие игру (`!is.na(dat$will_giv)`)
dict <- dat[dat$player == 1 & dat$filter == 1 & !is.na(dat$will_giv), ]

# Знакомимся с данными
str(dict)
dict$gender <- factor(dict$gender, levels = c(1, 2), labels = c('male', 'female'))
dict$identifi <- factor(dict$identifi, levels = c(1, 0), labels = c('id', 'unid'))
dict$in_out <- factor(dict$in_out, levels = c(1, 0), labels = c('in', 'out'))

# Каковы объемы выборок?
table(dict$gender)
table(dict$identifi, dict$in_out)

# Как выглядят данные?
library(ggplot2)
theme_set(theme_bw())

ggplot(dict, aes(x = in_out, y = will_giv, colour = identifi)) +
  stat_summary()

# Двухфакторный дисперсионный анализ. Параметризация индикаторов  --------------------

# Индикаторные переменные
contr.treatment(levels(dict$in_out))
contr.treatment(levels(dict$identifi))

## Подбираем линейную модель в параметризации индикаторов (contr.treatment)
mod_treatment <- lm(will_giv ~ in_out * identifi, data = dict)
mod_treatment

# Двухфакторный дисперсионный анализ. Параметризация эффектов  --------------------

# Переменные-эффекты
contr.sum(levels(dict$in_out))
contr.sum(levels(dict$identifi))

## Подбираем линейную модель в параметризации эффектов (contr.sum)
mod_sum <- lm(will_giv ~ in_out * identifi, data = dict,
              contrasts = list(in_out = 'contr.sum', identifi = 'contr.sum'))
mod_sum

# Диагностика линейной модели  -------------------
# Нужно проверить, выполняются ли условия применимости для модели в нужной параметризации

# Данные для анализа остатков
mod_treatment_diag <- fortify(mod_treatment) # функция из пакета ggplot2
head(mod_treatment_diag)

# График расстояния Кука
ggplot(mod_treatment_diag, aes(x = 1:nrow(mod_treatment_diag), y = .cooksd)) +
  geom_bar(stat = 'identity') +
  geom_hline(yintercept = 1, linetype = 2)

# График остатков от предсказанных значений
gg_resid <- ggplot(data = mod_treatment_diag, aes(x = .fitted, y = .stdresid)) +
  geom_jitter() + geom_hline(yintercept = 0)
gg_resid


# График зависимости остатков от предикторов в модели
ggplot(data = mod_treatment_diag, aes(x = in_out, y = .stdresid, colour = identifi)) +
  geom_boxplot() + geom_hline(yintercept = 0)
# Удобнее смотреть на боксплот.

# Квантильный график остатков
library(car)
qqPlot(mod_treatment, id = FALSE) # функция из пакета car

# Проблемы при анализе несбалансированных данных. Типы сумм квадратов  --------------------

# В нашем примере несбалансированные данные
table(dict$in_out, dict$identifi)

# Для несбалансированных данных в зависимости от
# ваших гипотез выберите подходящий тип сумм
# квадратов и параметризацию.


# Дисперсионный анализ в R  --------------------

# Как влияет параметризация на результаты дисперсионного анализа со II и III типом сумм квадратов

# Для демонстрации нам понадобятся две модели:
# Параметризация индикаторов
mod_treatment <- lm(will_giv ~ in_out * identifi, data = dict)
# Параметризация эффектов
mod_sum <- lm(will_giv ~ in_out * identifi, data = dict,
              contrasts = list(in_out = contr.sum, identifi = contr.sum))

# Дисперсионный анализ со II типом сумм квадратов
# Результат не зависит от выбранной параметризации
# Параметризация индикаторов
Anova(mod_treatment, type = 'II')
# Параметризация эффектов
Anova(mod_sum, type = 'II')


# Дисперсионный анализ c III типом сумм квадратов

# Чтобы пользоваться III типом сумм квадратов, нужно, чтобы взаимодействие было независимо от главных эффектов.

# В параметризации индикаторов взаимодействие коллинеарно с другими предикторами.
vif(mod_treatment)

# В параметризации эффектов коллинеарности нет
vif(mod_sum)

# При использовании III типа сумм квадратов \newline нужно правильно выбрать параметризацию.

# Параметризация индикаторов
Anova(mod_treatment, type = 'III') # Неправильно!
# Параметризация эффектов (правильно)
Anova(mod_sum, type = 'III')


# Пост хок тест для взаимодействия факторов  --------------------

# Делаем пост хок тест для взаимодействия факторов
# Создаем переменную-взаимодействие
dict$inter <- interaction(dict$in_out, dict$identifi)

# Подбираем линейную модель от этой переменной без свободного члена
mod_inter <- lm(will_giv ~ -1 + inter, data = dict)

# Делаем пост хок тест для этой модели
library(multcomp)
dat_tukey <- glht(mod_inter, linfct = mcp(inter = 'Tukey'))
summary(dat_tukey)


# Визуализация результатов многофакторного дисперсионного анализа  --------------------


# Данные для графика при помощи `predict()`
# У нас два дискретных фактора, поэтому вначале используем `expand.grid()`
# Значения предикторов
new_data <- expand.grid(in_out = levels(dict$in_out),
                      identifi = levels(dict$identifi))
# Добавляем предсказанные значения
new_data <- data.frame(
  new_data,
  predict(mod_treatment, newdata = new_data, interval = 'confidence'))
new_data


# Данные для графика вручную для модели в обычной параметризации
new_data <- expand.grid(in_out = levels(dict$in_out),
                      identifi = levels(dict$identifi))
X <- model.matrix(~ in_out * identifi, data = new_data) # Модельная матрица
betas <- coef(mod_treatment)                            # Коэффициенты
new_data$fit <- X %*% betas                             # Предсказанные значения
new_data$se <- sqrt(diag(X %*% vcov(mod_treatment) %*% t(X)))      # Стандартные ошибки
t_crit <- qt(0.975, df = nrow(dict) - length(coef(mod_treatment))) # t для дов. инт.
new_data$lwr <- new_data$fit - t_crit * new_data$se
new_data$upr <- new_data$fit + t_crit * new_data$se
new_data


# Черновой график результатов
gg_linep <- ggplot(data = new_data, aes(x = in_out, y = fit,
              ymin = lwr,  ymax = upr, colour = identifi)) +
  geom_point() +
  geom_line(aes(group = identifi)) +
  geom_errorbar(width = 0.1)
gg_linep

# Немного улучшенный график
pos <- position_dodge(width = 0.2)
gg_linep <- ggplot(data = new_data, aes(x = in_out, y = fit,
              ymin = lwr,  ymax = upr, colour = identifi)) +
  geom_point(position = pos) +
  geom_line(aes(group = identifi), position = pos) +
  geom_errorbar(width = 0.1, position = pos)
gg_linep

# Приводим график в приличный вид
gg_final <- gg_linep + labs(x = 'Принадлежность к группе',  y = 'Щедрость') +
  scale_colour_brewer(name = 'Партнер', palette = 'Dark2',
    labels = c('Идентифицируем', 'Неидентифицируем')) +
  scale_x_discrete(labels = c('Свой', 'Чужой'))
gg_final

