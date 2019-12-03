# Однофакторный дисперсионный анализ
# М.А. Варфоломеева, PhD
# Биологический факультет, СПбГУ

# Пример --- нектар с кофеином  -----------------------
# Растения используют кофеин для защиты от
# поедания травоядными. Как присутствие кофеина
# будет влиять на поведение опылителей?
# Пчелам предлагали кормушки с двумя чашками:
# - Чистый 20% сахарный сироп
# - Сироп с кофеином (50, 100, 150 или 200\textperthousand)
# По разнице съеденного в двух чашках можно судить о\ степени привлекательности сиропа с кофеином.
# Данные: Singaravelan et al. 2005

# Открываем данные и приводим их в порядок
library(readxl)
bees <- read_excel('data/caffeine_Singaravelan_et_al_2005.xlsx', sheet = 'bees_feeding')

# Все ли правильно открылось?
str(bees)

# Для удобства создадим короткие названия переменных
colnames(bees) <- c('conc', 'd')

# Концентрацию кофеина сделаем фактором
bees$conc <- factor(bees$conc)

# Есть ли пропущенные значения
colSums(is.na(bees))

# Какой объем выборки (объемы групп)
table(bees$conc)

# График
library(ggplot2)
theme_set(theme_bw())
ggplot(data = bees, aes(x = conc, y = d)) +
  geom_point(alpha = 0.5, colour = 'steelblue') +
  stat_summary(geom = 'pointrange', fun.data = mean_cl_normal,
               position = position_nudge(x = 0.1))


# Линейная модель с одним дискретным предиктором в параметризации индикаторов  -----------------------

# Подбираем линейную модель в параметризации индикаторов
mod_treatment <- lm(d ~ conc, data = bees)
mod_treatment

# Линейная модель с одним дискретным предиктором в параметризации эффектов -----------------------

# Подбираем линейную модель в параметризации эффектов
mod_sum <- lm(d ~ conc, data = bees, contrasts = list(conc = contr.sum))
coef(mod_sum)


# Диагностика линейной модели -----------------------

# Данные для анализа остатков
mod_treatment_diag <- fortify(mod_treatment) # функция из пакета ggplot2
head(mod_treatment_diag)

# График расстояния Кука
ggplot(mod_treatment_diag, aes(x = 1:nrow(mod_treatment_diag), y = .cooksd)) +
  geom_bar(stat = 'identity') +
  geom_hline(yintercept = 1, linetype = 2)

# График остатков от предсказанных значений
gg_resid <- ggplot(data = mod_treatment_diag, aes(x = .fitted, y = .stdresid)) +
  geom_point() + geom_hline(yintercept = 0)
gg_resid

# График зависимости остатков от предикторов в модели
ggplot(data = mod_treatment_diag, aes(x = conc, y = .stdresid)) +
  geom_boxplot() + geom_hline(yintercept = 0)
# Удобнее смотреть на боксплот.

# Квантильный график остатков
library(car)
qqPlot(mod_treatment, id = FALSE) # функция из пакета car


# Тестирование значимости коэффициентов модели с дискретным предиктором  -----------------------
# Мы уже подобрали коэффициенты модели, \newline и можем  теперь записать ее уравнение
coef(mod_treatment)

# t-тесты тестируют различия лишь некоторых групп
summary(mod_treatment)

# Структура общей изменчивости. Дисперсионный анализ  -----------------------

# Дисперсионный анализ в R -----------------------
# В R есть много функций, связанных с дисперсионным анализом:
# Мы рекомендуем `Anova()` из пакета `car`, т.к.
# она умеет тестировать влияние факторов в
# определенном порядке. Когда факторов будет
# больше одного, это станет важно для результатов.

# Способ параметризации не влияет на результаты однофакторного дисперсионного анализа
# Параметризация индикаторов
Anova(mod_treatment)
# Параметризация эффектов
Anova(mod_sum)


# Зоопарк пост хок тестов  -----------------------


# Пост хок тесты в R  -----------------------

library(multcomp)
bees_ph <- glht(mod_treatment, linfct = mcp(conc = 'Tukey'))

# Результаты попарных сравнений при помощи теста Тьюки
summary(bees_ph)


# Визуализация результатов дисперсионного анализа  -----------------------

# Данные для графика при помощи `predict()`
# Значения предикторов
new_data <- data.frame(conc = factor(levels(bees$conc), levels = levels(bees$conc)))
# Добавляем предсказанные значения
new_data <- data.frame(
  new_data,
  predict(mod_treatment, newdata = new_data, interval = 'confidence'))
new_data

# Данные для графика вручную
new_data <- data.frame(conc = factor(levels(bees$conc), levels = levels(bees$conc)))

X <- model.matrix(~ conc, data = new_data)   # Модельная матрица
betas <- coef(mod_treatment)                 # Коэффициенты
new_data$fit <- as.numeric(X %*% betas)      # Предсказанные значения
new_data$se <- sqrt(diag(X %*% vcov(mod_treatment) %*% t(X)))      # Стандартные ошибки
t_crit <- qt(0.975, df = nrow(bees) - length(coef(mod_treatment))) # t для дов. инт.
new_data$lwr <- new_data$fit - t_crit * new_data$se
new_data$upr <- new_data$fit + t_crit * new_data$se
new_data

# График предсказанных значений
gg_lsmeans <- ggplot(data = new_data, aes(x = conc, y = fit)) +
  geom_errorbar(aes(ymin = lwr, ymax = upr), width = 0.1) +
  geom_point(aes(colour = conc), size = 3) +
  labs(x = 'Концентрация кофеина, ‰', y = 'Разница поглощения сиропа\n(чистого и с кофеином)') +
  scale_colour_brewer(palette = 'Dark2') + theme(legend.position = 'none')
gg_lsmeans

# Можно привести результаты пост хок теста на графике
gg_lsmeans +
  geom_text(aes(label = c('AB', 'B', 'AC', 'AC')),
            size = 3, position = position_nudge(x = 0.2))

