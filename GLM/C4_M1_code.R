# Знакомство с обобщенными линейными моделями
# М.А. Варфоломеева, PhD
# Биологический факультет, СПбГУ

# ## Линейные модели для величин с разной формой распределения ######
# ## Зоопарк распределений ######

# ## Обобщенные линейные модели ######

# ## Обобщенные линейные модели (Generalized Linear Models)

# ## GLM с нормальным распределением отклика ######

# ## Пример -- питательная ценность икры ######

# ## Энергетическая ценность икры
# Один из показателей жизнеспособности икры -- доля сухого вещества. Она пропорциональна количеству запасенной энергии.
# Отличается ли энергетическая ценность икры большой озерной форели (Salvelinus namaycush) в сентябре и ноябре?
# Данные: Lantry et al., 2008
# Источник: пакет Stat2Data

# ## Открываем данные
library(Stat2Data)
data(FishEggs)

# Все ли правильно открылось?
str(FishEggs)

# Нет ли пропущенных значений?
colSums(is.na(FishEggs))

# ## Разбираемся с уровнями факторов

# Уровни факторов в исходных данных:
levels(FishEggs$Month)

# Делаем базовым уровнем сентябрь.
FishEggs$Month <- relevel(FishEggs$Month, ref = 'Sep')

# Теперь уровни в хронологическом порядке:
levels(FishEggs$Month)

# ## Каковы объемы выборок?
table(FishEggs$Month)
table(FishEggs$Month, FishEggs$Age)

# ## Нет ли значений-выбросов?
library(cowplot)
library(ggplot2)
theme_set(theme_bw())
gg_dot <- ggplot(FishEggs, aes(y = 1:nrow(FishEggs))) + geom_point()
plot_grid(gg_dot + aes(x = Age),
          gg_dot + aes(x = PctDM), nrow = 1)



# ## Подбор GLM c нормальным распределением отклика в R

# ## Какому распределению подчиняется зависимая переменная?

# Содержание сухого вещества выражено в процентах
range(FishEggs$PctDM)
# Мы для начала будем считать, что PictDM_i - нормально распределенная величина.
# Придется следить, чтобы предсказанные значения опадали в осмысленный диапазон от 0 до 100.

# ## Модель для описания питательной ценности икры
egg_model <- glm(PctDM ~ Age * Month, data = FishEggs)
egg_model

# Чтобы записать модель, нужна еще сигма.
sigma(egg_model)


# ## Разновидности остатков обобщенных линейных моделях ######

# ## Остатки в масштабе отклика (response residuals)
resid(egg_model, type = 'response')[1:5]

# ## Пирсоновские остатки (Pearson's residuals)
resid(egg_model, type = 'pearson')[1:5]

# Для GLM с нормальным распределением отклика остатки в масштабе отклика и пирсоновские остатки -- одно и то же для таких моделей.
rbind(resid(egg_model, type = 'response')[1:5],
      resid(egg_model, type = 'pearson' )[1:5])


# ## Диагностика GLM с нормальным распределением отклика ######

# ## Условия применимости GLM с нормальным распределением отклика
#
# - Случайность и независимость наблюдений внутри групп.
# - Нормальное распределение остатков.
# - Гомогенность дисперсий остатков.
# - Отсутствие коллинеарности предикторов.
#
# ## Проверка на коллинеарность
library(car)
vif(update(egg_model, . ~ . - Age:Month))

# ## Данные для анализа остатков
egg_model_diag <- fortify(egg_model) # функция из пакета ggplot2
head(egg_model_diag)

# ## График расстояния Кука
ggplot(egg_model_diag, aes(x = 1:nrow(egg_model_diag), y = .cooksd)) +
  geom_bar(stat = 'identity') +
  geom_hline(yintercept = 1, linetype = 2)

# ## График остатков от предсказанных значений
gg_resid <- ggplot(data = egg_model_diag, aes(x = .fitted, y = .stdresid)) +
  geom_point() + geom_hline(yintercept = 0)
gg_resid

# ## График зависимости остатков от предикторов в модели

ggplot(data = egg_model_diag, aes(x = Age, y = .stdresid)) +
  geom_point() + geom_hline(yintercept = 0)
ggplot(data = egg_model_diag, aes(x = Month, y = .stdresid)) +
  geom_boxplot() + geom_hline(yintercept = 0)


# ## Тестирование значимости коэффициентов при помощи критерия Вальда ######
summary(egg_model)

# ## Работаем с логарифмами правдоподобий. Анализ девиансы ######


# ## Анализ девиансы в R ######

# ## Тестируем значимость модели целиком при помощи LRT
null_model <- glm(PctDM ~ 1, data = FishEggs)
anova(null_model, egg_model, test = 'Chi')

# ## Тестируем значимость отдельных предикторов при помощи LRT
# Используем т.наз. II тип тестов ("II тип сумм квадратов").
# ## Тестируем значимость взаимодействия
drop1(egg_model, test = 'Chi')
# ## Тестируем значимость предикторов, когда взаимодействие удалено из модели
egg_model_without_interaction <- update(egg_model, . ~ . -Age:Month)
drop1(egg_model_without_interaction, test = 'Chi')


# ## Качество подгонки GLM ######

# ## Доля объясненной девиансы

(egg_model$null.deviance - egg_model$deviance) / egg_model$null.deviance

# ## Визуализация GLM ######

# ## Искуственные данные для предсказаний
library(dplyr)
new_data <- FishEggs %>% group_by(Month) %>%
  do(data.frame(Age = seq(min(.$Age), max(.$Age), length.out = 100)))

head(new_data, 2)


# ## Данные для графика при помощи `predict()`
Predictions <- predict(egg_model, newdata = new_data, se.fit = TRUE)
new_data$fit <- Predictions$fit                           # Предсказанные значения
new_data$se <- Predictions$se.fit                         # Стандартные ошибки
t_crit <- qt(0.975, df = nrow(FishEggs) - length(coef(egg_model))) # t для дов. инт.
new_data$lwr <- new_data$fit - t_crit * new_data$se
new_data$upr <- new_data$fit + t_crit * new_data$se

head(new_data, 2)

# ## Данные для графика вручную
X <- model.matrix(~ Age * Month, data = new_data)         # Модельная матрица
betas <- coef(egg_model)                                  # Коэффициенты
new_data$fit <- X %*% betas                               # Предсказанные значения
new_data$se <- sqrt(diag(X %*% vcov(egg_model) %*% t(X))) # Стандартные ошибки
t_crit <- qt(0.975, df = nrow(FishEggs) - length(coef(egg_model))) # t для дов. инт.
new_data$lwr <- new_data$fit - t_crit * new_data$se
new_data$upr <- new_data$fit + t_crit * new_data$se

head(new_data, 2)

# ## Строим график
Plot_egg <- ggplot(new_data, aes(x = Age, y = fit)) +
  geom_ribbon(alpha = 0.2, aes(ymin = lwr, ymax = upr, group = Month)) +
  geom_line(aes(colour = Month), size = 1) +
  geom_point(data = FishEggs, aes(x = Age, y = PctDM, colour = Month))
Plot_egg


# ## Последние штрихи к графику
Plot_final <- Plot_egg +
  labs(x = 'Возраст рыбы', y = 'Содержание \nсухого вещества, %',
       color = 'Месяц') +
  scale_colour_discrete(labels = c('Сентябрь', 'Ноябрь'))
Plot_final

