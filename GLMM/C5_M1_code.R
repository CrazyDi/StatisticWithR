# Знакомство со смешанными линейными моделями
# М.А. Варфоломеева, PhD
# Биологический факультет, СПбГУ

# ## Когда нужны смешанные модели #####

# ## Пример -- недосып и время реакции #####
# Степень усталости может влиять на скорость реакции, но как именно?
# В ночь перед началом исследования всем
# испытуемым давали поспать нормальное время, а в
# следующие 9 ночей --- только по 3 часа. Каждый
# день измеряли время реакции в серии тестов.
# Как время реакции людей зависит от продолжительности бессонницы?
# Данные: Belenky et al., 2003
# Источник: пакет `lme4`
#
# - `Reaction` --- среднее время реакции в серии тестов в день наблюдения, мс
# - `Days` --- число дней депривации сна
# - `Subject` --- номер испытуемого

# ## Открываем данные
library(lme4)
data(sleepstudy)

sleep <- sleepstudy
str(sleep)

# ## Знакомимся с данными
# Есть ли пропущенные значения?
colSums(is.na(sleep))
# Сколько субъектов?
length(unique(sleep$Subject))
# Сколько наблюдений для каждого субъекта?
table(sleep$Subject)

# ## Есть ли выбросы?
library(ggplot2)
theme_set(theme_bw())

ggplot(sleep, aes(x = Reaction, y = 1:nrow(sleep))) +
  geom_point()

# ## Как меняется время реакции разных субъектов?
ggplot(sleep, aes(x = Reaction, y = Subject, colour = Days)) +
  geom_point()
# Мы не можем игнорировать межиндивидуальную изменчивость.

# ## Недосып. Почему обычные методы не работают? #####

# ## Плохое решение: не учитываем группирующий фактор
Wrong1 <- glm(Reaction ~ Days, data = sleep)

summary(Wrong1)

ggplot(sleep, aes(x = Days, y = Reaction)) +
  geom_point() +
  geom_smooth(se = TRUE, method = "lm", size = 1)

# ## Громоздкое решение: группирующий фактор как фиксированный
Wrong2 <- glm(Reaction ~ Days + Subject, data = sleep)
coef(Wrong2)

summary(Wrong2)

ggplot(fortify(Wrong2), aes(x = Days, colour = Subject)) +
  geom_line(aes(y = .fitted, group = Subject)) +
  geom_point(data = sleep, aes(y = Reaction)) +
  guides(colour = guide_legend(ncol = 2))

# ## Фиксированные и случайные факторы #####

# ## GLMM со случайным отрезком #####

# `lmer` по умолчанию использует REML
mod1 <- lmer(Reaction ~ Days + (1 | Subject), data = sleep)

# ## Запишем уравнение модели со случайным отрезком
summary(mod1)

# ## Визуализация предсказаний GLMM со случайным отрезком #####

# ## Данные для графика предсказаний фиксированной части модели
library(dplyr)
new_data <- sleep %>% group_by(Subject) %>%
  do(data.frame(Days = seq(min(.$Days), max(.$Days), length = 10)))

head(new_data, 3)

# ## Предсказания фиксированной части модели при помощи predict()
?predict.merMod
# Функция `predict()` в `lme4` не считает стандартные ошибки и доверительные интервалы.
new_data$fit <- predict(mod1, new_data, type = 'response', re.form = NA)
head(new_data, 3)

# ## Предсказания фиксированной части модели в матричном виде
X <- model.matrix(~ Days, data = new_data)
betas <- fixef(mod1)
new_data$fit <- X %*% betas

# Cтандартные ошибки и __приблизительные__ доверительные интервалы.
new_data$se <- sqrt( diag(X %*% vcov(mod1) %*% t(X)) )
new_data$lwr <- new_data$fit - 2 * new_data$se
new_data$upr <- new_data$fit + 2 * new_data$se

# Более точные доверительные интервалы  можно получить при помощи бутстрепа (см. Faraway, 2017).

# ## График предсказаний фиксированной части модели
ggplot(data = new_data, aes(x = Days, y = fit)) +
  geom_ribbon(alpha = 0.35, aes(ymin = lwr, ymax = upr)) +
  geom_line() + geom_point(data = sleep, aes(x = Days, y = Reaction))

# ## Предсказания для каждого уровня случайного фактора тоже можно получить
new_data$fit_subj <- predict(mod1, new_data, type = 'response')
ggplot(new_data, aes(x = Days, y = fit_subj)) +
  geom_ribbon(alpha = 0.3, aes(ymin = lwr, ymax = upr)) +
  geom_line(aes(colour = Subject)) +
  geom_point(data = sleep, aes(x = Days, y = Reaction, colour = Subject))  +
  guides(colour = guide_legend(ncol = 2))


# ## Индуцированная корреляция #####


# ## Вычислим коэффициент внутриклассовой корреляции

# При помощи пакета `sjstats`:
library(sjstats)
icc(mod1)

# Вручную:
VarCorr(mod1)  # Случайные эффекты отдельно
37.124^2 / (37.124^2 + 30.991^2)

# ## Диагностика модели со случайным отрезком  #####

# ## Данные для анализа остатков
mod1_diag <- data.frame(
  sleep,
  .fitted = fitted(mod1),
  .resid = resid(mod1, type = 'pearson'),
  .scresid = resid(mod1, type = 'pearson', scaled = TRUE))

head(mod1_diag, 4)
# - `.fitted` --- предсказанные значения.
# - `.resid` --- Пирсоновские остатки.
# - `.scresid` --- стандартизованные Пирсоновские остатки.

# ## График остатков от предсказанных значений
gg_resid <- ggplot(mod1_diag, aes(y = .scresid)) +
  geom_hline(yintercept = 0)
gg_resid + geom_point(aes(x = .fitted))


# ## Графики остатков от ковариат в модели и не в модели
gg_resid + geom_boxplot(aes(x = factor(Days)))


# ## Графики остатков от ковариат в модели и не в модели
gg_resid + geom_boxplot(aes(x = Subject))


# ## GLMM со случайным отрезком и углом наклона #####

# ## Смешанная модель со случайным отрезком и углом наклона

# На графике индивидуальных эффектов было видно, что измерения для разных субъектов, возможно, идут непараллельными линиями.
new_data$fit_subj <- predict(mod1, new_data, type = 'response')
ggplot(new_data, aes(x = Days, y = fit_subj)) +
  geom_ribbon(alpha = 0.3, aes(ymin = lwr, ymax = upr)) +
  geom_line(aes(colour = Subject)) +
  geom_point(data = sleep, aes(x = Days, y = Reaction, colour = Subject))  +
  guides(colour = guide_legend(ncol = 2))
# Усложним модель --- добавим случайные изменения угла наклона для каждого из субъектов.

# ## Подберем модель со случайным отрезком и углом наклона

mod2 <- lmer(Reaction ~ Days + ( 1 + Days|Subject), data = sleep)

# ## Запишем уравнение модели со случайным отрезком
summary(mod2)


# ## Визуализация предсказаний GLMM со случайным отрезком и углом наклона #####

# ## Данные для графика предсказаний фиксированной части модели
library(dplyr)
new_data <- sleep %>% group_by(Subject) %>%
  do(data.frame(Days = seq(min(.$Days), max(.$Days), length = 10)))

new_data$fit <- predict(mod2, new_data, type = 'response', re.form = NA)
head(new_data, 3)

# ## Предсказания фиксированной части модели в матричном виде

# Предсказанные значения при помощи матриц
X <- model.matrix(~ Days, data = new_data)
betas <- fixef(mod2)
new_data$fit <- X %*% betas

# Cтандартные ошибки и __приблизительные__ доверительные интервалы.
new_data$se <- sqrt( diag(X %*% vcov(mod2) %*% t(X)) )

new_data$lwr <- new_data$fit - 2 * new_data$se
new_data$upr <- new_data$fit + 2 * new_data$se

# Более точные доверительные интервалы  можно получить при помощи бутстрепа (см. Faraway, 2017).

# ## График предсказаний фиксированной части модели
ggplot(data = new_data, aes(x = Days, y = fit)) +
  geom_ribbon(alpha = 0.35, aes(ymin = lwr, ymax = upr)) +
  geom_line() + geom_point(data = sleep, aes(x = Days, y = Reaction))


# ## Предсказания для каждого уровня случайного фактора
new_data$fit_subj <- predict(mod2, new_data, type = 'response')
ggplot(new_data, aes(x = Days, y = fit_subj)) +
  geom_ribbon(alpha = 0.3, aes(ymin = lwr, ymax = upr)) +
  geom_line(aes(colour = Subject)) +
  geom_point(data = sleep, aes(x = Days, y = Reaction, colour = Subject))  +
  guides(colour = guide_legend(ncol = 2))


# ## Диагностика модели со случайным отрезком и углом наклона #####

# ## Данные для анализа остатков
mod2_diag <- data.frame(
  sleep,
  .fitted = fitted(mod2),
  .resid = resid(mod2, type = 'pearson'),
  .scresid = resid(mod2, type = 'pearson', scaled = TRUE))

head(mod2_diag, 4)

# ## График остатков от предсказанных значений
gg_resid <- ggplot(mod2_diag, aes(y = .scresid)) +
  geom_hline(yintercept = 0)
gg_resid + geom_point(aes(x = .fitted))

# ## Графики остатков от ковариат в модели и не в модели
gg_resid + geom_boxplot(aes(x = factor(Days)))

# ## Графики остатков от ковариат в модели и не в модели
gg_resid + geom_boxplot(aes(x = Subject))


# ## Смешанные линейные модели #####

# ## Тестирование гипотез в смешанных моделях #####

# ## t-(или z-) тесты Вальда
coef(summary(mod2))

# ## Тесты отношения правдоподобий (LRT)

# ## LRT для случайных эффектов
mod2 <- lmer(Reaction ~ Days + (1 + Days | Subject), data = sleep, REML = TRUE)
mod2_null <- lmer(Reaction ~ Days + (1 | Subject), data = sleep, REML = TRUE)
anova(mod2, mod2_null, refit = FALSE)

# ## LRT для фиксированных эффектов
mod2.ml <- lmer(Reaction ~ Days + (1 + Days | Subject), data = sleep, REML = FALSE)
mod2_null.ml <- lmer(Reaction ~ 1 + (1 + Days | Subject), data = sleep, REML = FALSE)
anova(mod2.ml, mod2_null.ml)

# ## Сравнение моделей по AIC
AIC(mod2.ml, mod2_null.ml)


