# Смешанные линейные модели для счетных данных
# М.А. Варфоломеева, PhD
# Биологический факультет, СПбГУ

# ## Счетные данные в смешанных моделях #####


# ## Пример -- саламандры и добыча угля #####

# Добыча угля открытым способом сильно
# модифицирует ландшафт в Аппалачских горах. В
# результате этого в горных ручьях меняется ионный
# состав воды, pH и объем стока.
# Саламандры нескольких видов -- важный элемент
# экосистемы ручьев. Как разработка угля влияет на
# их численность?
# Источник: Price et al., 2015, 2016

# ## Знакомство с данными
library(glmmTMB)
data(Salamanders)
head(Salamanders)

str(Salamanders)

colSums(is.na(Salamanders))

# ## Как устроены данные?

# Считали 7 категорий саламандр (виды и жизненные стадии):
table(Salamanders$spp)

# В местах, чьи названия начинаются на VF, ведется угледобыча:
with(Salamanders, table(mined, site))

# В каждом месте 4 наблюдения, т.е. `site` нужно учесть как случайный эффект:
with(Salamanders, table(spp, site))


# ## Есть ли выбросы?
library(ggplot2); library(cowplot); theme_set(theme_bw())

gg_dot <- ggplot(Salamanders, aes(y = 1:nrow(Salamanders))) + geom_point()

plot_grid(gg_dot + aes(x = count), gg_dot + aes(x = cover),
          gg_dot + aes(x = Wtemp), nrow = 1)


# ## Отклик -- счетная переменная
ggplot(Salamanders, aes(x = count)) + geom_histogram()
mean(Salamanders$count == 0) # Какова пропорция нулей?

# ## Нет ли коллинеарности предикторов?
library(car)
vif(lm(count ~ spp + mined, Salamanders))

# ## Есть ли взаимодействие между дискретными предикторами?
ggplot(data = Salamanders) +
  stat_summary(aes(x = spp, y = count, colour = mined),
               fun.data = mean_cl_normal, position = position_dodge(width = 0.2))

# ## Смешанная модель с Пуассоновским распределением отклика #####


# ## Первая попытка подбора модели не удается
library(lme4)
mod_pois1 <- glmer(count ~ spp * mined + (1|site), data = Salamanders,
                  family = 'poisson')
# Warning message:
# In checkConv(attr(opt, "derivs"), opt$par, ctrl = control$checkConv,  :
#   Model failed to converge with max|grad| = 0.00689362 (tol = 0.001, component 1)


# Можно попытаться изменить настройки подбора модели при помощи `glmerControl()`
# Увеличим число итераций
mod_pois2 <- glmer(count ~ spp * mined + (1|site),
                   family = 'poisson', data = Salamanders,
                   control = glmerControl(optimizer = 'bobyqa',
                               optCtrl = list(maxfun = 200000)))

# ## Все, что нужно для записи уравнения модели
fixef(mod_pois2)
VarCorr(mod_pois2)

# ## Интерпретация коэффициентов модели
coef(summary(mod_pois2))

# ## Диагностика моделей с Пуассоновским распределением. Избыточность дисперсии #####

# ## График остатков
mod_pois2_diag <- data.frame(.fitted = predict(mod_pois2, type = 'response'),
                             .resid = residuals(mod_pois2, type = 'pearson'))
ggplot(mod_pois2_diag, aes(x = .fitted, y = .resid)) +
  geom_point() + geom_smooth() +
  geom_hline(yintercept = 0)


# ## Проверка на сверхдисперсию
library(sjstats)
overdisp(mod_pois2)

# ## Смешанная модель с отрицательным биномиальным распределением отклика #####

# ## Первая попытка подбора модели не удается
mod_nb1 <- glmer.nb(count ~ spp * mined + (1|site), data = Salamanders)
# Warning in checkConv(attr(opt, ”derivs”), opt$par, ctrl = control$checkConv, : Model
# failed to converge with max|grad| = 0.00689362 (tol = 0.001, component 1)

# ## После увеличения числа итераций модель сходится
mod_nb2 <- glmer.nb(count ~ spp * mined + (1|site),
                    data = Salamanders,
                    control = glmerControl(optimizer = 'bobyqa',
                                optCtrl = list(maxfun = 200000)))

# ## Все, что нужно для записи уравнения модели
fixef(mod_nb2)
VarCorr(mod_nb2)
getME(mod_nb2, 'glmer.nb.theta')


# ## Интерпретация коэффициентов модели
coef(summary(mod_nb2))

# ## Коэффициент внутриклассовой корреляции
icc(mod_nb2)


# ## Диагностика модели с отрицательным биномиальным распределением отклика #####

# ## График остатков
mod_nb2_diag <- data.frame(.fitted = predict(mod_nb2, type = 'response'),
                           .resid = residuals(mod_nb2, type = 'pearson'),
                           Salamanders)
gg_resid <- ggplot(mod_nb2_diag, aes(y = .resid)) + geom_hline(yintercept = 0)
gg_resid + geom_point(aes(x = .fitted))

# ## Проверка на сверхдисперсию
# Обратите внимание, у моделей с отрицательным
# биномиальным распределением добавляется еще один
# параметр.
overdisp(mod_nb2)

# ## Графики остатков от переменных, которые есть в модели
plot_grid(gg_resid + geom_boxplot(aes(x = mined)),
          gg_resid + geom_boxplot(aes(x = spp)),
          nrow = 1)
# ## Графики остатков от переменных, которых нет в модели
plot_grid(gg_resid + geom_point(aes(x = cover)),
          gg_resid + geom_point(aes(x = Wtemp)),
          nrow = 1)

# ## Тестирование гипотез #####

# ## Тестируем значимость отдельных предикторов при помощи LRT
# Используем т.наз. II тип тестов ("II тип сумм квадратов").
# Сначала тестируем значимость взаимодействия
mod_nb3 <- update(mod_nb2, .~. - spp:mined)
anova(mod_nb2, mod_nb3, test = 'Chi')


# ## Какие именно различия значимы?
library(emmeans)
## ?emmeans
## help('contrast-methods')

# ## Попарные различия в масштабе функции связи
mining_effect <- emmeans(mod_nb2, pairwise ~ mined | spp)
mining_effect$contrasts
# На сколько единиц различается логарифм
# численности саламандр в зависимости от того,
# ведется ли добыча угля?

# ## Попарные различия в масштабе отклика
mining_effect <- emmeans(mod_nb2, pairwise ~ mined | spp, type = 'response')
mining_effect$contrasts
# Во сколько раз различается численность саламандр
# в зависимости от того, ведется ли добыча угля?

# ## Можно визуализировать результаты
emmip(mod_nb2, ~ mined | spp, CIs = TRUE, type = 'response')


# ## Визуализация модели #####

# ## Данные для графика предсказаний фиксированной части модели
library(dplyr)
new_data <- expand.grid(mined = levels(Salamanders$mined), spp = levels(Salamanders$spp))

new_data$fit <- predict(mod_nb2, new_data, type = 'response', re.form = NA)
head(new_data, 3)

# ## Предсказания фиксированной части модели в матричном виде
# Вычислим __приблизительные__ доверительные интервалы.

# Предсказанные значения при помощи матриц
X <- model.matrix(~ spp * mined, data = new_data)
betas <- fixef(mod_nb2)

# Предсказанные значения и стандартные ошибки...
# ...в масштабе функции связи (логарифм)
new_data$fit_eta <- X %*% betas
new_data$se_eta <- sqrt(diag(X %*% vcov(mod_nb2) %*% t(X)))

# ...в масштабе отклика (применяем функцию, обратную функции связи)
new_data$fit_mu <- exp(new_data$fit_eta)
new_data$lwr <- exp(new_data$fit_eta - 2 * new_data$se_eta)
new_data$upr <- exp(new_data$fit_eta + 2 * new_data$se_eta)


# ## График предсказаний фиксированной части модели
pos <- position_dodge(width = 0.2)
ggplot(data = new_data, aes(x = spp, y = fit_mu, colour = mined)) +
  geom_errorbar(aes(ymin = lwr, ymax = upr), width = 0.1, position = pos) +
  geom_point(position = pos)

