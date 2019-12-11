# Обобщенные линейные модели для счетных данных
# М.А. Варфоломеева, PhD
# Биологический факультет, СПбГУ

# ## Счетные величины вокруг нас #####

# ## Пример -- гадючий лук и опылители #####

# ## Гадючий лук, копеечник и визиты опылителей
# Гадючий лук (мускари, Leopoldia comosa)--- представитель родной флоры острова Менорка. В 18-19вв туда завезли копеечник венечный (Hedysarum coronarium). Оба вида цветут одновременно и опыляются насекомыми.
# Как зависит число визитов опылителей на цветки гадючьего лука от присутствия вида-вселенца и разнообразия флоры в ближайшей окрестности?
# Источник: Montero-Castaño, Vilà, 2015
# https://doi.org/10.1371/journal.pone.0128595

# Подсчитывали число визитов опылителей на выбранное растение гадючьего лука (в пунктирной рамке) на трех типах участков:
# - Контроль -- только родные виды.
# - Родные виды и вселенец вместе.
# - Родные виды и вселенец вместе, у вселенца удалены цветы.

# ## Переменные в анализе
# Зависимая переменная:
# - `Visits` --- число визитов всех опылителей на цветок _Leopoldia_.
# Предиктор в фокусе исследования:
# - `Treatment` --- тип площадки, тритмент (фактор с 3 уровнями):
#     - `Invaded` --- _Leopoldia_ в смеси с видом-вселенцем _Hedysarum_;
#     - `Removal` --- _Leopoldia_ в смеси с видом-вселенцем с удаленными цветками;
#     - `Control` --- _Leopoldia_ без вида вселенца.
# Другие важные ковариаты:
# - `DiversityD_1` --- Разнообразие флоры на площадке ($exp(H’)$,  где $H'$ --- индекс Шеннона-Уивера) \newline (на луг с более разнообразной растительностью прилетит больше опылителей).
# - `Flowers` --- число цветков _Leopoldia_ на площадке (чем больше, тем больше опылителей).
# - `Hours` --- продолжительность наблюдений (чем дольше, тем больше насчитали).


# ## Открываем и знакомимся с данными
library(readxl)
pol <- read_excel('data/Montero-Castano_Vila_2015_S4_Table.xlsx', sheet = 1)
head(pol)

# Есть ли пропущенные значения?
sum(is.na(pol))

# ## Нет ли наблюдений-выбросов?
library(cowplot); library(ggplot2); theme_set(theme_bw())

gg_dot <- ggplot(pol, aes(y = 1:nrow(pol))) + geom_point()
plot_grid(gg_dot + aes(x = DiversityD_1), gg_dot + aes(x = Flowers),
          gg_dot + aes(x = Hours), nrow = 1)

# ## Каков объем выборки?
table(pol$Treatment)

# Как распределены короткие периоды наблюдений по тритментам?
table(pol$Hours, pol$Treatment)

# ## Нет ли коллинеарности непрерывных и дискретных предикторов?
gg_box <- ggplot(pol, aes(x = Treatment)) + geom_boxplot()

plot_grid(gg_box + aes(y = DiversityD_1),
          gg_box + aes(y = Flowers))

# ## Как распределена переменная-отклик?
ggplot(pol, aes(x = Visits)) + geom_histogram()
mean(pol$Visits == 0) # Какова пропорция нулей?

# ## Какова форма зависимости?
gg_shape1 <- ggplot(pol, aes(y = Visits/Hours, colour = Treatment)) +
  geom_point(aes(x = Flowers))
gg_shape2 <- gg_shape1 + geom_point(aes(x = DiversityD_1))

plot_grid(gg_shape1 + theme(legend.position = 'none'),
          gg_shape2,
          rel_widths = c(0.4, 0.6), nrow = 1)


# ## Опасности моделирования счетных величин при помощи обычной регрессии #####

# ## Что будет, если мы (ошибочно) подберем GLM с нормальным распределением отклика?
mod_norm <- glm(Visits ~ Treatment + DiversityD_1 + Flowers + Hours, data = pol)
coef(mod_norm)
sigma(mod_norm)


# ## Данные для графика предсказания простой линейной модели
library(dplyr)
new_data <- pol %>% group_by(Treatment)%>%
  do(data.frame(Flowers = seq(min(.$Flowers), max(.$Flowers), length.out = 100)))
new_data$DiversityD_1 = mean(pol$DiversityD_1)
new_data$Hours = mean(pol$Hours)

# Модельная матрица и коэффициенты
X <- model.matrix(~ Treatment + DiversityD_1 + Flowers + Hours, data = new_data)
b <- coef(mod_norm)
# Предсказания в масштабе функции связи (eta) совпадают с масштабом отклика (mu)
new_data$mu <- X %*% b
new_data$se_mu <- sqrt(diag(X %*% vcov(mod_norm) %*% t(X)))  # SE

head(new_data, 3)

# ## График предсказаний
ggplot(new_data, aes(x = Flowers, y = mu, fill = Treatment)) +
  geom_ribbon(aes(ymin = mu - 2 * se_mu, ymax = mu + 2 * se_mu), alpha = 0.3) +
  geom_line(aes(colour = Treatment)) +
  geom_hline(yintercept = 0)

# ## Смотрим на результаты подбора модели
summary(mod_norm)

# ## Анализ девиансы для модели с нормальным распределением отклика
drop1(mod_norm, test = 'Chi')


# ## Нет ли коллинеарности предикторов
library(car)
vif(mod_norm)

# ## График остатков от предсказанных значений
mod_norm_diag <- data.frame(.fitted = predict(mod_norm, type = 'response'),
                            .resid_p = residuals(mod_norm, type = 'pearson'))

ggplot(mod_norm_diag, aes(y = .resid_p)) + geom_hline(yintercept = 0) +
  geom_point(aes(x = .fitted))

# ## Модель с нормальным распределением отклика не подходит

# ## GLM с Пуассоновским распределением отклика #####
mod_pois <- glm(Visits ~ Treatment + DiversityD_1 + Flowers + Hours, data = pol,
                   family = 'poisson')

# ## Уравнение модели с Пуассоновским распределением отклика
coef(mod_pois)

# ## Смотрим на результаты подбора модели
summary(mod_pois)

# ## Анализ девиансы для модели с Пуассоновским распределением отклика
drop1(mod_pois, test = 'Chi')
# Можем ли мы доверять этим результатам? Пока не известно.

# ## Данные для предсказаний
new_data <- pol %>% group_by(Treatment)%>%
  do(data.frame(Flowers = seq(min(.$Flowers), max(.$Flowers), length.out = 100)))
new_data$DiversityD_1 = mean(pol$DiversityD_1)
new_data$Hours = mean(pol$Hours)

# ## Предсказания модели при помощи операций с матрицами
# Модельная матрица и коэффициенты
X <- model.matrix(~ Treatment + DiversityD_1 + Flowers + Hours, data = new_data)
b <- coef(mod_pois)

# Предсказанные значения и стандартные ошибки...
# ...в масштабе функции связи (логарифм)
new_data$fit_eta <- X %*% b
new_data$se_eta <- sqrt(diag(X %*% vcov(mod_pois) %*% t(X)))

# ...в масштабе отклика (применяем функцию, обратную функции связи)
new_data$fit_mu <- exp(new_data$fit_eta)
new_data$lwr <- exp(new_data$fit_eta - 2 * new_data$se_eta)
new_data$upr <- exp(new_data$fit_eta + 2 * new_data$se_eta)
head(new_data, 2)

# ## График предсказаний в масштабе функции связи
ggplot(new_data, aes(x = Flowers, y = fit_eta, fill = Treatment)) +
  geom_ribbon(aes(ymin = fit_eta - 2 * se_eta,
                  ymax = fit_eta + 2 * se_eta), alpha = 0.3) +
  geom_line(aes(colour = Treatment)) +
  geom_hline(yintercept = 0)

# ## График предсказаний в масштабе переменной-отклика
ggplot(new_data, aes(x = Flowers, y = fit_mu, fill = Treatment)) +
  geom_ribbon(aes(ymin = lwr,
                  ymax = upr),
              alpha = 0.3) +
  geom_line(aes(colour = Treatment)) +
  geom_hline(yintercept = 0)


# ## Диагностика моделей с Пуассоновским распределением отклика #####

# ## Условия применимости GLM с Пуассоновским распределением отклика
#
# - Случайность и независимость наблюдений внутри групп.
# - Линейность связи переменной отклика с предиктором (с учетом связывающей функции).
# - Отсутствие сверхдисперсии. (Дисперсия остатков равна мат.ожиданию при каждом уровне значений предикторов).
# - Отсутствие коллинеарности предикторов.

# ## График остатков
mod_pois_diag <- data.frame(.fitted = predict(mod_pois, type = 'response'),
                            .resid_p = residuals(mod_pois, type = 'pearson'))
ggplot(mod_pois_diag, aes(x = .fitted, y = .resid_p)) +
  geom_point() +
  geom_hline(yintercept = 0)

# ## Избыточность дисперсии

# Используем предложенную Беном Болкером функцию проверки на сверхдисперсию.

# Функция для проверки наличия сверхдисперсии в модели (автор Ben Bolker)
# http://bbolker.github.io/mixedmodels-misc/glmmFAQ.html
# Код модифицирован для работы с моделями, подобранными MASS::glm.nb()
overdisp_fun <- function(model) {
    rdf <- df.residual(model)  # Число степеней свободы N - p
    if (inherits(model, 'negbin')) rdf <- rdf - 1 ## учитываем k в NegBin GLM
    rp <- residuals(model,type='pearson') # Пирсоновские остатки
    Pearson.chisq <- sum(rp^2) # Сумма квадратов остатков, подчиняется Хи-квадрат распределению
    prat <- Pearson.chisq/rdf  # Отношение суммы квадратов остатков к числу степеней свободы
    pval <- pchisq(Pearson.chisq, df=rdf, lower.tail=FALSE) # Уровень значимости
    c(chisq=Pearson.chisq,ratio=prat,rdf=rdf,p=pval)        # Вывод результатов
}

# ## Проверка на сверхдисперсию
overdisp_fun(mod_pois)



# ## Квазипуассоновские модели #####

# ## Подбираем квази-пуассоновскую модель
mod_quasi <- glm(Visits ~ Treatment + DiversityD_1 + Flowers + Hours, data = pol,
                 family = 'quasipoisson')

# ## Уравнение квази-пуассоновской модели
coef(mod_quasi)
summary(mod_quasi)$dispersion

# ## Смотрим на результаты подбора модели
summary(mod_quasi)

# ## Анализ девиансы для квази-пуассоновской модели
drop1(mod_quasi, test = 'F')


# ## GLM с отрицательным биномиальным распределением отклика #####

# ## GLM с отрицательным биномиальным распределением отклика
library(MASS)
mod_nb <- glm.nb(Visits ~ Treatment + DiversityD_1 + Flowers + Hours, data = pol,
                 link = 'log')

# ## Уравнение модели с отрицательным биномиальным распределением отклика
coef(mod_nb)
summary(mod_nb)$theta

# ## Смотрим на результаты подбора модели
summary(mod_nb)

# ## Анализ девиансы модели с отрицательным биномиальным распределением отклика
drop1(mod_nb, test = 'Chi')
# Можем ли мы доверять этим результатам? Это нужно еще проверить.

# ## График остатков
mod_nb_diag <- data.frame(.fitted = predict(mod_nb, type = 'response'),
                          .resid_p = residuals(mod_nb, type = 'pearson'),
                          pol)
gg_resid <- ggplot(mod_nb_diag, aes(y = .resid_p)) + geom_hline(yintercept = 0)
gg_resid + geom_point(aes(x = .fitted))

# ## Проверка на сверхдисперсию
#
# Обратите внимание, у моделей с отрицательным биномиальным распределением добавляется еще один параметр.
overdisp_fun(mod_nb)

# ## Графики остатков от переменных, которые есть в модели
plot_grid(gg_resid + geom_boxplot(aes(x = Treatment)),
          gg_resid + geom_boxplot(aes(x = as.factor(Hours))),
          gg_resid + geom_point(aes(x = DiversityD_1)),
          gg_resid + geom_point(aes(x = Flowers)),
          nrow = 2)

# ## Графики остатков от переменных, которых нет в модели
plot_grid(gg_resid + geom_point(aes(x = Visits_NO_Apis)),
gg_resid + geom_point(aes(x = Total_1)),
gg_resid + geom_point(aes(x = Fruit)),
gg_resid + geom_point(aes(x = No_Fruit)),
nrow = 2)

# Осторожно! Из этих переменных только Total_1 может быть предиктором. Остальные - это потенциальные отклики - для них, в данном случае, не имеет смысла рисовать диагностические графики.

# ## Описание и визуализация модели #####

# ## Данные для предсказаний
new_data <- pol %>% group_by(Treatment)%>%
  do(data.frame(Flowers = seq(min(.$Flowers), max(.$Flowers), length.out = 100)))
new_data$DiversityD_1 = mean(pol$DiversityD_1)
new_data$Hours = mean(pol$Hours)


# ## Предсказания модели при помощи операций с матрицами

# Модельная матрица и коэффициенты
X <- model.matrix(~ Treatment + DiversityD_1 + Flowers + Hours, data = new_data)
b <- coef(mod_nb)

# Предсказанные значения и стандартные ошибки...
# ...в масштабе функции связи (логарифм)
new_data$fit_eta <- X %*% b
new_data$se_eta <- sqrt(diag(X %*% vcov(mod_nb) %*% t(X)))

# ...в масштабе отклика (применяем функцию, обратную функции связи)
new_data$fit_mu <- exp(new_data$fit_eta)
new_data$lwr <- exp(new_data$fit_eta - 2 * new_data$se_eta)
new_data$upr <- exp(new_data$fit_eta + 2 * new_data$se_eta)

head(new_data, 2)

# ## График предсказаний в масштабе функции связи
ggplot(new_data, aes(x = Flowers, y = fit_eta, fill = Treatment)) +
  geom_ribbon(aes(ymin = fit_eta - 2 * se_eta,
                  ymax = fit_eta + 2 * se_eta),
              alpha = 0.3) +
  geom_line(aes(colour = Treatment)) +
  geom_hline(yintercept = 0)


# ## График предсказаний в масштабе переменной-отклика
ggplot(new_data, aes(x = Flowers, y = fit_mu, fill = Treatment)) +
  geom_ribbon(aes(ymin = lwr,
                  ymax = upr),
              alpha = 0.3) +
  geom_line(aes(colour = Treatment)) +
  geom_hline(yintercept = 0)


# ## GLM с отрицательным биномиальным распределением отклика
ggplot(new_data, aes(x = Flowers, y = fit_mu, fill = Treatment)) +
  geom_ribbon(aes(ymin = lwr,
                  ymax = upr),
              alpha = 0.3) +
  geom_line(aes(colour = Treatment)) +
  geom_hline(yintercept = 0) + ylab('Visits')

