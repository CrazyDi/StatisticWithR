# Модели с  бинарным откликом
# В.М. Хайтов, к.б.н.
# Биологический факультет, СПбГУ

# чтобы избежать scientific notation
options(scipen = 6, digits = 3)

# ## Бинарные переменные вокруг нас  ######

# ## Пример -- морские звезды и мидии ######

# ## Различают ли морские звезды два вида мидий?
# Ценные промысловые моллюски, атлантические мидии, обитают в Белом море, но недавно туда вселились мидии другого вида -- тихоокеанские мидии.
# Вселенец имеет меньшую промысловую значимость и важно понять, что регулирует их численность. Наиболее значимый фактор -- это морские звезды, питающиеся мидиями.
# - Различают ли морские звезды два вида мидий?
# - Различают ли хищники мидий разных размеров?
# Данные: Khaitov et al, 2018
# Морских звезд вместе с мидиями двух видов сажали в контейнеры. Через четыре дня совместного существования с хищником регистрировали состояние мидий.
# Зависимая переменная:
# - `Outcome` -- состояние мидий
# ("eaten" -- съедена, "not_eaten" -- живая )
# Предикторы в фокусе исследования:
# - `Sp` -- вид мидий
# ("Ed" -- коренной вид, "Tr"\ -- вселенец),
# - `L` -- размер мидий (мм).
# Ковариата:
# - `Box` -- номер контейнера. Этот фактор нас не интересует, но его нельзя не учитывать.

# ## Читаем данные
astr <- read.csv('data/aster_mussel.csv', header = TRUE)
head(astr)

# Номер экспериментального контейнера закодирован числами,
# поэтому превращаем его в фактор.
astr$Box <- factor(astr$Box)

# Нет ли пропущенных значений?
colSums(is.na(astr))

# Каковы объемы выборок?
table(astr$Box)

# ## Нет ли коллинеарности
library(cowplot); library(ggplot2); theme_set(theme_bw())

Pl_Sp <- ggplot(astr, aes(x = Sp, y = L)) + geom_boxplot()
Pl_Box <- ggplot(astr, aes(x = Box, y = L)) + geom_boxplot()
plot_grid(Pl_Sp, Pl_Box, ncol = 2)

# ## Есть ли выбросы?
ggplot(astr, aes(y = 1:nrow(astr))) + geom_point(aes(x = L) )

# Бинарную переменную надо перекодировать в виде нулей и единиц:
astr$Out <- ifelse(test = astr$Outcome == 'eaten', yes = 1,  no = 0)

# ## Простой линейной регрессией не обойтись  ######

mod_norm <- glm(Out ~ Sp * L * Box, data = astr)

# ## Посмотрим что получилось
library(dplyr)
new_data <- astr %>% group_by(Sp, Box)%>%
  do(data.frame(L = seq(min(.$L), max(.$L), length.out = 100)))
new_data$fit <- predict(mod_norm, newdata = new_data) # Предсказанные значения
ggplot(new_data, aes(x = L, y = fit)) +
  geom_line(aes(group = Box)) + facet_wrap(~ Sp, ncol = 2) +
  geom_point(data = astr, aes(x = L, y = Out), size = 0.5, color = 'blue')

# ## Диагностика модели
mod_norm_diag <- fortify(mod_norm)
ggplot(mod_norm_diag, aes(x = .fitted, y = .stdresid)) +
  geom_point() + geom_vline(xintercept = 0)

# Простая линейная модель **категорически не годится**!

# ## Логистическая кривая  ######

# ## Шансы и логиты  ######

# ## Немного алгебры: Логиты в качестве зависимой переменной  ######

# ## Вернемся к морским звездам и мидиям  ######

# ## GLM с биномиальным распределением отклика
mod <- glm(Out ~ Sp*L*Box, family = binomial(link = 'logit'), data = astr)

# ## Анализ девиансы для полной модели
library(car)
Anova(mod)

# Эту модель можно упростить!
# ## Упрощение модели: Шаг 1
drop1(mod, test = 'Chi')
mod2 <- update(mod, . ~ . - Sp:L:Box)

# ## Упрощение модели: Шаг 2
drop1(mod2, test = 'Chi')
mod3 <- update(mod2, . ~ . - Sp:L)

# ## Упрощение модели: Шаг 3
drop1(mod3, test = 'Chi')
mod4 <- update(mod3, . ~ . - L:Box)

# ## Упрощение модели: Шаг 4
drop1(mod4, test = 'Chi')
mod5 <- update(mod4, . ~ . - Sp:Box)

# ## Упрощение модели: Шаг 5
drop1(mod5, test = 'Chi')
mod6 <- update(mod5, . ~ . - Box)

# ## Упрощение модели: Шаг 6
drop1(mod6, test = 'Chi')

# Больше никаких предикторов исключать нельзя: `mod6` -- финальная модель.

# ## AIC
AIC(mod, mod2, mod3, mod4, mod5, mod6)


# ## Смысл коэффициентов в моделях с бинарной переменной отклика  ######

# ## Что за модель мы построили?
summary(mod6)
# ## Трактуем коэффициенты модели

# ## Диагностика модели с бинарным откликом ######

# ## Условия применимости GLM с бинарной переменной-откликом
#
# - Случайность и независимость наблюдений.
# - Линейность связи переменной отклика с предиктором (с учетом связывающей функции).
# - Отсутствие сверхдисперсии (форма связи среднего с дисперсией должна быть как у величины с биномиальным распределением).
# - Отсутствие коллинеарности предикторов.
#
# ## Линейность связи
mod6_diag <- data.frame(.fitted = fitted(mod6, type = 'response'),
                         .resid_p = resid(mod6, type = 'pearson'))
ggplot(mod6_diag, aes(y = .resid_p, x = .fitted)) + geom_point() +
  geom_hline(yintercept = 0) +  geom_smooth(method = 'loess')

# ## Проверка на сверхдисперсию

# ## Еще раз смотрим на результаты
summary(mod6)
# Важная строчка
# (Dispersion parameter for binomial family taken to be 1)

# ## Проверка на сверхдисперсию

# Используем предложенную Беном Болкером функцию проверки на сверхдисперсию,
# Функция для проверки наличия сверхдисперсии в модели (автор Ben Bolker)
# http://bbolker.github.io/mixedmodels-misc/glmmFAQ.html
# Код модифицирован для работы с моделями, подобранными MASS::glm.nb()
overdisp_fun <- function(model) {
    rdf <- df.residual(model)  # Число степеней свободы N - p
    if (inherits(model, 'negbin')) rdf <- rdf - 1 ## учитываем k в NegBin GLMM
    rp <- residuals(model,type='pearson') # Пирсоновские остатки
    Pearson.chisq <- sum(rp^2) # Сумма квадратов остатков, подчиняется Хи-квадрат распределению
    prat <- Pearson.chisq/rdf  # Отношение суммы квадратов остатков к числу степеней свободы
    pval <- pchisq(Pearson.chisq, df=rdf, lower.tail=FALSE) # Уровень значимости
    c(chisq=Pearson.chisq,ratio=prat,rdf=rdf,p=pval)        # Вывод результатов
}
overdisp_fun(mod6)

# ## Визуализация модели  ######

# ## Данные для предсказаний
library(dplyr)
new_data <- astr %>% group_by(Sp)%>%
  do(data.frame(L = seq(min(.$L), max(.$L), length.out = 100)))

# ## Предсказания модели при помощи операций с матрицами

# Модельная матрица и коэффициенты
X <- model.matrix(~ Sp + L, data =  new_data)
b <- coef(mod6)
# Предсказанные значения и стандартные ошибки...
# ...в масштабе функции связи (логит)
new_data$fit_eta <- X %*% b
new_data$se_eta <- sqrt(diag(X %*% vcov(mod6) %*% t(X)))
# ...в масштабе отклика (применяем функцию, обратную функции связи)
logit_back <- function(x) exp(x)/(1 + exp(x)) # обратная логит-трансформация
new_data$fit_pi <- logit_back(new_data$fit_eta)
new_data$lwr <- logit_back(new_data$fit_eta - 2 * new_data$se_eta)
new_data$upr <- logit_back(new_data$fit_eta + 2 * new_data$se_eta)

head(new_data, 2)

# ## Визуализация в шкале логитов
ggplot(new_data, aes(x = L, y = fit_eta, fill = Sp)) +
  geom_ribbon(aes(ymin = fit_eta - 2 * se_eta,
                  ymax = fit_eta + 2 * se_eta),
              alpha = 0.5) +
  geom_line(aes(color = Sp))

# ## Визуализация в шкале вероятностей интуитивно понятнее
ggplot(new_data, aes(x = L, y = fit_pi, fill = Sp)) +
  geom_ribbon(aes(ymin = lwr,
                  ymax = upr),
              alpha = 0.5) +
  geom_line(aes(color = Sp)) +
  labs(y='Вероятность', title = 'Вероятность быть съеденной')

