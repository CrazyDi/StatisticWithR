# Смешанные линейные модели для бинарных данных
# В.М. Хайтов, к.б.н.
# Биологический факультет, СПбГУ

# ## Что мы знаем про моделирование бинарных данных #####

# ## Пример -- морские звезды и мидии #####

# ## Различают ли морские звезды два вида мидий: предыстория
# В Белое море, где обитают ценные промысловые
# моллюски, атлантические мидии, недавно вселились
# мидии тихоокеанские. Важно понять, могут ли
# хищные морские звезды регулировать численность
# вида-вселенца.
# Рассматривая этот пример в прошлом курсе, мы
# обсуждали только данные первого, разведочного
# эксперимента. Было показано, что шансы быть
# съеденными у вида-вселенца выше, чем у коренного
# вида.
# Данные: Khaitov et al, 2018


# ## Знакомимся с данными #####
astr2 <- read.csv('data/aster_mussel_full.csv', header = TRUE)
head(astr2)

# ## Наводим порядок в кодировке переменных
astr2$Year <- factor(astr2$Year)
astr2$Box <- factor(astr2$Box)
astr2$Sp <- factor(astr2$Sp)
astr2$Out <- ifelse(test = astr2$Outcome == 'eaten', yes = 1,  no = 0)


# Нет ли пропущенных значений?
colSums(is.na(astr2))

# Каковы объемы выборок?
table(astr2$Box)

# ## Нет ли коллинеарности
library(cowplot); library(ggplot2); theme_set(theme_bw())

Pl_Sp   <- ggplot(astr2, aes(x = Sp, y = L)) + geom_boxplot()
Pl_exp  <- ggplot(astr2, aes(x = Experiment, y = L)) + geom_boxplot()
Pl_year <- ggplot(astr2, aes(x = Year, y = L)) + geom_boxplot()
plot_grid(Pl_Sp, Pl_exp, Pl_year, ncol = 3, rel_widths = c(0.25, 0.45, 0.3))


# ## Есть ли выбросы?
ggplot(astr2, aes(y = 1:nrow(astr2))) + geom_point(aes(x = L) )


# ## Подбираем модель #####

# ## Модель со случайным свободным членом (random intercept model)
library(lme4)
model1_ri <- glmer(Out ~ L * Sp * Year + (1|Experiment/Box),
                   data = astr2, family = binomial(link = 'logit'))
# Модель не сошлась.
# О проблеме сходимости моделей:
# ?convergence
# https://bbolker.github.io/mixedmodels-misc/glmmFAQ.html

# ## Cтандартизация непрерывного предиктора
astr2$L_scaled <- as.numeric(scale(astr2$L))

model1_ri <- glmer(Out ~ L_scaled * Sp * Year + (1|Experiment/Box),
                   data = astr2, family = binomial(link = 'logit'))
# Все посчиталось.

# ## Модель со случайным свобобным членом и случайным угловым коэффициентом
model1_rsi <- glmer(Out ~ L_scaled * Sp * Year + (1 + L_scaled |Experiment/Box),
                      data = astr2, family = binomial(link = 'logit'))
# Модель не сошлась.

# Увеличим количество итераций.
model1_rsi <- glmer(Out ~ L_scaled * Sp * Year + (1 + L_scaled |Experiment/Box) ,
            data = astr2, family = binomial(link = 'logit'),
            control = glmerControl(optimizer = 'bobyqa', optCtrl = list(maxfun = 2e5)))
# Сработало.


# ## Сравниваем две модели
AIC(model1_ri, model1_rsi)

# Останавливаем выбор на модели со случайным отрезком `model1_ri`

# ## Диагностика модели: линейность связи
library(ggplot2)
model1_diag <- fortify(model1_ri)
ggplot(model1_diag, aes(x = .fitted, y = .scresid)) + geom_point() + geom_smooth()

# ## Диагностика модели: избыточность дисперсии
library(sjstats)
overdisp(model1_ri)

# ## Дорабатываем модель  #####

# ## Модель можно упростить
drop1(model1_ri)
model2 <- update(model1_ri, .~.-L_scaled:Sp:Year)

# ## Упроаем модель: шаг 2.
drop1(model2)
model3 <- update(model2, . ~ . - L_scaled:Year)

# ## Упроаем модель: шаг 3.
drop1(model3)
model4 <- update(model3, . ~ . - L_scaled:Sp)

# ## Упрощаем модель: шаг 4.
drop1(model4)
model5 <- update(model4, . ~ . - Sp:Year)

# ## Упрощаем модель: шаг 5.
drop1(model5)
model6 <- update(model5, . ~ . - Year)

# ## Финальная модель
drop1(model6)

# ## Начальная и финальная модель
AIC(model1_ri, model6)

# В фиксированной части модели осталось только два предиктора
summary(model6)$call

# ## Диагностика финальной модели: линейность связи
model6_diag <- fortify(model6)
ggplot(model6_diag, aes(x = .fitted, y = .scresid)) + geom_point() + geom_smooth()

# ## Диагностика финальной модели: избыточность дисперсии
overdisp(model6)


# ## Анализ итогов #####

# ## Первые итоги подбора модели
summary(model6)

# ## Приблизительные значения ICC
icc(model6)

# ## Визуализация модели #####

# Визуализировать модель можно двумя способами: в
# виде логистических кривых и в виде столбчатой
# диаграммы.

# ## Подготовка к визуализации в виде логистических кривых
logit_back <- function(x) exp(x)/(1 + exp(x)) # обратная логит-трансформация

library(dplyr)
new_data <- astr2 %>% group_by(Sp) %>%
  do(data.frame(L_scaled = seq(min(.$L_scaled), max(.$L_scaled), length.out = 100)))

X <- model.matrix(~ L_scaled + Sp, data = new_data)
b <- fixef(model6)

new_data$fit_eta <- X %*% b
new_data$se_eta <- sqrt(diag(X %*% vcov(model6) %*% t(X)))

new_data$fit_pi <- logit_back(new_data$fit_eta)
new_data$lwr <- logit_back(new_data$fit_eta - 2 * new_data$se_eta)
new_data$upr <- logit_back(new_data$fit_eta + 2 * new_data$se_eta)


# ## Логистические кривые
Pl_log <- ggplot(new_data, aes(x = L_scaled, y = fit_pi)) +
  geom_ribbon(aes(ymin = lwr, ymax = upr, fill = Sp), alpha = 0.5) +
  geom_line(aes(colour = Sp)) +
  labs(x = 'Стандартизированная длина', y = 'Вероятность \n быть съеденной' )
Pl_log


# ## Подготовка к визуализации в виде столбчатой диаграммы

# Эффект влияния дискретного предиктора  лучше
# отразить в виде столбчатой диаграммы, отражающей
# предсказание модели при среднем значении
# ковариаты.
# Ковариата стандартизована, ее среднее равно нулю.
new_data <- data.frame(Sp = c('Tr', 'Ed'), L_scaled = 0)

X <- model.matrix(~  L_scaled + Sp, data = new_data)
b <- fixef(model6)
new_data$fit_eta <- X %*% b
new_data$se_eta <- sqrt(diag(X %*% vcov(model6) %*% t(X)))
new_data$fit_pi <- logit_back(new_data$fit_eta)
new_data$lwr <- logit_back(new_data$fit_eta - 2 * new_data$se_eta)
new_data$upr <- logit_back(new_data$fit_eta + 2 * new_data$se_eta)


# ## Столбчатая диаграмма
ggplot(new_data, aes(x = Sp, y = fit_pi)) +
  geom_col(fill = 'grey') +
  geom_errorbar(aes(ymin = lwr, ymax = upr), width = 0.2) +
  labs(x = 'Вид мидий', y = 'Вероятность \n быть съеденной' )


# ## Дополнительная штрихи к модели #####

# ## Проблема представления первичных данных при визуализации модели

# Можно отразить долю съеденных моллюсков каждого
# вида среди особей разных размерных классов. То
# есть отразить поведение некоторых усредненных
# данных. Это не первичные данные в прямом смысле.

# ## Разбиваем на размерные классы приблизительно рвного объема
astr2$Size_class <- ntile(astr2$L_scaled, 10)
table(astr2$Size_class, astr2$Sp)

# ## Средние показатели в каждом из размерных классов
Mean_Out <- astr2 %>% group_by(Size_class, Sp) %>%
  do(data.frame(Out = mean(.$Out), L_scaled = mean(.$L_scaled)))
Pl_log + geom_point(data = Mean_Out, aes(x = L_scaled, y = Out, colour = Sp))


# ## Еще один штрих: тестовая выборка

# ## Тестовая выборка
# **Важно!** Это учебный пример. В реальных исследованиях лучше не применять данные, которые уже были использованы для формулировок каких-то гипотез, в повторном анализе.
astr_test <- read.csv('data/aster_mussel.csv', header = TRUE)

# ## Переподберем модель для нестандартизированной ковариаты для удобства
model6_unscaled <- glmer(Out ~ L + Sp +
                    (1|Experiment/Box) , data = astr2,
                    family = binomial(link = 'logit'))

# ## Ничего не изменилось по сути
summary(model6)
summary(model6_unscaled)

# ## Предсказания для новых данных
X <- model.matrix(~ L + Sp, data = astr_test)
b <- fixef(model6_unscaled)
astr_test$predicted_pi <- logit_back(X %*% b)


# ## Визуализация связи наблюдений и предсказаний.
ggplot(astr_test, aes(x = Outcome, y = predicted_pi)) +
  geom_boxplot() + labs(x = 'Наблюдаемый исход',
                        y = 'Предсказанная вероятность \n быть съеденной')

