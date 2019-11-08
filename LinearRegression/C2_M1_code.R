# Корреляционный анализ. Простая линейная регрессия
# В.М. Хайтов, к.б.н.
# М.А. Варфоломеева, PhD
# Биологический факультет, СПбГУ


## Пример --- размер мозга и IQ  ----------------------
## Зависит ли уровень интеллекта от размера головного мозга?
# Было исследовано 20 девушек и 20 молодых людей
# У каждого индивида измеряли:
# Вес
# Рост
# Размер головного мозга (количество пикселей на изображении ЯМР сканера)
# Уровень интеллекта измеряли с помощью нескольких IQ тестов

# Данные: Willerman et al., 1991

## Загружаем данные

# чтобы избежать "scientific notation"
options(scipen = 6, digits = 3)

brain <- read.csv("data/IQ_brain.csv", header = TRUE)
str(brain)

# Есть ли пропущенные значения?
sum(!complete.cases(brain))

# Где именно пропущенные значения?
colSums(is.na(brain))

# Что это за случаи?
brain[!complete.cases(brain), ]

# Каков объем выборки?
nrow(brain)


## Взаимосвязи между явлениями  ----------------------

## Связь между уровнем интеллекта и размером головного мозга
library (ggplot2)
theme_set(theme_bw())
pl_brain <- ggplot(data = brain, aes(x = MRINACount, y = PIQ)) +
  geom_point() +
  labs(x = "Размер мозга (MRINACount)", y = "Уровень интеллекта (PIQ)")
pl_brain


## Ковариация и корреляция  ----------------------


## Тестирование статистической значимости коэффициента корреляции  ----------------------

## Корреляционный анализ в R  ----------------------
?cor.test # справка о функции cor.test()

## Коэффициент корреляции Пирсона
cor.test(x = brain$PIQ, y = brain$MRINACount, method = "pearson")

## Ранговый коэффициент корреляции Спирмена
cor.test(x = brain$PIQ, y = brain$MRINACount, method = "spearman")


## Модели, как отражение взаимосвязи   ----------------------


## Простая линейная регрессия   ----------------------

## Метод наименьших квадратов  ----------------------


## Подбор коэффициентов линейной регрессии в R  ----------------------

?lm # справка о lm()

## Подберем модель в R
brain_model <- lm(PIQ ~ MRINACount, data = brain)
brain_model

# Уравнение модели: $\widehat {PIQ} = 1.74376 + 0.00012 \cdot MRINACount$



## Стандартные ошибки коэффициентов регрессии  ----------------------

## Результаты подбора линейной модели
summary(brain_model)



## Доверительные интервалы коэффициентов и доверительная зона регрессии  ---------------------
## Доверительные интервалы к параметрам регрессии в R
coef(brain_model)
confint(brain_model)


## Доверительная зона регрессии в R
ggplot(brain, aes(x = MRINACount, y = PIQ)) +
  geom_point() +
  labs(x = "Размер мозга (MRINACount)", y = "Уровень интеллекта (PIQ)") +
  geom_smooth(method = "lm")


## Использование регрессии для предсказаний  ----------------------

?predict.lm # справка о predict() для моделей, подобранных при помощи lm()

## Данные для предсказаний
new_data <- data.frame(MRINACount = 900000)
new_data

## Групповые предсказания
# Каково среднее значение IQ у людей с размером мозга 900000 пикселей?
predict(brain_model, newdata = new_data, interval = "confidence", level = 0.95)

## Индивидуальные предсказания
# Какое значение IQ можно ожидать у человека с размером головного мозга 900000 пикселей?
predict(brain_model, newdata = new_data, interval = "prediction", level = 0.95)

## Данные для графика доверительных зон регрессии и ее предсказаний

brain_predicted <- predict(brain_model, interval = "prediction") # Подготовка данных
head(brain_predicted, 3)

brain_predicted <- data.frame(brain, brain_predicted)          # Добавим исходные данные
head(brain_predicted, 3)

pl_limits <- ggplot(brain_predicted, aes(x = MRINACount, y = PIQ)) +
# Доверительная зона предсказаний
  geom_ribbon(aes(ymin = lwr, ymax = upr, fill = "Для предсказаний"), alpha = 0.5) +
# Линия регрессии и ее доверительная зона
  geom_smooth(method = "lm", aes(fill = "Для регрессии")) +
# Вручную настраиваем цвета заливки
  scale_fill_manual(name = "Доверительные \nзоны", values = c("powderblue", "gray")) +
# Исходные наблюдения
  geom_point() +
# Подписи
  labs(x = "Размер мозга (MRINACount)", y = "Уровень интеллекта (PIQ)",
       title = "Доверительные зоны регрессии и ее предсказаний")
pl_limits

