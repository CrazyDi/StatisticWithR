# Графики с использованием ggplot2 ------------------
# В.М. Хайтов, к.б.н.
# Биологический факультет, СПбГУ



# Визуализация данных и результатов как инструмент  ------------------



# Основы грамматики графиков  ------------------

# В основе подхода, реализованного в пакете ggplot2, лежит
# идея **грамматики графиков** "Semiology of
# Graphics" (Bertin, 1983). Более позднее
# обобщение “The Grammar of Graphics” (Wilkinson,
# Anand and Grossman, 2005).



# График как аппликация -----------------------

# Данные для визуализации. Вес новорожденных
# Данные: Hosmer, Lemeshow, 1989

# Чтение данных
library(MASS)
baby <- birthwt # чтение встроенных данных

# Робот строит простейший график
library(ggplot2)

# Робот, возьми лист бумаги
ggplot()

# Прочитай таблицу данных
ggplot(data = baby)

# По оси OX отложи данные, записанные в переменной `lwt`
# По оси OY отложи данные, записанные в переменной `bwt`
ggplot(data = baby, aes(x = lwt, y = bwt))

# Изобрази в виде точек исходные данные
ggplot(data = baby, aes(x = lwt, y = bwt)) + geom_point()



# Строим точечный график: geom_point()  ------------------

# Простейший точечный график
ggplot(data = baby, aes(x = lwt, y = bwt)) + geom_point()

# Этот график можно изменить.

# Изменим цвет точек
ggplot(baby, aes(x = lwt, y = bwt)) + geom_point(colour = 'red')


# Изменим размер точек
ggplot(baby, aes(x = lwt, y = bwt)) + geom_point(colour = 'red', size = 3)

# Изменим форму точек
ggplot(baby, aes(x = lwt, y = bwt)) + geom_point(colour = 'red', size = 4, shape = 17)



# Эстетики  ------------------

# Какая еще  информация скрыта в датасете?
ggplot(baby, aes(x = lwt, y = bwt)) + geom_point()
# На графике приведена лишь информация о весе матери (ось OX) и весе ребенка (ось OY ).
# То, что мы отражаем (`эстетики`), описывает функция   `aes(x = lwt, y = bwt)`.

# В датасете есть еще информация:
# - Cтатус матери в отношении курения: курящая или некурящая.
# - Раса матери
# - Количество посещений врача, и т.п.

# Подготовим данные для более наглядной визуализации
# Переименуем градации дискретных факторов
baby$smoke <- factor(baby$smoke,
                     levels = c(1, 0),
                     labels = c('Smoker', 'Non-smoker'))
baby$race <- factor(baby$race,
                    levels = c(1, 2, 3),
                    labels = c('White', 'Black', 'Other'))

# Вводим в график эстетику `colour`
# Изобразим разным цветом точки для курящих и
# некурящих матерей (эту информацию маркирует
# переменная `baby$smoke`)
ggplot(baby, aes(x = lwt, y = bwt, colour = smoke)) +
  geom_point(size = 2, shape = 17)
# Важно! Вся информация о цвете точек теперь
# должна быть сосредоточена в функции aes()


# Вводим в график эстетику `shape`
# Отразим ту же информацию, но с помощью разных
# форм точек для курящих и некурящих матерей.
ggplot(baby, aes(x = lwt, y = bwt, shape = smoke)) +
  geom_point(size = 2, colour = 'red')
# Важно! Вся информация о форме точек теперь
# должна быть сосредоточена в функции aes()


# Вводим еще одну эстетику -- `size`
# Отразим размером точки информацию о том, сколько
# раз в первом триместре женщина посещала врача
# (переменная `baby$ftv`)
ggplot(baby, aes(x = lwt, y = bwt, colour = smoke, size = ftv)) +
  geom_point() + guides(size = guide_legend(ncol = 4))



# Управление эстетиками. Шкалы  ------------------

# Меняем цветовые обозначения эстетики `colour`
ggplot(baby, aes(x = lwt, y = bwt, colour = smoke)) +
  geom_point(size = 2, shape = 17) +
    scale_colour_manual(values = c('blue', 'red'))

# С помощью функций семейства `scale_` можно
# регулировать форму, цвет заливки, размер и т. д.

# Цветовая схема Синтии Брюэр (Cynthia A. Brewer)
ggplot(baby, aes(x = lwt, y = bwt, colour = smoke)) +
  geom_point(size = 2, shape = 17) +
  scale_colour_brewer(palette = 'Set1')



# Фасетирование  ------------------

# При большом количестве эстетик график может не читаться
ggplot(baby, aes(x = lwt, y = bwt, colour = smoke, shape = race)) +
  geom_point(size = 2) +
  scale_colour_brewer(palette = 'Set1')

# Выход --- фасетирование

# Одна фасетирующая переменная
ggplot(baby, aes(x = lwt, y = bwt, colour = smoke, shape = race)) +
  geom_point(size = 2) +
  scale_colour_brewer(palette = 'Set1') +
  facet_wrap(~race, nrow = 1)

# Две фасетирующие переменные
ggplot(baby, aes(x = lwt, y = bwt, colour = smoke, shape = race)) +
  geom_point(size = 2) +
  scale_colour_brewer(palette = 'Set1') +
  facet_grid(smoke~race)



# Сохранение графиков в переменные  ------------------

# Пошаговое наслоение элементов на переменную

# Создаем переменную с базовым слоем
My_plot <- ggplot(baby, aes(x = lwt, y = bwt))

# Добавляем новые слои
My_plot_2 <- My_plot + geom_point(colour = 'red')
My_plot_2

# Меняем подписи осей
My_plot_3 <- My_plot_2 + labs(x = 'Вес матери (фунты)', y = 'Вес ребенка (г)')
My_plot_3

# Добавляем заголовок графика
My_plot_4 <- My_plot_3 + ggtitle('Соотношение веса матери и \nвеса новорожденного ребенка')
My_plot_4



# Темы оформления в ggplot2  ------------------

# Требований много, а ggplot2 один...

# Некоторые встроенные темы в ggplot2

Them_1 <- My_plot_2 + theme_gray() + ggtitle('theme_gray')

Them_2 <- My_plot_2 + theme_bw() + ggtitle('theme_bw')

Them_3 <- My_plot_2 + theme_classic() + ggtitle('theme_classic')

Them_4 <- My_plot_2 + theme_dark() + ggtitle('theme_dark')


library(gridExtra)
grid.arrange(Them_1, Them_2, Them_3, Them_4, ncol = 2)



# Управление элементами графика   ------------------

# Функция `theme()` регулирует характеристики всех
# элементов графика.

# Уменьшаем размер чисел на осях графика
My_plot_2 + theme_bw() + theme(axis.text = element_text(size = 4))

# Увеличиваем размер чисел на осях графика
My_plot_2 + theme_bw() + theme(axis.text = element_text(size = 14))

# Регулируем угол наклона чисел на осях графика
My_plot_2 + theme_bw() + theme(axis.text.x = element_text(angle = 45, colour = 'blue'))

# Убираем сетку внутри графика
My_plot_2 + theme_bw() +
  theme(axis.text.x = element_text(angle = 45, colour = 'blue'),
        panel.grid = element_blank())

# Меняем положение легенды
ggplot(baby, aes(x = lwt, y = bwt, colour = smoke)) +
  geom_point() +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, colour = 'blue'),
        panel.grid = element_blank())
# Меняем положение легенды
ggplot(baby, aes(x = lwt, y = bwt, colour = smoke)) +
  geom_point() +
  theme_bw()  +
  theme(axis.text.x = element_text(angle = 45, colour = 'blue'),
        panel.grid = element_blank(),
        legend.position = 'bottom')
# Меняем положение легенды
ggplot(baby, aes(x = lwt, y = bwt, colour = smoke)) +
  geom_point() +
  theme_bw()  +
  theme(axis.text.x = element_text(angle = 45, colour = 'blue'),
        panel.grid = element_blank(),
        legend.position = c(0.85, 0.3),
        legend.background = element_rect(fill = 'lightblue'))


# Создаем финальный рисунок
My_final_plot <- ggplot(baby, aes(x = lwt, y = bwt)) +  # задаем базовый слой
  geom_point(aes(colour = smoke)) +   # предсталяем результаты в виде точек
  labs(x = 'Вес матери (фунты)',     # подписываем ось OX
       y = 'Вес ребенка (г)',        # подписываем ось OY
       colour = 'Курение матери:' ) + # подписываем заголовок легенды
  ggtitle('Соотношение веса матери и ребенка') +   # заголовок
  scale_colour_manual(values = c('blue', 'red')) +  # меняем цвет для эстетики colour
  theme_bw()  +                                    # устанавливаем тему
  theme(panel.grid = element_blank(),              # убираем сетку
        legend.position = 'bottom',                # указываем положение легенды
        plot.title = element_text(hjust = 0.5))    # выравниваем заголовок по центру


# Итоговый рисунок
My_final_plot



# Визуализация частотных распределений  ------------------

# Частотное распределение
ggplot(baby, aes(x = bwt)) +
  geom_histogram() + theme_bw()

# Меняем классовый шаг
ggplot(baby, aes(x = bwt)) +
  geom_histogram(binwidth = 50) + theme_bw()
# Меняем классовый шаг
ggplot(baby, aes(x = bwt)) +
  geom_histogram(binwidth = 500) + theme_bw()

# Применяем эстетику `fill`
ggplot(baby, aes(x = bwt)) +
  geom_histogram(binwidth = 500, aes(fill = smoke)) +
  theme_bw()

# Читать неудобно!

# Фасетируем рисунок
ggplot(baby, aes(x = bwt)) + geom_histogram(binwidth = 500, aes(fill = smoke)) +
  theme_bw() +
  facet_wrap(~smoke, nrow = 2)

# Использование анализа, основанного на оценке ядерной плотности (kernel density)
ggplot(baby, aes(x = bwt)) +
  geom_density(aes(fill = smoke), alpha = 0.5) +
  theme_bw()

# Становится видна некоторая тенденция.


# geom_violin()
ggplot(baby, aes(x = smoke, y = bwt)) +
  geom_violin(aes(fill = smoke)) +
  theme_bw()



# Визуализация данных с простейшей статистической обработкой ------------------

# Средний вес ребенка у матерей разного возраста
ggplot(baby, aes(x = age, y = bwt, colour = smoke)) +
  stat_summary(fun.y = mean, geom = 'line') +
  theme_bw()

# Тенденции зависимости веса ребенка от возраста матери
ggplot(baby, aes(x = age, y = bwt, colour = smoke)) + geom_point() +
  stat_smooth(method = 'lm') +
  theme_bw()

# Усы, отражающие интервалы
ggplot(baby, aes(x = smoke, y = bwt)) +
  stat_summary(fun.y = mean, geom = 'bar', fill = 'gray', colour = 'black') +
  stat_summary(fun.data = mean_se, geom = 'errorbar',  width = 0.2) +
  theme_bw()



# Сохранение графиков в виде файлов  ------------------

# Сохранение графика в файл с помощью средств ggplot2

# Создаем график

Plot_to_save <- ggplot(baby, aes(x = smoke, y = bwt)) +
  stat_summary(fun.y = 'mean', geom = 'bar', fill = 'gray', colour = 'black') +
  stat_summary(fun.data = mean_se, geom = 'errorbar',  width = 0.2) +
  theme_bw()

# Сохраняем график в растровом формате

ggsave('c:/My_draw.jpg', Plt_to_save) # сохраняем в растровом формате .jpg

ggsave('c:/My_draw.png', Plot_to_save) # сохраняем в растровом формате .png


# Сохраняем график в векторном формате

ggsave('c:/My_draw.pdf', Plot_to_save) # сохраняем в формате .pdf

ggsave('c:/My_draw.wmf', Plot_to_save) # сохраняем в векторном формате .wmf

ggsave('c:/My_draw.eps', Plot_to_save) # сохраняем в векторном формате .eps



# Литературное программирование  ------------------



# Создание RMD-документа  ------------------


