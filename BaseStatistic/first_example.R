# Знакомство с R - Первый пример -----------------
# М.А. Варфоломеева, PhD
# В.М. Хайтов, к.б.н.
# Биологический факультет, СПбГУ

## Организация рабочего пространства ----------

## Проверка пути к рабочей директории
getwd()

## Установка пути к рабочей директории

# Способ 1:
# Выберите в меню Session -> Set working Directory -> Choose Directory...

# Способ 2:
# Запишите в кавычках путь к нужной папке
setwd('')


## Как получить помощь  ----------

?log      # Вызов справки через "?"
help(log) # Вызов справки с помощью функции help()
# То же самое можно сделать, если поставить текстовый курсор на название функции и нажать "F1".

## Код из раздела `Examples` можно выполнять

log(exp(3))
log10(1e7) # = 7

# `RStudio` может подсказывать и сама

# Начните набирать название функции log и нажмите `Tab` или `Ctrl + Space`.


# Не бойтесь сообщений об ошибках и предупреждений

sqr(4) # ошибка - неправильное имя функции
sqrt(-1) # предупреждение - корень из отрицательного числа - не число (NaN - not a number)


# Что делать, если консоль неистово плюсует?
# sqrt(4
# Для выхода из этого положения нажмите `Esc`.

## Установка пакетов  ----------
install.packages('ggplot2') # Устанавливаем пакет ggplot2
install.packages('tidyr')   # Устанавливаем пакет tidyr

# Команда `library()` активирует нужный пакет, если он уже установлен,
# и выполняется один раз за сеанс работы в R.
library(ggplot2) # Активируем пакет ggplot2



## `R` как калькулятор. Математические операции  ----------

## Организация кода

# Это пример использования комментариев в коде
2 + 2 # Все, что написано после значка '#--- это комментарий


## Простейшие арифметические действия

2 + 2         # Сложение
4 - 1         # Вычитание
3 * 9         # Умножение
1024 / 2      # Деление

## Степени, корни

2 ^ 4         # Возведение в степень
sqrt(9)      # Квадратный корень
27 ^ (1 / 3)  # Кубический корень

## Логарифмы

log(100)
log(100, base = 10)

## Иррациональные константы

exp(1)
pi

# Порядок действий можно менять при помощи скобок.

27 ^ 1 / 3
27 ^ (1 / 3)

## Внимательно следите за расстановкой скобок

1 + 2 * 3 - 1 ^ -1

1 + 2 * (3 - 1) ^ -1

1 + (2 * (3 - 1)) ^ -1


## Переменные  ----------

## Создаем переменную и записываем в нее значение

box_weight <- 1.3 # вес коробки
apples <- 7 # число яблок

## Чтобы увидеть содержимое переменной, нужно вызвать ее по имени

apples
box_weight


## Код --- это линейная последовательность действий

# К переменным можно обращаться только после того как они были созданы.

# Этот код вызовет ошибку при первом использоваии:
my_variable
# Error: object 'my_variable' not found
my_variable <- 10

# Этот код работает:
another_variable <- 10
another_variable


## Переменные можно использовать в расчетах

# Радиус апельсина 4.7 см, а толщина кожуры --- 1 см. Чего больше в этом апельсине, кожуры или съедобной мякоти?
# Объем шара: V = 4/3 *pi * R^3

R <- 4.7
r <- R - 1
# весь апельсин
whole_orange <- 4 / 3 * pi * R ^ 3
# объем съедобной части
edible_flesh <- 4 / 3 * pi * r ^ 3
edible_flesh
# объем кожуры
peel <- whole_orange - edible_flesh
peel
