Sys.setlocale(category ="LC_ALL", locale = "Ru_Ru") 
library(dplyr)
library(tidyr)
library(ggplot2)

df <- read.csv2("line-lolipop.csv")
df <- tbl_df(df) # превращаем в tibble
df$year <- factor(df$year) # превращаем в фактор категоризирующую переменную года

# отдельно извлекаем данные для большего значения по каждой области (преимущественно 2013 год)
y2013 <- df %>%
  group_by(oblast) %>% # группируем таблицу по областям
  arrange(desc(protests)) %>% # ранжируем по протестам 
  slice(1) # выбираем первое (большее) значение для каждой из областей

# делаем то же самое для меньших значений - slice(2)
y2011 <- df %>%
  group_by(oblast) %>%
  arrange(desc(protests)) %>%
  slice(2)

#  создаем таблицу, которая будет включать только те области, где значение 2013 года больше, чем 2011
normal <- df%>%
  spread(year, protests) %>%  # разбиваем год на 2 столбика (2011 и 2013 отдельно)
  group_by(oblast)  # группируем по областям
# добавляем переменную, которая = 1 если значение 2013>2011, и = 0 если нет
normal$probe <- ifelse(normal$`2013`- normal$`2011` > 0, 1, 0)  
# оставляем лишь строки где probe = 1
normal <- filter(normal, probe==1)  

# выстраиваем порядок в оси у (от наибольших значений к наименьшим)
df$oblast <- factor(df$oblast, 
                    levels = y2013[order(y2013$protests, decreasing = F),]$oblast)

# оставляем в y2013 и y2011 только строки, где значение 2013 года больше, чем 2011
y2013 <- filter(y2013, oblast %in% normal$oblast)
y2011 <- filter(y2011, oblast %in% normal$oblast)

# создаем еще один массив, который будет иметь первоначальный вид но только с областями, где значение 2013 года больше, чем 2011
highlite <- filter(df, oblast %in% normal$oblast)
highlite <- arrange(highlite, year, protests)  # ранжируем


# добавляем укр названия 
df$obl.ukr <- c("Рівне", "Хмельницький", "Чернігів", "Кропивницький", "Луцьк",
             "Чернівці", "Вінниця", "Полтава", "Суми", "Запоріжжя",
             "Черкаси", "Ужгород", "Тернопіль", "Житомир", "Дніпро",
             "Херсон", "Івано-Франківськ", "Луганськ", "АР Крим",
             "Харків", "Донецьк", "Миколаїв", "Львів", "Одеса", "Київ")

# график
ggplot(df, aes(protests, oblast)) +
  geom_line(aes(group = oblast), alpha = 0.2) + # наносим полупрозрачные линии-отрезки от меньшего к большему значению на графике
  geom_point(aes(color = year), size = 2.5, alpha = 0.2) +  # и полупрозрачные точки, где цвет определяется годом
  geom_line(data = highlite, aes(group = oblast)) +  # наносим отрезки но только уже не прозрачные из данных highlite
  geom_point(data = highlite, aes(color = year), size = 2.5) +  # и точки
  geom_text(data = y2011, aes(color = year, label = round(protests, 0)), # подписи - из y2011, цвет тоже определяется годом
            size = 3, hjust = 1.5) + # сдвинутые на 1.5 (чтобы рядом)
  geom_text(data = y2013, aes(color = year, label = round(protests, 0)),  # подписи - из y2013
            size = 3, hjust = -0.5) + # сдвинутые на -0.5
  scale_color_discrete(labels = c("2011 рік","2013 рік (до 21 листопада)")) + # подписи в легенде
  labs(x = "Кількість протестів", y = "",  # подписи осей
       title = "\nЗбільшення кількості протестів в областях Україні в 2011-2013 рр.\n\n",
       caption = "Підписи відображені лише для областей, \nде відбулось зростання протестної активності") +
  scale_y_discrete(labels = df$obl.ukr) +  # подписи оси у - украинские
  theme_minimal() +
  theme(panel.grid.major.x = element_blank(),  # убираем вертикальную сетку 
        panel.grid.minor = element_blank(),  # и второстепеные
        legend.title = element_blank(),  # без названия легенды
        legend.direction = "horizontal",  # расположение легенды - горизонтальное
        legend.position = c(0.4, 1.05),  # определить позицию легенды
        text = element_text(family = "Georgia"),  # шрифт
        plot.title = element_text(size = 15, margin = margin(b=10),  # положение названия
                                  hjust = 0.4, vjust=-10),
        plot.caption = element_text(size = 10, margin = margin(t=10),color = "grey40"))  # положение и цвет комментария


# сохранить график
ggsave("line-lolipop.png")  