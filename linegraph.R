library(ggplot2)
Sys.setlocale(category ="LC_ALL", locale = "Ru_Ru") # для работы с кириллицей
dat <- read.csv2("linegraph.csv") 
dat$year <- factor(dat$year) # превращем в факторы переменную оси х и группирующую переменную
dat$Month <- factor(dat$Month)

# Вытягиваем данные только для "жирной" линии - чтобы потом нанести сверху
dat.all <- subset(dat, year == "2011-2013")  

# Создаем переменную с украинскими названиями месяцев
dat.all$names <- factor(c("Січень", "Лютий", "Березень", "Квітень", "Травень", # 
                   "Червень", "Липень", "Серпень", "Вересень", "Жовтень",
                   "Листопад", "Грудень"))

# График:
ggplot(data=dat, aes(x = Month, y = protests, group = year)) + # группируем по году = одна линия для одного года
  geom_hline(yintercept = 8.3, color="cadetblue2",linetype = "longdash", size = 1.2) + # добавляем горизонтальную линию (тип = пунктир)
  geom_line(color = "grey80") + # строим линейный график, цвет - серый
  geom_line(data = dat.all, aes(x = Month, y = protests), color = "cornflowerblue",size = 1.5) +  # наносим сверху жирную линию тренда за три года
  scale_x_discrete(labels = dat.all$names) +  # подписи оси х на украинском
  geom_text(aes(label="Якби всі протести були розподілені рівномірно (8.3%)",  # наносим текст
                x = 10.6, y = 16), color="cadetblue2", size = 3.5) + # определяем его положение, цвет и размер шрифта
  annotate("segment", x = 12, xend = 12, y = 15.5, yend = 8.6, colour = "cadetblue2", size=0.8,  # рисуем стрелку
             arrow=arrow(length = unit(0.2, "inches")))+ # определяем размер наконечнмка стрелы
  geom_text(aes(label="Середнє значення за 2011-2013рр.",  # наносим текст
                x = 4, y = 19.5), color="cornflowerblue", size = 3.5) + 
  annotate("segment", x = 4, xend = 3.3, y = 19, yend = 11.3, colour = "cornflowerblue", size=1,  # и рисуем от него стрелу
           arrow=arrow(length = unit(0.2, "inches")))+ 
  labs(x = "Місяць",  # подпись оси х
       y = "% від річної кількості протестів", # подпись оси у
       title = "\nСезонність протестної активності в Україні?\n", # название
       caption = "*за даними моніторингу протестів, репресій та поступок")+ # комментарий внизу графика
  theme_minimal()+ # задаем минималистичную тему
  theme(panel.grid.major.x = element_blank(), # убираем вертикальную сетку
        panel.grid.minor = element_blank(), # убираем второстепенную сетку
        legend.position = "none", # убираем легенду
        plot.caption = element_text(size = 10, margin = margin(t=10), color = "grey40"), # определяем цвет, размер и положение подписи
        plot.title = element_text(size = 15, margin = margin(b=10), # То же самое для названия графика
                                  hjust = 0.5, vjust=-10), # положение по вертикали и горизонтали
        text = element_text(family = "Georgia")) # шрифт

# сохраняем в формате png
ggsave("linegraph.png")
