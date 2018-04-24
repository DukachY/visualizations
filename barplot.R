library(ggplot2)
Sys.setlocale(category ="LC_ALL", locale = "Ru_Ru") # чтобы работать с кириллицей
dat1 <- read.csv2("barplot.csv") # загрузить массив
dat1$year <- factor(dat1$year, levels = c("2013", "2012", "2011")) # превращаем в фактор переменную для оси х, выстраиваем порядок 

ggplot(dat1, aes(x = year, y = protests, fill = net.protest))+ # fill - выбираем переменную, которая разбивает столбики на части
  geom_col(position = "fill") + # уточняем, что это столбиковая нормированная с накоплением (fill)
  coord_flip() + # разворачиваем график горизонтально
  theme_minimal() + # упрощаем себе жизнь, выбирая тему
  geom_text(aes(label="16,3%", x = 3, y = 0.92), color="white", size = 5) + # добавляем подписи
  geom_text(aes(label="16,7%", x = 2, y = 0.92), color="white", size = 5) + # определяя их озиции по х и у
  geom_text(aes(label="24,3%", x = 1, y = 0.885), color="white", size = 6) + 
  geom_text(aes(label="2011 рік", x = 3, y = 0.05), color="white", size = 4) + 
  geom_text(aes(label="2012 рік", x = 2, y = 0.05), color="white", size = 4) + 
  geom_text(aes(label="2013 рік", x = 1, y = 0.05), color="white", size = 4) + 
  labs(x = "", y = "", # убираем названия осей
       title = "\nЧастка мережевих протестів в Києві, 2011-2013рр. (%)\n", # добавляем название графика
       caption = "*за даними моніторингу протестів, репресій та поступок")+ # подпись внизу
  theme(panel.grid.major = element_blank(), # убираем основные линии сетки
        panel.grid.minor = element_blank(), # убираем второстепенные линии сетки
        axis.text.y = element_blank(), # убираем подписи оси у
        legend.position = "none", # без легенды
        plot.caption = element_text(size = 12, margin = margin(t=5), # margin - 5 условных отступов сверху
                                    color = "grey40"), # цвет текста
        plot.title = element_text(size = 15, margin = margin(b=10), # margin - 10 условных отступов снизу
                                  hjust = 0.5, vjust=-10), # определяем положение по горизонтали и вертикали
        text = element_text(family = "Georgia")) # меняем шрифт графика

# сохраняем последний плот
ggsave("barplot.png")



installed.packages()
