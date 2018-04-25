Sys.setlocale(category ="LC_ALL", locale = "Ru_Ru") 
library(ggplot2)

dat.ess <- read.csv2("linegraph with median.csv")
# превращаем в фактор и выстраиваем порядок для оси у
dat.ess$Country <- factor(dat.ess$Country, 
                          levels = dat.ess[order(dat.ess$Protest, decreasing = F),]$Country)

# добавляем укр. названия стран
dat.ess$Country.ukr <- c("Іспанія", "Ісландія", "Італія", "Франція", "Ірландія", "Ізраїль",
                         "Норвегія", "Німеччина", "Албанія", "Швеція", "Чехія", "Португалія",
                         "Косово", "Болгарія", "Бельгія", "Кіпр", "Швейцарія", "Данія", "Росія",
                         "Словаччина", "Естонія", "Угорщина", "Словенія", "Великобританія",
                         "Нідерланди", "УКРАЇНА", "Польща", "Литва", "Фінляндія")
# тоже превращаем в упорядоченную факторную переменную
dat.ess$Country.ukr <- factor(dat.ess$Country.ukr, 
                          levels = dat.ess[order(dat.ess$Protest, decreasing = F),]$Country.ukr)

# Есть 2 варианта - считать среднее или медиану. 
mean(dat.ess$Protest)
median(dat.ess$Protest)

# Но среднее значительно выше (из-за "выброса" Испании), потому будем использовать медиану

dat.ess$Median <- median(dat.ess$Protest)

# новая переменная, обозначающа выше ли значение чем медиана (=TRUE) или ниже (=FALSE)
dat.ess$Above.med <- ifelse(dat.ess$Protest - dat.ess$Median > 0, TRUE, FALSE)

# делаем сабсет для Украины (чтобы потом выделить на графике)
dat.Ukr <- subset(dat.ess, dat.ess$Country == "Ukraine")


# график
ggplot(dat.ess, aes(x = Protest, y = Country.ukr)) +
  geom_vline(xintercept = dat.ess$Median, color = "darkturquoise", linetype = "longdash", alpha = 0.3) + # рисуем линию медианы
  geom_segment(aes(x = Median, xend = Protest, y = Country.ukr, yend = Country.ukr), color = "grey50", alpha = 0.5)+ # рисуем серые отрезки от медианы до значения Protest
  geom_point(aes(color = dat.ess$Above.med),alpha = 0.8, size = 7) + # рисуем точки, цвет зависит от значения Above.med, (прозрачность = 0.8)
  geom_text(aes(label = paste0(Protest,"%")), size = 2, color = "white")+ # подписываем значения, добавляем знак "%" + размер и цвет
  geom_segment(data = dat.Ukr, aes(x = Median, y = Country.ukr, xend = Protest, yend = Country.ukr), color = "black")+
  geom_point(data = dat.Ukr, size = 8, color = "firebrick")+ # выделяем точку для Украины
  geom_text(data = dat.Ukr, aes(label = paste0(Protest,"%")),size = 2.5, color = "white")+ # подписываем значение для Украины
  geom_text(aes(label="Медіана = 5.2%", x=dat.ess$Median+0.3, y=8, angle = 90, size = 2.5), color="#00BFC4")+ # подписываем медиану, угол 90
  labs(x = "% респондентів, які впродовж 12 місяців брали участь у санкціонованих мітингах або демонстраціях",# название графика и осей
       y = "",
       title = "\nПротестна активність в Європейському регіоні, 2012 р.\n",
       caption = "*за даними European Social Survey")+
  theme_minimal()+
  theme(panel.grid.major.x = element_blank(), # наводим марафет в теме
        panel.grid.minor = element_blank(),
        axis.ticks.x = element_blank(),
        axis.text.x = element_blank(),
        legend.position = "none",
        plot.caption = element_text(size = 10, margin = margin(t=10),color = "grey40"),
        plot.title = element_text(size = 15, margin = margin(b=10),
                                  hjust = 0.4, vjust=-10),
        text = element_text(family = "Georgia")) # меняем шрифт

ggsave("linegraph with median.png")

