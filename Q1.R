setwd('~/Dropbox/iidata')

ffclean6 = read.csv('ffclean6.csv')
str(ffclean6)






#Question 1

levels(ffclean6$countries_en)

France = "France|French|fr"

France_count1 = regexpr(France, levels(ffclean6$countries_en))

France_label = levels(ffclean6$countries_en)[which(France_count1 != -1)]

#france data
France_data = ffclean6[ffclean6$countries_en %in% France_label,]
dim(France_data)

#non france data
NonFrance_data = ffclean6[!(ffclean6$countries_en %in% France_label),]
dim(NonFrance_data)

# NA or not
table(is.na(NonFrance_data$nutrition_score_fr_100g))#7412
table(is.na(France_data$nutrition_score_fr_100g))#23737

# plot
library(ggplot2)
ggplot(France_data, aes(nutrition_score_fr_100g))+
  geom_histogram(col="white", 
                 fill= '#74D2FF',binwidth=1)+
  labs(list(title = "", x = "nutrition score (100g)"))+
  theme_bw()+
  theme(axis.title = element_text(size=16,colour='#575757'), axis.text.y =element_text(size=13,colour='#575757'),axis.text.x =element_text(size=13,colour='#575757'))+
  theme(legend.position="none",
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank())+
  theme(axis.line.x = element_line(color="black", size = 0.2),
        axis.line.y = element_line(color="black", size = 0.2))
  #scale_x_continuous(breaks = round(seq(45,95, by = 5),1)) +
  #scale_y_continuous(breaks = round(seq(0,90, by = 20),1))

ggplot(NonFrance_data, aes(nutrition_score_fr_100g))+
  geom_histogram(col="white", 
                 fill= '#74D2FF',binwidth=1)+
  labs(list(title = "", x = "nutrition score (100g)"))+
  theme_bw()+
  theme(axis.title = element_text(size=16,colour='#575757'), axis.text.y =element_text(size=13,colour='#575757'),axis.text.x =element_text(size=13,colour='#575757'))+
  theme(legend.position="none",
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank())+
  theme(axis.line.x = element_line(color="black", size = 0.2),
        axis.line.y = element_line(color="black", size = 0.2))


plot(density(fr))
lines(density(nonfr),col='red')

# t test
fr = France_data$nutrition_score_fr_100g

nonfr = NonFrance_data$nutrition_score_fr_100g

ks.test(x=fr,y='pnorm',alternative='two.sided')

ks.test(x=nonfr,y='pnorm',alternative='two.sided')

qqnorm(fr)
qqline(fr, col='red')

qqnorm(nonfr)
qqline(nonfr, col='red')

t.test(fr, nonfr,alternative = c("less"))

wilcox.test(fr, nonfr,alternative = c("greater"))#x>y




  
  
  
