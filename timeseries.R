library(lubridate)
library(stackr)
library(dplyr)

load(file = "proba.Rdata")
load(file = "analisis.Rdata")
load(file = "algebra.Rdata")
load(file = "calculus.Rdata")
load(file = "equations.Rdata")



proba <- select(proba, answer_count, creation_date)
analisis <- select(analisis, answer_count, creation_date)
algebra <- select(algebra, answer_count, creation_date)
calculus <- select(calculus, answer_count, creation_date)
equations <- select(equations, answer_count, creation_date)





analisis$creation_date <- month(analisis$creation_date)
proba$creation_date <- month(proba$creation_date)
algebra$creation_date <- month(algebra$creation_date)
calculus$creation_date <- month(calculus$creation_date)
equations$creation_date <- month(equations$creation_date)

meses.proba <- split(proba,proba$creation_date)
meses.analisis <- split(analisis,analisis$creation_date)
meses.algebra <- split(algebra,algebra$creation_date)
meses.calculus <- split(calculus,calculus$creation_date)
meses.equations <- split(equations,equations$creation_date)





t <-sample(1:874,50)
tt <- sample(1:535,50)
ttt <- sample(1:1141,100)
tttt <- sample(1:1059, 85)

meses$`12` <- meses$`12`[t,]
meses$`11` <- meses$`11`[tttt,]
meses$`10` <- meses$`10`[ttt,]
meses$`9` <- meses$`9`[tt,]


enero <- sum(meses$`1`$answer_count)
 
febrero <- sum(meses$`2`$answer_count)
marzo <- sum(meses$`3`$answer_count)
abril <- sum(meses$`4`$answer_count)
mayo <- sum(meses$`5`$answer_count)
junio <- sum(meses$`6`$answer_count)
julio <- sum(meses$`7`$answer_count)
agosto <- sum(meses$`8`$answer_count)
septiembre <- sum(meses$`9`$answer_count)
octubre <- sum(meses$`10`$answer_count)
noviembre <- sum(meses$`11`$answer_count)
diciembre <- sum(meses$`12`$answer_count)
serie <- c(enero,febrero,marzo, abril,mayo,junio,julio,agosto,septiembre,octubre,noviembre,diciembre)
serie2 <- 1:12

se <- cbind(serie2,serie)

plot(ts(serie,start = 1 ,frequency = 12))

 ggplot(as.data.frame(se), aes(x = serie2, y = serie)) + geom_line(stat = "identity", aes(group = 1, col = "red") ) + scale_x_continuous(name = "Month",breaks = 1:12,labels = c("1"="Jan","2"="Feb","3"="Mar","4"="Apr","5"="May","6"="Jun","7"="Jul","8"="Aug","9"="Sep","10"="Oct","11"="Nov","12"="Dec")) + ylab("Respuestas") + 



