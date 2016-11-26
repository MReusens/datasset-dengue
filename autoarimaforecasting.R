library(forecast)
library(ggplot2)

worldbank <- fread("C:\\DengueHack\\1500SriLankaDataWorldBank.csv", sep=";")

worldbank$sanirural = worldbank[,1087,with=F]
fakesanirural= rep(0, 72)
fakesaniurban= rep(0, 72)

worldbank$`Indicator Name`

for(i in 1:6){
  for (j in 1:12){
  fakesanirural[(i-1)*12+j]=worldbank$sanirural[50+i]
  }
}
worldbank$saniurban = worldbank[,1088, with=F]

names(worldbank)

?ts
climatemerge$admin_lvl1_name
climategalle= climatemerge[admin_lvl1_name=="galle"]
myts <- ts(data= climategalle$dengue_total_cases, start=c(2010,1), frequency=12)
start(myts)
print(myts)
plot(myts)
abline(reg=lm(myts~time(myts)))
plot(aggregate(myts, FUN=mean))
boxplot(myts~cycle(myts))

#xreg nog niet optimaal
xreg <- cbind(Temperature=climategalle$temp,Moist=climategalle$prcp, SaniUrban= fakesaniurban, SaniRural= fakesanirural)


autoloads<- auto.arima(myts, xreg= xreg)

plot(forecast(autoloads))
plot(myts, col="red")
lines(fitted(autoloads), col="blue")

?accuracy
accuracy(fitted(autoloads),myts)

