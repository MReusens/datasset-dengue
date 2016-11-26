library(forecast)
library(ggplot2)
library(data.table)
library(sqldf)
worldbank <- fread("C:\\DengueHack\\1500SriLankaDataWorldBank.csv", sep=";")
climatemerge <- fread("C:\\DengueMichael\\datasset-dengue\\climateMergedToDengue.csv", sep=",")


worldbank$sanirural = worldbank[,1087,with=F]
worldbank$saniurban = worldbank[,1088, with=F]

fakesanirural= rep(0, 72)
fakesaniurban= rep(0, 72)

worldbank$`Indicator Name`

for(i in 1:6){
  for (j in 1:12){
    fakesaniurban[(i-1)*12+j]=worldbank$saniurban[50+i]
  }
}

print(names(worldbank))

?ts
unique(climatemerge$admin_lvl1_name)
climate1= climatemerge[admin_lvl1_name=="galle"]
climate2= climatemerge[admin_lvl1_name=="vavuniya"]

allprovinces=unique(climatemerge$admin_lvl1_name)

doArimaProvince("galle")
doArimaProvince("vavuniya")

provinceName= "galle"
doArimaProvince <-function (provinceName){
  climate2= climatemerge[admin_lvl1_name==provinceName & as.numeric(substr(date, 1, 4))<2016]
  i=2
  
  
  
  
  predictions = data.table()
  NB_MONTHS_TO_HOLD_OUT = 48
  for(provinceName in unique(climatemerge$admin_lvl1_name)){
    
    provinceName= "vavuniya"
    
    
    climate2= climatemerge[admin_lvl1_name==provinceName & as.numeric(substr(date, 1, 4))<2016]
    i=2
    for(i in 1:NB_MONTHS_TO_HOLD_OUT){
      training = climate2[1:(i-1+nrow(climate2)-NB_MONTHS_TO_HOLD_OUT)]
      test = climate2[nrow(climate2)-NB_MONTHS_TO_HOLD_OUT+i]
      
      myts <- ts(training$dengue_total_cases,start=c(2010,1), frequency=12)
      
     
       #xreg <- cbind(Temperature=training$temp)
      
      
      # fittedModel = auto.arima(myts,xreg=xreg)
      fittedModel = auto.arima(myts)
      
      # prediction = forecast(fittedModel,xreg = xreg,h=1)
      prediction = forecast(fittedModel,h=1)
      
      predictions = rbind(predictions,data.table(province = test$admin_lvl1_name, date = test$date,predicted = prediction$mean[1] ))
    }
  }
  
  predictionmerge = merge(climatemerge,predictions, by.x = c("admin_lvl1_name","date"),by.y = c("province","date"),all.x = TRUE)
  write.table(predictionmerge,"C:\\DengueMichael\\datasset-dengue\\climateMergedToDengueWithPred.csv", sep=";")
  
  par(mfrow=c(2,2))
  
  
  
  predictionmerge$AE= abs((predictionmerge$predicted-predictionmerge$dengue_total_cases))
  
  summary(predictionmerge$dengue_total_cases) #MAE 45.95
  
  summary(predictionmerge$AE, rm.na=T) #Mean Actuals= 128
  names(predictionmerge)
  
  maeprov <- sqldf("SELECT avg(AE) as MAE , avg(dengue_total_cases) as MA, admin_lvl1_name FROM predictionmerge  WHERE AE IS NOT \"NA\" GROUP BY admin_lvl1_name")
  library(ggrepel)
  ggplot(maeprov, aes(MA, MAE))+ geom_point(size=2)+  geom_text(aes(label=ifelse(MAE>50,as.character(admin_lvl1_name),'')),hjust=1.1,vjust=-.1,size=5)+
theme_bw()+geom_abline(intercept= 7.43, slope=0.3)+ xlab("Mean Actual Dengue Cases")+ylab("Mean Average Error")
  
  coef(lm(MAE ~ MA, data = maeprov))
  

  par(mfrow=c(1,2))
  myts <- ts(data= climate2$dengue_total_cases, start=c(2010,1), frequency=12)
  #start(myts)
  #print(myts)
  plot(myts)
  abline(reg=lm(myts~time(myts)))

  #plot(aggregate(myts, FUN=mean))
  boxplot(myts~cycle(myts), main="Number of Dengue Cases in Vavuniya Province", 
          ylab="Number of Cases", xlab="Seasons (2010-2015)")
  
  
  #xreg nog niet optimaal
  xreg <- cbind(Temperature=climate2$temp)
  
  
  autoloads<- auto.arima(myts, xreg= xreg)
  #autoloads<- auto.arima(myts)
  
  print(autoloads)
  print((1-pnorm(abs(autoloads$coef)/sqrt(diag(autoloads$var.coef))))*2)
  
  #plot(forecast(autoloads2))
  plot(myts, col="red")
  lines(fitted(autoloads), col="blue")
  accuracy(fitted(autoloads),myts)
}

