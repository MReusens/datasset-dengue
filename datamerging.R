library(data.table)
library(sqldf)
library(lubridate)

climate <- fread("C:\\DengueHack\\daily_climate_asia\\daily_climate_asia.csv")


climatebackup = climate



stations <- fread("C:\\DengueHack\\weatherstationsXY.csv")
denguesri <- fread("C:\\DengueHack\\denguehack\\medical_dengue\\admin_level\\sri_lanka\\data\\lka_mihealth_data_refactored.csv")
denguesri[,admin_lvl1_name := tolower(admin_lvl1_name)]
parsedDates = as.Date(strptime(denguesri$period_date,"%Y%m%d"))
denguesri[,date := parsedDates]
stations[,`STATION NAME`:= tolower(`STATION NAME`)]



names(climate)
names(denguesri)
data$date
range(strptime(DT$date,"%Y-%m-%d"))

?strptime
str(climate)

provinces = unique(denguesri$admin_lvl1_name)
stationssri = stations[`STATION NAME` %in% provinces]



climate = climate[stn %in% stationssri$USAF]
climate[,date := as.Date(fast_strptime(paste0(substr(yearmoda,1,6),"01"),"%Y%m%d"))]
climate = climate[,.SD[1],by=c("stn","date")]

mergedsri <- merge(denguesri, stationssri[,.(`STATION NAME`,LAT,LON,USAF,`ELEV(M)`)],  by.x="admin_lvl1_name", by.y="STATION NAME", all.x=T)
climatemerge = merge(mergedsri,climate, by.x = c("USAF","date"),by.y=c("stn","date"),all.x = TRUE)

write.table(climatemerge,"C:\\DengueHack\\climateMergedToDengue.csv",row.names = FALSE,sep="," )

