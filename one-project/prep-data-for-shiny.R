# Code Andreas - prep-data-for-shiny.R - gets 'source'-d in Global.R

if (!require("dplyr")) install.packages("dplyr") ; suppressWarnings(library(dplyr))
if (!require("ggmap")) install.packages("ggmap") ; suppressWarnings(library(ggmap))
if (!require("shiny")) install.packages("shiny") ; suppressWarnings(library(shiny))
if (!require("shinydashboard")) install.packages("shinydashboard") ; suppressWarnings(library(shinydashboard))
if (!require("dygraphs")) install.packages("dygraphs") ; suppressWarnings(library(dygraphs))
if (!require("leaflet")) install.packages("leaflet") ; suppressWarnings(library(leaflet))
if (!require("highcharter")) install.packages("highcharter") ; suppressWarnings(library(highcharter))
if (!require("xts")) install.packages("xts") ; suppressWarnings(library(xts))

if (!require("sp")) install.packages("sp") ; suppressWarnings(library(sp))
if (!require("geojsonio")) install.packages("geojsonio") ; suppressWarnings(library(geojsonio))
if (!require("reshape2")) install.packages("reshape2") ; suppressWarnings(library(reshape2))


# Sri Lanka data

path = 'input-files/srilankaoutbreaks.csv'
sri_lanka = read.csv(path)
sri_lanka$DATE = as.POSIXct(strptime(sri_lanka$period_date, format = "%Y%m%d"))
sri_lanka$period_date <- NULL
sri_lanka$country_name <- NULL
sri_lanka$admin_lvl1_id <- NULL
sri_lanka$country_iso2_id <- NULL
sri_lanka$admin_lvl1_name = as.character(sri_lanka$admin_lvl1_name)
colnames(sri_lanka)[1] <- 'District'

cities = unique(sri_lanka$District)

# population data

path = 'input-files/SRI_LANKA_Population.csv'
population = read.csv(path, header = T, sep = ';', dec = ',')
population = subset(population, select = c('District', 'Population'))
population$District = as.character(population$District)
population$Population = as.numeric(gsub(pattern = ',', replacement = '', x = population$Population))
population[population$District == 'Mullaitivu', 'District'] <- 'Mulativu'
population[population$District == 'Mullaitivu', 'District'] <- 'Moneragala'

analysis_data = dplyr:::left_join(sri_lanka, population, by = 'District')
analysis_data$pct = analysis_data$dengue_total_cases / analysis_data$Population

# City Timeseries

df = data.frame(city = cities)
df$coordinates = geocode(as.character(df$city), source="google")
df$lon = df$coordinates$lon
df$lat = df$coordinates$lat
df$coordinates <- NULL
df[df$city == 'Matara', c('lon', 'lat')] = c(80.550834, 5.951736)
df$city <- as.character(df$city)

colnames(analysis_data)[1] <- 'city'
analysis_data = dplyr:::left_join(analysis_data, df, by = 'city')


# # # Visualization


library(sp)
require(geojsonio)

srilankamap <- geojson_list(readRDS("input-files/LKA_adm1.rds"))
srilankamap <- geojson_list(srilankamap)

##
real_data <- read.csv("input-files/climateMergedToDengueWithPred.csv", sep = ';')# unique(real_data$admin_lvl1_name)
real_data$admin_lvl1_name <- as.character(real_data$admin_lvl1_name)

firstup <- function(x) {
  substr(x, 1, 1) <- toupper(substr(x, 1, 1))
  x
}

real_data$admin_lvl1_name <- firstup(real_data$admin_lvl1_name)

real_data2 <- real_data[, c("admin_lvl1_name",
                            "date",
                            "dengue_total_cases",
                            "predicted")]

real_data2$date <- as.character(real_data2$date)
real_data2$date <- substr(real_data2$date,1,7)


# each month should be a column
# there should be 25 rows

library(reshape2)
real_data2 <- dcast(real_data2,
                    admin_lvl1_name ~ date,
                    value.var = "dengue_total_cases")

## ABSOLUTE

base_table <- real_data2

## RELATIVE

population_corr <- population
population_corr[population_corr$District == 'Monaragala', 'District'] <- 'Moneragala'
real_data2[real_data2$admin_lvl1_name == 'Nuwara eliya', 'admin_lvl1_name'] <- 'Nuwara Eliya'

relative_data <- dplyr:::left_join(real_data2, population_corr, by = c('admin_lvl1_name' = 'District'))
relative_data[relative_data$admin_lvl1_name == 'Mulativu', 'admin_lvl1_name'] <- 'Mullaitivu'

data_cols <- colnames(relative_data)[!colnames(relative_data) %in% c('admin_lvl1_name', 'Population')]
for (i in data_cols) {
  relative_data[,i] <- relative_data[,i] / relative_data[,'Population']
}
relative_data$Population <- NULL


library(highcharter)

obj <- highchart() %>%
  hc_title(text = "Nr of Dengue cases") %>%
  hc_add_series_map(srilankamap,
                    relative_data,
                    name = "Number of Dengue Cases",
                    value = "2010-01",
                    joinBy = c("NAME_1", "admin_lvl1_name"))

sequence_data <- relative_data[, -1]

# add sequence info
i <- 1
while(i <= length(obj$x$hc_opts$series[[1]]$data)) {
  obj$x$hc_opts$series[[1]]$data[[i]][['sequence']] <- as.numeric(sequence_data[i, ])
  i <- i + 1
}

# add motion nu
obj$x$hc_opts$motion <- list(enabled = TRUE,
                             labels = colnames(sequence_data),
                             series = 0)

# print viz
obj
map_II <- obj

