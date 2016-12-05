library(sp)
require(geojsonio)

srilankamap <- geojson_list(readRDS("C:/Users/id094328/Desktop/Dengue/LKA_adm1.rds"))
srilankamap <- geojson_list(srilankamap)
# 
# example <- read.csv("C:/Users/id094328/Desktop/Dengue/Andy_example.csv",
#                     sep = ";")
# example <- example[, 1:2]

##
real_data <- read.csv("C:/Users/id094328/Desktop/Dengue/climateMergedToDengueWithPred.csv", sep = ';')# unique(real_data$admin_lvl1_name)
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
  
  relative_data <- left_join(real_data2, population_corr, by = c('admin_lvl1_name' = 'District'))
  relative_data[relative_data$admin_lvl1_name == 'Mulativu', 'admin_lvl1_name'] <- 'Mullaitivu'

  data_cols <- colnames(relative_data)[!colnames(relative_data) %in% c('admin_lvl1_name', 'Population')]
  for (i in data_cols) {
    relative_data[,i] <- relative_data[,i] / relative_data[,'Population']
  }
  relative_data$Population <- NULL
  
## With predictions
  
  
  
  
  
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
