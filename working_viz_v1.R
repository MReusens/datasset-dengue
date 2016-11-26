library(sp)
require(geojsonio)

srilankamap <- readRDS("./Gadm/LKA_adm1.rds")

srilankamap <- geojson_list(srilankamap)

example <- read.csv("Andy_example.csv",
                    sep = ";")
example <- example[, 1:2]

obj <- highchart() %>%
  hc_title(text = "Nr of Dengue cases") %>%
  hc_add_series_map(srilankamap,
                    example,
                    name = "Nr of Dengue Cases",
                    value = "cases",
                    joinBy = c("NAME_1", "city"))

# add sequence voor alle rijen
n <- 1:25
sequence_data <- data.frame( 0,
                             100*n,
                             200*n,
                             300*n,
                             400*n,
                             500*n,
                             600*n,
                             700*n,
                             800*n,
                             900*n
)

# add sequence info
i <- 1
while(i <= length(obj$x$hc_opts$series[[1]]$data)) {
  obj$x$hc_opts$series[[1]]$data[[i]][['sequence']] <- as.numeric(sequence_data[i, ])
  i <- i + 1
}

# add motion nu
obj$x$hc_opts$motion <- list(enabled = TRUE,
                             labels = 2000:2009,
                             series = 0)

# print viz 
obj
