

### Fill in paths to relevant folders

      # Path to R Packages
      .libPaths("C:/Users/id094328/R/Packages")
      
      # Path to files
      path <- 'C:/Users/id094328/Desktop/Dengue'
      setwd(path)
      
      # Source functions that are used within APP
      source(file = "Scripts/Script.R")

### Launch and host app
      
      # shiny::runApp('Shiny')
      
      # Host WebPortal from own desktop pc
      x <- system("ipconfig", intern=TRUE)
      z <- x[grep("IPv4", x)]
      ip_adrr <- gsub(".*? ([[:digit:]])", "\\1", z)[1]
      shiny::runApp('Shiny', host = ip_adrr, port = 5050)
