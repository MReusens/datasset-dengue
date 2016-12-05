# Code Andreas - shiny server

shinyServer(function(input, output) {
  
  ts_data <- reactive({
    
    if(input$select_ts == 'Absolute Numbers') {
      column = 'dengue_total_cases'
    } else {column = 'pct'}
    
    if(input$select_city == 'ALL') {
      
      subset_data <- analysis_data[analysis_data$city == 'Colombo',]
      ts = xts(x = subset_data[, column], order.by = subset_data$DATE)
      ts[is.na(ts)] <- 0
      colnames(ts) <- i
      all_ts <- ts
      
      cities_no_colombo <- cities[cities != 'Colombo' ]
      
      for (i in cities_no_colombo) {
        
        subset_data <- analysis_data[analysis_data$city == i,]
        ts = xts(x = subset_data[,column], order.by = subset_data$DATE)
        ts[is.na(ts)] <- 0
        colnames(ts) <- i
        
        all_ts = cbind(all_ts, ts)
        
      }
    } else {
      
      subset_data <- analysis_data[analysis_data$city == input$select_city,]
      ts = xts(x = subset_data[, column], order.by = subset_data$DATE)
      ts[is.na(ts)] <- 0
      colnames(ts) <- i
      all_ts <- ts
      
    }
    
    return(all_ts)
    
  })
  
  #   map_data <- reactive({
  #     
  #     ts
  #     return(ts)
  #     
  #   })
  
  output$highchart_map <- renderHighchart({
    obj
  })
  
  output$highchart_map_II <- renderHighchart({
    map_II
  })
  
  
  output$leaflet_map <- renderLeaflet({
    
    # color
    if(input$select_map == 'Absolute Numbers') {
      column = 'dengue_total_cases'
    } else if (input$select_map == 'Relative Numbers') {
      column = 'pct'
    } else {
      column = NULL
    }
    
    # date = as.POSIXct(as.Date("01/01/2010", "%m/%d/%Y"))
    
    # "2016-01-01"
    # as.POSIXct(as.Date(input$select_day, "%Y-%m-%d"))
    
    # substr(unique(analysis_data$DATE), 1, 10)[1] %in% substr(unique(analysis_data$DATE), 1, 10)
    
    # "2016-01-01"
    leaflet_data = analysis_data[substr(unique(analysis_data$DATE), 1, 10) == input$select_day,]
    leaflet_data[, 'input_column'] <- leaflet_data[, column]
    
    max_value = max(leaflet_data[, 'input_column'], na.rm = TRUE)
    leaflet_data[, 'input_column_scaled'] = leaflet_data[, 'input_column'] / max_value
    
    leaflet(data = leaflet_data) %>% 
      addTiles() %>%
      # addProviderTiles("Hydda.Full") %>% 
      addCircleMarkers(~lon, ~lat,
                       # color = ~color_pal_time(time_interval),
                       radius = ~input_column_scaled*25,
                       popup = ~paste(city, " - ", input_column, "cases"))
  })
  
  #   output$highchart_ts <- renderHighchart({
  #     airforecast <- forecast(auto.arima(ts(ts()), ), level = 95)
  #     hchart(airforecast)
  #   })
  
  output$dygraph_ts <- renderDygraph({
    dygraph(ts_data())
  })
  
  output$dygraph_prediction <- renderDygraph({
    
    district <- input$select_district
    
    df <- analysis_data
    df <- subset(df, subset = city == district)
    ts1 <- xts(df$dengue_total_cases, order.by = df$DATE)
    colnames(ts1) <- 'Actual'
    
    ts_data = ts(as.numeric(ts1), start = c(2010, 1), frequency = 12)
    hw <- HoltWinters(ts_data)
    predicted <- predict(hw, n.ahead = 4, prediction.interval = FALSE)
    ts_total <- cbind(Actuals = ts(as.numeric(ts1), start = c(2010, 1), frequency = 12), Predicted = predicted)
    
    dygraph(ts_total, main = "Predicted number of people impacted") %>% 
      dyRangeSelector(dateWindow = c("2015-01-01", "2017-01-01"))
    
  })
  
  output$leaflet_map_II <- renderLeaflet({
    
    past_period <- as.numeric(input$past_period)
    
    empty_vector = vector()
    
    for (i in cities) {
      district <- i
      
      df <- analysis_data
      df <- subset(df, subset = city == district)
      ts1 <- xts(df$dengue_total_cases, order.by = df$DATE)
      
      ts_data = ts(as.numeric(ts1), start = c(2010, 1), frequency = 12)
      hw <- HoltWinters(ts_data)
      predicted <- predict(hw, n.ahead = 6, prediction.interval = FALSE)
      predicted <- list(as.numeric(predicted))
      
      empty_vector <- c(empty_vector, predicted)
    }
    
    df <- data.frame(matrix(unlist(empty_vector), nrow=length(empty_vector), byrow=T),stringsAsFactors=FALSE)
    df <- df[,past_period]
    df <- data.frame(city = cities, score = df, stringsAsFactors=FALSE)
    plot_data <- left_join(df, unique(analysis_data[,c('city', 'lon', 'lat')]), by = 'city')
    
    max_value = max(plot_data$score)
    plot_data$scaled <- plot_data$score / max_value
    
    leaflet(data = plot_data) %>% 
      addTiles() %>%
      addCircleMarkers(~lon, ~lat,
                       # color = ~color_pal_time(time_interval),
                       radius = ~scaled*25,
                       popup = ~paste(city, " - ", score, "cases"))
  })
  
  
})