# Code Andreas - Dashboard

shinyUI(dashboardPage(# skin = "purple",
  
  dashboardHeader(title = "Dengue Dashboard"),
  dashboardSidebar(disable = TRUE),
  dashboardBody(
    
    # Boxes need to be put in a row (or column)
    
    fluidPage(
      # h1("Highcharter Demo"),
      fluidRow(
        box(width = 4, 
            selectInput("select_city", label = h3("Select City"), 
                        choices = c('ALL', cities), selected = 'Colombo'),
            selectInput("select_ts", label = h3("Display timeseries according to"), 
                        choices = c('Absolute Numbers', 'Relative Numbers'), selected = 'Absolute Numbers')
        ),
        
        box(width = 8,
            # highchartOutput("highchart_ts",height = "500px")
            dygraphOutput("dygraph_ts",height = "350px")
        )
      )
      ,
      fluidRow(
        box(width = 2, 
            selectInput("select_map", label = h4("Color map according to"), 
                        choices = c('Absolute Numbers', 'Relative Numbers'), selected = 'Relative Numbers'),
            selectInput("select_day", h4("Select Day"), substr(unique(analysis_data$DATE), 1, 10), 
                        selected = substr(unique(analysis_data$DATE), 1, 10)[1])
        ),
        box(width = 5, leafletOutput("leaflet_map"))
        , box(width = 5, highchartOutput("highchart_map"))
      ),
      fluidRow(box(selectInput("select_district", h4("Select District to predict"), cities, 
                               selected = 'Colombo'),
                   selectInput("past_period", h4("Select period to predict"), c(1:5), 
                               selected = 1)
      )),
      fluidRow(
        box(width = 6, 
            leafletOutput("leaflet_map_II")),
        box(width = 6, 
            dygraphOutput("dygraph_prediction",height = "400px")
        )
      )
    )
    
  )
  
))
