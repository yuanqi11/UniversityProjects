# Imports
library('ggplot2')
library('dplyr')
library('magrittr')
library('glue')
library('vroom')
library('lubridate')
library('ggrepel')
library('maps')
library('mapproj')
library('shiny')
library('imager')
library('magick')

# Initial Load of Data ----
sites <- vroom('./Data/Prepared Data Files For App/Sites.csv')
file_names <- dir(path = './Data/Prepared Data Files For App/Inidividual Sites/')
# Make sure to only load csv's to ensure local/cloud compatibility
file_names <- file_names[grepl('.csv', file_names)]
setwd('./Data/Prepared Data Files For App/Inidividual Sites/')
# Put all csvs into one single dataframe
all_data <- do.call(rbind,lapply(file_names, read.csv))
setwd('../../../')

# Transform ob_time into datetime
all_data$ob_time <- lubridate::as_datetime(all_data$ob_time)

# Transform site id into character data type
all_data$Site <- as.character(all_data$Site)
sites$Site_ID <- as.character(sites$Site_ID)

shinyServer(function(input, output, session){
  
  # Handle Impossible Input Combinations ----
  
  observeEvent(input$aggregation, {
    
    # If the user selects a daily aggregation (average, min, max)...
    if (input$aggregation %in% c('davg_agg',
                                 'dmax_agg',
                                 'dmin_agg')){
      #... then the x_axis can only be calendar time or a day within the week
      updateSelectizeInput(session,
                           'x_axis',
                           'Choose the type of X-Axis',
                           choices = c('Calendar Time' = 'cal_time',
                                       'Day Within the Week' = 'dayw_time'),
                           options = list(maxItems = 1))
    } 
    # If the user selects a monthly aggregation...
    else if (input$aggregation == 'mavg_agg'){
      
      #...then the x-axis an only be calendar time
      updateSelectizeInput(session,
                           'x_axis',
                           'Choose the type of X-Axis',
                           choices = c('Calendar Time' = 'cal_time'),
                           options = list(maxItems = 1))
      
    } 
    
    # In all other cases...
    else {
      
      # ...display all option for the x-axis
      updateSelectizeInput(session,
                           'x_axis',
                           'Choose the type of X-Axis',
                           choices = c('Calendar Time' = 'cal_time',
                                       'Hour Within the Week' = 'hourw_time',
                                       'Day Within the Week' = 'dayw_time',
                                       'Hour in the Day' = 'hourd_time'),
                           options = list(maxItems = 1))
    }
  })
  
  # REACTIVE ----
  
  select_dataset <- reactive({
    
    # Filter the desired sites
    selected_data <- all_data %>%
      filter(Site %in% input$station)  
    
    # Perform Selected Aggregation ----
    
    # Aggregate daily averages
    if (input$aggregation == 'davg_agg'){
      agg_data <- selected_data %>% 
        group_by(month, day, Site) %>% 
        summarise(ob_time = first(ob_time),
                  wind_speed = mean(wind_speed),
                  air_temperature = mean(air_temperature),
                  rltv_hum = mean(rltv_hum),
                  visibility = mean(visibility), 
                  Site = first(Site))
    
    # Aggregate monthly averages
    } else if (input$aggregation == 'mavg_agg'){
      agg_data <- selected_data %>% 
        group_by(month, Site) %>% 
        summarise(ob_time = first(ob_time),
                  wind_speed = mean(wind_speed),
                  air_temperature = mean(air_temperature),
                  rltv_hum = mean(rltv_hum),
                  visibility = mean(visibility), 
                  Site = first(Site))
      
    # Aggregate daily maxima  
    } else if (input$aggregation == 'dmax_agg'){
      agg_data <- selected_data %>% 
        group_by(month, day, Site) %>% 
        summarise(ob_time = first(ob_time),
                  wind_speed = max(wind_speed),
                  air_temperature = max(air_temperature),
                  rltv_hum = max(rltv_hum),
                  visibility = max(visibility), 
                  Site = first(Site))
      
    # Aggregate daily minima    
    } else if (input$aggregation == 'dmin_agg'){
      agg_data <- selected_data %>% 
        group_by(month, day, Site) %>% 
        summarise(ob_time = first(ob_time),
                  wind_speed = min(wind_speed),
                  air_temperature = min(air_temperature),
                  rltv_hum = min(rltv_hum),
                  visibility = min(visibility), 
                  Site = first(Site))
    
    # No aggregation
    } else {agg_data <- selected_data}
    
    # Handling of x-axis ----
    
    # Calculate the hour within the week
    if (input$x_axis == 'hourw_time'){
      
      final_data <- agg_data %>%
        # Calculate the weekday for each entry...
        mutate(weekday = wday(ob_time,
                              week_start = getOption('lubridate.week.start', 1)),
               #... then calculate the hour within this week
               ob_time = (weekday - 1) * 24 + hour)
    
      # Calculate the day of the week
    } else if (input$x_axis == 'dayw_time'){
      
      final_data <- agg_data %>%
        mutate(ob_time = wday(ob_time,
                              label = TRUE,
                              week_start = getOption('lubridate.week.start', 1)))
      
      # Calculate the hour of the day
    } else if (input$x_axis == 'hourd_time'){
      
      final_data <- agg_data %>%
        mutate(ob_time = strftime(ob_time, format='%H'))
      
      # Take raw hourly data
    } else {final_data <- agg_data}
    
    return(final_data)
       
  })
  
  dataset <- select_dataset
  
  # MAIN PLOT ----
  
  output$main_plot <- renderPlot({
    
    # Inner join to get the name of the sites
    plot_data <- dataset() %>%
      inner_join(sites, by = c('Site' = 'Site_ID'))
    
    # Start plot...
    plot <- ggplot(data = plot_data) +          # Set data source
      aes_string(x = 'ob_time',                 # ob_time always on x-axis
                 y = input$y_axis,              # y-axis according to input
                 color = 'Site_Name') +         # Color always by site
      ggtitle(plot_title(input$aggregation,     # Set plot title
                         input$y_axis)) +
      xlab(x_label(input$x_axis)) +             # Set x-axis label
      ylab(y_label(input$y_axis)) +             # Set y-axis label
      theme_bw() +                              # Change the theme
      theme(legend.position = 'bottom', 
            legend.direction = 'horizontal',
            legend.key = element_blank(),
            legend.title = element_blank(),
            legend.text = element_text(size = 16),
            axis.title.x = element_text(size = 16),
            axis.title.y = element_text(size = 16),
            plot.title = element_text(size = 18))
    
    # ...decide between line and scatter plot according to project requirement
    if (input$x_axis == 'cal_time')
      plot <- plot + geom_line(size = 1.05, alpha = 0.7)
    
    else
      plot <- plot + geom_point(size = 3, alpha = 0.5)
    
    
    # Add image and note if no station is selected
    if(is.null(input$station) == TRUE){
      
      # Load image from web
      im <- imager::load.image('https://image.freepik.com/free-vector/weather-concept-illustration_114360-1234.jpg')

      # Display the image
      par(mai=c(0,0.0,0.2,5))
      plot <- plot(im,
                   axes = FALSE,
                   cex = 1.5)
      
      # Display the message
      title('Please Choose 1 to 5 Weather Stations', 
            line = -1,
            col.main = '#0bbba3', 
            cex.main = 2,
            font.main = 1)

    }
   
   plot
    
  })
    
  # TABLE UNDER MAIN PLOT ----
  
  output$table <- renderDataTable({
    
    table_data <- all_data %>%
      filter(Site %in% input$station) %>% 
      filter(ob_time > '2020-11-24') %>%                           # Filter the last 7 days
      inner_join(sites, by = c('Site' = 'Site_ID')) %>%            # Get names of sites
      mutate(ob_time = paste('2020', month, day, sep = '-')) %>%   # Construct date
      group_by(ob_time, Site_Name) %>%                             # For each site and date...
      summarise('Wind Speed' = mean(wind_speed),                   # ... get average of variables
                'Air Temperature' = mean(air_temperature),
                'Relative Humidity' = mean(rltv_hum),
                'Visibility' = mean(visibility)) %>% 
      rename('Date' = ob_time,                                     # Rename column for clarity
             'Site' = Site_Name)
    
    table_data
    
  })
  
  # MAP PLOT UNDER TABLE ----
  
  output$map_plot <- renderPlot({
    
    map_data <- dataset() %>%
      inner_join(sites,
                 by = c('Site' = 'Site_ID')) %>%      # Get long/lat for selected sites
      group_by(Site_Name) %>% 
      summarise(Latitude = first(Latitude),
                Longitude = first(Longitude))

    UK <- map_data(map = 'world', region = 'UK')      # Get spatial data for the UK
    map <- ggplot() + 
      geom_polygon(aes(x = UK$long,                   # Draw shape of the UK
                       y = UK$lat,
                       group = UK$group)) +
      coord_map() +
      theme(axis.line = element_blank(),              # Remove all chart elements
            axis.text.x = element_blank(),
            axis.text.y = element_blank(),
            axis.ticks = element_blank(),
            axis.title.x = element_blank(),
            axis.title.y = element_blank(),
            legend.position = 'none',
            panel.background = element_blank(),
            plot.title = element_text(size = 16,
                                      face = 'bold'),
            plot.margin = margin(0, 0, 0, -15, 'cm')) # Remove whitespace on left side of map
    
    if (is.null(input$station) == FALSE){             # If the user selects a station...
      
      map <- map +                                    # ... add location of the stations...
        geom_point(aes(map_data$Longitude,
                       map_data$Latitude,
                       color = map_data$Site_Name,    # ... with same color as main plot
                       fill = map_data$Site_Name,
                       size = 10),
                   shape = 20) +
        geom_label_repel(aes(map_data$Longitude,      # Add label to station's location
                            map_data$Latitude,
                            label = map_data$Site_Name,
                            color = map_data$Site_Name),
                         size = 5)
    }
    
    map
    
  })
  
  # HUTTON CRITERIA ----
  
  output$hutton_criteria <- renderTable({
    
    # For 'January' we don't have data to report the HC for up to 2 days...
    if (input$hutton_station == 1){
      
      hutton_table <- all_data %>%
        # Filter data only for selected month
        filter(month == input$hutton_month)
      
    }
    
    # ...for all other months we also need to consider the last days of the previous month
    else {
      
      hutton_table <- all_data %>%
        # Filter data only for selected month
        filter(month %in% c(input$hutton_month,
                            as.character(as.integer(input$hutton_month) - 1)))
  
    }
    
    hutton_table <- hutton_table %>%
      
      # Filter data only for selected station
      filter(Site == input$hutton_station) %>%
      # Include 'month' in group_by to get data for previous month
      group_by(month, day, hour) %>%
      summarise(min_temp = min(air_temperature), 
                rel_humd = rltv_hum) %>%  
      # Check if humidity if over 90% for each hour
      mutate(prop_humd = ifelse(rel_humd > 90, 1, 0)) %>%
      group_by(month, day) %>% 
      # Calculate the minimum temp. and hours with humid. over 90% per day
      summarise(min_temp = min(min_temp),
              rel_humd = sum(prop_humd)) %>% 
      ungroup() %>%
      # Check if both criteria for the Hutton Criteria are fulfilled
      mutate('Hutton' = ifelse((lag(min_temp, 2) >= 10) &     # Temperature last two days
                                 (lag(min_temp, 1) >= 10) &   # Temperature last day
                                 (lag(rel_humd, 2) >= 6) &    # Humidity last two days
                                 (lag(rel_humd, 1) >= 6),     # Humidity last day
                               'Yes',
                               'No')) %>% 
      rename('Humidity > 90% [h]' = rel_humd,
             'Minimum Temperature [C]' = min_temp,
             'Date' = day,
             'Hutton Criteria Fulfilled' = Hutton) %>% 
      # Only show days of the month the user has selected
      filter(month == input$hutton_month) %>% 
      select(-month)
    
    # Format the 'Date' column
    hutton_table$Date <- paste('2020',
                               input$hutton_month,
                               hutton_table$Date,
                               sep = '-')

    hutton_table
    
  })
  
  # Download Table ----
  
  output$download_table <- downloadHandler(
    filename = 'table_data.csv',
    content = function(file) {
      
      table_data <- all_data %>%
        filter(Site %in% input$station) %>% 
        filter(ob_time > '2020-11-24') %>%                   # Filter the last 7 days
        inner_join(sites, by = c('Site' = 'Site_ID')) %>%    # Get names of sites
        mutate(ob_time = paste('2020',
                               month,
                               day,
                               sep = '-')) %>%               # Construct date
        group_by(ob_time, Site_Name) %>%
        summarise('Wind Speed' = mean(wind_speed),
                  'Air Temperature' = mean(air_temperature),
                  'Relative Humidity' = mean(rltv_hum),
                  'Visibility' = mean(visibility)) %>% 
        rename('Date' = ob_time,
               'Site' = Site_Name)
      
      write.csv(table_data, file, row.names = FALSE)
    }
  )
  
  # Download Report ----
  
  output$download_report <- downloadHandler(
    
    filename = 'report.doc',
    content = function(file) {

      # Set up parameters to pass to Rmd document
      var <- list(station = input$station,
                  y_axis = input$y_axis,
                  aggregation = input$aggregation,
                  x_axis = input$x_axis)
      
      # Knit the document
      rmarkdown::render('report.Rmd', 
                        output_format = 'word_document',
                        output_file = file,
                        params = var
      )
    }
    
  )
  
  # Handle x and y label of plots ----
  
  # The following function formats the x-axis label
  x_label <- function(x_axis){
    
    if (x_axis == 'cal_time') return('Calendar Time')
    else if (x_axis == 'hourw_time') return('Hour of the Week')
    else if (x_axis == 'dayw_time') return('Weekday')
    else return('Hour of the Day')
    
  }
  
  # The following function formats the x-axis label
  y_label <- function(y_axis){
    
    if (y_axis == 'wind_speed') return('Wind Speed (Knots)')
    else if (y_axis == 'air_temperature') return('Air Temperature (Celsius)')
    else if (y_axis == 'rltv_hum') return('Relative Humidity (%)')
    else return('Visibility (m)')
    
  }
  
  # The following function formats the title
  plot_title <- function(aggregation, y_axis){
    
    if (aggregation == 'no_agg'){
      
      if (y_axis == 'wind_speed') return('Hourly Data for Wind Speed')
      else if (y_axis == 'air_temperature') return('Hourly Data for Air Temperature')
      else if (y_axis == 'rltv_hum') return('Hourly Data for Relative Humidity')
      else return('Hourly Data for Visibility')
      
    } else if (aggregation == 'davg_agg'){
      
      if (y_axis == 'wind_speed') return('Daily Average for Wind Speed')
      else if (y_axis == 'air_temperature') return('Daily Average for Air Temperature')
      else if (y_axis == 'rltv_hum') return('Daily Average for Relative Humidity')
      else return('Daily Average for Visibility')
      
    } else if (aggregation == 'mavg_agg'){
      
      if (y_axis == 'wind_speed') return('Monthly Average for Wind Speed')
      else if (y_axis == 'air_temperature') return('Monthly Average for Air Temperature')
      else if (y_axis == 'rltv_hum') return('Monthly Average for Relative Humidity')
      else return('Monthly Average for Visibility')
      
    } else if (aggregation == 'dmax_agg'){
      
      if (y_axis == 'wind_speed') return('Daily Maxima for Wind Speed')
      else if (y_axis == 'air_temperature') return('Daily Maxima for Air Temperature')
      else if (y_axis == 'rltv_hum') return('Daily Maxima for Relative Humidity')
      else return('Daily Maxima for Visibility')
      
    } else {
      
      if (y_axis == 'wind_speed') return('Daily Minima for Wind Speed')
      else if (y_axis == 'air_temperature') return('Daily Minima for Air Temperature')
      else if (y_axis == 'rltv_hum') return('Daily Minima for Relative Humidity')
      else return('Daily Minima for Visibility')
      
    }
    
  }
  
})
