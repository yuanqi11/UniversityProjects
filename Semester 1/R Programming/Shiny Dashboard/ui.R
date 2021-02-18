# Imports
library('shiny')
library('vroom')
library('leaflet')
library('shinycssloaders')

# Prepare the data ----
sites <- vroom('./Data/Prepared Data Files For App/Sites.csv')
sites <- sites[order(sites$Site_Name),]
site_names <- sites$Site_Name
site_ids <- sites$Site_ID
site_ids <- setNames(site_ids, site_names)

# Define UI for app ----
ui <- fluidPage(
  
  # Control style/color of the app ----
  tags$head(
    tags$style(HTML(
      'hr{border-top: 2px solid #e6e6e6;}',            # Horizontal line formatting
      '.butt_table{background-color: #05a18e;
                   color: white}',                     # Table download formatting
      '.butt_table:hover {background-color: #088c7c;
                          color: white}',
       '.butt_report{background-color: #15c2ad;
                     color: white;}',                  # Report download formatting
       '.butt_report:hover {background-color: #0ead9a;
                            color: white}',
       '.h1{color: #296999;
            font-size: 44px}',                         # H1 title formatting
       '.h2{color: #296999;                            
            font-size: 22px;}',                        # H2 title formatting
       '.h3{color: #296999;
            font-size: 18px;
            font-weight: bold;}',                      # H3 title formatting
       '.h4{font-size: 14px;}'))                       # H4 title formatting  
  ),
  
  # App title ----
  titlePanel(h1('UK Weather Data',                     # Main app title
                align = 'center', class = 'h1')),      # Sub title
  titlePanel(h4('by Gabriel Berardi (2585814B)',
                align = 'center', class = 'h4')),
  
  # Add vertical space
  br(),
  
  # Sidebar with input and output definitions ----
  sidebarLayout(
    
    # Sidebar panel for inputs ----
    sidebarPanel(
      
      # Title for main inputs
      h2('Inputs for "Main" Tab', class = 'h3'),
      
      # Input: multi selection for the weather station ----
      selectizeInput('station',
                     'Choose the Weather Stations (max. 5)',
                     choices = site_ids,
                     multiple = TRUE,
                     options = list(maxItems = 5,
                                    'plugins' = list('remove_button'),
                                    'create' = TRUE,
                                    'persist' = FALSE,
                                     placeholder = 'Please Choose 1-5 Locations')),
      
      # Input: Select the weather variable to plot ----
      selectizeInput('y_axis',
                     'Choose Variable to Plot',
                     choices = c('Wind Speed' = 'wind_speed',
                                 'Air Temperature' = 'air_temperature',
                                 'Relative Humidity' = 'rltv_hum',
                                 'Visibility' = 'visibility'),
                     options = list(maxItems = 1)),
      
      # Input: Select the aggregation to plot ----
      selectizeInput('aggregation',
                     'Choose an Aggregation',
                     choices = c('Raw Hourly Data (No Aggregation)' = 'no_agg',
                                 'Daily Averages' = 'davg_agg',
                                 'Monthly Averages' = 'mavg_agg',
                                 'Daily Maxima' = 'dmax_agg',
                                 'Daily Minima' = 'dmin_agg'),
                     options = list(maxItems = 1)),
      
      # Input: Select the time handling ----
      selectizeInput('x_axis',
                     'Choose the Type of X-Axis',
                     choices = c('Calendar Time' = 'cal_time',
                                 'Hour Within the Week' = 'hourw_time',
                                 'Day Within the Week' = 'dayw_time',
                                 'Hour of the Day' = 'hourd_time'),
                     options = list(maxItems = 1)),
      
      # Horizontal Line ----
      hr(),
      
      # Title for main inputs
      h2('Inputs for "Hutton Criteria" Tab', class = 'h3'),
      
      # Input: Select the station for the Hutton Criteria ----
      selectizeInput('hutton_station',
                     'Hutton Criteria: Choose the Weather Stations',
                     choices = site_ids),
      
      # Input: Select the month for the Hutton Criteria ----
      selectizeInput('hutton_month',
                     'Hutton Criteria: Choose the Month',
                     choices = c('January' = 1,
                                 'February' = 2,
                                 'March' = 3,
                                 'April' = 4,
                                 'May' = 5,
                                 'June' = 6,
                                 'July' = 7,
                                 'August' = 8,
                                 'September' = 9,
                                 'October' = 10,
                                 'November' = 11)),
      
      # Vertical Spacing ----
      br(),
      
      # Download Button For Table Data ----
      downloadButton('download_table', 'Download Table Data', class = 'butt_table'),
      
      # Download Button For Report ----
      downloadButton('download_report', 'Download Report', class = 'butt_report'),
      
    ),
    
    # Main panel for displaying outputs ----
    mainPanel(
      
      # Output: Tabset with main plot, table and map ----
      tabsetPanel(type = 'tabs',
                  tabPanel('Main', 
                           br(),
                           h2('Plot For Selected Input', class = 'h2'),
                           br(),
                           plotOutput('main_plot') %>%
                             withSpinner(color='#0dc5c1'),
                           br(),
                           h2('Location of Weather Stations', class = 'h2'),
                           br(),
                           plotOutput('map_plot') %>%
                             withSpinner(color='#0dc5c1'),
                           br(),
                           h2('Data For Last Seven Days', class = 'h2'),
                           br(),
                           dataTableOutput('table') %>%
                             withSpinner(color='#0dc5c1'),
                           br()),

                  tabPanel('Hutton Criteria',
                           br(),
                           h2('Hutton Criteria For Selected Input', class = 'h2'),
                           br(),
                           tableOutput('hutton_criteria'))
      )
    )
  )
)
