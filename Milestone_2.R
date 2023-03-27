library(tidyverse)
library(shiny)
library(DT)
library(dplyr)
library(usmap)
library(maps)
library(leaflet)
library(rsconnect)
theme_set(theme_classic())

airlines = read_csv("https://uwmadison.box.com/shared/static/24nrm1vrwz8bf3cmr0tzciwx3n2wiik1.csv")

airlines = airlines %>%
  mutate(Class = as.factor(Class))

# original source: https://ais-faa.opendata.arcgis.com/maps/e747ab91a11045e8b3f8a3efd093d3b5
federal = read_csv("https://uwmadison.box.com/shared/static/fiqrtcxd9khjw49h46fx013e652ki2f0.csv")

# because there are some discrepancies in the codes between the two datasets, adjust
# to align the federal dataset's codes with the airlines dataset's codes

replace_airport_code <- function(code_to_replace, replacement_code) {
  federal %>% 
    filter(IDENT == code_to_replace) %>% 
    mutate(IDENT = replacement_code)
}

federal[which(federal$IDENT == "GPI"),] = replace_airport_code("GPI","FCA")
federal[which(federal$IDENT == "SAW"),] = replace_airport_code("SAW","MQT")
federal[which(federal$IDENT == "NYL"),] = replace_airport_code("NYL","YUM")
federal[which(federal$IDENT == "CRQ"),] = replace_airport_code("CRQ","CLD")
federal[which(federal$IDENT == "BBG"),] = replace_airport_code("BBG","BKG")
federal[which(federal$IDENT == "UNV"),] = replace_airport_code("UNV","SCE")
federal[which(federal$IDENT == "UTA"),] = replace_airport_code("UTA","UTM")

federal = federal %>%
  filter((IDENT %in% airlines$AirportFrom) | (IDENT %in% airlines$AirportTo))

# check to make sure that all airlines in airlines dataset are in the federal dataset
# unique(airlines$AirportFrom[which(!(airlines$AirportFrom %in% federal$IDENT))])
# unique(airlines$AirportTo[which(!(airlines$AirportTo %in% federal$IDENT))])

# this block of code converts the character longitude and latitude to doubles for use with sf
federal$LATITUDE = str_replace(federal$LATITUDE, "N", "")
federal$LONGITUDE = str_replace(federal$LONGITUDE, "W", "")

reformatted_location_column <- function(col){
  temp_col = numeric(length({{col}}))
  
  splitted_col = str_split({{col}}, "-")
  
  for (i in 1:length(splitted_col)){
    d = as.double(splitted_col[[i]][1])
    m = as.double(splitted_col[[i]][2])
    s = as.double(splitted_col[[i]][3])
    
    temp_col[i] = d + m/60 + s/3600
  }
  temp_col
}

federal = federal %>%
  mutate(LATITUDE  = reformatted_location_column(federal$LATITUDE), 
         LONGITUDE = -1*reformatted_location_column(federal$LONGITUDE))

joined_data <- airlines %>% 
  left_join(federal, by = c("AirportFrom" = "IDENT")) %>%
  left_join(federal, by = c("AirportTo" = "IDENT")) %>%
  select(Flight, Time, Length, Airline, AirportFrom, AirportTo, DayOfWeek, Class, NAME.x, LATITUDE.x, LONGITUDE.x, NAME.y, LATITUDE.y, LONGITUDE.y)

colnames(joined_data) <- c("FlightID", "TimeOfDeparture", "LengthOfFlight", "Airline", "AirportFrom", "AirportTo", "DayOfWeek", "Class", "AirportNameFrom", "LatitudeFrom", "LongitudeFrom", "AirportNameTo", "LatitudeTo", "LongitudeTo")

airport_popularity = airlines %>% # head(1000) %>% 
  select(AirportFrom, AirportTo, DayOfWeek) %>% 
  pivot_longer(c(AirportFrom, AirportTo), names_to = "_", values_to = 'Airport') %>% 
  select(Airport, DayOfWeek) %>% 
  group_by(Airport, DayOfWeek) %>%
  summarise(n = n()) %>% 
  arrange(-n)

initial_code_values = unique(
  airport_popularity %>% 
    filter(str_detect(Airport, paste("^O",sep=""))) %>% 
    select(Airport) %>% arrange(Airport)
)

ui_tweaks <-  list(tags$head(
  tags$style(
  HTML("
      .multicol { 
        -webkit-column-count: 2; /* Chrome, Safari, Opera */
        -moz-column-count: 2;    /* Firefox */ 
        column-count: 2; 
        -moz-column-fill: auto;
        -column-fill: auto;
      }"
  ))))

ui <- fluidPage(ui_tweaks,
                titlePanel(
                  h1(strong("Airplane Delays Analysis Dashboard"),
                     align = "center"),
                  windowTitle = "Airplane Delays Analysis Dashboard"
                ),
                tags$br(),
                fluidRow(
                  titlePanel( h2(strong("Flights Delayed by Departure Airports"), align = "center"), ),
                  h5("(Click on the points for more information. Zoom into the plot for accurate locations)", align='center')
                ),
                fluidRow(
                  column(10, leafletOutput("map"), offset = 1)
                ),
                tags$hr(),
                fluidRow(
                  titlePanel( h2(strong("Distribution of Flight Lengths"), align = "center"), ),
                ),
                fluidRow(
                  column(2, 
                         tags$div(align = 'left', class = 'multicol', 
                                  checkboxGroupInput("airlines", "Airlines: ", sort(unique(airlines$Airline)), "AA", inline = FALSE)
                         )
                  ),
                  # checkboxGroupInput("Airlines", "Airlines", sort(unique(airlines$Airline)), "AA", inline = TRUE) ),
                  #plotOutput("histogram"),
                  column(10, plotOutput("scatterplot")),
                ),
                fluidRow(
                  dataTableOutput("dt"),
                ),
                tags$hr(),
                fluidRow( h2(strong("Flight-traffic per airport"), align = "center") ),
                sidebarLayout(
                  sidebarPanel (
                    fluidRow(
                      radioButtons("filter_letter","Airport code starts with",LETTERS, "O", inline = T),
                      selectizeInput("airport_codes", "Airport Codes", 
                                     NULL, 
                                     multiple = T, 
                                     options = list(placeholder = "Select values to plot"))
                    )
                  ),
                  mainPanel(plotOutput("airport_popularity_line_plot"))
                )
)

server <- function(input, output, session) {
  delay <- joined_data %>% 
    group_by(AirportNameFrom) %>% 
    summarize(delay_count = sum(Class == 1), total = n()) %>% 
    ungroup()
  #delay
  
  # Add latitude and longitude coordinates for each airport
  coords <- joined_data %>% 
    select(AirportNameFrom, LatitudeFrom, LongitudeFrom) %>% 
    distinct()
  
  # Merge data frames
  merge <- left_join(delay, coords, by = "AirportNameFrom")
  # Render map
  output$map <- renderLeaflet({
    leaflet(merge) %>% 
      setView(lng = -102.7129, lat = 41.0902, zoom = 3) %>%
      addTiles() %>% 
      addCircleMarkers(
        ~LongitudeFrom,
        ~LatitudeFrom,
        popup = paste("Airport: ", merge$AirportNameFrom, "<br>", 
                      "Departed Flights: ", merge$total, "<br>",
                      "Delayed Flights: ", merge$delay_count),
        radius = 3,
        color = "#7954a1",
        fillOpacity = 0.7,
      )
  })
  
  airline_sub <- reactive({
    airlines %>%
      mutate(
        selected = (Airline %in% input$airlines)
      ) %>%
      filter(selected == TRUE)
  })
  
  # output$histogram <- renderPlot(histogram_fun(airline_sub()))
  output$scatterplot = renderPlot({
    ggplot(airline_sub()) +
      geom_jitter(aes(Length, DayOfWeek, col=Class), alpha = .5, height = .25) +
      scale_y_discrete(limits=seq(1,7,1)) +
      labs(x = "Flight Length", y = "Day of Week")
  })
  
  output$dt <- DT::renderDataTable(
    airline_sub(),
    colnames = c("Flight #", "Time", "Length (Min)", "Airline key", "Departing from", "Arriving to", "Day of the week", "Late?", "selected")
  )
  
  filter_letter <- reactive({input$filter_letter})
  airport_codes <- reactive({ input$airport_codes})
  
  observeEvent(input$filter_letter, {
    codes = airport_popularity %>% 
      filter(str_detect(Airport, paste("^",filter_letter(),sep=""))) %>% 
      select(Airport) %>% arrange(Airport)
    
    updateSelectInput(session, "airport_codes", 
                      choices = unique(codes),
                      selected = codes[1,1])
  })
  
  output$airport_popularity_line_plot <- renderPlot(
    airport_popularity %>%
      filter(Airport %in% airport_codes()) %>%
      ggplot() +
      geom_line(aes(DayOfWeek, n, col=Airport)) +
      scale_x_discrete(limits=seq(1,7,1)) + 
      labs(x = "Day of Week", y="Number of flights") +
      ggtitle("Flight Traffic per Airport") +
      theme(plot.title = element_text(hjust = 0.5))
  )
}

shinyApp(ui, server)