library(tidyverse)
library(shiny)
library(DT)
library(dplyr)
library(usmap)
library(maps)
library(leaflet)
library(rsconnect)
library(bslib)
theme_set(theme_classic())

airlines = read_csv("https://uwmadison.box.com/shared/static/24nrm1vrwz8bf3cmr0tzciwx3n2wiik1.csv")

# preprocess data - change days, reencode airline codes, change times
airlines = airlines %>%
  mutate(Delayed = sapply(Class, function(x){if (x == 0) return("No") else return("Yes")})) %>%
  mutate(DayOfWeek = sapply(DayOfWeek, function(x){
    if(x==1){
      return("Monday")
    }
    else if (x==2){
      return("Tuesday")
    }
    else if (x==3){
      return("Wednesday")
    }
    else if (x==4){
      return("Thursday")
    }
    else if (x==5){
      return("Friday")
    }
    else if (x==6){
      return("Saturday")
    }
    else if (x==7){
      return("Sunday")
    }
  })) %>%
  mutate(DayOfWeek = factor(DayOfWeek, levels = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"))) %>%
  mutate(Airline = recode(Airline,
                          "DL" = "Delta Airlines",
                          "AA" = "American Airlines",
                          "OO" = "SkyWest Airlines",
                          "B6" = "JetBlue Airways",
                          "US" = "US Airways",
                          "FL" = "Airtran Airways",
                          "WN" = "Southwest Airlines",
                          "CO" = "Continental Airlines",
                          "YV" = "Mesa Airlines",
                          "EV" = "Atlantic Southeast Airlines",
                          "XE" = "ExpressJet Airlines",
                          "9E" = "Endeavor Air",
                          "OH" = "PSA Airlines",
                          "UA" = "United Airlines",
                          "MQ" = "Envoy Air",
                          "AS" = "Alaska Airlines",
                          "F9" = "Frontier Airlines",
                          "HA" = "Hawaiian Airlines"
  )) %>%
  mutate(Hours = Time %/% 60) %>%
  mutate(Minutes = Time %% 60) %>%
  mutate(Minutes = sapply(Minutes, function(x){if(x<10) return(paste(0,x,sep="")) else return(x)})) %>%
  mutate(Time = paste(Hours, Minutes, sep = ":")) %>%
  select(-Flight, -Class, -Hours, -Minutes) 

# original source: https://ais-faa.opendata.arcgis.com/maps/e747ab91a11045e8b3f8a3efd093d3b5
federal = read_csv("https://uwmadison.box.com/shared/static/fiqrtcxd9khjw49h46fx013e652ki2f0.csv")

# because there are some discrepancies in the codes between the two data sets, adjust
# to align the federal data set's codes with the airlines data set's codes
replace_airport_names <- function() {
  new_airport_names = c(
    "GPI"="FCA",
    "SAW"="MQT",
    "NYL"="YUM",
    "CRQ"="CLD",
    "BBG"="BKG",
    "UNV"="SCE",
    "UTA"="UTM"
  )
  
  for (old_code in names(new_airport_names)){
    federal[which(federal$IDENT == old_code),] = federal %>%  
      filter(IDENT == old_code) %>% 
      mutate(IDENT = new_airport_names[old_code])
  }
  federal
}

federal = replace_airport_names()

federal = federal %>% 
  filter((IDENT %in% airlines$AirportFrom) | (IDENT %in% airlines$AirportTo))

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

# create new dataframe from original data plus the federal names and coordinates
joined_data <- airlines %>% 
  left_join(federal, by = c("AirportFrom" = "IDENT")) %>%
  left_join(federal, by = c("AirportTo" = "IDENT")) %>%
  select(Time, Length, Airline, AirportFrom, AirportTo, DayOfWeek, Delayed, NAME.x, LATITUDE.x, LONGITUDE.x, NAME.y, LATITUDE.y, LONGITUDE.y)

colnames(joined_data) <- c("TimeOfDeparture", "LengthOfFlight", "Airline", "AirportFrom", "AirportTo", "DayOfWeek", "Delayed", "AirportNameFrom", "LatitudeFrom", "LongitudeFrom", "AirportNameTo", "LatitudeTo", "LongitudeTo")

# create a tidy dataframe with airport, day of the week, and number of flights for columns
airport_popularity = joined_data %>% 
  select(AirportNameFrom, AirportNameTo, DayOfWeek) %>% 
  pivot_longer(c(AirportNameFrom, AirportNameTo), names_to = "_", values_to = 'Airport') %>% 
  select(Airport, DayOfWeek) %>% 
  group_by(Airport, DayOfWeek) %>%
  summarise(n = n()) %>% 
  arrange(-n)



###### SHINY STARTS HERE
ui_tweaks <- 
  list(tags$head(
    tags$style(
      HTML("
        .multicol { 
          -webkit-column-count: 2; /* Chrome, Safari, Opera */
          -moz-column-count: 2;    /* Firefox */ 
          column-count: 2; 
          -moz-column-fill: auto;
          -column-fill: auto;
        }
        .container-fluid > :nth-child(5) > .col-sm-4 > .well{
          height:400px;
          border-radius:10px;
          border-color:black;
        }
        #DataTables_Table_0_filter input:hover{
          border-color: #8042eb;
        }
        table.dataTable.table-hover > tbody > tr:hover > *{
          box-shadow: inset 0 0 0 9999px rgba(128, 66, 235, 0.075) !important;
        }
        .table.dataTable tbody td.active, .table.dataTable tbody tr.active > td{
          background-color: initial !important;
          color: initial !important;
        }
        hr{
          color: #8c8c8c; 
        }
        .col-sm-6:nth-child(1){
          padding-right:20px;
          border-right: 2px solid rgb(140,140,140,.25);
        }
        .col-sm-6:nth-child(2){
          padding-left:20px;
        }
        .shiny-input-container .checkbox-inline input:hover{
            border-color: #c0a1f5;
            outline: 0;
            box-shadow: 0 0 0 .25rem rgba(128,66,235,0.25);
        }
        .selectize-input > .item:hover{
            border-color: #c0a1f5;
            outline: 0;
           box-shadow: 0 0 0 .2rem rgba(128,66,235,0.5);
        }
        .checkbox-inline{
          font-size:12px;
          margin-right:.5rem !important;
        }"
      ))))

### Defines the app, encodes where each plot and each user interface renders and their properties
ui <- fluidPage(
  theme = bs_theme(
    bootswatch = "simplex",
    fg = "#261912",
    bg = "#fafafa",
    primary = "#8042eb",
    base_font = font_google("Source Sans Pro"),
  ),
  ui_tweaks,
  titlePanel(
    h1(strong(tags$u("US Airplane Delays Analysis Dashboard")),
       align = "center"),
    windowTitle = "Airplane Delays Analysis Dashboard"
  ),
  tags$br(),
  fluidRow(
    titlePanel(h2(strong("Origin/Destination Airports of Delayed Flights"), align = "center"), ),
  ),
  sidebarLayout(
    sidebarPanel(
      tags$div(
        id = "userInfo",
        tags$p("This dashboard is intended so that you may find which airlines and/or which airports tend to have the most flight delays."),
        tags$p("To use this application,"),
        tags$ul(
          tags$li("You can examine airports on the map to the right, where each airport's point size represents the log of the total flights."),
          tags$li("You may also examine flights by certain airline(s) with the plot on the left below, where more details about the selected airline(s) will be displayed in the data table underneath."),
          tags$li("Another option is to compare flight traffic for certain airports with the plot on the right below.")
        ),
        radioButtons('airport_category',"Switch between airport types in the map:", choiceNames = c('Origin Airports', 'Destination Airports'), choiceValues = c('AirportNameFrom','AirportNameTo'), selected = 'AirportNameFrom')
      ),
    ),
    mainPanel(
      leafletOutput("map")
    )
  ),
  tags$hr(),
  fluidRow(
    column(6, 
           fluidRow(
             h2(strong("Distribution of Flight Lengths"), align = "center"),
             checkboxGroupInput("airlines", "Airlines: ", sort(unique(airlines$Airline)), "American Airlines", inline = TRUE)
           ),
           plotOutput("density_plot")
    ),
    column(6,
           fluidRow(
             h2(strong("Flight Traffic per Airport"), align = "center"),
             selectizeInput("airport_names", "Select multiple airport Names:", 
                            sort(unique(airport_popularity$Airport)), 
                            multiple = T, 
                            options = list(placeholder = "Select values to plot"), 
                            selected=c("Newark Liberty Intl", "John F Kennedy Intl"),
                            width = "100%")
           ),
           plotOutput("airport_popularity_line_plot")
    )
  ),
  tags$hr(),
  fluidRow(
    dataTableOutput("dt"),
  ),
)

# this function defines the proportion of delayed flights for all airports filtered by departure/destination distinction
get_merged_df <- function(airport_category) {
  
  if(airport_category=='AirportNameFrom') {
    delay <- joined_data %>% 
      group_by(AirportNameFrom) %>% 
      summarize(delay_count = sum(Delayed == "Yes"), total = n()) %>% 
      mutate(proportion_delayed = delay_count / total) %>% 
      ungroup()
    coords <- joined_data %>% 
      select(AirportNameFrom, LatitudeFrom, LongitudeFrom) %>% 
      distinct()
    merge <- left_join(delay, coords, by = 'AirportNameFrom')
  }
  else  {
    delay <- joined_data %>% 
      group_by(AirportNameTo) %>% 
      summarize(delay_count = sum(Delayed == "Yes"), total = n()) %>% 
      mutate(proportion_delayed = delay_count / total) %>% 
      ungroup()
    coords <- joined_data %>% 
      select(AirportNameTo, LatitudeTo, LongitudeTo) %>% 
      distinct()
    merge <- left_join(delay, coords, by = 'AirportNameTo')
  }
  
  merge
}

# server that renders all of the plots
server <- function(input, output, session) {
  airport_category <- reactive({input$airport_category})
  
  # Render map with standardized legend with color of points reflecting the proportions of delays for each airport whether they are a destination or departure airport
  output$map <- renderLeaflet({
    merge <- get_merged_df(airport_category())
    pal <- colorBin(palette = "Spectral", domain = merge$proportion_delayed, 
                    reverse = TRUE, bins=seq(0.0,1.0,0.1))
    
    # Merging the information to display on the popup
    if (airport_category()=="AirportNameFrom"){
      latitude_col <- merge$LatitudeFrom
      longitude_col <- merge$LongitudeFrom
      airport_col <- merge$AirportNameFrom
    }
    else  {
      latitude_col <- merge$LatitudeTo
      longitude_col <- merge$LongitudeTo
      airport_col <- merge$AirportNameTo
    }
    
    leaflet(merge) %>% 
      setView(lng = -90, lat = 38, zoom = 4) %>%
      addTiles() %>% 
      addCircleMarkers(
        ~longitude_col,
        ~latitude_col,
        popup = paste("Airport: ", airport_col, "<br>", 
                      "Total Flights: ", merge$total, "<br>",
                      "Delayed Flights: ", merge$delay_count),
        radius = log(merge$total) * 1.3,
        color = ~pal(merge$proportion_delayed),
        fillOpacity = 0.8,
      ) %>%
      addLegend(
        position = "bottomright",
        pal = pal,
        values = ~proportion_delayed,
        title = "Proportion of delayed flights",
        opacity = 1
      )
  })
  
  # creates a subsample of the data based on selected airline for the density plot
  airline_sub <- reactive({
    airlines %>%
      mutate(
        selected = (Airline %in% input$airlines)
      ) %>%
      filter(selected == TRUE)
  })
  # density plot output
  output$density_plot = renderPlot({
    ggplot(slice_sample(airline_sub(), prop = .25)) +
      geom_density(aes(Length, ..density.., fill=Delayed), alpha = .3) +
      labs(x = "Flight Length", y = "Density", fill = "Late?") +
      facet_wrap(~DayOfWeek, ncol=2) +
      scale_fill_manual(values = c("#16b7cc", "#fc1303")) +
      theme(
        panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "transparent"),
        plot.background = element_rect(fill = "transparent", color = NA),
        legend.background = element_rect(fill = "transparent"),
        panel.border = element_rect(fill = NA, color = "#0c0c0c", linewidth = 0.6),
        axis.text = element_text(size = 14),
        axis.title = element_text(size = 16)
      )
  }, bg = "transparent")
  
  # render data table, with specific column names
  output$dt <- DT::renderDataTable(
    {
      airline_sub() %>%
        select(-selected)
    },
    rownames = FALSE,
    colnames = c("Time", "Length (Min)", "Airline", "Departing from", "Arriving to", "Day of the week", "Late?")
  )
  
  airport_names <- reactive({ input$airport_names})
  
  # renders line plot based on airport names in selectizeInput
  output$airport_popularity_line_plot <- renderPlot({
    airport_popularity %>%
      filter(Airport %in% airport_names()) %>%
      ggplot() +
      geom_line(aes(as.numeric(DayOfWeek), n, col=Airport), linewidth=1.2) +
      scale_x_discrete(limits=seq(1,7,1),labels = c("Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun")) + 
      labs(x = "Day of Week", y="Number of flights") +
      theme(
        panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "transparent"),
        plot.background = element_rect(fill = "transparent", color = NA),
        legend.background = element_rect(fill = "transparent"),
        panel.border = element_rect(fill = NA, color = "#0c0c0c", linewidth = 0.6),
        axis.text = element_text(size = 14),
        axis.title = element_text(size = 16),
        text = element_text(size=15)
      )
  }, bg="transparent")
}

shinyApp(ui, server)
