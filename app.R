library(shiny)
library(leaflet)
library(tidyverse)
library(ggmap)
library(rsconnect)

data <- read_csv("alt_fuel_stations (Jun 14 2020).csv")

gas_station <- readxl::read_excel("200521 CALTRANS PRA Response_EV charging highway exit sign locations, station owners.xlsx", 
                                  sheet = "Highway Exist")

gas_station <- gas_station %>%
    select(-`Highway Exists`) %>%
    mutate(Type = 1)

Good_key <- "AIzaSyAPj9oRq3gB3BOjJrFO5sLoXZy4F25qCbU"

register_google(key = Good_key, account_type = "standard", day_limit = 100)


temp <- data %>%
    select(City, Latitude, Longitude) %>%
    mutate(Type = 0)



temp <- rbind(gas_station , temp)


getColor <- function(temp) {
    sapply(temp$Type, function(Type) {
        if(Type == 1) {
            "green"
        } else if(Type == 0) {
            "orange"
        } else {
            "red"
        } })
}



icons <- awesomeIcons(
    icon = 'ios-close',
    iconColor = 'black',
    library = 'ion',
    markerColor = getColor(temp)
)



cal <- data %>%
    group_by(City) %>%
    summarise(Count = n()) %>%
    arrange(desc(Count)) %>%
    slice(1:10)


cal2 <- data %>%
    group_by(`Groups With Access Code`) %>%
    summarise(Count = n())



ui <- fluidPage(
    titlePanel("Electric Vehicle Charging Stations"),
    titlePanel("Highway Exists in California"),
    
    
    
    mainPanel("",
              fluidRow(splitLayout(cellWidths = c("70%", "80%"), 
                                   plotOutput("distPlot"), 
                                   plotOutput("distPlot1"))
              ), 
              h3("Electrical Stations and Highway Exists Distribution in California")),
    
    

    
    
    leafletOutput("mymap", width = "100%", height = 500),
    
    p(strong("U.S. Department of Energy")),
    a(href = "https://afdc.energy.gov/fuels/electricity_locations.html#/find/nearest?fuel=ELEC&ev_levels=dc_fast&ev_levels=3", "link to Alternative Data Center"),
    
    p(
        strong("Level 3 equipment"), " commonly known as ", strong("DC fast charging. "),"Most Level 3 chargers provide an 80% charge in 30 minutes.  Cold weather can lengthen the time required to charge."
    ),
    
    p('Green icons represent highway exist', style = "color:green"),
    p('Orange icons represent charge points', style = "color:orange"),
    
    absolutePanel(
        top = 600,
        right = 14,
        selectInput('City', "California Cities", temp$City, selected = 'San Jose', multiple = T)
    )
    
    
)

server <- function(input, output, session) {
    
    
    output$mymap <- renderLeaflet({
        
        temp %>%
            filter(City == input$City)  %>%
            leaflet() %>%
            addTiles() %>%
            addAwesomeMarkers(~Longitude, ~Latitude,icon=icons)
    })
    
    output$distPlot <- renderPlot({ggplot(cal, aes(x = fct_reorder(City, Count, .desc=TRUE), y= Count, fill = City))  + geom_bar(stat = 'identity') + labs(x = 'City', y = '') + theme(legend.position = "none")   +  ggtitle("Number of charging points \n by city")+ geom_text(aes(label=Count),  vjust=-0.5)
    })
    
    output$distPlot1 <- renderPlot({ggplot(cal2, aes(x=fct_reorder(`Groups With Access Code`, Count), y= Count, fill =`Groups With Access Code` )) + geom_bar(stat = 'identity') + labs(x = '', y = '') + coord_flip() + theme(legend.position = "none")   +  ggtitle("Number of charging points \n by access type") + geom_text(aes(label=Count), hjust = -0.1)})
    
}

shinyApp(ui, server)
















