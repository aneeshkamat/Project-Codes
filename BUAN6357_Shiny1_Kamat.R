### Assignment 1 - Shiny Apps
### Author - Aneesh Anand Kamat (axk170065)

# Please uncomment the 'install_github' (LOC #15), if at first it shows the error: 
# "Could not find "register_google" function.
# This function is present in the modified ggmap function, found on github, developed by 'dkhale'.
# You need to load ggmap again after installing the github version.
# Internet has to be working since ggmap takes the map from the internet. 
# Allow around 5 seconds for map to load in the second plot.


# Load packages
library(shiny)
library(tidyverse)
library(devtools)
#devtools::install_github("dkahle/ggmap")
library(ggmap)
library(DT)


# Load data
tr_col <- read.csv("traffic-collision-data-from-2010-to-present.csv", header = T)

# Subset Data
tr_col$Year <- substr(tr_col$Date.Occurred,1,4)
tr_col_sub <- tr_col[tr_col$Year=='2018',-25]

# Prepare data for Plot 1
tr_col_sub$hour <- as.integer(substr(tr_col_sub$Time.Occurred, 1, nchar(tr_col_sub$Time.Occurred)-2)) + 1
tr_col_sub$hour[is.na(tr_col_sub$hour)] <- 1


# Prepare data for Plot 2
tr_col_sub$lat <- substr(tr_col_sub$Location,15,21)
tr_col_sub$lat <- parse_number(tr_col_sub$lat)
tr_col_sub$lon <- substr(tr_col_sub$Location,30,50)
tr_col_sub$lon <- parse_number(tr_col_sub$lon)
tr_col_sub <- tr_col_sub[tr_col_sub$lat != 0.0,]
register_google(key = "AIzaSyA8BE1gI1BKAUdzanjNAmOfsvoDRPwjjek")
map_LA <- geocode("Los Angeles")
LAMap <- get_map(location = c(map_LA$lon, map_LA$lat), zoom = 10, maptype = 'roadmap')


# Prepare data for plot 3
hm_data <- tr_col_sub %>%
    group_by(Area.Name, hour) %>%
    summarise(
        Accidents = length(DR.Number)
    )

# Define UI for application that plots all the plots in diferent tabs
ui <- fluidPage( 
    mainPanel( 
        uiOutput("tb")
        )
)

# Define server function required to create the plots
server <- function(input, output) {
    
    output$barchart <- renderPlot({
        ggplot(tr_col_sub) +
            geom_bar(aes(x=tr_col_sub$hour), fill = "mediumvioletred") +
            scale_x_continuous(breaks=seq(1, 24, 1)) +
            labs(title = "Number of Accidents Each Hour",
                 x = "Hour of Day",
                 y = "Number of Accidents")
    })
    
    output$p1 <- renderText({
        paste0("With 3954 accidents, the most accidents have occured at around the 6pm rush hour.")
    })
    
    output$table <- DT::renderDataTable({
        tr_col_sub %>% 
            group_by(hour) %>% 
            summarise(
                Accidents = length(DR.Number)) %>% 
            select(hour, Accidents)
    })

    output$mapplot <- renderPlot({
        ggmap(LAMap) + 
            geom_point(data = tr_col_sub, aes(x = lon, y = lat), 
                       size = .25, color = "red", alpha = 0.3) + 
            labs(title = "Location of Accidents",
                 x = "Longitude",
                 y = "Latitude")
            
    })
        
    output$heatchart <- renderPlot({
        ggplot(hm_data, aes(hour, Area.Name)) +
            geom_tile(aes(fill = Accidents), color = "white")+
            theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
            scale_fill_gradient(low="green",
                                high="red",
                                breaks = seq(0,max(hm_data$Accidents),50)) +
            scale_x_continuous(breaks=seq(1, 24, 1)) +
            labs(title = "Number of Accidents in each area (Hourly)",
                 fill = "Number of Accidents",
                 x = "Hour of Day",
                 y = "Area Name")
    })
    
    output$p3 <- renderText({
        paste0("The most accidents (277) have occured in the 77th Street Area at around 6pm.")
    })
    
    output$table1 <- DT::renderDataTable({
        hm_data
    })

    output$tb <- renderUI({
        tabsetPanel(tabPanel("Tab 1",
                             plotOutput("barchart"),
                             verbatimTextOutput("p1"),
                             dataTableOutput("table")),
                    tabPanel("Tab 2",
                             plotOutput("mapplot")),
                    tabPanel("Tab 3",
                             plotOutput("heatchart"),
                             verbatimTextOutput("p3"),
                             dataTableOutput("table1")) 
                    )
    })

}
# Create a Shiny app object
shinyApp(ui = ui, server = server)
