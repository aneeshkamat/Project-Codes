### Assignment 2 - Shiny Apps
### Author - Aneesh Anand Kamat (axk170065)

# Install Packages only if not already installed
list.of.packages <- c("shiny","tidyverse","devtools","gtools",
                      "DT","tm","SnowballC","lsa","caret","ggwordcloud")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

# Load packages
library(shiny)
library(tidyverse)
library(devtools)
library(DT)
library(tm)
library(SnowballC)  
library(lsa)
library(caret)
library(gtools)
library(ggwordcloud)
options(digits = 0)


# Load data
bb_db <- read.csv("billboard_lyrics_1964-2015.csv", header = T)

# Add 'Decade' column for last chart
for(i in 1:nrow(bb_db)){
    if(bb_db$Year[i] < 1970){
        bb_db$Decade = '1960s'
    } else if(bb_db$Year[i] >= 1970 & bb_db$Year[i] < 1980){
        bb_db$Decade[i] = '1970s'
    } else if(bb_db$Year[i] >= 1980 & bb_db$Year[i] < 1990){
        bb_db$Decade[i] = '1980s'
    } else if(bb_db$Year[i] >= 1990 & bb_db$Year[i] < 2000){
        bb_db$Decade[i] = '1990s'
    } else if(bb_db$Year[i] >= 2000 & bb_db$Year[i] < 2010){
        bb_db$Decade[i] = '2000s'
    } else if(bb_db$Year[i] >= 2010){
        bb_db$Decade[i] = '2010s'
    } else{
        bb_db$Decade[i] = 'Sort this out'
    }
}

ui <- fluidPage(
    tabsetPanel(
        tabPanel("rankPlot", fluid = TRUE,
                 titlePanel("Question 1"),

                sidebarLayout(
                    sidebarPanel(
                        sliderInput("rank",
                                    "Choose Rank",
                                    min = 1,
                                    max = 100,
                                    value = 30)
                    ),

                mainPanel(
                    fluidRow(
                    plotOutput("rankPlot"),
                    verbatimTextOutput("t1")
                )
                )
                )
        ),
        tabPanel("lyricsPlot", fluid = TRUE,
                 titlePanel("Question 2"),
                 
                 sidebarLayout(
                     sidebarPanel(
                         sliderInput("Y1",
                                     "Choose First Year",
                                     min = 1965,
                                     max = 2015,
                                     value = 1965),
                         sliderInput("Y2",
                                     "Choose Second Year",
                                     min = 1965,
                                     max = 2015,
                                     value = 2015),
                         sliderInput("nw",
                                     "Choose Number of Words",
                                     min = 1,
                                     max = 50,
                                     value = 25),
                         sliderInput("year",
                                     "Choose Year for bar graph",
                                     min = 1965,
                                     max = 2015,
                                     value = 2010),
                         sliderInput("n1",
                                     "Choose Number of Words for bar graph",
                                     min = 1,
                                     max = 25,
                                     value = 10)
                     ),
                     
                     mainPanel( 
                         fluidRow(
                             plotOutput("lyricswordPlot"),
                             verbatimTextOutput("t2"),
                             plotOutput("lyricsPlot")
                         )
                     )
                 )
        ),
        tabPanel("titlesPlot", fluid = TRUE,
                 titlePanel("Question 3"),
                 
                 sidebarLayout(
                     sidebarPanel(
                         sliderInput("yr1",
                                     "Choose First Year",
                                     min = 1965,
                                     max = 2015,
                                     value = 1965),
                         sliderInput("yr2",
                                     "Choose Second Year",
                                     min = 1965,
                                     max = 2015,
                                     value = 2015),
                         sliderInput("nw1",
                                     "Choose Number of Words",
                                     min = 1,
                                     max = 50,
                                     value = 25),
                         sliderInput("year2",
                                     "Choose Year for bar graph",
                                     min = 1965,
                                     max = 2015,
                                     value = 2010),
                         sliderInput("n2",
                                     "Choose Number of Words for bar graph",
                                     min = 1,
                                     max = 25,
                                     value = 10)
                     ),
                     
                     mainPanel( 
                         fluidRow(
                             plotOutput("titleswordPlot"),
                             verbatimTextOutput("t3"),
                             plotOutput("titlesPlot")
                         )
                     )
                 )
        ),
        tabPanel("lastPlot", fluid = TRUE,
                 titlePanel("Question 4"),
                 
                 sidebarLayout(
                     sidebarPanel(
                         sliderInput("rank2",
                                     "Choose Rank",
                                     min = 1,
                                     max = 100,
                                     value = 25),
                         selectInput("decade",
                                     "Choose Decade",
                                     choices = c("1960s", "1970s", "1980s", 
                                                 "1990s", "2000s", "2010s",
                                                 "All"),
                                     selected = "2010s")
                     ),
                     
                     mainPanel(
                         fluidRow(
                             plotOutput("lastPlot"),
                             verbatimTextOutput("t4")
                         )
                     )
                 )
        )
    )
)

server <- function(input, output) {

    output$rankPlot <- renderPlot({
        sub <- bb_db[bb_db$Rank==input$rank,]
        sub <- 
            sub %>%
            group_by(Artist) %>%
            summarise(count=n())

        ggplot(sub) +
            geom_col(aes(x=reorder(Artist,count), y=count), fill="darkorchid") +
            coord_flip() +
            labs(x="Artist Name", y="Count",
                   title=paste0("Number of hits at Rank ",input$rank)) +
            scale_y_continuous(breaks=c(0,max(sub$count),1))
    })
    
    output$lyricsPlot <- renderPlot({
        sub2 <- bb_db %>%
            filter(Year == input$year)
        bb_corp1 <- Corpus(VectorSource(sub2$Lyrics))
        bb_corp2 <- tm_map(bb_corp1, stripWhitespace)
        bb_corp3 <- tm_map(bb_corp2, removePunctuation)
        bb_corp4 <- tm_map(bb_corp3, removeWords, stopwords("english"))
        bb_corp5 <- tm_map(bb_corp4, stemDocument)
        bb_tdm1 <- TermDocumentMatrix(bb_corp5)
        frequency <- slam::row_sums(bb_tdm1)
        frequency <- frequency[frequency >= 1]
        frequency_df <-
            data.frame(words = names(frequency), freq = frequency , row.names = NULL)
        frequency_df <-
            frequency_df %>%
            arrange(desc(freq)) %>%
            head(input$n1)

        ggplot(frequency_df) +
            geom_col(aes(x=reorder(words,-freq), y=freq), fill = "tomato") +
            labs(x="Words", y="Count",
                 title=paste0(input$n1, " most common words in song lyrics for the year ", 
                              input$year))

    })
    
    output$titlesPlot <- renderPlot({
        sub2 <- bb_db %>%
            filter(Year == input$year2)
        bb_corp1 <- Corpus(VectorSource(sub2$Song))
        bb_corp2 <- tm_map(bb_corp1, stripWhitespace) 
        bb_corp3 <- tm_map(bb_corp2, removePunctuation)
        bb_corp4 <- tm_map(bb_corp3, removeWords, stopwords("english"))
        bb_corp5 <- tm_map(bb_corp4, stemDocument) 
        bb_tdm1 <- TermDocumentMatrix(bb_corp5)
        frequency <- slam::row_sums(bb_tdm1)
        frequency <- frequency[frequency >= 1]
        frequency_df <- 
            data.frame(words = names(frequency), freq = frequency , row.names = NULL)
        frequency_df <-
            frequency_df %>%
            arrange(desc(freq)) %>%
            head(input$n2)
        
        ggplot(frequency_df) +
            geom_col(aes(x=reorder(words,-freq), y=freq), fill = "slateblue1") +
            labs(x="Words", y="Count",
                 title=paste0(input$n2, " Most common words in song titles for the year ", 
                              input$year2))
        
    })
    
    output$lastPlot <- renderPlot({
        if(input$decade == 'All'){
            sub3 <- bb_db %>%
                filter(Rank <= input$rank2) %>%
                group_by(Artist) %>%
                summarise(
                    count = n()
                ) %>%
                arrange(desc(count)) %>%
                head(10)
        } else{
        sub3 <- bb_db %>%
            filter(Decade == input$decade & Rank <= input$rank2) %>%
            group_by(Artist) %>%
            summarise(
                count = n()
            ) %>%
            arrange(desc(count)) %>%
            head(10)
        }
        ggplot(sub3) +
            geom_col(aes(y=count, x=reorder(Artist, count)), fill = "limegreen") +
            labs(x="Artist", y="Count",
                 title=paste0("Most Hits between rank 1 and ",input$rank2, 
                              " in ", input$decade)) +
            coord_flip()
    })
    
    output$lyricswordPlot <- renderPlot({
        for(i in 1:2){
            if (i==1){
                sub7 <- 
                    bb_db %>%
                    filter(Year == input$Y1)
            } else { 
                sub7 <- 
                    bb_db %>%
                    filter(Year == input$Y2)
            }
            bb_corp1 <- Corpus(VectorSource(sub7$Lyrics))
            bb_corp2 <- tm_map(bb_corp1, stripWhitespace) 
            bb_corp3 <- tm_map(bb_corp2, removePunctuation)
            bb_corp4 <- tm_map(bb_corp3, removeWords, stopwords("english"))
            bb_corp5 <- tm_map(bb_corp4, stemDocument) 
            bb_tdm1 <- TermDocumentMatrix(bb_corp5)
            frequency <- slam::row_sums(bb_tdm1)
            frequency <- frequency[frequency >= 1]
            frequency_df <- 
                data.frame(words = names(frequency), freq = frequency , row.names = NULL)
            frequency_df <-
                frequency_df %>%
                arrange(desc(freq)) %>%
                head(input$nw)
            if (i==1){
                frequency_df$Year = input$Y1
                finaldf  <- frequency_df
            } else { 
                frequency_df$Year = input$Y2
                finaldf <- smartbind(finaldf, frequency_df)
            }}
            
            
            ggplot(finaldf, aes(label = words, size = freq)) +
                geom_text_wordcloud() +
                scale_size_area(max_size = 10) +
                labs(title = paste0("Comparison between most common words in ", 
                                    input$Y1, " and ", input$Y2)) +
                facet_wrap(~Year)
            
    })
    
    output$titleswordPlot <- renderPlot({
        for(i in 1:2){
            if (i==1){
                sub8 <- 
                    bb_db %>%
                    filter(Year == input$yr1)
            } else { 
                sub8 <- 
                    bb_db %>%
                    filter(Year == input$yr2)
            }
            bb_corp1 <- Corpus(VectorSource(sub8$Song))
            bb_corp2 <- tm_map(bb_corp1, stripWhitespace) 
            bb_corp3 <- tm_map(bb_corp2, removePunctuation)
            bb_corp4 <- tm_map(bb_corp3, removeWords, stopwords("english"))
            bb_corp5 <- tm_map(bb_corp4, stemDocument) 
            bb_tdm1 <- TermDocumentMatrix(bb_corp5)
            frequency <- slam::row_sums(bb_tdm1)
            frequency <- frequency[frequency >= 1]
            frequency_df <- 
                data.frame(words = names(frequency), freq = frequency , row.names = NULL)
            frequency_df <-
                frequency_df %>%
                arrange(desc(freq)) %>%
                head(input$nw1)
            if (i==1){
                frequency_df$Year = input$yr1
                finaldf1  <- frequency_df
            } else { 
                frequency_df$Year = input$yr2
                finaldf1 <- smartbind(finaldf1, frequency_df)
            }}
        
        
        ggplot(finaldf1, aes(label = words, size = freq)) +
            geom_text_wordcloud() +
            scale_size_area(max_size = 10) +
            labs(title = paste0("Comparison between most common words in ", 
                                input$yr1, " and ", input$yr2)) +
            facet_wrap(~Year)
        
    })
    
    output$t1 <- renderText({
        paste0("This graph shows which artist have had how many hits at a particular rank.
You can change the rank using the slider.")
    })
    
    output$t2 <- renderText({
        paste0("The wordcloud shows the comparison between the top words in the lyrics 
of songs of the selected years. 
The bar graph shows the how many times the top words were used in a particular year.
The words have changed over the years. Till 2000, 'love' was the most common word almost every
year but since 2000 words like 'dont', 'yeah', 'like', 'get', etc. have been more common.")
    })
    
    output$t3 <- renderText({
        paste("The wordcloud shows the comparison between the top words in the
names of songs of the selected years. 
The bar graph shows the how many times the top words were used in a particular year.
The words have changed less in titles as compared to lyrics over the years. 
Till 2000, 'love' was the most common word almost every year, just like in lyrics but 
in song titles, it has continued to dominate most times even after 2000. However in some years, 
words like 'want', 'dont', 'girl', 'like', etc. have been more common.")
    })
    
    output$t4 <- renderText({
        paste0("The graph shows which artist have had the maximum hits between 1 and the 
selected rank for the selected decade. 
This graph can be used to find out which artists were the best in a particular decade.")
    })
}

shinyApp(ui = ui, server = server)
