library(readxl)
library(sqldf)
library(lubridate)
library(tidyverse)
library(factoextra)
library(gtools)
library(reshape2)
library(plotly)

setwd("C:/Users/HP/Documents/UT Dallas/Spring 2020/BUAN 6390 - Analytics Practicum/Intuilize Datasets")
options(scipen = 999)

#Import Data
sales <- read_xlsx("Sales_Data.xlsx")
product <- read_xlsx("Product.xlsx", col_types = "text")
inventory <- read_xlsx("Inventory_Data.xlsx")
customer <- read_xlsx("Customer_Data.xlsx")
#RFMsegments <- read_xlsx("RFM_Segments.xlsx")

#################################################################################################
#Change 29 Feb 2018 to 28 Feb 2018
sales$`Invoice Date`[is.na(sales$`Invoice Date`)]<-'2018-02-28' 

# COnverting POSIXct to Date
sales$Inv_Date <- as.Date(sales$`Invoice Date`, format = "%Y-%m-%d") 

#Convert Stock Codes to Upper
product$StockCode <- toupper(product$StockCode)
sales$StockCode <- toupper(sales$StockCode)
inventory$StockCode <- toupper(inventory$StockCode)

#Convert Qty to Integer and converted 0 qty to 1
sales$`Quantity Shipped` <- ceiling(sales$`Quantity Shipped`)
#sales$`Quantity Shipped`[sales$`Quantity Shipped`==0] <- 1 

inventory$`QUANTITY ON HAND` <- ceiling(inventory$`QUANTITY ON HAND`)
#inventory$`QUANTITY ON HAND`[inventory$`QUANTITY ON HAND`==0] <- 1 

#Calculate Ext-Price + ext cost
sales$ExtPrice_New <- sales$UnitPrice * sales$`Quantity Shipped`
sales$ExtCost_New <- sales$COST * sales$`Quantity Shipped`
inventory$Cost <- inventory$`QUANTITY ON HAND` * inventory$`UNIT COST`

#Calculate Gross Margin
sales$GrossMargin <- (sales$ExtPrice_New-sales$ExtCost_New)/sales$ExtPrice_New

################### Customer Segmentation

cust_data <- sqldf("select CustomerID, max(Inv_Date) as RecencyDate__Date, 
                   count(distinct(InvoiceNo)) as Frequency,
                   avg(GrossMargin) as Monetary,
                   sum(ExtPrice_New) as Sales
                   from sales group by CustomerID",
                   method = "name__class")

#Convert Recency to Number of Days from last date in data
cust_data$Recency <- as.numeric(cust_data$RecencyDate-min(cust_data$RecencyDate))

#Get R, F and M scores
set.seed(123)
cust_data$R_Score <- ntile(cust_data$Recency, 5)
cust_data$F_Score <- ntile(cust_data$Frequency, 5)
cust_data$M_Score <- ntile(cust_data$Monetary, 5)

#Concatenate R, F and M to get RFM score in 1 column
cust_data$RFM_Score <- cust_data$R_Score*100 + cust_data$F_Score*10 + cust_data$M_Score*1

#############
cust_data_RStats <- cust_data %>%
  group_by(R_Score) %>%
  summarise(Min = min(Recency),
            Max = max(Recency),
            MinDate = min(RecencyDate),
            MaxDate = max(RecencyDate))

cust_data_FStats <- cust_data %>%
  group_by(F_Score) %>%
  summarise(Min = min(Frequency),
            Max = max(Frequency))

cust_data_MStats <- cust_data %>%
  group_by(M_Score) %>%
  summarise(Min = min(Monetary),
            Max = max(Monetary))

################
for(i in 1:nrow(cust_data)){
   if(cust_data$M_Score[i] == 5){
    cust_data$Segments[i] <- 'Elite' 
  } else if(cust_data$Monetary[i] <=0) {
    cust_data$Segments[i] <- 'Drain'
  } else if(cust_data$Monetary[i] <= 0.07353){
    cust_data$Segments[i] <- 'Standard'
  } else {
    cust_data$Segments[i] <- 'VIP'
  } 
}

stats <- cust_data %>%
  group_by(Segments) %>%
  summarise(Count = n(),
            Median_R = median(Recency),
            Mean_R = mean(Recency),
            Median_F = median(Frequency),
            Mean_F = mean(Frequency),
            Median_M = median(Monetary),
            Mean_M = mean(Monetary),
            Median_S = median(Sales),
            Mean_S = mean(Sales))

################### Product Segmentations

prod_data <- sqldf("select StockCode, max(Inv_Date) as RecencyDate__Date, 
                   count(distinct(InvoiceNo)) as Frequency,
                   sum(ExtPrice_New) as Sales,
                   avg(GrossMargin) as Monetary,
                   sum(`Quantity Shipped`) as QtySold
                   from sales group by StockCOde",
                   method = "name__class")

#Convert Recency to Number of Days from last date in data
prod_data$Recency <- as.numeric(prod_data$RecencyDate-min(prod_data$RecencyDate))

#Get R, F and M scores
prod_data$Q_Score <- with(prod_data, factor(findInterval(
  QtySold, c(-Inf, quantile(QtySold, probs=c(0.2, 0.4, 0.6, 0.8)), Inf)), 
  labels=c("1","2","3","4","5")
))

prod_data$F_Score <- with(prod_data, factor(findInterval(
  Frequency, c(-Inf, quantile(Frequency, probs=c(0.2, 0.4, 0.6, 0.8)), Inf)), 
  labels=c("1","2","3","4","5")
))
prod_data$M_Score <- with(prod_data, factor(findInterval(
  Monetary, c(-Inf, quantile(Monetary, probs=c(0.2, 0.4, 0.6, 0.8)), Inf)), 
  labels=c("1","2","3","4","5")
))
prod_data$Q_Score <- as.integer(prod_data$Q_Score)
prod_data$F_Score <- as.integer(prod_data$F_Score)
prod_data$M_Score <- as.integer(prod_data$M_Score)

#Concatenate R, F and M to get RFM score in 1 column
prod_data$QFM_Score <- prod_data$Q_Score*100 + prod_data$F_Score*10 + prod_data$M_Score*1


#############
prod_data_QStats <- prod_data %>%
  group_by(Q_Score) %>%
  summarise(Count = n(),
            Min = min(QtySold),
            Max = max(QtySold))

prod_data_FStats <- prod_data %>%
  group_by(F_Score) %>%
  summarise(Count = n(),
            Min = min(Frequency),
            Max = max(Frequency))

prod_data_MStats <- prod_data %>%
  group_by(M_Score) %>%
  summarise(Count = n(),
            Min = min(Monetary),
            Max = max(Monetary))

for(i in 1:nrow(prod_data)){
  if(prod_data$M_Score[i] == 5){
    prod_data$Segments[i] <- 'Gold' 
  } else if(prod_data$Monetary[i] <=0) {
    prod_data$Segments[i] <- 'Bronze'
  } else {
    prod_data$Segments[i] <- 'Silver'
  } 
}

prodstats <- prod_data %>%
  group_by(Segments) %>%
  summarise(Count = n(),
            Median_Q = median(QtySold),
            Median_F = median(Frequency),
            Median_M = median(Monetary),
            Mediam_S = median(Sales),
            Mean_Q = mean(QtySold),
            Mean_F = mean(Frequency),
            Mean_M = mean(Monetary),
            Mean_S = mean(Sales))

############## Correlating Customer + Product

Cust_Prod <- sqldf("select CustomerID, StockCode, sum(`Quantity Shipped`) as Qty, 
                    sum(ExtPrice_New) as Sales from sales group by CustomerID, StockCode")
Cust_Prod <- sqldf("select cp.*, c.Segments as CustSegment from Cust_Prod cp left join
                   cust_data c on cp.CustomerID = c.CustomerID")
Cust_Prod <- sqldf("select cp.*, p.Segments as ProdSegment from Cust_Prod cp left join
                   prod_data p on cp.StockCode = p.StockCode")

ggplot(data=Cust_Prod) +
  geom_bar(aes(x=CustSegment, y=Sales, fill=ProdSegment), stat="sum", position = "dodge")


############ Graphs

Graph1 <- cust_data %>%
  group_by(Segments) %>%
  summarise(
    Count =n(),
    AverageGM = round(mean(Monetary),2),
    AverageSales = round(mean(Sales),2))


ggplot(Graph1)  + 
  geom_bar(aes(x=Segments, y=AverageSales, fill=Segments),stat="identity", colour="black", width = 0.30)+
  geom_line(aes(x=Segments, y=AverageGM*max(Graph2$AverageSales), group=1),stat="identity")+
  geom_text(aes(label=AverageGM, x=Segments, y=(AverageGM-0.025)*max(Graph2$AverageSales)), colour="black", size=4)+
  geom_text(aes(label=AverageSales, x=Segments, y=0.95*AverageSales), colour="black", size=4)+
  scale_y_continuous("Average Sales/Customer", breaks = c(0, 5000, 10000, 15000),
                     sec.axis = sec_axis(~./max(Graph2$AverageSales), name = 'Avg Gross Margin/Customer')) +
  scale_fill_manual(values=c("tomato3 ", "chartreuse3", "orange3", "dodgerblue3")) +
  ggtitle("Customer Segments (Sales and Gross Margin)") +
  theme_minimal() + theme(legend.position = "none")

Graph2 <- prod_data %>%
  group_by(Segments) %>%
  summarise(
    Count =n(),
    AverageGM = round(mean(Monetary),2),
    AverageSales = round(mean(Sales),2))


ggplot(Graph2)  + 
  geom_bar(aes(x=Segments, y=AverageSales, fill=Segments),stat="identity", colour="black", width = 0.25)+
  geom_line(aes(x=Segments, y=AverageGM*max(Graph2$AverageSales), group=1),stat="identity")+
  geom_text(aes(label=AverageGM, x=Segments, y=(AverageGM-0.025)*max(Graph2$AverageSales)), colour="black", size=4)+
  geom_text(aes(label=AverageSales, x=Segments, y=0.95*AverageSales), colour="black", size=4)+
  scale_y_continuous("Average Sales/Customer", breaks = c(0, 5000, 10000, 15000),
                     sec.axis = sec_axis(~./max(Graph2$AverageSales), name = 'Avg Gross Margin/Customer')) +
  scale_fill_manual(values=c("tan3 ", "gold", "gray70")) +
  ggtitle("Product Segments (Sales and Gross Margin)") +
  theme_minimal() + theme(legend.position = "none")


################ Scoring
ScoringData <- sqldf("select CustomerID, max(Inv_Date) as RecencyDate__Date, min(Inv_Date) as EarliestDate__Date, 
                      count(distinct(InvoiceNo)) as Frequency, sum(`QUANTITY SHIPPED`) as Quantity, 
                      sum(ExtPrice_New) as Sales, avg(GrossMargin) as GrossMargin
                      from sales group by CustomerID", method = "name__class")

#ScoringData$TimeBetnOrders <- (ScoringData$RecencyDate - ScoringData$EarliestDate)/(ScoringData$Frequency-1)
ScoringData$TimeBetnOrders <- if_else(ScoringData$Frequency==1, 999, as.numeric((ScoringData$RecencyDate - ScoringData$EarliestDate)
                                                                                /(ScoringData$Frequency-1)))
ScoringData$AvgOrderSize <- ScoringData$Quantity/ScoringData$Frequency
ScoringData$AvgOrderValue <- ScoringData$Sales/ScoringData$Frequency
ScoringData$TimeBetnOrders <- as.numeric(ScoringData$TimeBetnOrders)

ScoringData <- ScoringData[,c(1,2,3,8,4,5,9,6,7,10)]

ScoringData$Check <- ScoringData$RecencyDate - ScoringData$EarliestDate

###### Recency
RecencyData <- ScoringData[,c(1,2,3,4)]
RecencyData$RecencyDateDays <- as.numeric(RecencyData$RecencyDate-min(sales$Inv_Date))
RecencyData$EarliestDateDays <- as.numeric(max(sales$Inv_Date)-RecencyData$EarliestDate)

quantile(RecencyData$RecencyDateDays, c(0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1))
quantile(RecencyData$EarliestDateDays, c(0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1))
quantile(RecencyData$TimeBetnOrders, c(0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1))

# Recency
RecencyData$RecScore <- with(RecencyData, factor(findInterval(
  RecencyDateDays, c(-Inf, quantile(RecencyDateDays, probs=c(0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9)), Inf)), 
  labels=c("1","2","3","4","5","6","7","8","9","10")
))
RecencyData$RecScore <- as.numeric(RecencyData$RecScore)

# Eariest
RecencyData$EarliestScore <- with(RecencyData, factor(findInterval(
  EarliestDateDays, c(-Inf, quantile(EarliestDateDays, probs=c(0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9)), Inf)), 
  labels=c("1","2","3","4","5","6","7","8","9","10")
))
RecencyData$EarliestScore <- as.numeric(RecencyData$EarliestScore)

# # Recency
# sub1 <- RecencyData[RecencyData$TimeBetnOrders==999,]
# sub2 <- RecencyData[RecencyData$TimeBetnOrders<999,]
# sub1$TimeScore <- 1
# sub2$TimeScore <- with(sub2, factor(findInterval(
#   TimeBetnOrders, c(-Inf, quantile(TimeBetnOrders, probs=c(0.11, 0.22, 0.33, 0.44, 0.55, 0.66, 0.77, 0.88)), Inf)), 
#   labels=c("10","9","8","7","6","5","4","3","2")
# ))
# RecencyData <- smartbind(sub1, sub2)
# RecencyData$TimeScore <- as.numeric(RecencyData$TimeScore)

# Freq_Final_Score
RecencyData$R_Score <- RecencyData$RecScore*0.3 + 
  RecencyData$EarliestScore*0.7 
# + RecencyData$TimeScore*0.375

######## Frequency
FrequencyData <- ScoringData[,c(1,5,6,7)]
quantile(FrequencyData$Frequency, c(0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1))
quantile(FrequencyData$Quantity, c(0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1))
quantile(FrequencyData$AvgOrderSize, c(0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1))


# Frequency
## 1, 2, 3-5, 6-10, 11+
for(i in 1:nrow(FrequencyData)){
  if(FrequencyData$Frequency[i]==1){
    FrequencyData$FreqScore[i] <- 1
  } else if(FrequencyData$Frequency[i]==2){
    FrequencyData$FreqScore[i] <- 2
  } else if(FrequencyData$Frequency[i]==3){
    FrequencyData$FreqScore[i] <- 3
  } else if(FrequencyData$Frequency[i]==4){
    FrequencyData$FreqScore[i] <- 4
  } else if(FrequencyData$Frequency[i]==5){
    FrequencyData$FreqScore[i] <- 5
  }else if(FrequencyData$Frequency[i]==6){
    FrequencyData$FreqScore[i] <- 6
  } else if(FrequencyData$Frequency[i]<=8){
    FrequencyData$FreqScore[i] <- 7
  } else if(FrequencyData$Frequency[i]<=10){
    FrequencyData$FreqScore[i] <- 8
  } else if(FrequencyData$Frequency[i]<=12){
    FrequencyData$FreqScore[i] <- 9
  } else if(FrequencyData$Frequency[i]>=13){
    FrequencyData$FreqScore[i] <- 10
  }
}

# Qty
FrequencyData$QtyScore <- with(FrequencyData, factor(findInterval(
  Quantity, c(-Inf, quantile(Quantity, probs=c(0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9)), Inf)), 
  labels=c("1","2","3","4","5","6","7","8","9","10")
))
FrequencyData$QtyScore <- as.numeric(FrequencyData$QtyScore)

# AOS
FrequencyData$AOSScore <- with(FrequencyData, factor(findInterval(
  AvgOrderSize, c(-Inf, quantile(AvgOrderSize, probs=c(0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9)), Inf)), 
  labels=c("1","2","3","4","5","6","7","8","9","10")
))
FrequencyData$AOSScore <- as.numeric(FrequencyData$AOSScore)

# Freq_Final_Score
FrequencyData$F_Score <- FrequencyData$FreqScore*0.7 + 
  FrequencyData$QtyScore*0.3
#FrequencyData$QtyScore*0.25 +


######## Monetary
MonetaryData <- ScoringData[,c(1,8,9,10)]
summary(MonetaryData$Sales)
quantile(MonetaryData$Sales, c(0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1))
summary(MonetaryData$GrossMargin)
quantile(MonetaryData$GrossMargin, c(0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1))
summary(MonetaryData$AvgOrderValue)
quantile(MonetaryData$AvgOrderValue, c(0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1))

# Sales
MonetaryData$SalesScore <- with(MonetaryData, factor(findInterval(
  Sales, c(-Inf, quantile(Sales, probs=c(0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9))), Inf)), 
  labels=c("1","2","3","4","5","6","7","8","9","10")
)
MonetaryData$SalesScore <- as.numeric(MonetaryData$SalesScore)

# Gross Margin
MonetaryData$GMScore <- with(MonetaryData, factor(findInterval(
  GrossMargin, c(-Inf, quantile(GrossMargin, probs=c(0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9))), Inf)), 
  labels=c("1","2","3","4","5","6","7","8","9","10")
)
MonetaryData$GMScore <- as.numeric(MonetaryData$GMScore)

# AOV
MonetaryData$AOVScore <- with(MonetaryData, factor(findInterval(
  AvgOrderValue, c(-Inf, quantile(AvgOrderValue, probs=c(0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9))), Inf)), 
  labels=c("1","2","3","4","5","6","7","8","9","10")
)
MonetaryData$AOVScore <- as.numeric(MonetaryData$AOVScore)

# Monetary_Final_Score
MonetaryData$M_Score <-  
  MonetaryData$GMScore*0.85 + MonetaryData$SalesScore*0.15
#MonetaryData$SalesScore*0.2 +


### Merge
FinalScoringData <- sqldf("select r.*, f.*, m.* from RecencyData r 
                          join FrequencyData f on r.CustomerID=f.CustomerID
                          join MonetaryData m on m.CustomerID=f.CustomerID")

FinalScoringData$CustomerScore <- round((FinalScoringData$R_Score*0.17 + FinalScoringData$F_Score*0.33 +
                                           FinalScoringData$M_Score*0.5)*100,2)


Score_Segment <- sqldf("select c.*, f.CustomerScore from cust_data c join 
                       FinalScoringData f on c.CustomerID=f.CustomerID")

Scorestats <- Score_Segment %>%
  group_by(Segments) %>%
  summarise(MeanScore = mean(CustomerScore),
            MedianScore = median(CustomerScore),
            MinScore = min(CustomerScore),
            MaxScore = max(CustomerScore))
