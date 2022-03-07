library(tidyverse)
library(rvest)
library(lubridate)
library(nycflights13)
library(xml2)
library(rvest)
library(readxl)
library(dplyr)
library(rscorecard)

#setwd("~/Desktop/metrics4/NAEP_DATA/marijuana_price")
# table for 2019 to 2021
###########################
# extracting all the tables for 2010 to 2020
# list of links
#-----------------
#library(XML)
# Extract hyperlink from Excel file in R
#source: https://stackoverflow.com/questions/24149821/extract-hyperlink-from-excel-file-in-r
linkTable<- read_csv("WeedPrice.csv")

#TempTable <- PriceData
linkTable$url <- as.character(linkTable$url)
linkTable$url <- linkTable$url %>% gsub(" ","%20",.)


#link1="https://www.priceofweed.com/prices/United-States/Alabama.html"
node22 <- ".avg_box"

# this function takes the index of the url
# returns scraped table from the url
# the node is defined outside of the function
scrapePrices <- function(k){
  
  link1 <- linkTable$url[k]
  
  # call the title with the date
  col_page <- read_html(link1)
  
  # calling the table
  col_table0 <- col_page %>% html_nodes(node22) %>% 
    html_table() %>% as.data.frame() %>% select(1:2)  %>% 
    rename(quality_type=X1,  price=X2)
  
  # separate the measurement unit
  col_table <- col_table0[-1,]
  unit_mesrmt <- col_table0[1,]
  
  col_table$url <- link1
  col_table$Year <- linkTable$Year[k]
  col_table$Jurisdictions <- linkTable$Jurisdictions[k]
  col_table$measure_unit <- unit_mesrmt[1,2]
  
  return(as_data_frame(col_table) )}

# save the first entry
PriceData <- scrapePrices(1)

# save and bind with the first entry
for (j in 2:nrow(linkTable)) {
  PriceData <-    rbind(PriceData, scrapePrices(j))
  print(j)
}

# cleaning the price variable from extraneous characters 
PriceData$measure_unit %>% unique() # the same unit

# I checked the web and the text means no entry
PriceData$price[PriceData$price=="I feel bad for these guys -->"] <- NA

# convert price to double
PriceData$price <- PriceData$price %>% gsub("\\$","",.) %>% as.numeric()

PriceData$quality_type %>% unique()


# ploting the price 
ggplot(data = PriceData)+
  geom_point(aes(x=Year, y=price))+
  facet_wrap(~quality_type)

# looking at the distribution 
ggplot(data = PriceData,aes(price))+
  geom_histogram()+
  facet_wrap(~quality_type)

# remove the low quality as it has lots of missing values and years
PriceData1 <- PriceData %>% filter(quality_type!="Low Quality")

# # ploting the price 
# ggplot(data = PriceData1)+
#               geom_point(aes(x=Year, y=price))+
#               facet_wrap(~quality_type)
# 
# # looking at the distribution 
ggplot(data = PriceData1,aes(price))+
  geom_histogram()+
  facet_wrap(~quality_type)

# aggregate price over quality, year, and state by taking the mean

PriceData_states <- aggregate(by=list(Year=PriceData1$Year, Jurisdictions=PriceData1$Jurisdictions),
                              x= PriceData1[,2], FUN = mean, na.rm=TRUE, na.action=na.omit) %>% 
  rename(Jurisdiction=Jurisdictions)

PriceData_national <- aggregate(by=list(Year=PriceData1$Year),
                                x= PriceData1[,2], FUN = mean, na.rm=TRUE, na.action=na.omit)

PriceData_national$Jurisdiction <- "NATIONAL" 

ggplot(PriceData_national)+ geom_line(aes(x=Year, y=price),color="blue") + 
  theme_bw() 

write_csv(PriceData1,"medium_low_prices.csv")
write_csv(PriceData_national,"aggregated_prices.csv")

## API for Scorecard
#-----------------
## usethis::edit_r_environ()

## KEY SOURCE:
# https://collegescorecard.ed.gov/data/documentation/
sc_key(Sys.getenv("SCORECARD_API_KEY"))

# download some data
df <- sc_init() %>% 
  sc_filter(region == 2, ccbasic == c(21,22,23), locale == 41:43) %>% 
  sc_select(unitid, instnm,stabbr, st_fips,zip, latitude,longitude, ugds, ugds_white, ugds_black, ugds_hisp, ugds_asian) %>% 
  sc_year(2016) %>% 
  sc_get()


#UGDS_WHITE,UGDS_BLACK,UGDS_HISP,UGDS_ASIAN

library(rvest)
library(tidyverse)

h <- read_html("https://en.wikipedia.org/wiki/Current_members_of_the_United_States_House_of_Representatives")

reps <- h %>%
  html_node(".jquery-tablesorter") %>%
  html_table()

reps <- reps[,c(1:2,4:9)] %>%
  as_tibble()
