library(tidyverse)
library(dplyr)

library(scales)
library(urbnmapr)      # package for plotting
library(urbnthemes)
library(devtools)

## data source: https://www.census.gov/data/tables/time-series/demo/popest/2010s-counties-detail.html
## load the census data
raw_df <- read_csv("census_raw_data/co-est2020-alldata.csv")

## gather all variables but geographical variables
census_df <- raw_df %>% gather("census_type", "census_value",-c(1:7))


## parse numbers to create a year variable
#https://stackoverflow.com/questions/14543627/extracting-numbers-from-vectors-of-strings
census_df$year <- as.numeric(gsub("[^\\d]+", "", census_df$census_type, perl=TRUE))

## check the parsed years
census_df$year %>% unique()

## omit the years in census_type
# https://stackoverflow.com/questions/13590139/remove-numbers-from-alphanumeric-characters
census_df$census_type <- gsub('[[:digit:]]+', '', census_df$census_type)

## we can also spread the data as follow
## spread the category                                     
census_df2 <-  census_df %>% spread(key=census_type,value = census_value) 

## combine state and county fips 
census_df2$county_fips <- paste0(census_df2$STATE,census_df2$COUNTY)

# get FIPS consistent with other data FIPS key
## FIPS is a 5 digit number, so append zero if 4 digits
census_df2$county_fips <- str_pad(census_df2$county_fips, 5, pad = "0")

census_data <- census_df2 %>% select(year,county_fips,STNAME,CTYNAME,
                                     POPESTIMATE,BIRTHS,RBIRTH,DEATHS,RDEATH,NETMIG)
## glance at the data statistics
census_data %>% summary()

## convert to numeric
for (j in 5:10) {
  census_data[[j]] <- census_data[[j]] %>% as.numeric()
}

census_data$LOG_POPESTIMATE <- census_data$POPESTIMATE %>% log(.+1)
census_data$LOG_BIRTHS <- census_data$BIRTHS %>% log(.+1)
census_data$LOG_DEATHS <- census_data$DEATHS %>% log(.+1)

census_data %>% summary()


## save the cleaned data
#write_csv(census_data,"cleaned_data/census_data.csv")



## remove state and keep only county observations
census_data$state_dedect <- census_data$county_fips %>% substr(.,3,5) %>% as.numeric()

census_data <- census_data %>% filter(state_dedect!=0)




#################
## mapping
# source: https://github.com/UrbanInstitute/urbnmapr
##############################################
# library(urbnmapr)
# library(urbnthemes)
#set_urbn_defaults(style = "map")

spatial_data <- left_join(get_urbn_map(map = "counties", sf = TRUE),
                          census_data[census_data$year==2020,],
                          by = "county_fips")

ggplot(spatial_data) +
  geom_sf(mapping = aes(fill = NETMIG),
          color = "#ffffff", size = .001) +
  #scale_fill_gradientn(labels = scales::percent) +
  labs(fill = "NETMIG") +
  coord_sf(datum = NA)

#########
## state map
#############


## aggregate net migration over states
state_migration <- census_df2 %>% select(year,county_fips,STNAME,CTYNAME,NETMIG) %>% 
                                filter(year==2019)
                                  
state_migration$NETMIG <- state_migration$NETMIG %>% as.numeric()

state_migration <- aggregate(NETMIG ~ STNAME, data = state_migration, sum) %>% 
                      dplyr::rename(state_name = STNAME)

#state_migration$state_name <- state_migration$state_name %>% tolower()

df <- left_join(get_urbn_map(map = "states",sf = TRUE),
                          state_migration,
                          by = "state_name")

ggplot(df) +
  geom_sf(mapping = aes(fill = NETMIG),
          color = "#ffffff", size = .001) +
  #scale_fill_gradientn(labels = scales::percent) +
  labs(fill = "Net migration") +
  coord_sf(datum = NA)


############
# migration plots
#########################

ggplot(census_data[census_data$STNAME %in% c("Texas", "Florida","Arizona"), ],aes(x=factor(year),y=NETMIG))+
  geom_text(aes(label = CTYNAME), position = position_dodge(0.2))+
  geom_jitter()+
  facet_wrap(~STNAME)+
  theme_bw()+
  ylab("Net migration") +
  scale_x_continuous(breaks=seq(2010,2020,by=2))

ggplot(census_data[census_data$STNAME %in% c("Texas", "Florida","Arizona") & census_data$NETMIG>25000, ],aes(x=year,y=NETMIG))+
  geom_text(aes(label = CTYNAME))+
  geom_jitter()+
  facet_wrap(~STNAME)+theme_bw()+
  ylab("Net migration")+
  scale_x_continuous(breaks=seq(2010,2020,by=2))


ggplot(census_data[census_data$STNAME %in% c("Texas", "Florida","Arizona") & census_data$NETMIG<0, ],aes(x=year,y=NETMIG))+
  geom_text(aes(label = CTYNAME))+
  geom_jitter()+
  facet_wrap(~STNAME)+theme_bw()+
  ylab("Net migration")+
  scale_x_continuous(breaks=seq(2010,2020,by=2))


