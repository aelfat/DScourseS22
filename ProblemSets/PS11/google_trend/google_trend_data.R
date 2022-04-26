# loading packages
library(rvest)
library(tidyverse)
library(gtrendsR)
setwd("~/Desktop/metrics4/IPEDS_files_scraped/google_trend")
#-----------------------------------------------------
## begin downloading the data using gtrendsR libary
#-----------------------------------------------------

# ## get the google trend for marijuana related search words
# res <- gtrends(c("dispensary","marijuana", "weed","pot"),
#                geo = c(paste0("US-",state.abb[1])),time="all" )
# 
# df <- res$interest_over_time
# 
#     for (j in 2:length(state.abb)){
#       df2 <- gtrends(c("dispensary","marijuana", "weed","pot"),
#                      geo = c(paste0("US-",state.abb[j])),time="all" )$interest_over_time
#       df <- df %>% rbind(df2)
# 
#     }
# #-------
# ## END
#-------
#write_csv(df,"trends_raw.csv")


#---------------------
## begin data cleaning
#----------------------
#"~/Desktop/metrics4/IPEDS_files_scraped/google_trend/trends_raw.csv"


df <- read_csv("trends_raw.csv")

# df %>% colnames()
# df %>% head()

## keeping the year only
df$YEAR <- df$date %>% as.character() %>% substr(.,1,4) %>% as.numeric()

## state abbreviation 
df$STABBR <- substr(df$geo,4,5)

## consider hits less than 1 zero
# df$hits %>% unique()
df$hits[df$hits=="<1"] <- "0"
df$hits <- df$hits %>% as.numeric()

## removing incomplete period 
df <- df %>% filter(YEAR<2022) %>%  
             select(YEAR,STABBR,keyword,hits) %>% 
             aggregate(hits ~ keyword + STABBR + YEAR, data = ., mean)


#-------
## END
#-------
#df %>% summary()

# medical_st <- c("AK","DC","OR","WA","NV","HI","CO","VT","MT","RI","NM","MA","CT","NH","IL","NY","MN", "GM",
#              "OH","PA", "ND","AR","FL","LA","WV","MO","UT","OK","CA","ME","MD","MI","NJ","AZ","DE")
# 
# medical_yr <- c(1998,1998,1998,1998,2000,2000,2000,2014,2014,2007,2007,2012,2012,2013,2013,
#                   2014,2014,2014,2016,2016,2016,2016,2016,2017,2017,2018,2018,2018,1996,1999,
#                   2003,2008,2009,2010,2011)
# 
# state_info = tibble(state_name = state.name,state=state.abb )
# 
# med_marij <- data_frame(
#         state = medical_st,
#         date_med = medical_yr
# ) %>% left_join(state_info  )
# 
# marij_legal_st <- c("CO","WA","AK","OR","MA","CA","NV","MI")
# marij_yr_st <- c(2012,2012,2015,2015,2015,2016,2017,2018)
# 
# 
# law_marij <- data_frame(
#                         state = marij_legal_st,
#                         date_law = marij_yr_st
# )
# 
# 
# 
# not_mari_or_med <- state.abb[!(state.abb %in% c(medical,marij_legal_st) )]
# 
# marij_legal_yr <- c(2012,2015,2017,2014,2016,2018)
# 
# df$legal <- ifelse(df$state %in% marij_legal_st ,"Legal_states","Not_legal_states")


## spread the data over keywords and take
## the average of marijuana related words
df2 <- df %>% spread(key=keyword,value = hits)

## not necessary referring to dispensary stores
df2$marij_related_words <- rowMeans(df2[,c("marijuana","pot","weed")])

df2 <- df2 %>% select(-c(marijuana,pot,weed)) 
write_csv(df2,"google_trend_df.csv")


##%>% gather(key=keyword,value =hits,-c(state,date,legal) )


### further analysis of the data

# df2 <- df2 %>% left_join(law_marij)
# ## plot for legal vs not legal
# df_groups <- aggregate(hits ~ keyword + legal + date,data=df2,mean)
# 
# ggplot(df_groups, 
#        aes(x=date, y=hits, color=legal)) +  facet_wrap(~keyword)+
#        geom_line()+
#       theme_bw()+
#       xlab("")+ylab("Search hits")+
#       geom_vline(xintercept = c(2012,2015,2017)) 
# 
# ggplot(aggregate(hits ~ keyword + date,data=df2,mean) %>% filter(keyword=="dispensary"), 
#        aes(x=date, y=hits)) +  
#   geom_line()+
#   theme_bw()+
#   xlab("")+ylab("Search hits")+
#   geom_vline(xintercept = c(2012,2015,2017)) 
# 
# 
# 
# 
# ggplot( (df2 %>% filter(keyword=="dispensary")),
#        aes(x=date, y=hits) ) + geom_jitter() + 
#        geom_smooth(se = FALSE)+theme_bw()+geom_vline(xintercept = c(2014,2016,2018), linetype="dashed")
# 
# ggplot( (df_groups %>% filter(keyword=="dispensary")),
#         aes(x=date, y=hits, color=legal) )  + 
#         geom_smooth(se = FALSE)+theme_bw()+
#         geom_vline(xintercept = c(2014,2016,2018), linetype="dashed")
# 
# 
# ########
# ## spread the data over keywords and take
# ## the average of marijuana related words
# df2 <- df %>% filter(state %in% c(marij_legal_st)) %>% 
#             spread(key=keyword,value = hits)
# 
# ## not necessary referring to dispensary stores
# df2$marij_related_words <- rowMeans(df2[,c("marijuana","pot","weed")])
# 
# df2 <- df2 %>% select(-c(marijuana,pot,weed)) %>% gather(key=keyword,value =hits,-c(state,date,legal) )
# 
# ## plot for legal vs not legal
# 
# df_groups2 <- aggregate(hits ~ keyword + legal + date,data=df2,mean)
# 
# ggplot( (df_groups2 %>% filter(keyword=="dispensary")),
#         aes(x=date, y=hits, color=legal) )  + 
#   geom_smooth(se = FALSE)+theme_bw()+
#   geom_vline(xintercept = c(2014,2016,2018), linetype="dashed")
# 
# ggplot( (df2 %>% filter(keyword=="dispensary")),
#         aes(x=date, y=hits, color=legal) )  + 
#   geom_smooth(se = FALSE) + theme_bw() + facet_grid(~state)+
#   geom_vline(aes(xintercept = date_law), linetype="dashed")
