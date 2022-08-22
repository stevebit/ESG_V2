library(gtrendsR)
library(reshape2)
library(dplyr)
library(lubridate)
library(data.table)
library(ggplot2)
library("memisc" )

# db <- gtrends(keyword = c("esg"), geo = "", time="all", low_search_volume = F)
# 
# 
# time <- db$interest_over_time %>% 
#   data.table() %>%
#   mutate_if(is.character, as.factor)
# 
# time %>%
#   filter(date>=ymd("2015-01-01")) %>%
#   ggplot(aes(date,hits,colour=hits))+
#   geom_line(size=1)+
#   ggtitle("Global Google Trend Searches for ESG")+
#   xlab("Date") +
#   ylab("Relative Daily Searches") +
#   theme(legend.position="none")
#  
# 
# 
# 
# country <- db$interest_by_country %>% 
#   data.table() %>%
#   mutate_if(is.character, as.factor)
# 
# country %>%
#   filter(!is.na(hits)) %>%
#   ggplot(aes(reorder(location,hits),hits,fill=location ))+
#   geom_bar( stat = "identity")+
#   coord_flip()+
#   ggtitle("Global Google Trend Searches for ESG")+
#   ylab("Relative Searches") +
#   xlab("Country") +
#   theme(legend.position="none")





#"UK


countries <- c("MY","TH","VN","SG","IN", "PH","CN","BE","PT","TR","FR")


interest_over_time <- data.table()
interest_by_region <- data.table()
interest_by_city <- data.table()
related_topics <- data.table()
related_queries <- data.table()


for (i in 1:length(countries)){
  
  print(i)
  trends = gtrends("ESG", gprop = "web",geo=countries[i], time = ("2015-01-01 2022-08-22")) 
  
  time <- trends$interest_over_time %>%
    data.table() %>%
    mutate_if(is.character, as.factor)
  interest_over_time <- bind_rows(time,interest_over_time)
  
  region <- trends$interest_by_region %>%
    data.table() %>%
    mutate_if(is.character, as.factor)
  interest_by_region <- bind_rows(region,interest_by_region)
  
  city <- trends$interest_by_city %>%
    data.table() %>%
    mutate_if(is.character, as.factor)
  interest_by_city <- bind_rows(city,interest_by_city)
  
  topics <- trends$related_topics %>%
    data.table() %>%
    mutate_if(is.character, as.factor)
  related_topics <- bind_rows(topics,related_topics)
  
  queries <- trends$related_queries %>%
    data.table() %>%
    mutate_if(is.character, as.factor)
  related_queries <- bind_rows(queries,related_queries)
  
  rm(time,region,city,topics,queries,trends)
}

#countries <- c("MY","TH","VN","SG","IN", "PH","CN","BE","PT","TR","FR")

db <- interest_over_time


interest_over_time %>%
  filter(date>=ymd("2018-01-01")) %>%
  ggplot(aes(date,hits,colour=geo))+
  geom_line(size=0.7)+
  ggtitle("Global Google Trend Searches for ESG")+
  xlab("Date") +
  ylab("Relative Daily Searches") 
sad

db %>%
  filter(geo %in% c("MY")) %>%
  ggplot(aes(reorder(location,hits),hits,fill=location))+
  geom_bar(stat = "identity") +
  coord_flip() +
  theme(legend.position="none") +
  facet_grid(geo~.) +
  ggtitle("Global Google Trend Searches for ESG")+
  ylab("Relative Searches") +
  xlab("Country") 



