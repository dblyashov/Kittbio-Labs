library(gtrendsR)
library(tidyverse)

# pulling all keywords from google trends into a variable
# make sure to include low search volume = true 

trends_keywords <-gtrends(keyword = c("fitness", "sleep", "human performance", "WHOOP"),
                         time = "today 12-m", geo="US", low_search_volume = TRUE)

# selecting which lists (interest by region or city)

trends_keywords_region <- trends_keywords[["interest_by_region"]] %>% 
  rename(state = location)
trends_keywords_city <- trends_keywords[["interest_by_city"]]

# removing missing data points 
# changed na values to 0 
 trends_region <- trends_keywords_region
 trends_region[is.na(trends_region)]<-0
 
 trends_city <- trends_keywords_city  
 trends_city[is.na(trends_city)]<-0

# removing extra data sets

remove(trends_keywords)
remove(trends_keywords_region)
remove(trends_keywords_city)

# trying to add state code to trends_region data

trends_region <- trends_region %>% 
  mutate(code = state.abb[match(state, state.name)]) %>% 
  replace_na(list(code = "DC")) %>% 
  select(-geo, -gprop)
