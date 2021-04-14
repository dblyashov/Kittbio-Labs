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


# adding code for ALL model table

# linear regress on keyword "fitness" with all other keywords


linear_regress <- trends_region %>%
  pivot_wider(names_from = keyword,
              values_from = hits)

# creating fits for all variables

fit_fitness <- stan_glm( data = linear_regress,
                         formula = fitness ~ sleep + `human performance`+ WHOOP,
                         refresh = 0)
fit_sleep <- stan_glm( data = linear_regress,
                       formula = sleep ~  fitness + `human performance`+ WHOOP,
                       refresh = 0)
fit_human_performance <- stan_glm( data = linear_regress,
                                   formula = `human performance` ~ sleep + fitness + WHOOP,
                                   refresh = 0)
fit_WHOOP <- stan_glm( data = linear_regress,
                       formula = WHOOP ~  fitness + sleep + `human performance`,
                       refresh = 0)


# making a table with fits

model_table_fitness <- fit_fitness %>%
  tbl_regression(include = everything(),
                 intercept = TRUE,
                 estimate_fun = function(x) style_sigfig(x, digits = 3)) %>%
  as_gt() %>%
  tab_header(title = "A Model of 'Fitness' on other Keywords",
             subtitle = "The Results are ambigious") 

model_table_sleep <- fit_sleep %>%
  tbl_regression(include = everything(),
                 intercept = TRUE,
                 estimate_fun = function(x) style_sigfig(x, digits = 3)) %>%
  as_gt() %>%
  tab_header(title = "A Model of 'Fitness' on other Keywords",
             subtitle = "The Results are ambigious") 


model_table_human_performance <- fit_human_performance %>%
  tbl_regression(include = everything(),
                 intercept = TRUE,
                 estimate_fun = function(x) style_sigfig(x, digits = 3)) %>%
  as_gt() %>%
  tab_header(title = "A Model of 'Fitness' on other Keywords",
             subtitle = "The Results are ambigious") 

model_table_WHOOP <- fit_WHOOP %>%
  tbl_regression(include = everything(),
                 intercept = TRUE,
                 estimate_fun = function(x) style_sigfig(x, digits = 3)) %>%
  as_gt() %>%
  tab_header(title = "A Model of 'Fitness' on other Keywords",
             subtitle = "The Results are ambigious") 

