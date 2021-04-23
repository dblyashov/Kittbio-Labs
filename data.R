library(gtrendsR)
library(tidyverse)
#library(rstanarm)
library(gt)
library(gtsummary)

# pulling all keywords from google trends into a variable
# make sure to include low search volume = true... maybe not
# gtrends can only pull 5 keywords at once
# will need to create multiple tibbles and then rbind
# comment out once downloaded

# gtrends_set0 <-gtrends(keyword = c("meditation", "exercise","Peleton","WHOOP"),
#                           time = "today 12-m", geo="US"#low_search_volume = TRUE
#                           )
# gtrends_set1 <- gtrends(keyword = c("health related fitness"),
#         time = "today 12-m", geo="US"#low_search_volume = TRUE
# )
# gtrends_set2 <- gtrends(keyword = c("improve sleep"),
#         time = "today 12-m", geo="US"#low_search_volume = TRUE
# )
 
# gtrends_set3 <- gtrends(keyword = c("improve focus","improve health",
# "health supplements","time management","manage stress"),
#          time = "today 12-m", geo="US"#low_search_volume = TRUE
# )
 
# saving gtrends region results into csv file
# comment out once ran once
 
# write_csv(gtrends_set1$interest_by_region, "gtrends_set1.csv")
# write_csv(gtrends_set2$interest_by_region, "gtrends_set2.csv")
# write_csv(gtrends_set3$interest_by_region, "gtrends_set3.csv")
# write_csv(gtrends_set0$interest_by_region, "gtrends_set0.csv")
 
# doing the same for cities ^^^
# some keywords won't have cities data... need to select only those that do
#comment out once ran
 
# write_csv(gtrends_set3$interest_by_city, "gtrends_set3_city.csv")
# write_csv(gtrends_set0$interest_by_city, "gtrends_set0_city.csv")
 
# combined the 4 tibbles I pulled
# first renamed the newly created csv files
# comment out once ran
 
# set_0 <- read.csv("gtrends_set0.csv")
# set_1 <- read.csv("gtrends_set1.csv")
# set_2 <- read.csv("gtrends_set2.csv")
# set_3 <- read.csv("gtrends_set3.csv")
 
# doing the same for cities ^^^ 
# comment out once ran
 
# set_0_city <- read.csv("gtrends_set0_city.csv")
# set_3_city <- read.csv("gtrends_set3_city.csv")
 
# merging all sets and changing N/A to 0 plus renaming
# comment out once ran
 
# merged <- rbind(set_0, set_1, set_2, set_3) %>% 
#   rename(state = location)
# merged[is.na(merged)]<-0
 
# can now remove sets combined in above step
# comment out once ran
 
# remove(set_0, set_1, set_2, set_3)
 
# doing the same for cities ^^^
# comment out once ran
 
# merged_city <- rbind(set_0_city, set_3_city)
# merged_city[is.na(merged_city)]<-0
 
# can now remove sets combined in above step
# comment out once ran
 
# remove(set_0_city, set_3_city)
 
# saving merged tibble into csv file
# comment out once ran
 
# write_csv(merged, "trends_regionv2.csv")
 
# doing the same for cities ^^^
# comment out once ran
 
# write_csv(merged_city, "trends_cityv2.csv")
 
# renaming merged tibble to trends_region
 
# trends_region <- merged

# doing the same for cities ^^^
 
#trends_city <- merged_city

  
 trends_city <- read.csv("trends_cityv2.csv") 
 trends_region <- read.csv("trends_regionv2.csv")


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

# fit_fitness <- stan_glm( data = linear_regress,
                         #formula = fitness ~ sleep + `human performance`+ WHOOP,
                         #refresh = 0) 

# fit_sleep <- stan_glm( data = linear_regress,
                       #formula = sleep ~  fitness + `human performance`+ WHOOP,
                       #refresh = 0)
# fit_human_performance <- stan_glm( data = linear_regress,
                                   #formula = `human performance` ~ sleep + fitness + WHOOP,
                                   #refresh = 0)
# fit_WHOOP <- stan_glm( data = linear_regress,
                       #formula = WHOOP ~  fitness + sleep + `human performance`,
                       #refresh = 0)


# making a table with fits

# model_table_fitness <- fit_fitness %>%
#  tbl_regression(include = everything(),
#                 intercept = TRUE,
#                 estimate_fun = function(x) style_sigfig(x, digits = 3)) %>%
#  as_gt() %>%
#  tab_header(title = "A Model of 'Fitness' on other Keywords",
#             subtitle = "The Results are ambigious") 

#model_table_sleep <- fit_sleep %>%
#  tbl_regression(include = everything(),
#                 intercept = TRUE,
#                 estimate_fun = function(x) style_sigfig(x, digits = 3)) %>%
#  as_gt() %>%
#  tab_header(title = "A Model of 'Sleep' on other Keywords",
#             subtitle = "The Results are ambigious") 


#model_table_human_performance <- fit_human_performance %>%
#  tbl_regression(include = everything(),
#                 intercept = TRUE,
#                 estimate_fun = function(x) style_sigfig(x, digits = 3)) %>%
#  as_gt() %>%
#  tab_header(title = "A Model of 'Human Performance' on other Keywords",
#             subtitle = "The Results are ambigious") 

#model_table_WHOOP <- fit_WHOOP %>%
#  tbl_regression(include = everything(),
#                 intercept = TRUE,
#                 estimate_fun = function(x) style_sigfig(x, digits = 3)) %>%
#  as_gt() %>%
#  tab_header(title = "A Model of 'WHOOP' on other Keywords",
#             subtitle = "The Results are ambigious") 

