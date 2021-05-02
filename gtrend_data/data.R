library(gtrendsR)
library(tidyverse)
library(rstanarm)
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

# fit_meditation <- stan_glm( data = linear_regress,
#                         formula = meditation ~ `health related fitness` + `improve focus` + `improve sleep` + `improve health` + `health supplements` + `time management` + `manage stress` + exercise + Peleton + WHOOP,
#                         refresh = 0) 
# saveRDS(fit_meditation, "fit_meditation")

fit_meditation <- readRDS("fit_meditation")

# fit_health_related_fitness <- stan_glm( data = linear_regress,
#                         formula = `health related fitness` ~ `meditation` + `improve focus` + `improve sleep` + `improve health` + `health supplements` + `time management` + `manage stress` + exercise + Peleton + WHOOP,
#                         refresh = 0) 
# saveRDS(fit_health_related_fitness, "fit_health_related_fitness")

fit_health_related_fitness <- readRDS("fit_health_related_fitness")

# fit_improve_focus <- stan_glm( data = linear_regress,
#                         formula = `improve focus` ~ `meditation` + `health related fitness` + `improve sleep` + `improve health` + `health supplements` + `time management` + `manage stress` + exercise + Peleton + WHOOP,
#                         refresh = 0) 
# saveRDS(fit_improve_focus, "fit_improve_focus")

fit_improve_focus <- readRDS("fit_improve_focus")

# fit_improve_sleep <- stan_glm( data = linear_regress,
#                               formula = `improve sleep` ~ `meditation` + `health related fitness` + `improve focus` + `improve health` + `health supplements` + `time management` + `manage stress` + exercise + Peleton + WHOOP,
#                               refresh = 0) 
# saveRDS(fit_improve_sleep, "fit_improve_sleep")

fit_improve_sleep <- readRDS("fit_improve_sleep")

# fit_improve_health <- stan_glm( data = linear_regress,
#                               formula = `improve health` ~ `meditation` + `health related fitness` + `improve focus` + `improve sleep` + `health supplements` + `time management` + `manage stress` + exercise + Peleton + WHOOP,
#                               refresh = 0) 
# saveRDS(fit_improve_health, "fit_improve_health")

fit_improve_health <- readRDS("fit_improve_health")

# fit_health_supplements <- stan_glm( data = linear_regress,
#                                formula = `health supplements` ~ `meditation` + `health related fitness` + `improve focus` + `improve sleep` + `improve health` + `time management` + `manage stress` + exercise + Peleton + WHOOP,
#                                refresh = 0) 
# saveRDS(fit_health_supplements, "fit_health_supplements")

fit_health_supplements <- readRDS("fit_health_supplements")

# fit_time_management <- stan_glm( data = linear_regress,
#                                formula = `time management` ~ `meditation` + `health related fitness` + `improve focus` + `improve sleep` + `improve health` + `health supplements` + `manage stress` + exercise + Peleton + WHOOP,
#                                refresh = 0) 
#  saveRDS(fit_time_management, "fit_time_management")

fit_time_management <- readRDS("fit_time_management")

# fit_manage_stress <- stan_glm( data = linear_regress,
#                                 formula = `manage stress` ~ `meditation` + `health related fitness` + `improve focus` + `improve sleep` + `improve health` + `health supplements` + `time management` + exercise + Peleton + WHOOP,
#                                 refresh = 0) 
# saveRDS(fit_manage_stress, "fit_manage_stress")

fit_manage_stress <- readRDS("fit_manage_stress")

#fit_exercise <- stan_glm( data = linear_regress,
#                               formula = exercise ~ `meditation` + `health related fitness` + `improve focus` + `improve sleep` + `improve health` + `health supplements` + `time management` + `manage stress` + Peleton + WHOOP,
#                               refresh = 0) 
# saveRDS(fit_exercise, "fit_exercise")

fit_exercise <- readRDS("fit_exercise")

# fit_Peleton <- stan_glm( data = linear_regress,
#                               formula = Peleton ~ `meditation` + `health related fitness` + `improve focus` + `improve sleep` + `improve health` + `health supplements` + `time management` + `manage stress` + exercise + WHOOP,
#                               refresh = 0) 
# saveRDS(fit_Peleton, "fit_Peleton")

fit_Peleton <- readRDS("fit_Peleton")

 fit_WHOOP <- stan_glm( data = linear_regress,
                               formula = WHOOP ~ `meditation` + `health related fitness` + `improve focus` + `improve sleep` + `improve health` + `health supplements` + `time management` + `manage stress` + exercise + Peleton,
                               refresh = 0) 
 saveRDS(fit_WHOOP, "fit_WHOOP")

fit_WHOOP <- readRDS("fit_WHOOP")

# making a table with fits

# FIRST TABLE

 model_table_meditation <- fit_meditation %>%
  tbl_regression(include = everything(),
                 intercept = TRUE,
                 estimate_fun = function(x) style_sigfig(x, digits = 3)) %>%
  as_gt() %>%
  tab_header(title = "A Model of 'Meditation' on other Keywords",
            subtitle = "The Results are ambigious") 
 
 model_table_meditation

 # SECOND TABLE
 
 model_table_health_related_fitness <- fit_health_related_fitness %>%
   tbl_regression(include = everything(),
                  intercept = TRUE,
                  estimate_fun = function(x) style_sigfig(x, digits = 3)) %>%
   as_gt() %>%
   tab_header(title = "A Model of 'Health Related Fitness' on other Keywords",
              subtitle = "The Results are ambigious") 
 
 model_table_health_related_fitness
 
 # THIRD TABLE
 
 model_table_exercise <- fit_exercise %>%
   tbl_regression(include = everything(),
                  intercept = TRUE,
                  estimate_fun = function(x) style_sigfig(x, digits = 3)) %>%
   as_gt() %>%
   tab_header(title = "A Model of 'Exercise' on other Keywords",
              subtitle = "The Results are ambigious") 
 
 model_table_exercise

# FOURTH TABLE
 
 model_table_health_supplements <- fit_health_supplements %>%
   tbl_regression(include = everything(),
                  intercept = TRUE,
                  estimate_fun = function(x) style_sigfig(x, digits = 3)) %>%
   as_gt() %>%
   tab_header(title = "A Model of 'Health Supplements' on other Keywords",
              subtitle = "The Results are ambigious") 
 
 model_table_health_supplements
 
 # FIFTH TABLE 
 
 model_table_improve_focus <- fit_improve_focus %>%
   tbl_regression(include = everything(),
                  intercept = TRUE,
                  estimate_fun = function(x) style_sigfig(x, digits = 3)) %>%
   as_gt() %>%
   tab_header(title = "A Model of 'Improve Focus' on other Keywords",
              subtitle = "The Results are ambigious") 
 
 model_table_improve_focus
 
 # Sixth Table
 
 model_table_improve_health <- fit_improve_health %>%
   tbl_regression(include = everything(),
                  intercept = TRUE,
                  estimate_fun = function(x) style_sigfig(x, digits = 3)) %>%
   as_gt() %>%
   tab_header(title = "A Model of 'Improve Health' on other Keywords",
              subtitle = "The Results are ambigious") 
 
 model_table_improve_health
 
 # Seventh Table
 
 model_table_improve_sleep <- fit_improve_sleep %>%
   tbl_regression(include = everything(),
                  intercept = TRUE,
                  estimate_fun = function(x) style_sigfig(x, digits = 3)) %>%
   as_gt() %>%
   tab_header(title = "A Model of 'Improve Sleep' on other Keywords",
              subtitle = "The Results are ambigious") 
 
 model_table_improve_sleep
 
 # Eigthhh Table
 
 model_table_manage_stress <- fit_manage_stress %>%
   tbl_regression(include = everything(),
                  intercept = TRUE,
                  estimate_fun = function(x) style_sigfig(x, digits = 3)) %>%
   as_gt() %>%
   tab_header(title = "A Model of 'Manage Stress' on other Keywords",
              subtitle = "The Results are ambigious") 
 
 model_table_manage_stress
 
 # Ninth Table
 
 model_table_time_management <- fit_time_management %>%
   tbl_regression(include = everything(),
                  intercept = TRUE,
                  estimate_fun = function(x) style_sigfig(x, digits = 3)) %>%
   as_gt() %>%
   tab_header(title = "A Model of 'Time Management' on other Keywords",
              subtitle = "The Results are ambigious") 
 
 model_table_time_management

 # Tenth Table
 
 model_table_Peleton <- fit_Peleton %>%
   tbl_regression(include = everything(),
                  intercept = TRUE,
                  estimate_fun = function(x) style_sigfig(x, digits = 3)) %>%
   as_gt() %>%
   tab_header(title = "A Model of 'Peleton' on other Keywords",
              subtitle = "The Results are ambigious") 
 
 model_table_Peleton
 
 # Eleventh Table
 
 model_table_WHOOP <- fit_WHOOP %>%
   tbl_regression(include = everything(),
                  intercept = TRUE,
                  estimate_fun = function(x) style_sigfig(x, digits = 3)) %>%
   as_gt() %>%
   tab_header(title = "A Model of 'WHOOP' on other Keywords",
              subtitle = "The Results are ambigious") 
 
 model_table_WHOOP
