library(gtrendsR)
library(tidyverse)
library(usmap)
library(plotly)
source("data.R")
library(rstanarm)
library(gtsummary)
library(gt)

# interest by city for "fitness"


 top10_fitness_city <- trends_city %>% 
  filter(keyword == "fitness") %>%
  as_tibble() %>% 
  arrange(desc(hits)) %>% 
  slice(1:10) %>% 
  select(City = location, Interest = hits)

# interest by region on US MAP for "fitness"

fitness_region <- trends_region %>% 
  filter(keyword == "fitness")

fitness_usmap <- plot_usmap(data = fitness_region,
                            values = "hits",
                            regions = "states",
                            labels = TRUE,
                            label_color = "white") + 
  labs(title = "Google Trends Hits",
       subtitle = "Search Trends for Fitness") + 
  theme(legend.position = "right",
        panel.background=element_blank())


ggsave("fitness_usmap.png", fitness_usmap)


# plot us region map using plotly 
# manually selecting data for keyword

trends_region %>% 
  filter(keyword == "fitness") %>% 
  plot_geo(locationmode = 'USA-states')%>% 
  add_trace(z = ~hits,
            text = ~keyword,
            locations = ~code,
            color = ~hits,
            colors = 'Purples')%>%
  colorbar(title = "Search Interest")%>%
  layout(
    title = 'US Search Interest by State<br>(Hover for breakdown)',
    geo = g
  )
  


# creating model 
# want to look at how fitness is correllated to other key words 
# tibble with column 1 = fitness and hits below that 
# rest of columns with right hand variables and hits below them 


fitness = intercept + sleep(beta1) + health(beta2)


customer_model <- trends_region %>%
  pivot_wider(names_from = keyword,
              values_from = hits) %>% 
  mutate(model = 0.25*fitness + .5*sleep + .25*`human performance`)


g <- list(
  scope = 'usa')

try1 %>% 
  plot_geo(locationmode = 'USA-states')%>% 
  add_trace(z = ~model,
            text = "Model",
            locations = ~code,
            color = ~model,
            colors = 'Purples')%>%
  colorbar(title = "Search Interest")%>%
  layout(
    title = 'US Search Interest by State<br>(Hover for breakdown)',
    geo = list(
      scope = 'usa')
  )



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

print(fit_1, detials = FALSE )
print(fit_2, detials = FALSE )

# making a table with fits

model_table_fitness <- fit_fitness %>%
  tbl_regression(include = everything(),
                 intercept = TRUE,
                 estimate_fun = function(x) style_sigfig(x, digits = 3)) %>%
  as_gt() %>%
  tab_header(title = "A Model of 'Fitness' on other Keywords",
             subtitle = "The Results are ambigious") 

model_table_sleep <- fit_sleep %>%
  
  # tbl_regression puts a fit into a table
  
  # we need intercept so set it equal to true 
  
  tbl_regression(include = everything(),
                 intercept = TRUE,
                 estimate_fun = function(x) style_sigfig(x, digits = 3)) %>%
  
  # gt() helps put a label and title to the table 
  
  as_gt() %>%
  
  # Adding title, subtitle and source notes
  
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


# working on customer model city top 10 table

trial <- trends_city %>%
  pivot_wider(names_from = keyword,
              values_from = hits) %>% 
  mutate(model = (25*fitness)/100 + (25*sleep)/100 + (50*WHOOP)/100)%>% 
  select(City = location, Interest = model)%>% 
  as_tibble() %>% 
  arrange(desc(Interest))%>% 
  slice(1:10) %>% 

#as_gt()%>% 
#tab_header(title = "Top Interest by Cities")
  
  
# working on merging two of my gtrends tables 
# this process works 
  
  set_1 <- read.csv("gtrends_set1.csv")
  set_2 <- read.csv("gtrends_set2.csv")
  
  merged <- rbind(set_1, set_2)
  merged[is.na(merged)]<-0

  trial1 <- merged %>%
    pivot_wider(names_from = keyword,
                values_from = hits)
  
# fixing the interactive model for cities
  
  test <- trends_city %>%
    pivot_wider(names_from = keyword,
                values_from = hits,
                values_fill = 0) %>%
    select(-`health supplements`) %>% 
    select(-`improve health`) %>% 
    mutate(model =(1*`time management`)/100 + (1*`meditation`)/100 + (1*exercise)/100 + (1*Peleton)/100 + (1*WHOOP)/100) %>% 
    select(City = location, Interest = model) %>% 
    as_tibble() %>% 
    arrange(desc(Interest)) %>% 
    slice(1:10)
  
  
