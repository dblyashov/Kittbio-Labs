library(gtrendsR)
library(tidyverse)
library(usmap)
library(plotly)
source("data.R")
library(rstanarm)


# interest by city for "fitness"


# top10_fitness_city <- trends_city %>% 
#  filter(keyword == "fitness") %>%
#  as_tibble() %>% 
#  slice(1:10) %>% 
#  select(City = location, Interest = hits)

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


fit_1 <- stan_glm( data = linear_regress,
                   formula = fitness ~ ....
                   refresh = 0)


print(fit_1, detials = FALSE )
