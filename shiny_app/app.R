library(shiny)
library(tidyverse)
library(DT)
source("data.R")
library(plotly)
library(shinythemes)



# Define UI for application 

ui <- navbarPage(
  "Kittbio Labs: Customer Demo",

# defining my about page

  tabPanel("About",
           fluidPage(theme = shinytheme("cosmo"),
           titlePanel("About"),
           h3("Project Background and Motivations"),
           HTML("<h5> In this project I partnered with Kittbio Labs to improve 
              their marketing efforts for their human performance products. 
              I sought out to understand the customer demographics across the 
              United States using google search trends as my tool. <h5>"),
           HTML("<h5> To learn more about how google serach trends works, visit this link 
                 <a href='https://support.google.com/trends/answer/4365533?hl=en&ref_topic=6248052'> Google Search Trends FAQ </a> <h5>"),
           HTML("<h5> The theory behind this project is that one can expect certain google
              search terms to be correlated with our customer demographic. For example, 
              the Kittbio Labs Product improves sleep quality in its clients. 
              Google search trends for the keyword improve sleep should in theory 
              provide a glimpse into potential customers across the United States.
              Within our application we include the following pages each with its
              own ability to explore data. <h5>"),
           HTML("<h5> 1. Google Search Trends: a basic analysis of search interest broken
                                       down by keyword. This data is then displayed
                                       by the Top 10 Cities as well as a national 
                                       breakdown on a US Map. <h5>"),
           HTML("<h5> 2. Customer Demo. Model: an interactive analysis allowing users
                                       to set certain weights to keywords on a 0-100 scale,
                                       modeling the potential customer demographic. Results
                                       are iprojected on a United States heat map as well 
                                       as the Top 10 States & Regions are displayed. <h5>"),
           h3("About Me"),
           HTML("<h5>My name is Dennis Blyashov and I study Economics and 
             Neuroscience. 
             You can reach me at dblyashov@college.harvard.edu.<h5>"))),
 
# defining my next page, Google Trends
# using column( width = ) to set the dimensions of the page

  tabPanel("Google Trends",
           titlePanel("Keyword Analysis"),
        fluidPage(theme = shinytheme("cosmo"),
           column(
             width = 9,
             p("Use the dropdown box to select a desired keyword. Anaylze
               google search trend interest broken down across US Cities / States.
               Hover over the US Map to examine the interest for the selected 
               keyword on a scale of 0-100")
                  ),
           column(
             width = 3,

# selectInput() used here to create a dropdown list of choices

             selectInput(
                          inputId = "keyword",
                          label = "Search Term",
                          choices = c("health related fitness", "improve focus",
                                      "improve sleep", "improve health",
                                      "health supplements","time management",
                                      "manage stress","meditation","exercise",
                                      "Peleton","WHOOP"),
                          selected = "meditation")
                  ),
           column(
             width = 3,
               h3("Interest by US city"),
               align="middle",
               tableOutput("interactive_city_table")),
           column(
             width = 9,
               h3("Interest by Region US MAP"),align="middle",
               plotlyOutput(outputId = "us_region_plot")
           ))),
  
# defining my next tab, Model
# using similar appraoch as before 

  tabPanel("Model",
      fluidPage(theme = shinytheme("cosmo"),
        titlePanel("Model Title"),
          sidebarLayout(
               sidebarPanel(
                 selectInput(
                   inputId = "model_type",
                   label = "Keyword for Regression",
                   choices = c("health related fitness", "improve focus",
                               "improve sleep", "improve health",
                               "health supplements","time management",
                               "manage stress","meditation","exercise",
                               "Peleton","WHOOP")
                             )),
                 mainPanel(
                   gt_output("table")
                           ) 
                        )
           )),
  
# defining the last tab, Customer Demo. Model
# same logic as before 

  tabPanel("Customer Demo. Model",
           titlePanel("Customer Demographic Model"),
           fluidPage(theme = shinytheme("cosmo"),
           p("Welcome to the interactive Customer Demographic Model
             page. Use the sliders below each keyword to create 
             example customer demographic models by assigning the 
             keywords a weight from 0-100. The results will be 
             translated onto a US Map broken down by states as well as
             the Top 10 States & Cities will be shown below. Please note...
             Make sure your weights add up to 100 to ensure proper analysis."),
           column(
             width = 3,
             wellPanel(
          
# sliderInput() creates a slider function to use in my model
               
             sliderInput("health_related_fitness",
                         label = "Health Related Fitness", 
                         min = 0, 
                         max = 100, 
                         value = 0),
             sliderInput("improve_focus",
                         label = "Improve Focus", 
                         min = 0, 
                         max = 100, 
                         value = 0),
             sliderInput("improve_sleep",
                         label = "Improve Sleep", 
                         min = 0, 
                         max = 100, 
                         value = 0),
             sliderInput("improve_health",
                         label = "Improve Health", 
                         min = 0, 
                         max = 100, 
                         value = 0),
             sliderInput("health_supplements",
                         label = "Health Supplements", 
                         min = 0, 
                         max = 100, 
                         value = 0),
             sliderInput("time_management",
                         label = "Time Management", 
                         min = 0, 
                         max = 100, 
                         value = 0),
             sliderInput("manage_stress",
                         label = "Manage Stress", 
                         min = 0, 
                         max = 100, 
                         value = 0),
             sliderInput("meditation",
                         label = "Meditation", 
                         min = 0, 
                         max = 100, 
                         value = 0),
             sliderInput("exercise",
                         label = "Exercise", 
                         min = 0, 
                         max = 100, 
                         value = 0),
             sliderInput("Peleton",
                         label = "Peleton", 
                         min = 0, 
                         max = 100, 
                         value = 0),
             sliderInput("WHOOP",
                         label = "WHOOP", 
                         min = 0, 
                         max = 100, 
                         value = 0)
           )),
           column(
             width = 9,align="middle",
               h3("Customer Demographic Model (US Map)"),
             plotlyOutput(outputId = "model_map")
           ),
           column(width = 1),
           column(
             width = 4, align="middle",
             h3("Top 10 States"),
             tableOutput("customer_model_regions")
           ),
           column(
             width = 4,align="middle",
             h3("Top 10 Cities"),
             tableOutput("customer_model_cities")
           ))
  
              ))

# Define server logic 

server <- function(input, output) {

  
# table for model page based on drop down choices
  
  output$table <- render_gt({
    
    if(input$model_type == "Meditation"){
    fit_meditation %>%
      tbl_regression(include = everything(),
                     intercept = TRUE,
                     estimate_fun = function(x) style_sigfig(x, digits = 3)) %>%
      as_gt() %>%
      tab_header(title = "A Model of 'Meditation' on other Keywords",
                 subtitle = "The Results are ambigious") 
    }
    else if(input$model_type == "health related fitness"){
      fit_health_related_fitness %>%
        tbl_regression(include = everything(),
                       intercept = TRUE,
                       estimate_fun = function(x) style_sigfig(x, digits = 3)) %>%
        as_gt() %>%
        tab_header(title = "A Model of 'Health Related Fitness' on other Keywords",
                   subtitle = "The Results are ambigious")
    }
    else if(input$model_type == "exercise"){
      fit_exercise %>%
        tbl_regression(include = everything(),
                       intercept = TRUE,
                       estimate_fun = function(x) style_sigfig(x, digits = 3)) %>%
        as_gt() %>%
        tab_header(title = "A Model of 'Exercise' on other Keywords",
                   subtitle = "The Results are ambigious")
    }
    else if(input$model_type == "health supplements"){
      fit_health_supplements %>%
        tbl_regression(include = everything(),
                       intercept = TRUE,
                       estimate_fun = function(x) style_sigfig(x, digits = 3)) %>%
        as_gt() %>%
        tab_header(title = "A Model of 'Health Supplements' on other Keywords",
                   subtitle = "The Results are ambigious")
    }
    else if(input$model_type == "improve focus"){
      fit_improve_focus %>%
        tbl_regression(include = everything(),
                       intercept = TRUE,
                       estimate_fun = function(x) style_sigfig(x, digits = 3)) %>%
        as_gt() %>%
        tab_header(title = "A Model of 'Improve Focus' on other Keywords",
                   subtitle = "The Results are ambigious")
    }
    else if(input$model_type == "improve health"){
      fit_improve_health %>%
        tbl_regression(include = everything(),
                       intercept = TRUE,
                       estimate_fun = function(x) style_sigfig(x, digits = 3)) %>%
        as_gt() %>%
        tab_header(title = "A Model of 'Improve Health' on other Keywords",
                   subtitle = "The Results are ambigious")
    }
    else if(input$model_type == "improve sleep"){
      fit_improve_sleep %>%
        tbl_regression(include = everything(),
                       intercept = TRUE,
                       estimate_fun = function(x) style_sigfig(x, digits = 3)) %>%
        as_gt() %>%
        tab_header(title = "A Model of 'Improve Sleep' on other Keywords",
                   subtitle = "The Results are ambigious")
    }
    else if(input$model_type == "manage stress"){
      fit_manage_stress %>%
        tbl_regression(include = everything(),
                       intercept = TRUE,
                       estimate_fun = function(x) style_sigfig(x, digits = 3)) %>%
        as_gt() %>%
        tab_header(title = "A Model of 'Manage Stress' on other Keywords",
                   subtitle = "The Results are ambigious")
    }
    else if(input$model_type == "time management"){
      fit_time_management %>%
        tbl_regression(include = everything(),
                       intercept = TRUE,
                       estimate_fun = function(x) style_sigfig(x, digits = 3)) %>%
        as_gt() %>%
        tab_header(title = "A Model of 'Time Management' on other Keywords",
                   subtitle = "The Results are ambigious")
    }
    else if(input$model_type == "Peleton"){
      fit_Peleton %>%
        tbl_regression(include = everything(),
                       intercept = TRUE,
                       estimate_fun = function(x) style_sigfig(x, digits = 3)) %>%
        as_gt() %>%
        tab_header(title = "A Model of 'Peleton' on other Keywords",
                   subtitle = "The Results are ambigious")
    }
    else if (input$model_type == "WHOOP"){
      fit_WHOOP %>%
        tbl_regression(include = everything(),
                       intercept = TRUE,
                       estimate_fun = function(x) style_sigfig(x, digits = 3)) %>%
        as_gt() %>%
        tab_header(title = "A Model of 'WHOOP' on other Keywords",
                   subtitle = "The Results are ambigious")
    }
  })

# interactive city top 10 table using inputID = term

  output$interactive_city_table <- renderTable({
  
    trends_city %>%
      filter(keyword == input$keyword) %>%
      as_tibble() %>%
      arrange(desc(hits)) %>% 
      slice(1:10) %>% 
      select(City = location, Interest = hits)
  
})
  
# interactive US map using keyword dropdown
  
  output$us_region_plot <- renderPlotly({
    
    
    # specify some map projection/options
    g <- list(
      scope = 'usa')
    
    trends_region %>% 
      filter(keyword == input$keyword) %>% 
      plot_geo(locationmode = 'USA-states')%>% 
      add_trace(z = ~hits,
                text = ~keyword,
                locations = ~code,
                color = ~hits,
                colors = 'Blues')%>%
      colorbar(title = "Search Interest")%>%
      layout(
        geo = list(
          scope = 'usa')
      )
  })
  
# creating a US map using slider inputs in the model
  
  output$model_map <- renderPlotly({
    
    trends_region %>%
      pivot_wider(names_from = keyword,
                  values_from = hits) %>% 
      mutate(model = (input$health_related_fitness*`health related fitness`)/100 + (input$improve_focus*`improve focus`)/100 + (input$improve_sleep*`improve sleep`)/100 + (input$improve_health*`improve health`)/100 + (input$health_supplements*`health supplements`)/100 + (input$time_management*`time management`)/100 + (input$manage_stress*`manage stress`)/100 + (input$meditation*meditation)/100 + (input$exercise*exercise)/100 + (input$Peleton*Peleton)/100 + (input$WHOOP*WHOOP)/100) %>% 
      plot_geo(locationmode = 'USA-states')%>% 
      add_trace(z = ~model,
                text = "Model",
                locations = ~code,
                color = ~model,
                colors = 'Blues',
                zmin = 0,
                zmax = 100)%>%
      colorbar(title = "Search Interest")%>%
      layout(
        geo = list(
          scope = 'usa')
      )
    
  })
  
  
# adding top 10 regions table with new model data
  
  output$customer_model_regions <- renderTable({
    
    trends_region %>%
      pivot_wider(names_from = keyword,
                  values_from = hits) %>% 
      mutate(model = (input$health_related_fitness*`health related fitness`)/100 + (input$improve_focus*`improve focus`)/100 + (input$improve_sleep*`improve sleep`)/100 + (input$improve_health*`improve health`)/100 + (input$health_supplements*`health supplements`)/100 + (input$time_management*`time management`)/100 + (input$manage_stress*`manage stress`)/100 + (input$meditation*meditation)/100 + (input$exercise*exercise)/100 + (input$Peleton*Peleton)/100 + (input$WHOOP*WHOOP)/100) %>% 
      select(State = state, Interest = model) %>% 
      as_tibble() %>% 
      arrange(desc(Interest)) %>% 
      slice(1:10)
    
  })
  

# adding top 10 cities table with new model data
# needed to include values_fill = 0 for math to add up 
  
  output$customer_model_cities <- renderTable({
    
    trends_city %>%
      pivot_wider(names_from = keyword,
                  values_from = hits,
                  values_fill = 0) %>%
      select(-`health supplements`) %>% 
      select(-`improve health`) %>% 
      mutate(model =(input$time_management*`time management`)/100 + (input$meditation*meditation)/100 + (input$exercise*exercise)/100 + (input$Peleton*Peleton)/100 + (input$WHOOP*WHOOP)/100) %>% 
      select(City = location, Interest = model) %>% 
      as_tibble() %>% 
      arrange(desc(Interest)) %>% 
      slice(1:10)
    
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
