
library(shiny)
library(tidyverse)
library(DT)
source("data.R")
library(plotly)
library(shinythemes)



# Define UI for application that draws a histogram
ui <- navbarPage(
  "Kittbio Labs: Customer Demo",
  
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
              own ability to explore data. 
              
              1. Google Search Trends: a basic analysis of search interest broken
                                       down by keyword. This data is then displayed
                                       by the Top 10 Cities as well as a national 
                                       breakdown on a US Map. 
              
              
              
              
               To begin the anaylsis certain keyterms trends
              are pulled from Google Search Trends and plotted by 'Interest
              across Cities' as well as 'Interest by State' on a map of the US. 
              The ability to explore a set of keywords is avaliable in the 
              'Google Search Trends' tab. 
              
              Secondly, I attempted to create an adjustable value weighted model
              for sets of keywords. This allows the user to change values under
              each keyword to explore the best model exibiting the customer 
              demographic. The 'heat map' for the user created model is then 
              plotted across states on a US map.<h5>")
              
              ,
           h3("About Me"),
           numericInput(
             inputId = "trial",
             label = "trial",
             value = 0,
             min = 0,
             max = 100,
             step = 1,
             width = NULL
           ),
           p("My name is Dennis Blyashov and I study Economics and 
             Neuroscience. 
             You can reach me at dblyashov@college.harvard.edu."))),
  
  tabPanel("Google Trends",
           titlePanel("Keyword Analysis"),
        fluidPage(theme = shinytheme("cosmo"),
           column(
             width = 9,
             p("Select out of the dropdown list a google search term to analyze
                 by interest across city / US states")
                  ),
           column(
             width = 3,
             selectInput(
                          inputId = "keyword",
                          label = "Search Term",
                          choices = c("fitness", "sleep", "human performance", "WHOOP"),
                          selected = "fitness")
                  ),
           column(
             width = 4,
             wellPanel(
               h3("Interest by US city"),
               tableOutput("interactive_city_table"))
                  ),
           column(
             width = 8,
             wellPanel(
               h3("Interest by Region US MAP"),
               plotlyOutput(outputId = "us_region_plot")
                  )
           ))),
  
  tabPanel("Model",
      fluidPage(theme = shinytheme("cosmo"),
        titlePanel("Model Title"),
          sidebarLayout(
               sidebarPanel(
                 selectInput(
                   "plot_type",
                   "Plot Type",
                   c("Option A" = "a", "Option B" = "b")
                             )),
                 mainPanel(
    
                           ) 
                        )
           )),
  
  tabPanel("Customer Demo. Model",
           titlePanel("Customer Demographic Model"),
           fluidPage(theme = shinytheme("cosmo"),
           p("Weight the different keywords with a value
             between 0-100 to represent a potential customer
             demographic. Make sure values add up to 100"),
           column(
             width = 3,
             sliderInput("fitness",
                         label = "Fitness", 
                         min = 0, 
                         max = 100, 
                         value = 0),
             sliderInput("sleep",
                         label = "Sleep", 
                         min = 0, 
                         max = 100, 
                         value = 0),
             sliderInput("human_performance",
                         label = "Human Performance", 
                         min = 0, 
                         max = 100, 
                         value = 0),
             sliderInput("WHOOP",
                         label = "WHOOP", 
                         min = 0, 
                         max = 100, 
                         value = 0)
           ),
           column(
             width = 9,
               h3("Model Map"),
             plotlyOutput(outputId = "model_map")
           ))
  
              ))

# Define server logic required to draw a histogram

server <- function(input, output) {


# interactive city top 10 table using inputID = term

  output$interactive_city_table <- renderTable({
  
    trends_city %>%
      filter(keyword == input$keyword) %>%
      as_tibble() %>% 
      slice(1:10) %>% 
      select(City = location, Interest = hits)
  
})
  
# interactive US map 
  
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
                colors = 'Purples')%>%
      colorbar(title = "Search Interest")%>%
      layout(
        geo = list(
          scope = 'usa')
      )
  })
  
  
  # slider outputs
  
  # output$value <- renderPrint({ input$fitness })
# making a table with slider inputs 
  
  output$model_map <- renderPlotly({
    
    trends_region %>%
      pivot_wider(names_from = keyword,
                  values_from = hits) %>% 
      mutate(model = (input$fitness*fitness)/100 + (input$sleep*sleep)/100 + (input$human_performance*`human performance`)/100 + (input$WHOOP*WHOOP)/100) %>% 
      plot_geo(locationmode = 'USA-states')%>% 
      add_trace(z = ~model,
                text = "Model",
                locations = ~code,
                color = ~model,
                colors = 'Purples',
                zmin = 0,
                zmax = 100)%>%
      colorbar(title = "Search Interest")%>%
      layout(
        geo = list(
          scope = 'usa')
      )
    
  })
  
  
}

# Run the application 
shinyApp(ui = ui, server = server)
