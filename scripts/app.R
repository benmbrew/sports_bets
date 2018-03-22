library(shiny)
library(googleVis)
library(DT)
library(RColorBrewer)
library(networkD3)
options(gvis.plot.tag = 'chart')
library(shinyBS)
library(shinyLP)
library(ggplot2)
library(shinythemes)
library(shinydashboard)
library(readr)
library(dplyr)
library(tidyr)
library(broom)
library(memisc)
library(leaflet)
library(plotly)

source('read_in.R')

# add tab for raw data
# summary stats second 
# add in more group

ui <- dashboardPage(skin = 'red',
                    
                    
                    dashboardHeader(
                      title = "Betting with Gaybe",
                      titleWidth = 300
                    ),
                    
                    dashboardSidebar(width = 300,
                                     
                                     sidebarMenu(
                                       menuItem('Overview',
                                                icon = icon(''),
                                                tabName = 'overview'),
                                       menuItem('Betting summary statistics',
                                                icon = icon('table'),
                                                tabName = 'nba_stats'))),
                    dashboardBody(
                      tags$head(
                        tags$link(rel = "stylesheet", type = "text/css", href = "custom.css")
                      ),
                      tabItems(
                        tabItem(tabName = 'overview',
                                fluidRow(h2('All NBA bets since Oct, 2017'), 
                                         align = 'center'),
                                fluidRow(column(12,
                                                textOutput('total_text'))
                                                ),
                                fluidRow(column(6,
                                                leafletOutput('cumulative_plot')),
                                         column(6,
                                                plotOutput('bet_plot')))
                                ),
                        tabItem(tabName = "nba_stats",
                                fluidRow(column(6,
                                                selectInput('type_of_bet',
                                                            "Type of bet",
                                                            choices = c('The spread', 'The over under'))),
                                         column(6,
                                                dateRangeInput('date_picker',
                                                               'Choose Dates',
                                                               start = min(dat$date),
                                                               end = max(dat$date),
                                                               format = 'yyyy/mm/dd',
                                                               separator = '--',
                                                               startview = "Month"))),
                                fluidRow(
                                         column(4,
                                                selectInput('ben_picks',
                                                              "Ben's picks",
                                                            choices = nba_teams,
                                                            multiple = FALSE)),
                                         column(4,
                                                selectInput('gabe_picks',
                                                              "Gabe's picks",
                                                            choices = nba_teams,
                                                            multiple = FALSE)),
                                         column(4,
                                                uiOutput('line_or_ou'))),
                                
                                tabsetPanel(
                                  tabPanel('Table',
                                           fluidRow(column(12,
                                                           DT::dataTableOutput('nba_table')
                                           ))),
                                  tabPanel('Plot',
                                           fluidRow(column(12,
                                                           DT::dataTableOutput('nba_plot')
                                           ))),
                                  
                                  tabPanel('Map', 
                                           leafletOutput('nba_map')))))))



# date, ben_picks, gabe_picks, favorite
# Define server 
server <- function(input, output) {
  
  #
  
  # create output obect for line_or_ou
  output$line_or_ou <- renderUI({
    if(input$type_of_bet == 'The over under'){
      sliderInput("ou_range", "The over under",
                  min = 180, 
                  max = 250,
                  value = c(190,230))
    } else if(input$type_of_bet == 'The spread'){
      sliderInput("spread_range", "The spread",
                  min = -20, 
                  max = 20,
                  value = c(-10,10))
    }
    
  })
  
  # get data for dates speccified
  get_sliders <- reactive({
    # type_of_bet, date_picker, ben_picks, gabe_picks
    # date_beginning <- "2017-10-17"
    # date_end <- "2018-03-10"
    
    # get slider range
    if(input$type_of_bet == 'The spread') {
      slider_input <- input$spread_range
    } else if(input$type_of_bet == 'The over under') {
      slider_input <- input$ou_range
    }
    
    # betting constraint
    low_number <- slider_input[1]
    high_number <- slider_input[2]
    
    # for dates
    date_beginning <- input$date_picker[1]
    date_end <- input$date_picker[2]

    x <- dat
    x <- x[x$line >= low_number & x$line <= high_number,]
    x <- x[x$date >= date_beginning & x$date <= date_end,]
    x$cum_sum_date <- cumsum(x$money_outcome)
    
    return(x)
    
    
  })
  
  
  # create reactive object that takes 4 inputs and returns the appropriate data set
  get_data_line <- reactive({
    
    
    x <- get_sliders()
    x <- x %>% filter(over_under_fac == 'The line')
    x$cum_sum_line <- cumsum(x$money_outcome)
    
    return(x)
  })

  
  # create reactive object that takes 4 inputs and returns the appropriate data set
  get_data_over_under <- reactive({
    
    x <- get_sliders()
    x <- x %>% filter(over_under_fac == 'Over/under')
    x$cum_sum_over_under <- cumsum(x$money_outcome)
    
    return(x)
    
  })
  
  # # create full data set from the other two
  # get_data_all <- reactive({
  #   
  #   data_line <- get_data_line()
  #   data_ou <- get_data_over_under()
  #   
  #   # remove line and over under spefic vars
  #   data_line$cum_sum_line <- NULL
  #   data_ou$cum_sum_over_under <- NULL
  #   temp_data <- rbind(data_line, data_ou)
  #   temp_data <- temp_data[order(temp_data$date),]
  #   
  # })
  # 
  output$nba_table <- renderDataTable({
    
    # get inputs
    type_of_bet <- input$type_of_bet
    ben_team <- input$ben_picks
    gabe_team <-input$gabe_picks
    
    # filter type of bet input
    if (type_of_bet == 'The spread'){
      temp_data <- get_data_line()
    } else if(type_of_bet =='The over under'){
      temp_data <- get_data_over_under()
    }
    
    if (ben_team == 'All' & gabe_team == 'All'){
      temp_data <- temp_data
    } else if (ben_team == 'All' & gabe_team != 'All') {
      temp_data <- temp_data %>% filter(gabe_picks %in% gabe_team)
    } else if (ben_team != 'All' & gabe_team == 'All') {
      temp_data <- temp_data %>% filter(ben_picks %in% ben_team)
    } else {
      both_picks <- c(ben_team, gabe_team)
      temp_data <- temp_data %>% filter(ben_picks %in% both_picks)
    }
    
    temp_data
    
  })
  


  
}

# Run the application 
shinyApp(ui = ui,
         # htmlTemplate("www/index.html"), 
         server)

