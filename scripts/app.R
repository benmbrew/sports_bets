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
                                fluidRow(column(4,
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
                                fluidRow(column(12,
                                                uiOutput('over_under_picks')),
                                         fluidRow(column(4,
                                                         uiOutput('ben_box')),
                                                  column(4,
                                                         uiOutput('gabe_box')),
                                                  column(4,
                                                         uiOutput('both_box'))
                                                 
                                         )),
                                tabsetPanel(
                                  tabPanel('Table',
                                           fluidRow(column(12,
                                                           DT::dataTableOutput('nba_table')))
                                           ),
                                  tabPanel('Plot',
                                           fluidRow(column(12,
                                                           DT::dataTableOutput('nba_plot')
                                           ))),
                                  
                                  tabPanel('Map', 
                                           leafletOutput('nba_map')))))))



# date, ben_picks, gabe_picks, favorite
# Define server 
server <- function(input, output) {
  
  
  # create output obect for line_or_ou
  output$line_or_ou <- renderUI({
    if(input$type_of_bet == 'The over under'){
      sliderInput("ou_range", "The over under",
                  min = 180, 
                  max = 250,
                  value = c(185,235))
    } else if(input$type_of_bet == 'The spread'){
      sliderInput("spread_range", "The spread",
                  min = -25, 
                  max = 25,
                  value = c(-20,20))
    }
    
  })
  
  # get data for dates speccified
  get_sliders <- reactive({
    
    type_of_bet <- 'The over under'
    slider_input <- c(150, 250)
    date_picker <- c("2015-10-17", "2018-05-07")
    
    # get slider range
    if(input$type_of_bet == 'The spread') {
      slider_input <- input$spread_range
    } else if(input$type_of_bet == 'The over under') {
      slider_input <- input$ou_range
    }
    
    date_picker <- input$date_picker
    
    # betting constraint
    low_number <- slider_input[1]
    high_number <- slider_input[2]
    
    # for dates
    date_beginning <- date_picker[1]
    date_end <- date_picker[2]

    x <- dat
    x <- x[x$line >= low_number & x$line <= high_number,]
    x <- x[x$date >= date_beginning & x$date <= date_end,]

    return(x)
    
    
  })
  
  
  # create reactive object that takes 4 inputs and returns the appropriate data set
  get_data_line <- reactive({
    
    
    x <- get_sliders()
    x <- x %>% filter(over_under_fac == 'The spread')
    x <- x[, c('date', 'ben_picks', 'gabe_picks', 
               'line', 'outcome', 'money_outcome', 'under_dog', 'cum_sum')]
    colnames(x) <- c('Date', "Ben's picks", "Gabe's picks", "The spread/OU", 'Result', 'Money result', "Ben's team",
                     'Cumulative winngings')

    
    return(x)
  })

  
  # create reactive object that takes 4 inputs and returns the appropriate data set
  get_data_over_under <- reactive({
    
    x <- get_sliders()
    
    if(is.null(x)) {
      NULL
    } else {
      x <- x %>% filter(over_under_fac == 'The over under')
      x <- x[, c('date', 'ben_picks','ben_over_under_team', "gabe_over_under_team" , 
                 'line', 'outcome', 'money_outcome', 'cum_sum')]
      
      colnames(x) <- c('Date', "Ben's over/under picks" ,"Ben's picks", "Gabe's picks", 
                       "The spread/OU", 'Result', 'Money result', 'Cumulative winngings')
      
      

      
      
      return(x)
    }
    
    
  })
  
  

  # # create output obect for line_or_ou
  # output$over_under_picks<- renderUI({
  #   
  #   if(input$type_of_bet == 'The over under'){
  #     selectInput("ou_picks", 
  #                 "Ben's choice",
  #                 choices = c('Over', 'Under'))
  #   } else {
  #     NULL
  #   }
  #   
  # })
  # 

  # HERE Make sure this can be transferred to gab e box
  output$ben_box <- renderUI({
    

    type_of_bet <- 'The over under'
    ben_team <- 'All'
    # temp_data <- x
    # get inputs
    type_of_bet <- input$type_of_bet
    ben_team <- input$ben_picks

    # filter type of bet input
    if (type_of_bet == 'The spread'){
      temp_data <- get_data_line()
    } else if(type_of_bet =='The over under'){
      temp_data <- get_data_over_under()
    }
    
    if(is.null(type_of_bet) | is.null(ben_team) | is.null(temp_data)) {
      NULL
    } else {
      if (ben_team != 'All'){
        temp_data <- temp_data %>% filter(`Ben's picks` %in% ben_team)
        
      } 
      
      # criteria of under dog of over under 
      if(type_of_bet != 'The over under'){
        sum_under_dog_or_under <- nrow(temp_data[temp_data$`Ben's team` == 'Under dog',])
        under_dog_under_text <-'# of underdog bets: '
        
      }else {
        sum_under_dog_or_under <- nrow(temp_data[temp_data$`Ben's over/under picks` == 'Under',])
        under_dog_under_text <-'# of bets on the under: '
        
      }
      
      total_winnings <- sum(temp_data$`Money result`)
      if(total_winnings < 0){
        total_string <- 'Net Losses'
        mean_string <- 'Average losses per bet'
      } else{
        total_string <- 'Net winnings'
        mean_string <- 'Average winnings per bet'
        
      }
      #edit type of bet
      type_of_bet <- unlist(strsplit(type_of_bet, ' ',fixed = TRUE))[2]
      
      mean_line_ou <- round(mean(temp_data$`The spread/OU`, na.rm = TRUE),2)
      total_w <- nrow(temp_data[temp_data$Result == 'W',])
      total_l <- nrow(temp_data[temp_data$Result == 'L',])
      total_games <- total_w + total_l
      mean_winnings<- round(mean(temp_data$`Money result`, na.rm = TRUE), 2)
    
      
      valueBox(
        paste0("Ben's stats"), 
        HTML(paste0("Average ", type_of_bet, " : ", mean_line_ou,
                    "<br/> Team chosen: ", ben_team,
                    "<br/> Games played: ", total_games,
                    "<br/> Wins: ", total_w,
                    "<br/> Losses: ", total_l,
                    "<br/>", total_string, ": ", total_winnings,' $',
                    "<br/>", mean_string, ": ", mean_winnings,' $',
                    "<br/>", under_dog_under_text,  sum_under_dog_or_under)), 
        color = 'olive',
        width = 10
      )
    
    }
  })
  
  output$gabe_box <- renderUI({
    
    
    type_of_bet <- 'The over under'
    gabe_team <- 'All'
    # temp_data <- x
    # get inputs
    type_of_bet <- input$type_of_bet
    gabe_team <- input$gabe_picks
    
    # filter type of bet input
    if (type_of_bet == 'The spread'){
      temp_data <- get_data_line()
    } else if(type_of_bet =='The over under'){
      temp_data <- get_data_over_under()
    }
    
    if(is.null(type_of_bet) | is.null(gabe_team) | is.null(temp_data)) {
      NULL
    } else {
      if (gabe_team != 'All'){
        temp_data <- temp_data %>% filter(`Gabe's picks` %in% gabe_team)
        
      } 
      
      # criteria of under dog of over under 
      if(type_of_bet != 'The over under'){
        sum_under_dog_or_under <- nrow(temp_data[temp_data$`Ben's team` != 'Under dog',])
        under_dog_under_text <-'# of underdog bets: '
        
      }else {
        sum_under_dog_or_under <- nrow(temp_data[temp_data$`Ben's over/under picks` != 'Under',])
        under_dog_under_text <-'# of bets on the under: '
        
      }
      
      total_winnings <- sum(temp_data$`Money result`)
      if(total_winnings < 0){
        total_string <- 'Net Losses'
        mean_string <- 'Average losses per bet'
      } else{
        total_string <- 'Net winnings'
        mean_string <- 'Average winnings per bet'
        
      }
      #edit type of bet
      type_of_bet <- unlist(strsplit(type_of_bet, ' ',fixed = TRUE))[2]
      
      mean_line_ou <- round(mean(temp_data$`The spread/OU`, na.rm = TRUE),2)
      total_w <- nrow(temp_data[temp_data$Result == 'W',])
      total_l <- nrow(temp_data[temp_data$Result == 'L',])
      total_games <- total_w + total_l
      mean_winnings<- round(mean(temp_data$`Money result`, na.rm = TRUE), 2)
      
      
      valueBox(
        paste0("gabe's stats"), 
        HTML(paste0("Average ", type_of_bet, " : ", mean_line_ou,
                    "<br/> Team chosen: ", gabe_team,
                    "<br/> Games played: ", total_games,
                    "<br/> Wins: ", total_w,
                    "<br/> Losses: ", total_l,
                    "<br/>", total_string, ": ", total_winnings,' $',
                    "<br/>", mean_string, ": ", mean_winnings,' $',
                    "<br/>", under_dog_under_text,  sum_under_dog_or_under)), 
        color = 'olive',
        width = 10
      )
      
    }
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
    
    type_of_bet <- 'The spread'
    ben_team <- 'All'
    gabe_team <- 'All'
    temp_data <- x
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
      criteria_cumulative_winnings <- cumsum(temp_data$`Money result`)
      temp_data
    } else if (ben_team == 'All' & gabe_team != 'All') {
      temp_data <- temp_data %>% filter(`Gabe's picks` %in% gabe_team)
    } else if (ben_team != 'All' & gabe_team == 'All') {
      temp_data <- temp_data %>% filter(`Ben's picks` %in% ben_team)
    } else {
      temp_data <- temp_data %>% filter(`Ben's picks` %in% ben_team) %>% filter(`Gabe's picks` %in% gabe_team)
    }
    
    
    
  })
  


  
}

# Run the application 
shinyApp(ui = ui,
         # htmlTemplate("www/index.html"), 
         server)

