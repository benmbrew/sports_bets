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
                                                uiOutput('over_under_picks'))),
                                tabsetPanel(
                                  tabPanel('Table',
                                           fluidRow(column(6,
                                                           uiOutput('box_current')),
                                                    column(6,
                                                           uiOutput('box_bet_type'))),
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
    
    type_of_bet <- 'The spread'
    slider_input <- c(150, 300)
    date_picker <- c("2015-10-17", "2018-03-17")
    
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
    x <- x %>% filter(over_under_fac == 'The over under')
    x <- x[, c('date', 'ben_picks','ben_over_under_team', "gabe_over_under_team" , 
               'line', 'outcome', 'money_outcome', 'cum_sum')]
    
    colnames(x) <- c('Date', "Ben's over/under picks" ,"Ben's picks", "Gabe's picks", 
                     "The spread/OU", 'Result', 'Money result', 'Cumulative winngings')
    
    ou_picks <- input$ou_picks
    
    x <- x[x$`Ben's over/under picks` == ou_picks,]

    
    
    return(x)
    
  })
  
  

  # create output obect for line_or_ou
  output$over_under_picks<- renderUI({
    
    if(input$type_of_bet == 'The over under'){
      selectInput("ou_picks", 
                  "Ben's choice",
                  choices = c('Over', 'Under'))
    } else {
      NULL
    }
    
  })
  

  # HERE - total_money, total_spread, total_over_under
  output$box_current <- renderUI({
    
    # input
    type_of_bet <- input$type_of_bet
    ben_team <- input$ben_picks
    gabe_team <-input$gabe_picks
    
    temp_data <- dat

    if (ben_team == 'All' & gabe_team == 'All'){
      temp_data <- temp_data
    } else if (ben_team == 'All' & gabe_team != 'All') {
      temp_data <- temp_data %>% filter(gabe_picks %in% gabe_team)
    } else if (ben_team != 'All' & gabe_team == 'All') {
      temp_data <- temp_data %>% filter(ben_picks %in% ben_team)
    } else {
      temp_data <- temp_data %>% filter(ben_picks %in% ben_team) %>% filter(gabe_picks %in% gabe_team)
    }
    
    total_money <- sum(temp_data$money_outcome, na.rm = TRUE)
    if(total_money > 0){
      valueBox(
        paste0('Ben +', total_money), 
        paste0('All bets'), 
        color = 'olive',
        width = 12
      )
    } else {
      valueBox(
        paste0('Gabe +', abs(total_money)), 
        paste0('All bets'), 
        color = 'olive',
        width = 12
      )
    }
    
    
  })
  
  output$box_bet_type <- renderUI({
    
    # input
    type_of_bet <- input$type_of_bet
    ben_team <- input$ben_picks
    gabe_team <-input$gabe_picks

    temp_data <- dat[dat$over_under_fac == type_of_bet, ]
    
    if (ben_team == 'All' & gabe_team == 'All'){
      temp_data <- temp_data
    } else if (ben_team == 'All' & gabe_team != 'All') {
      temp_data <- temp_data %>% filter(gabe_picks %in% gabe_team)
    } else if (ben_team != 'All' & gabe_team == 'All') {
      temp_data <- temp_data %>% filter(ben_picks %in% ben_team)
    } else {
      temp_data <- temp_data %>% filter(ben_picks %in% ben_team) %>% filter(gabe_picks %in% gabe_team)
    }
    
    total_money <- sum(temp_data$money_outcome, na.rm = TRUE)
    if(total_money > 0){
      valueBox(
        paste0('Ben +', total_money, '$'), 
        paste0('by ', type_of_bet), 
        color = 'navy',
        width = 12
      )
    } else if(total_money == 0) {
      valueBox(
        paste0('Even'), 
        paste0('suck it'),
        color = 'navy',
        width = 12
      )
    } else {
      valueBox(
        paste0('Gabe +', abs(total_money), '$'), 
        paste0('by ', type_of_bet), 
        color = 'navy',
        width = 12
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
    
    type_of_bet <- 'The over under'
    ben_team <- 'All'
    gabe_team <- 'All'
    # temp_data <- x
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

