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

ui <- dashboardPage(skin = 'black',
                    
                    
                    dashboardHeader(
                      title = "Betting with Gaybe",
                      titleWidth = 200
                    ),
                    
                    dashboardSidebar(width = 200,
                                     
                                     sidebarMenu(
                                       menuItem('NBA bets',
                                                icon = icon('table'),
                                                tabName = 'nba_stats'))),
                    dashboardBody(
                      tags$head(
                        tags$link(rel = "stylesheet", type = "text/css", href = "custom.css")
                      ),
                      tabItems(
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
                                         fluidRow(column(6,
                                                         uiOutput('ben_box')),
                                                  column(6,
                                                         uiOutput('gabe_box'))),
                                         fluidRow(
                                           column(2),
                                           column(8,
                                                  uiOutput('both_box')),
                                           column(2)
                                         )),
                                tabsetPanel(
                                  tabPanel('Overtime',
                                           radioButtons('view_as',
                                                        'View as',
                                                        choices = c('Ben', 'Gabe'), 
                                                        selected = 'Gabe',
                                                        inline = TRUE),
                                           fluidRow(column(12,
                                                           plotlyOutput('nba_time_plot')))
                                           ),
                                  tabPanel('More visualizations',
                                           fluidRow(column(12,
                                                           plotlyOutput('nba_bar_plot')
                                           ))),
                                  
                                  tabPanel('Table', 
                                           leafletOutput('nba_table')))))))



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
    
    type_of_bet <- 'The spread'
    slider_input <- c(-20, 20)
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
                     'Cumulative winnings')

    
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
                       "The spread/OU", 'Result', 'Money result', 'Cumulative winnings')
      
      

      
      
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
      type_of_bet <- 'spread'
      
    } else if(type_of_bet =='The over under'){
      temp_data <- get_data_over_under()
      type_of_bet <- 'over under'
      
    }
    
    if(is.null(type_of_bet) | is.null(ben_team) | is.null(temp_data)) {
      NULL
    } else {
      if (ben_team != 'All'){
        temp_data <- temp_data %>% filter(`Ben's picks` %in% ben_team)
        
      } 
      
      # criteria of under dog of over under 
      if(type_of_bet == 'spread'){
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
      mean_line_ou <- round(mean(temp_data$`The spread/OU`, na.rm = TRUE),2)
      total_w <- nrow(temp_data[temp_data$Result == 'W',])
      total_l <- nrow(temp_data[temp_data$Result == 'L',])
      total_games <- total_w + total_l
      mean_winnings<- round(mean(temp_data$`Money result`, na.rm = TRUE), 2)
    
      # get custom title 
      if(ben_team == 'All'){
        ben_sub <- "For all teams"
      } else {
        ben_sub <-paste0("When Ben chooses the ", ben_team)
      }
      
      valueBox(
        paste0("Ben's stats"), 
        HTML(paste0(ben_sub,
                    "<br/> Average ", type_of_bet, " : ", mean_line_ou,
                    "<br/> Games played: ", total_games,
                    "<br/> Wins: ", total_w,
                    "<br/> Losses: ", total_l,
                    "<br/>", total_string, ": ", total_winnings,' $',
                    "<br/>", mean_string, ": ", mean_winnings,' $',
                    "<br/>", under_dog_under_text,  sum_under_dog_or_under)), 
        color = 'navy',
        width = 10
      )
    
    }
  })
  
  
  # HERE Make sure this can be transferred to gab e box
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
      type_of_bet <- 'spread'
      
    } else if(type_of_bet =='The over under'){
      temp_data <- get_data_over_under()
      type_of_bet <- 'over under'
      
    }
    
    if(is.null(type_of_bet) | is.null(gabe_team) | is.null(temp_data)) {
      NULL
    } else {
      if (gabe_team != 'All'){
        temp_data <- temp_data %>% filter(`Gabe's picks` %in% gabe_team)
      } 
      #HERE
      # criteria of under dog of over under 
      if(type_of_bet == 'spread'){
        sum_under_dog_or_under <- nrow(temp_data[temp_data$`Ben's team` != 'Under dog',])
        under_dog_under_text <-'# of underdog bets: '
        
      }else {
        sum_under_dog_or_under <- nrow(temp_data[temp_data$`Ben's over/under picks` != 'Under',])
        under_dog_under_text <-'# of bets on the under: '
        
      }
      
      # multiply by -1 to get the inverse
      temp_data$`Money result gabe` <- (temp_data$`Money result`)*(-1)
      
      total_winnings <- sum(temp_data$`Money result gabe`)
      if(total_winnings < 0){
        total_string <- 'Net Losses'
        mean_string <- 'Average losses per bet'
      } else{
        total_string <- 'Net winnings'
        mean_string <- 'Average winnings per bet'
        
      }
      #edit type of bet
      mean_line_ou <- round(mean(temp_data$`The spread/OU`, na.rm = TRUE),2)
      total_w <- nrow(temp_data[temp_data$Result == 'L',])
      total_l <- nrow(temp_data[temp_data$Result == 'W',])
      total_games <- total_w + total_l
      mean_winnings<- round(mean(temp_data$`Money result gabe`, na.rm = TRUE), 2)
      
      # get custom title 
      if(gabe_team == 'All'){
        gabe_sub <- "For all teams"
      } else {
        gabe_sub <-paste0("When Gabe chooses the ", gabe_team)
      }
      
      valueBox(
        paste0("Gabe's stats"), 
        HTML(paste0(gabe_sub,
                    "<br/> Average ", type_of_bet, " : ", mean_line_ou,
                    "<br/> Games played: ", total_games,
                    "<br/> Wins: ", total_w,
                    "<br/> Losses: ", total_l,
                    "<br/>", total_string, ": ", total_winnings,' $',
                    "<br/>", mean_string, ": ", mean_winnings,' $',
                    "<br/>", under_dog_under_text,  sum_under_dog_or_under)), 
        color = 'blue',
        width = 10
      )
      
    }
  })
  
  # Valid colors are: red, yellow, aqua, blue, light-blue, green, navy, teal, olive, lime, orange, fuchsia, purple, maroon, black.
  
  # if the teams chosen match both
  output$both_box <- renderUI({
   
    ben_team <- 'Cavs'
    gabe_team <- 'Pistons'
    type_of_bet <- 'The spread'
    type_of_bet <- input$type_of_bet
    ben_team <- input$ben_picks
    gabe_team <- input$gabe_picks
    
    if(is.null(ben_team) | is.null(gabe_team) | ben_team == 'All' | gabe_team == 'All') {
      NULL
    } else {
    
      # filter type of bet input
      if (type_of_bet == 'The spread'){
        temp_data <- get_data_line()
        type_of_bet <- 'spread'
        
      } else if(type_of_bet =='The over under'){
        temp_data <- get_data_over_under()
        type_of_bet <- 'over under'
        
      }
      
      # see if data exists for the criteria
      temp <- temp_data %>% filter(`Ben's picks` %in% ben_team) %>% filter(`Gabe's picks` %in% gabe_team)
      
      if(nrow(temp) > 0) {
        
        # get temp discriptive objects
      
        matchup_teams <- paste0(ben_team, ' vs ', gabe_team)
        num_games <- nrow(temp)
        mean_line_ou <- round(mean(temp_data$`The spread/OU`, na.rm = TRUE),2)
        ben_w <- nrow(temp[temp$Result == 'W',])
        gabe_w <- nrow(temp[temp$Result == 'L',])
        num_push <- nrow(temp[temp$Result == 'P',])
        
        if(ben_w > gabe_w) {
          win_text <- 'Total winnings for Ben: '
          
          total_winnings <- sum(temp$`Money result`)
          
        } else {
          win_text <- 'Total winnings for Gabe: '
          
          temp$`Money result gabe` <- (temp$`Money result`)*(-1)
          total_winnings <- sum(temp$`Money result gabe`)
          
        }
       
        
        valueBox(
          paste0(matchup_teams), 
          HTML(paste0("When Ben chooses the ", ben_team ," and Gabe chooses the ", gabe_team, 
                      "<br/> Number of times this has happened = ", num_games,
                      "<br/> The Average ", type_of_bet, "when this happens is ", mean_line_ou,
                      "<br/> Ben has won this matchup ", ben_w, ' times ',
                      "<br/> Gabe has won this matchup ", gabe_w,' times ',
                      "<br/> Number of ties ", num_push,
                      "<br/>", win_text, total_winnings, '$')), 
          color = 'red',
          width = 10
        )
      } else {
        return(NULL)
      }
     
      
    }
      
  
  })
  
  # Valid colors are: red, yellow, aqua, blue, light-blue, green, navy, teal, olive, lime, orange, fuchsia, purple, maroon, black.
 
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
  output$nba_time_plot <- renderPlotly({
    
   
    # get inputs
    type_of_bet <- input$type_of_bet
    ben_team <- input$ben_picks
    gabe_team <-input$gabe_picks
    view_as <- input$view_as
    
    if(is.null(type_of_bet) | is.null(ben_team) | is.null(gabe_team) | is.null(view_as)) {
      g <-  ggplot() + 
        theme_base() +
        labs(title = 'You must select a location plot')
      
      g_plot <- plotly::ggplotly(g, tooltip = 'text') %>% config(displayModeBar = F) %>%
        layout(showlegend = F)
      return(g_plot)
      
      
    } else {
      
      if(view_as != 'Ben'){
        
        if(type_of_bet == 'The spread'){
          temp_data <- get_data_line()
          temp_data$`The spread/OU` <- temp_data$`The spread/OU`*(-1)
        } else {
          temp_data <- get_data_over_under()
        }
        
        # get rid of cumulative winnings column
        temp_data$`Cumulative winngings` <- NULL
        # change data to fit gabe
        temp_data$`Money result` <- temp_data$`Money result`*(-1)
        # reverse results 
        temp_data$Result <- ifelse(temp_data$Result == 'W', 'L', 
                                   ifelse(temp_data$Result == 'L', 'W', temp_data$Result))
        
        # select team
        if(gabe_team != 'All'){
          temp_data <- temp_data %>% filter(`Gabe's picks` %in% gabe_team)
          temp_data$`Cumulative winnings` <- cumsum(temp_data$`Money result`)
        } else {
          temp_data$`Cumulative winnings` <- cumsum(temp_data$`Money result`)
        }
        
        # subset to necessary columns 
        temp_data <- temp_data[, c('Date', "Ben's picks","Gabe's picks", "The spread/OU", "Cumulative winnings")]
        temp_data <- as.data.frame(temp_data)
        
        # create variable to id game 
        temp_data$game <- paste0(temp_data$`Gabe's picks`, ' vs ', temp_data$`Ben's picks`)
        temp_data$`Ben's picks` <- NULL
        
        
        # plot data with points, line, and maybe smooth
        g <- ggplot(data = temp_data,
                    aes(x = Date,
                        y = `Cumulative winnings`,
                        group = 1,
                        text = paste0('Cumulative winnings at ', Date, ' = ', `Cumulative winnings`,
                                      '<br> Game: ', game))) + 
          geom_point(size = 3, alpha = 0.5, col = 'blue') +
          geom_smooth(size = 1, col = 'darkgrey', se = FALSE, linetype = 'dash', alpha = 0.4) +
          labs(x = '', y = '$', 
               title = 'Gabe cumulative winnings over time')
        
        
        g_plot <- plotly::ggplotly(g, tooltip = 'text') %>% config(displayModeBar = F) %>%
          layout(showlegend = F)
      } else {
        
        if(type_of_bet == 'The spread'){
          temp_data <- get_data_line()
        } else {
          temp_data <- get_data_over_under()
        }
        
        # get rid of cumulative winnings column
        temp_data$`Cumulative winngings` <- NULL
        
        # select team
        if(ben_team != 'All'){
          temp_data <- temp_data %>% filter(`Ben's picks` %in% ben_team)
          temp_data$`Cumulative winnings` <- cumsum(temp_data$`Money result`)
        } else {
          temp_data$`Cumulative winnings` <- cumsum(temp_data$`Money result`)
        }
        
        # subset to necessary columns 
        temp_data <- temp_data[, c('Date', "Ben's picks","Gabe's picks", "The spread/OU", "Cumulative winnings")]
        temp_data <- as.data.frame(temp_data)
        
    
        # create variable to id game 
        temp_data$game <- paste0(temp_data$`Ben's picks`, ' vs ', temp_data$`Gabe's picks`)
        temp_data$`Gabe's picks` <- NULL
        
        
        # plot data with points, line, and maybe smooth
        g <- ggplot(data = temp_data,
                    aes(x = Date,
                        y = `Cumulative winnings`,
                        group = 1,
                        text = paste0('Cumulative winnings at ', Date, ' = ', `Cumulative winnings`,
                                      '<br> Game: ', game))) + 
          geom_point(size = 3, alpha = 0.5, col = 'blue') +
          geom_smooth(size = 1, col = 'darkgrey', se = FALSE, linetype = 'dash', alpha = 0.4) +
          labs(x = '', y = '$', 
               title = 'Ben cumulative winnings over time')
        
        
        g_plot <- plotly::ggplotly(g, tooltip = 'text') %>% config(displayModeBar = F) %>%
          layout(showlegend = F)
        
        return(g_plot)
        
        
      }
    }
      
    
   
    
  })
  
  # output$nba_bar_plot <- renderPlotly({
  #   
  #   type_of_bet <- 'The spread'
  #   ben_team <- 'All'
  #   gabe_team <- 'All'
  #   temp_data <- x
  #   # get inputs
  #   type_of_bet <- input$type_of_bet
  #   ben_team <- input$ben_picks
  #   gabe_team <-input$gabe_picks
  #   
  #   # filter type of bet input
  #   if (type_of_bet == 'The spread'){
  #     temp_data <- get_data_line()
  #   } else if(type_of_bet =='The over under'){
  #     temp_data <- get_data_over_under()
  #   }
  #   
  #   if (ben_team == 'All' & gabe_team == 'All'){
  #     temp_data$cum_sum<- cumsum(temp_data$`Money result`)
  #   } else if (ben_team == 'All' & gabe_team != 'All') {
  #     temp_data <- temp_data %>% filter(`Gabe's picks` %in% gabe_team)
  #   } else if (ben_team != 'All' & gabe_team == 'All') {
  #     temp_data <- temp_data %>% filter(`Ben's picks` %in% ben_team)
  #   } else {
  #     temp_data <- temp_data %>% filter(`Ben's picks` %in% ben_team) %>% filter(`Gabe's picks` %in% gabe_team)
  #   }
  #   
  #   
  #   
  # })
  
  
# 
#   output$nba_table <- renderDataTable({
#     
#     type_of_bet <- 'The spread'
#     ben_team <- 'All'
#     gabe_team <- 'All'
#     temp_data <- x
#     # get inputs
#     type_of_bet <- input$type_of_bet
#     ben_team <- input$ben_picks
#     gabe_team <-input$gabe_picks
#     
#     # filter type of bet input
#     if (type_of_bet == 'The spread'){
#       temp_data <- get_data_line()
#     } else if(type_of_bet =='The over under'){
#       temp_data <- get_data_over_under()
#     }
#     
#     if (ben_team == 'All' & gabe_team == 'All'){
#       temp_data$cum_sum<- cumsum(temp_data$`Money result`)
#     } else if (ben_team == 'All' & gabe_team != 'All') {
#       temp_data <- temp_data %>% filter(`Gabe's picks` %in% gabe_team)
#     } else if (ben_team != 'All' & gabe_team == 'All') {
#       temp_data <- temp_data %>% filter(`Ben's picks` %in% ben_team)
#     } else {
#       temp_data <- temp_data %>% filter(`Ben's picks` %in% ben_team) %>% filter(`Gabe's picks` %in% gabe_team)
#     }
#     
#     
#     
#   })
#   

  
}

# Run the application 
shinyApp(ui = ui,
         # htmlTemplate("www/index.html"), 
         server)

