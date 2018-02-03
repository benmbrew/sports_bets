library(shinyapps)
source('read_in.R')

shinyUI(pageWithSidebar(
  
  
  # Application title
  headerPanel("LFS"),
  
  # Sidebar with a slider input for number of bins
  sidebarPanel(
    conditionalPanel(
      condition = "input.tabs == 'Model Results'",
      selectInput('models',
                  'Models',
                  choices = c('Logit' = 'perf_logit', 
                              'Random Forest' = 'perf_rf',
                              'SVM' = 'perf_svm',
                              'Ridge'= 'perf_ridge',
                              'Lasso' = 'perf_lasso',
                              'All Models'),
                  selected = 'All Models')
    ),
    
    conditionalPanel(
      condition = "input.tabs == 'Cancer Counts'",
      selectInput('cancer', 
                  'Cancer',
                  c(unique(sort(as.character(cancer$cancer))), 'All cancers'),
                  selected = 'All cancers')
    ),
    conditionalPanel(
      condition = "input.tabs == 'Mean Age of Onset'",
      selectInput('cancer', 
                  'Cancer',
                  c(unique(sort(as.character(cancer$cancer))), 'All cancers'),
                  selected = 'All cancers')
    )
    #uiOutput("test2"),
    
  ),
  
  # Show a plot of the generated distribution
  mainPanel(
    tabsetPanel(id = 'tabs',
                tabPanel('Model Results',
                         plotOutput('plot3')),
                tabPanel('Cancer Counts',
                         plotOutput("plot1"),
                         tableOutput('table1')),
                tabPanel('Mean Age of Onset',
                         plotOutput('plot2'),
                         tableOutput('table2'))
    )
  )
  
))


