source('functions.R')


header <- dashboardHeader(title="Better Work Research Portal")
sidebar <- dashboardSidebar(
  sidebarMenu(
    menuItem(
      text="Ben's home",
      tabName="main",
      icon=icon("eye")),
    menuItem(
      text="youre lost",
      tabName="ki",
      icon=icon("key")),
    menuItem(
      text="take this cock",
      tabName="basic",
      icon=icon("pencil-square")),
    menuItem(
      text="anal happens here",
      tabName="advanced",
      icon=icon("keyboard-o")),
    menuItem(
      text = 'About',
      tabName = 'about',
      icon = icon("cog", lib = "glyphicon"))))

body <- dashboardBody(
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "custom.css")
  ),
  tabItems(
    tabItem(
      tabName="main",
      fluidPage(
        
        h3('Welcome!'),
        fluidRow(
          column(6,
                 
                 p(paste0("Welcome to the 'Better Work Research Portal', a collaboration between Better Work, Tufts University, and the World Bank Group.
                          This app is intended to help researchers to explore the results of the 5 country 'Better Work' survey.
                          To get started, select a country (right), then choose visit the 'Advanced'' analysis (for users familiar with R) or the 'Basic' analysis tab (for all users).")),
                 
                 p(paste0('Welcome to the "Better Work Research Portal", a collaboration ',
                          'between Better Work, Tufts University, and the World Bank. ')),
                 p(paste0('This app is intended to help researchers to explore ',
                          'the results of the five-country Better Work survey.')),
                 p(a("Betterwork homepage",     href="https://betterwork.org/")),
                 p(a('Interwoven report', href = 'https://openknowledge.worldbank.org/bitstream/handle/10986/22699/99729.pdf?sequence=1&isAllowed=y')),
                 p(a('Betterwork compliance data', href = 'https://portal.betterwork.org/transparency/compliance'))),
          column(6,
                 selectInput('country', 'Country: ',
                             choices = c('Haiti' = 'haiti'), 
                             multiple = TRUE),
                 # plotOutput('country_map', height = 300),
                 h1(textOutput('ready_text'), align = 'center'),
                 withSpinner(leafletOutput('leaf', height = 300)))
          # plotlyOutput('key_indicators_plot'))
                 )
        
          )
),
 
abItem(
  tabName = 'advanced',
  fluidPage(
    fluidRow(column(4,
                    uiOutput('team b')),
             column(4,
                    uiOutput('team g'))), 
    fluidRow(
      column(12,
             plotOutput('model_plot')),
      column(12,
             DT::dataTableOutput('model_table')),
      column(12,
             h1('Methodology'),
             helpText('For outcome variables with two levels, we estimate a binomial logistic regression,
                       or a linear probability model (with robust stand errors). 
                       For outcome variables with more than two levles, 
                       we estimate a multinomial logistic model. This functionality is still under construction.'))
    )
    # THIS IS WHERE WE NEED TO BUILD MODELING INPUTS AND OUTPUTS
  )
),
tabItem(
  tabName = 'about',
  fluidPage(
    fluidRow(
      column(6,
             includeMarkdown('includes/about.md')),
      column(6,
             h1('Survey documentation'),
             helpText('For full details on column names and',
                      'meanings, download the survey documentation below.'),
             
             # uiOutput('file_downloader'),
             selectInput('filenames',
                         'Choose a document to download:',
                         choices = download_list),
             downloadLink('downloadData', 'Download'),
             h1('Survey "data dictionaries"'),
             helpText('The column names and levels can be confusing',
                      'to those not familiar with the survey.',
                      'See the below',
                      '"dictionaries" to decipher headers and responses.',
                      'As with the survey documentation,',
                      'if you have multiple countries selected,',
                      'data will be restricted only to the country',
                      ' which appears first alphabetically.'),
             h2('Headers dictionary'),
             dataTableOutput('simple_dictionary_table'),
             h2('Responses dictionary'),
             dataTableOutput('complete_dictionary_table'))
    )),
  fluidPage(
    fluidRow(
      div(img(src='logo_clear.png', align = "center"), style="text-align: center;"),
      h4('Built in partnership with ',
         a(href = 'http://databrew.cc',
           target='_blank', 'Databrew'),
         align = 'center'),
      p('Empowering research and analysis through collaborative data science.', align = 'center'),
      div(a(actionButton(inputId = "email", label = "info@databrew.cc", 
                         icon = icon("envelope", lib = "font-awesome")),
            href="mailto:info@databrew.cc",
            align = 'center')), 
      style = 'text-align:center;'
    )
  )
)
  )
)

# UI
ui <- dashboardPage(header, sidebar, body, skin="blue")

# Server
server <- function(input, output) {
 
  
  # Reactive country
  df <- reactive({
     dat_full_temp
  })
  
  output$outcome_var <- renderUI({
    
    if(!is.null(x)){
      selectInput('outcome_var',
                  'Select variable interest',
                  choices = names(x),
                  multiple = FALSE,
                  selected = c('Injured at factory'))
    } else {
      NULL
    }
  })
  
  output$predictors <-renderUI({
    x <- df()
    x_sub <- x[, sapply(x, class) == 'character']
    
    bad_var_flag <- apply(x_sub, 2, function(x) length(unique(x)) < 3)
    x_sub <- x_sub[ , !bad_var_flag]
    x_names <- colnames(x_sub) 
    if(!is.null(x_names)){
      selectInput('predictors',
                  'Select predictor variable(s)',
                  choices = x_names,
                  multiple = TRUE,
                  selected = c('Sex'))
    } else {
      NULL
    }
  })
  
  # MLR , OLR
  output$model_type <-renderUI({
    model_type <- c('Logistic', 'Linear probability model')
    if(!is.null(model_type)){
      selectInput('model_type',
                  'Select the model',
                  choices = model_type,
                  multiple = FALSE,
                  selected = c('Logistic'))
    } else {
      NULL
    }
  })
  
  
  output$model_table <- DT::renderDataTable({
    # get specificaitons 
    y_side <- input$outcome_var
    x_side <- input$predictors
    model_type <- input$model_type
    d <- df()
    
    if(is.null(y_side) | is.null(x_side) | is.null(model_type) | is.null(d)) {
      return(NULL)
    } else {
      d_sub <- d[, sapply(d, class) == 'character']
      pred_sub <- as.data.frame(d_sub[, colnames(d_sub) %in% x_side])
      pred_sub$outcome_y <- unlist(d[, y_side])
      pred_sub <- pred_sub[complete.cases(pred_sub),]
      
      if(apply(pred_sub, 2, function(x) length(unique(x)) <2) & ncol(pred_sub) >1) {
        DT::datatable(data_frame(' ' = 'The combination yields a factor with one level'), rownames = FALSE, options = list(dom = 't'))
        
      } else {
        if (nrow(pred_sub) < 20){
          DT::datatable(data_frame(' ' = 'Too many NAs for this variable combination'), rownames = FALSE, options = list(dom = 't'))
        } else {
          if(model_type == 'Linear probability model') {
            
            if(length(unique(pred_sub$outcome_y)) == 2) {
              unique_levles <-  unique(pred_sub$outcome_y)
              pred_sub$outcome_y[pred_sub$outcome_y == unique_levles[1]] <- 0 
              pred_sub$outcome_y[pred_sub$outcome_y == unique_levles[2]] <- 1
              
              # change to factor
              # pred_sub <-  as.data.frame(apply(pred_sub, 2, function(x) as.factor(x)), stringsAsFactors = T)
              mod_results <- as.data.frame(broom::tidy(lm(outcome_y~ ., data = pred_sub)))
              
              mod_results[, 2:ncol(mod_results)] <- apply(mod_results[, 2:ncol(mod_results)], 2, function(x) round(x, 3))
              
              
            } else {
              DT::datatable(data_frame(' ' = 'The linear probability model requires an outcome with 2 categories'), rownames = FALSE, options = list(dom = 't'))
              
            }
            
          } else if(model_type == 'Logistic') {
            # change to factor
            pred_sub <-  as.data.frame(apply(pred_sub, 2, function(x) as.factor(x)), stringsAsFactors = T)
            # get x_side length
            if(length(unique(pred_sub$outcome_y)) > 2) {
              mod_summary<- multinom(outcome_y ~., data = pred_sub)
              var_coef <- round(summary(mod_summary)$coefficients, 3)
              var_std <- round(summary(mod_summary)$standard.errors, 3)
              z <- var_coef[, 2:(length(x_side) +1)]/var_std[, 2:(length(x_side) +1)]
              # wald test to obtain pvalue
              p <- round((as.data.frame((1 - pnorm(abs(z), 0 , 1))*2)), 2)
              colnames(p)[1:length(x_side)] <- paste0(x_side, '_',rep.int('p_value', length(x_side)))
              odds_ratio <- round(exp(var_coef),2)
              mod_results <- as.data.frame(cbind(odds_ratio, p_value = p))
              
              # 
              # if(is.null(mod_results)){
              #   return(NULL)
              # } else {
              #   mod_results
              # }
              # 
            } else if(length(unique(pred_sub$outcome_y)) == 2) {
              mod_results <- as.data.frame(broom::tidy(glm(outcome_y~ ., family = binomial(link = 'logit'), data = pred_sub)))
              
              mod_results[, 2:ncol(mod_results)] <- apply(mod_results[, 2:ncol(mod_results)], 2, function(x) round(x, 3))
              
              if(is.null(mod_results)){
                return(NULL)
              } else {
                mod_results
              }
            } else {
              DT::datatable(data_frame(' ' = 'Pick an outcome variable with 2 or more levels'), rownames = FALSE, options = list(dom = 't'))
            }
            
          }
          
        }
        
      }
      
      }
      
          
  })
  
  
  # 
  output$model_plot <- renderPlot({
    # get specificaitons 
    y_side <- input$outcome_var
    x_side <- input$predictors
    model_type <- input$model_type
    d <- df()
  
    
    if(is.null(y_side) | is.null(x_side) | is.null(model_type) | is.null(d)) {
      return(NULL)
    } else {
      d_sub <- d[, sapply(d, class) == 'character']
      pred_sub <- as.data.frame(d_sub[, colnames(d_sub) %in% x_side])
      pred_sub$outcome_y <- unlist(d[, y_side])
      pred_sub <- pred_sub[complete.cases(pred_sub),]
      
      if(apply(pred_sub, 2, function(x) length(unique(x)) <2) & ncol(pred_sub) >1) {
        DT::datatable(data_frame(' ' = 'The combination yields a factor with one level'), rownames = FALSE, options = list(dom = 't'))
        
      } else {
        if(nrow(pred_sub) < 20){
          DT::datatable(data_frame(' ' = 'Too many NAs for this variable combination'), rownames = FALSE, options = list(dom = 't'))
        } else {
          if(model_type == 'Linear probability model') {
            
            if(length(unique(pred_sub$outcome_y)) == 2) {
              unique_levles <-  unique(pred_sub$outcome_y)
              pred_sub$outcome_y[pred_sub$outcome_y == unique_levles[1]] <- 0 
              pred_sub$outcome_y[pred_sub$outcome_y == unique_levles[2]] <- 1
              
              # change to factor
              # pred_sub <-  as.data.frame(apply(pred_sub, 2, function(x) as.factor(x)), stringsAsFactors = T)
              mod_results <- as.data.frame(broom::tidy(lm(outcome_y~ ., data = pred_sub)))
              
              mod_results[, 2:ncol(mod_results)] <- apply(mod_results[, 2:ncol(mod_results)], 2, function(x) round(x, 3))
              
              cols <- colorRampPalette(brewer.pal(n = 9, 'Spectral'))(length(unique(mod_results$term)))
              p <- ggplot(mod_results, aes(term, estimate)) + geom_bar(stat = 'identity',  alpha = 0.7) +
                xlab('') + ylab('') +
                scale_fill_discrete(name = '') +
                geom_errorbar(aes(ymin=estimate-std.error, ymax=estimate+std.error), width=.1) +
                theme_world_bank()
              
              return(p)
              
            } else {
              DT::datatable(data_frame(' ' = 'The linear probability model requires an outcome with 2 categories'), rownames = FALSE, options = list(dom = 't'))
              
            }
            
          } else if(model_type == 'Logistic') {
            # change to factor
            pred_sub <-  as.data.frame(apply(pred_sub, 2, function(x) as.factor(x)), stringsAsFactors = T)
            # get x_side length
            if(length(unique(pred_sub$outcome_y)) > 2) {
              mod_summary<- multinom(outcome_y ~., data = pred_sub)
              var_coef <- round(summary(mod_summary)$coefficients, 3)
              var_std <- round(summary(mod_summary)$standard.errors, 3)
              z <- var_coef[, 2:(length(x_side) +1)]/var_std[, 2:(length(x_side) +1)]
              # wald test to obtain pvalue
              p <- round((as.data.frame((1 - pnorm(abs(z), 0 , 1))*2)), 2)
              colnames(p)[1:length(x_side)] <- paste0(x_side, '_',rep.int('p_value', length(x_side)))
              odds_ratio <- round(exp(var_coef),2)
              mod_results <- as.data.frame(cbind(odds_ratio, p_value = p))
              mod_results$outcome <- rownames(mod_results)
              
              mod_results <- melt(mod_results, id.vars = 'outcome')
              
              p <- ggplot(mod_results, aes(outcome, value, fill = variable)) + 
                geom_bar(stat = 'identity', position = 'dodge',  alpha = 0.7) + theme_light() + 
                xlab('') + ylab('') +
                scale_fill_discrete(name = '') +
                theme_world_bank()
              
              return(p)
              
              # 
              # if(is.null(mod_results)){
              #   return(NULL)
              # } else {
              #   mod_results
              # }
              # 
            } else if(length(unique(pred_sub$outcome_y)) == 2) {
              mod_results <- as.data.frame(broom::tidy(glm(outcome_y~ ., family = binomial(link = 'logit'), data = pred_sub)))
              
              mod_results[, 2:ncol(mod_results)] <- apply(mod_results[, 2:ncol(mod_results)], 2, function(x) round(x, 3))
              
              p <- ggplot(mod_results, aes(term, estimate)) + geom_bar(stat = 'identity', alpha = 0.7) +
                geom_errorbar(aes(ymin=estimate-std.error, ymax=estimate+std.error), width=.1)  +
                xlab('') + ylab('') + 
                scale_fill_discrete(name = '') +
                theme_world_bank()
              
              if(is.null(p)){
                return(NULL)
              } else {
                p
              }
            } else {
              DT::datatable(data_frame(' ' = 'Pick an outcome variable with 2 or more levels'), rownames = FALSE, options = list(dom = 't'))
            }
            
          }
          
        }
        
      }
      
      }
      
  })
  
}


shinyApp(ui, server)