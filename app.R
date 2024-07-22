library(shiny)
library(DT)
library(stringr)
library(dplyr)
library(reticulate)
library(RSQLite)
library(DBI)
library(shinyWidgets)
library(writexl)
#library(shiny.semantic)
#reticulate::use_condaenv('base')
reticulate::import('requests')
reticulate::import('pandas')
reticulate::import('datetime')
# setwd("/Users/franciscotacora/Desktop/Folders/books/MasteringShiny/practical/practical_finance")

### DataSets format
currency_list = read.csv2('currency_list.csv', sep = ',')

baseExpenses = readxl::read_xlsx('FinApp_planilla.xlsx',sheet = 1)
baseIncome = readxl::read_xlsx('FinApp_planilla.xlsx',sheet = 2)
baseCreditCards = readxl::read_xlsx('FinApp_planilla.xlsx',sheet = 3)
baseCreditExpenses = readxl::read_xlsx('FinApp_planilla.xlsx',sheet = 4)
baseDebt = readxl::read_xlsx('FinApp_planilla.xlsx',sheet = 5)

#### Columns and datatypes
## for expenses and income
# Date = date with strptime(as.character(Sys.Date()), '%Y-%m-%d') %>% format('%Y-%m-%d')
# Amount = Numeric with sprintf('%.2f', 0)
# Currency = character
# USD_price = Numeric with sprintf('%.2f', 0)
# USD_amount = Numeric with sprintf('%.2f', 0)
# From = character
# To = character
# Description = character
### ITF adding for creditExpenses
# ITF = Numeric with sprintf('%.2f', 0)

## for credit cards
# Bank = character
# Card_label = character
# limit = Numeric with sprintf('%.2f', 0)
# Billing_Closure = Numeric with sprintf('%.2f', 0)
# Payment_Due_Date = date with strptime(as.character(Sys.Date()), '%Y-%m-%d') %>% format('%Y-%m-%d')
# Payment_Day = date with strptime(as.character(Sys.Date()), '%Y-%m-%d') %>% format('%Y-%m-%d')

# from_who = character
# to_whom = character
# date_of_debt = date with strptime(as.character(Sys.Date()), '%Y-%m-%d') %>% format('%Y-%m-%d')
# debt_amount = Numeric with sprintf('%.2f', 0)
# USD_price = Numeric with sprintf('%.2f', 0)
# USD_amount = Numeric with sprintf('%.2f', 0)
# paymentInstallment = Numeric with sprintf('%.2f', 0)
# Payment_Due_Date = date with strptime(as.character(Sys.Date()), '%Y-%m-%d') %>% format('%Y-%m-%d')

generate_empty = function(NAs = F, datasetFormat = NULL, setDefault = T){
  if(is.null(datasetFormat)) stop('generate_empty() needs datasetFormat')
  
  fill_default_data = function(df){
    # browser()
    dateType = c('Date', 'Payment_Due_Date', 'Payment_Day', 'date_of_debt')
    numeric_currencyType = c('Amount', 'USD_price', 'USD_amount', 'ITF',
                             'limit', 'Billing_Closure', 'debt_amount',
                             'paymentInstallment')
    
    ## Assigning date default format
    x_pivot = colnames(df) %in% dateType
    if(any(x_pivot)){
      df[, x_pivot] = strptime(as.character(Sys.Date()), '%Y-%m-%d') %>% 
        format('%Y-%m-%d')
    }
    
    ## Assigning numeric currency
    x_pivot = colnames(df) %in% numeric_currencyType
    if(any(x_pivot)){
      df[, x_pivot] = sprintf('%.2f', 0)
    }
    
    return(df)
  }
  
  values = ifelse(NAs, NA, character(0))
  dimen = dim(datasetFormat)[2]
  matr = matrix(values, 1, dimen) %>% data.frame()
  colnames(matr) = colnames(datasetFormat)
  if(setDefault){
    matr = fill_default_data(matr)
  }
  return(matr)
}

new_Data = list(Expenses = generate_empty(datasetFormat = baseExpenses),
                Income = generate_empty(datasetFormat = baseIncome),
                Creditcards = generate_empty(datasetFormat = baseCreditCards),
                Credit_expenses = generate_empty(datasetFormat = baseCreditExpenses),
                Debt = generate_empty(datasetFormat = baseDebt))

## Padronizing the in_out variable
values_inout = c('in', 'out', 'earned', 'spent', 1, -1)

## Showing modules

### Modules to record inputs into the tables and display them on the hole app
table_suits_UI <- function(id) {
  ns <- NS(id)
  tagList(
    DT::dataTableOutput(ns('mySheet')),
    fluidRow(
      column(2,
             numericInput(ns('indexToDrop'), 'Index', min = 0, value = 0)),
      column(1,
             br(),
             actionButton(ns('dropline'), 'Drop Line')),
      column(6,
             br(),
             actionButton(ns('addline'), 'Add Line'))
    )
  )
}

## in this case, reactivePort will be theFile, because it needs to be check if it is null
table_suits_Server <- function(id, reactivePort, section) { 
  moduleServer(
    id,
    function(input, output, session) {
      ## preparing the display for the data
      
      output$mySheet = renderDT({
        if(!is.null(reactivePort[['data']])){
          file_u = reactivePort[['data']]
          return(file_u[[section]])
        }
      },
      editable = list(target = 'cell', desible = list(columns = c(5))), 
      server = T,
      selection = 'single',
      class = 'cell-border stripe')
      
      ## Adding a line to the dataset
      addme_lines = reactive({
        file_u = reactivePort[['data']][[section]]
        
        new_line = generate_empty(datasetFormat = file_u)
        reactivePort[['data']][[section]] = rbind(file_u, new_line)
        
        updateNumericInput(inputId = 'indexToDrop', 
                           value = nrow(reactivePort[['data']][[section]]))
      })
      
      ## Action to add a line
      observeEvent(input$addline, {
        addme_lines()
      })
      
      ## Action to delete a line
      observeEvent(input$dropline, {
        # The drop line section doesn't conserve the indexes, it reformulates them
        pivot = reactivePort[['data']][[section]]
        if(nrow(pivot) > 1){ ## Can't drop more lines than 1
          if(input$indexToDrop > 1){
            index_delete = input$indexToDrop
          }else{
            index_delete = nrow(pivot)
          }
          
          pivot = pivot[-index_delete, ]
          
          # Reformulating indexes
          rownames(pivot) = 1:nrow(pivot)
          reactivePort[['data']][[section]] = pivot
          updateNumericInput(inputId = 'indexToDrop', 
                             value = nrow(reactivePort[['data']][[section]])) 
        }else{
          showNotification('Can\'t drop more lines!', type = 'warning')
        }
      })
      
      # updating input$indexToDrop to then delete, or drop, a selected line
      observeEvent(input$mySheet_rows_selected, {
        updateNumericInput(inputId = 'indexToDrop',
                           value = input$mySheet_rows_selected)
      })
      
      ## Adicionando y editando datos
      
      observeEvent(input$mySheet_cell_edit, {
        
        info = input$mySheet_cell_edit
        row_dt = info$row
        col_dt = info$col
        value_dt = info$value
        
        file_u = reactivePort[['data']][[section]]
        
        # getting the colname selected
        c_names = colnames(file_u)[col_dt]
        
        ### variables that belong to the evaluation:
        dateType = c('Date', 'Payment_Due_Date', 'Payment_Day', 'date_of_debt')
        numeric_currencyType = c('Amount', 'USD_price', 'USD_amount', 'ITF',
                                 'limit', 'Billing_Closure', 'debt_amount',
                                 'paymentInstallment')
        
        ## If the input doesn't belong to the format, this section stops with a null
        list_values = list(value_dt, c_names)
        if(c_names %in% dateType){
          value_dt = evaluate_data[['Date_type']](list_values)
        }else if(c_names %in% numeric_currencyType){
          value_dt = evaluate_data[['numeric_currency_type']](list_values)
        }
        
        file_u = reactivePort[['data']][[section]]
        
        file_u[row_dt, col_dt] = value_dt
        reactivePort[['data']][[section]] = file_u
        
        if(nrow(file_u) == row_dt){
          addme_lines()
        }
      })
      
      ## Bellow we have a list of functions
      ## one function per variable type (date type, numeric currency type, character type)
      evaluate_data = list(
        Date_type = function(value_list){
          fecha = value_list[[1]]
          fecha_format = strptime(as.character(fecha), '%Y-%m-%d') %>%
            format('%Y-%m-%d')
          gotrep = grepl(pattern = '^\\d{2}/\\d{2}/\\d{4}$', x = fecha)
          
          if(is.na(fecha_format) | !gotrep){
            showNotification('The date format must be YYYY-MM-DD',
                             type = 'warning')
            return(strptime(as.character(Sys.Date()), '%Y-%m-%d') %>%
                     format('%Y-%m-%d'))
          }else{return(fecha)}
        },
        numeric_currency_type = function(value_list){
          value = value_list[[1]] %>% as.numeric()
          if(is.na(value)){
            showNotification('You got to insert a number. Don\'t use a comma',
                             type = 'warning')
            return('0.00')
          }else{return(value)}
        }
      )
      ####
    }
  )
}

display_tabIMG = function(id){
  ns <- NS(id)
  tagList(
    tabsetPanel(
      id = ns('display_it'),
      type = 'hidden',
      tabPanel('meanWhileIMG',
               tags$h3('Upload your data to proceed'),
               imageOutput(ns('imagem_meanwhile'))
      ),
      tabPanel('table',
               table_suits_UI(id = ns('teste12'))
      )
    )
  )
}

display_tabIMG_server = function(id, newDoc_in, usersDoc_in,  samplecols,
                                 reactivePort, section){
  # stopifnot(is.reactive(newDoc_in))
  moduleServer(
    id,
    function(input, output, session){
      observeEvent(newDoc_in(), {
        updateTabsetPanel(inputId = 'display_it', selected = 'table')
      })
      
      observeEvent(usersDoc_in(), {
        updateTabsetPanel(inputId = 'display_it', selected = 'table')
      })
      
      output$imagem_meanwhile <- renderImage({
        list(src = "shiba_dance.gif",
             height = 300, 
             width = 300, 
             type = "image/gif",
             style="display: block; margin-left: auto; margin-right: auto;",
             align = "center")
      },deleteFile = F)
      
      table_suits_Server('teste12',
                         reactivePort = reactivePort,
                         section = section)
    }
  )
}

## Total USD output
Total_USD_UI = function(id){
  ns = NS(id)
  tagList(
    uiOutput(ns('totalValue'))
  )
}

Total_USD_Server = function(id, reactivePort){
  moduleServer(
    id,
    function(input, output, session){
      totalUsD = reactive({
        if(is.null(reactivePort[['data']]) || 
           (nrow(reactivePort[['data']][['Expenses']]) == 0 && nrow(reactivePort[['data']][['Income']]) == 0)){
          return('0,00 USD')
        }else{
          pivot_Expenses = reactivePort[['data']][['Expenses']][['USD_amount']] %>%
            as.numeric() %>% sum()
          pivot_Income = reactivePort[['data']][['Income']][['USD_amount']] %>% 
            as.numeric() %>% sum()
          pivot = pivot_Income - pivot_Expenses
          
          total = sprintf('%.2f', sum(pivot, na.rm = TRUE))
          total = paste(total, ' USD')
          return(total)
        }
      })
      
      output$totalValue = renderUI({
        total <- totalUsD()
        color <- ifelse(as.numeric(gsub("[^0-9.-]", "", total)) > 0, "green", 
                        ifelse(as.numeric(gsub("[^0-9.-]", "", total)) < 0, 'red',
                               'black'))
        tags$h1(style = paste("color:", color, ";"), total)
      })
    }
  )
}

# Define UI for application that draws a histogram
ui <- fluidPage( #theme = bslib::bs_theme(bootswatch = "darkly"),
  fluidRow(column(4,
                  tags$h1('Practical Sheet')),
           column(4,
                  br(),
                  selectInput('currency_list1',
                                label = 'Currency',
                                choices = currency_list$currency)
                  ),
           column(4,
                  #tags$h1(textOutput('totalValue')),
                  Total_USD_UI('totalUSD'),
                  tags$h6(textOutput('USD_pricing'))
                  )
           ),
  tabsetPanel(type = 'tabs',
              br(),
              tabPanel('Upload',
                       fixedRow(
                         column(6,
                                column(4,
                                       fileInput('inputData', 'Upload your Data')),
                                column(2,
                                       br(),
                                       actionButton('newDoc', 'New file')),
                                column(4,
                                       selectInput('sampleSheet', label = 'Sheets:',
                                                   choices = c('Upload your file'),
                                                   selected = character(0))),
                                column(2,
                                       ## this button could be put more to the right
                                       br(),
                                       downloadButton('downloadData', 'Download'))
                         )
                       ),
                       DT::dataTableOutput('sampleMyFiles')
                       ),
              tabPanel('Expenses',
                       display_tabIMG('expense_input')
                       ),
              tabPanel('Income',
                       display_tabIMG('income_input')
                       ),
              tabPanel('Credit',
                       shinyWidgets::verticalTabsetPanel(
                         id = 'creditCards',
                         verticalTabPanel(
                           title = 'My Cards',
                           # tags$h1('Here comes the credit cards record'),
                           display_tabIMG('creditCards_input'),
                           box_height = "50px"
                         ),
                         verticalTabPanel(
                           title = 'My Expenses',
                           # tags$h1('Here comes the credit expenses')
                           display_tabIMG('creditExpenses_input')
                         )
                       )),
              tabPanel('Debt',
                       # tags$h1('Here comes the debt records')
                       display_tabIMG('Debt_input'))
    
  )
  
)


# Define server logic required to draw a histogram
server <- function(input, output) {
  
  ###### Auxiliar functions ####################
  ## creating the universal value
  theFile = reactiveValues(data = NULL, other = NULL)
  
  ## creating a notification function personalized
  show_semaforo = function(message, RedYellowGreenBlue = NULL){
    messagetype = c('error', 'warning', 'success', 'message')
    colorme = messagetype[RedYellowGreenBlue]
    if(is.na(colorme)){
      showNotification(message)
    }else{
      showNotification(message, type = colorme)
    }
    
  }
  
  ## A function for checking the data format
  dataset_checking = function(a_file, the_file){
    action_boolean = ifelse(all(colnames(a_file) == colnames(the_file)), T, F)
    return(action_boolean)
  }
  
  ## ----------------------------------------------------------------------------------------- ##
  
  ###### Upload/New Data Section ####################
  
  ## displaying a sample of the data uploaded
  
  ## prepare new data
  ### Here we generate new datasets and assign it to a reactive value as a list
  observeEvent(input$newDoc, {
    theFile$data = new_Data
    updateSelectInput(inputId = 'sampleSheet', choices = c(names(theFile$data)))
  })
  
  # observeEvent(input$inputData, {
  #   updateSelectInput(inputId = 'sampleSheet', choices = c(names(theFile$data)))
  # })
  
  ## upload user's data
  ###--### prepare it for multiple datasets upload
  observeEvent(input$inputData, {
    if(tools::file_ext(input$inputData$datapath) == 'xlsx'){
      updateTabsetPanel(inputId = 'display_it', selected = 'table') ## this one if for expenses only
      sheets_names = readxl::excel_sheets(input$inputData$datapath)
      file_u = list()
      for(i in sheets_names){
        file_u[[i]] = readxl::read_xlsx(input$inputData$datapath,sheet = i) %>%
          data.frame()
      }
      names(file_u) = stringr::str_to_sentence(sheets_names)
      theFile$data = file_u
      print(names(theFile$data))
      updateSelectInput(inputId = 'sampleSheet', choices = c(names(file_u)))
    }else{
      show_semaforo('It must be an excel file', 1)
    }
    updateSelectInput(inputId = 'sampleSheet', choices = c(names(theFile$data)))
  })

  
  # return_sample = eventReactive(input$sampleSheet, {
  #   return(tail(theFile$data[[input$sampleSheet]]))
  # })
  
  sampleSheetData <- reactive({
    input$newDoc  # trigger dependency on input$newDoc
    input$inputData  # trigger dependency on input$inputData
    
    input$sampleSheet  # trigger dependency on input$sampleSheet
    
    if (is.null(theFile$data)) {
      return(NULL)
    }
    
    tail(theFile$data[[input$sampleSheet]])
  })

  ### Este módulo se está ejecutando dos veces, porque?
  output$sampleMyFiles = renderDT({
    # return(return_sample())
    sampleSheetData()
  }, selection = 'none')
  
  ## ----------------------------------------------------------------------------------------- ##
  
  ## Display of #Expenses section
  # file_piv = 
  display_tabIMG_server(id = 'expense_input',
                        newDoc_in = reactive(input$newDoc),
                        usersDoc_in = reactive(input$inputData),
                        samplecols = reactive(input$sampleSheet),
                        reactivePort = theFile,
                        section = 'Expenses')
  
  ## Display of #Income section
  # file_piv =
  display_tabIMG_server(id = 'income_input',
                        newDoc_in = reactive(input$newDoc),
                        usersDoc_in = reactive(input$inputData),
                        samplecols = reactive(input$sampleSheet),
                        reactivePort = theFile,
                        section = 'Income')


  ## Display of #Credit section
  ### #CreditCards.
  # file_piv =
  display_tabIMG_server(id = 'creditCards_input',
                        newDoc_in = reactive(input$newDoc),
                        usersDoc_in = reactive(input$inputData),
                        samplecols = reactive(input$sampleSheet),
                        reactivePort = theFile,
                        section = 'Creditcards')

  ### #CreditExpenses
  # file_piv =
  display_tabIMG_server(id = 'creditExpenses_input',
                        newDoc_in = reactive(input$newDoc),
                        usersDoc_in = reactive(input$inputData),
                        samplecols = reactive(input$sampleSheet),
                        reactivePort = theFile,
                        section = 'Credit_expenses')

  ### #Debt
  # file_piv =
  display_tabIMG_server(id = 'Debt_input',
                        newDoc_in = reactive(input$newDoc),
                        usersDoc_in = reactive(input$inputData),
                        samplecols = reactive(input$sampleSheet),
                        reactivePort = theFile,
                        section = 'Debt')
  
  # observe({theFile = file_piv()})
  
  ### To Download the data
  output$downloadData <- downloadHandler(
    filename = function() {
      # Use the selected dataset as the suggested file name
      paste0('myData', ".xlsx")
    },
    content = function(file) {
      # Write the dataset to the `file` that will be downloaded
      if(is.null(theFile$data)){
        show_semaforo('There is no data to save', 1)
        return(NULL)
      }else{
        writexl::write_xlsx(theFile$data, file)
        # write.csv(theFile$data, file)
      }
    }
  )
  
  
  ## To output the amount of total USD
  Total_USD_Server('totalUSD', reactivePort = theFile)

  
  ### Price of the dollar
  import_pythonscript = function(){
    reticulate::source_python('CurrencyAPI.py', envir = globalenv())
  }
  
  pricing_output = eventReactive(input$currency_list1,{
    pivot = df_exchange[df_exchange['Currency'] == input$currency_list1, ]
    return(pivot[1, 'USD_price'])
  })
  
  USDprice_timer = reactiveTimer(3600*(10**3))
  
  ### The proces bellow not only download the data from the API
  ## It also saves the data to the data lake
  observe({
    USDprice_timer()
    
    # oppening the dataset
    drv = dbDriver('SQLite')
    con = dbConnect(drv, dbname = 'USD_practicalFinance.db')
    
    dbtables = dbListTables(con) %>% str_split_fixed(pattern = '_', n = 3)
    dbdates0 = dbtables[, 3] %>% as.numeric()
    dbdates = as.POSIXct(dbdates0, origin = "1970-01-01") + 3600
    dbdates = dbdates >= Sys.time()
    
    if(!any(dbdates)){
      message('Inputing data from the API')
      import_pythonscript()
      
      #### creating the names for the tables to be saved
      
      df_exchange_name = 'df_exchangeUSD' %>% 
        paste0('_', df_exchange[1, 'timestamp'])
      df_meta_name = 'df_metaUSD' %>%
        paste0('_', df_meta[df_meta[['keys']] == 'timestamp', 'values'])
      
      if(!dbExistsTable(con, df_exchange_name)){
        dbWriteTable(con, df_exchange_name, df_exchange)
      }
      
      if(!dbExistsTable(con, df_meta_name)){
        df_meta[['values']] = unlist(df_meta[['values']])
        dbWriteTable(con, df_meta_name, df_meta)
      }
    }else{
      df_exchange <<- dbReadTable(con, paste0('df_exchangeUSD_', max(dbdates0)))
      df_meta <<- dbReadTable(con, paste0('df_metaUSD_', max(dbdates0)))
    }
    
    DBI::dbDisconnect(con)
  })
  
  output$USD_pricing = renderText({
    pivot = pricing_output()
    
    date_it = df_meta[df_meta[['keys']] == 'date', 'values'][[1]] %>%
      as.Date(format = '%d-%m-%Y')
    expr = "^([0-9]{2}):([0-9]{1}):([0-9]{1})$"
    time = df_meta[df_meta[['keys']] == 'time', 'values']
    time = str_replace(time, expr, "\\1:00:00")
    
    the_message = paste(pivot, 'at', date_it, time)
    
    return(the_message)
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)

## Quiero colocar dateInput() en la columna de data de DT table. Es posible.
## Visitar el siguiente link
# https://stackoverflow.com/questions/55034483/shiny-widgets-in-dt-table
# https://stackoverflow.com/questions/48160173/r-shiny-extract-values-from-numericinput-datatable-column



## vamos a crear una aba más donde irán las facturas, que consistirán en tarjetas, facturas
## fijas que se repitan.

## hacer una aba donde se listarán el total de monetario en el tipo de moneda en el que fue
## registrado.


## Esta aplicaion necesita de internet. Por lo tanto, tengo que buscar una forma de que
## funcione sin internet. Se me ocurrió una forma: en el script de python, hacer unas
## lineas con el comando 'try' y si sale error, el script crea un comando que pasará a
## R y con su existencia, R asumirá el valor más reciente que la API captó
## O buscar como detectar si hay internet desde R mismo



## Vamos a adicionar una aba más donde se visualizarán los datos de forma descriptiva
## intentaré hacer gráficos
## --> Vamos a adicionar una cadena de pestañas verticales por el lado izquierdo
## habrán 3 abas. la primera de input en donde se ponen estas informaciones
## segunda aba serán gráficos y series temporales, con tablas descriptivas.
## tercera aba aun está por decidirse
