library(shiny)
library(DT)
library(stringr)
library(dplyr)
library(reticulate)
library(RSQLite)
library(DBI)
#reticulate::use_condaenv('base')
reticulate::import('requests')
reticulate::import('pandas')
reticulate::import('datetime')

currency_list = read.csv2('currency_list.csv', sep = ',')

generate_empty = function(NAs = F, datasetFormat = NULL){
  if(!is.null(datasetFormat)){
    dimen = dim(datasetFormat)[2]
    matr = matrix(NA, 1, dimen) %>% data.frame()
    colnames(matr) = colnames(datasetFormat)
    return(matr)
  }
  
  if(NAs){
    a = data.frame(Date = NA,
                   Amount = NA,
                   Currency = NA,
                   USD_price = NA,
                   USD_amount = NA,
                   From = NA,
                   To = NA,
                   Description = NA,
                   In_Out = NA)
    return(a)
  }
  a = data.frame(Date = character(0),
                Amount = character(0),
                Currency = character(0),
                USD_price = character(0),
                USD_amount = character(0),
                From = character(0),
                To = character(0),
                Description = character(0),
                In_Out = character(0))
  return(a)
}


new_Data = generate_empty()

## Padronizing the in_out variable
values_inout = c('in', 'out', 'earned', 'spent', 1, -1)

display_tabIMG = tabsetPanel(
  id = 'display_it',
  type = 'hidden',
  tabPanel('meanWhileIMG',
           imageOutput('imagem_meanwhile')
           #HTML('<center><img src="shiba_dance.gif" width="400" type="image/gif"></center>')
           #HTML('<center><img src="shiba_dance.gif" width="200" type="image/gif"></center>')
           ),
  tabPanel('table',
           DT::dataTableOutput('mySheet'),
           fluidRow(
             column(2,
                    numericInput('indexToDrop', 'Index', min = 0, value = 0)),
             column(1,
                    br(),
                    actionButton('dropline', 'Drop Line')),
             column(6,
                    br(),
                    actionButton('addline', 'Add Line')),
             column(1,
                    br(),
                    downloadButton('downloadData', 'Download'))
           ))
)

# Define UI for application that draws a histogram
ui <- tagList(
  tags$style(HTML(".nbcontainer .container{
                    margin: 0 auto;
                    padding:0;
                    min-width: 1200px;
                    width: auto !important;
                  }
                  .container-fluid {
                    min-width: 1200px;
                    width: auto !important;
                  }
                  .navbar.navbar-default.navbar-static-top {
                    margin-bottom:0px;
                  }")),
  tags$div(class = 'nbcontainer',
           fluidPage( #theme = bslib::bs_theme(bootswatch = "darkly"),
             fixedRow(
               column(6,
                      tags$h1('Practical Sheet'),
                      column(4,
                             fileInput('inputData', 'Import your Data')),
                      column(4,
                             br(),
                             actionButton('newDoc', 'New file')),
                      column(4,
                             column(6, selectInput('currency_list1', 
                                                   label = 'Currency',
                                                   choices = currency_list$currency)))
               ),
               column(4,
                      tags$h1(textOutput('totalValue')),
                      column(6, tags$h6(textOutput('USD_pricing'))))
             ),
             display_tabIMG
           ))
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
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
  
  ## Mean while picture
  ### It's a picture that will fill the main panel while there is no data inputed
  output$imagem_meanwhile <- renderImage({
    list(src = "shiba_dance.gif",
         height = 300, 
         width = 300, 
         type = "image/gif",
         style="display: block; margin-left: auto; margin-right: auto;",
         align = "center")
  },deleteFile = F)
  
  ## prepare new data
  observeEvent(input$newDoc, {
    updateTabsetPanel(inputId = 'display_it', selected = 'table')
    theFile <<- reactiveValues(data = new_Data)
  })
  
  ## import data
  observeEvent(input$inputData, {
    updateTabsetPanel(inputId = 'display_it', selected = 'table')
    file_u = read.csv(input$inputData$datapath, sep = ',')
    theFile <<- reactiveValues(data = file_u)
  })
  
  ## preparing the display for the data
  output$mySheet = renderDT({
    if(isTruthy(input$newDoc) | isTruthy(input$inputData)){
      theFile$data
    }
  },
  editable = list(target = 'cell', desible = list(columns = c(5))), 
  server = T,
  selection = 'none',
  class = 'cell-border stripe')
  
  ## A function for checking the data format
  dataset_checking = function(a_file, the_file){
    action_boolean = ifelse(all(colnames(a_file) == colnames(the_file)), T, F)
    return(action_boolean)
  }
  
  ## Adding a line to the dataset
  addme_lines = reactive({
    action_boolean = dataset_checking(a_file = theFile$data,
                                      the_file = new_Data)
    
    if(action_boolean){
      new_line = generate_empty(T)
      dateit = strptime(as.character(Sys.Date()), '%Y-%m-%d')
      dateit = format(dateit, '%d-%m-%Y')
      new_line[['Date']] = dateit
      new_line[['Amount']] = sprintf('%.2f', 0)
      new_line[['USD_price']] = sprintf('%.2f', 0)
      new_line[['USD_amount']] = sprintf('%.2f', 0)
      theFile$data <<- rbind(theFile$data, new_line)
    }else{
      show_semaforo('This data has a different format from what is expected.', 2)
      new_line = generate_empty(datasetFormat = theFile$data)
      
      theFile$data <<- rbind(theFile$data, new_line)
    }
    
    updateNumericInput(inputId = 'indexToDrop', value = nrow(theFile$data))
  })
  
  ## Adding a new line
  observeEvent(input$addline, {
    if(!exists('theFile')){
      # in case the file doesn't exist, a notification will appear
      show_semaforo('No data was submited or created yet.', 1)
    }else{
      addme_lines()
      # print(theFile$data)
    }
  })
  
  observeEvent(input$dropline, {
    
    if(!exists('theFile')){
      show_semaforo('No data was submited or created yet.', 1)
    }else if(is.null(theFile$data) | (nrow(theFile$data) == 0)){
      show_semaforo('No data was submited or created yet.', 1)
    }else if(nrow(theFile$data) == 1){
      theFile$data <<- new_Data
    }else{
      # The drop line section doesn't conserve the indexes, it reformulates them
      pivot = theFile$data
      pivot = pivot[-input$indexToDrop, ]
      
      # Reformulating indexes
      rownames(pivot) = 1:nrow(pivot)
      theFile$data <<- pivot
      
      updateNumericInput(inputId = 'indexToDrop', value = nrow(theFile$data)) 
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
    
    
    ## Making sure the in_out variable is either 'in' or 'out'
    ## or 'earned' 'spent' or '1' '-1'
    c_names = colnames(new_Data)[col_dt]
    
    ## generando una linsta de valores:
    ## valordt, c_names, row_dt, col_dt
    list_values = list(value_dt, c_names)
    evaluate_data[[c_names]](list_values)
    
    pivot = theFile$data
    
    pivot[row_dt, col_dt] = value_dt
    theFile$data <<- pivot
    
    if(nrow(pivot) == row_dt){
      addme_lines()
    }
  })
  
  ## Bellow we have a list of functions
  ## one function per variable
  evaluate_data = list(
    Date = function(value_list){
      fecha = value_list[[1]]
      
      ## Getting the wantted pattern for dates
      ## dd/mm/yyyy, no more digits
      ## the ^ in the regex indicates the beggining of the pattern
      ## the $ means the end of the regex
      gotrep = grepl(pattern = '^\\d{2}/\\d{2}/\\d{4}$', x = fecha)
      
      if(!gotrep){ ## Verifying that the date follows the global pattern date
        show_semaforo('Date input must be in this numerical format: dd/mm/yyyy', 
                      2)
        return(NULL)
      }else{
        dayMonth = c(31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31)
        names(dayMonth) = 1:12
        
        ## Detecting the LEAP year
        is_leap_year <- function(year) {
          ifelse(((year %% 4 == 0 && year %% 100 != 0) || year %% 400 == 0),
                 return(TRUE),  # It's a leap year,
                 return(FALSE))  # It's not a leap year
        }
        
        ## Processing the date as a numeric vector
        date_num = as.vector(str_split_fixed(fecha, '/', 3)) %>% as.numeric()
        
        ## The year must be greater than 2000
        if(date_num[3] < 2000){
          show_semaforo('Please input a date after 01/01/2000', 2)
          return(NULL)
        }
        
        ## month data must be between 1 and 12
        if(!(date_num[2] %in% 1:12)){
          show_semaforo('The month must be between 1 and 12', 2)
          return(NULL)
        }
        
        ## day must be grater or equal to 1
        if(date_num[1] < 1){
          show_semaforo('The day must be no less than 01.' , 2)
        }
        
        ## For the second month in a leap year
        if(date_num[2] == 2 & is_leap_year(date_num[3])){
          ## last day of february must be till 29
          ifelse(date_num[1] > 29, 
                 show_semaforo('The day must be less than 29', 2),
                 return(NULL))
          
          ## else, must be till 30 or 31, depending on the month
        }else if(!(date_num[1] <= dayMonth[date_num[2]])){
          nota_date = paste('Input a valid day, must be bellow ', 
                            dayMonth[date_num[2]])
          show_semaforo(nota_date, 2)
          return(NULL)
        }
      }
    },
    Amount = function(value_list){
      value = value_list[[1]] %>% as.numeric()
      if(is.na(value)){
        show_semaforo('The amount must be a number', 2)
        return(NULL)
      }
    },
    Currency = function(value_list){},
    USD_price = function(value_list){
      value = value_list[[1]] %>% as.numeric()
      if(is.na(value)){
        show_semaforo('The USD_price must be a number', 2)
        return(NULL)
      }
    },
    USD_amount = function(value_list){
      value = value_list[[1]] %>% as.numeric()
      if(is.na(value)){
        show_semaforo('The USD_amount must be a number', 2)
        return(NULL)
      }
    },
    Description = function(value_list){},
    In_Out = function(value_list){
      value = value_list[[1]]
      c_name = value_list[[2]]
      ## Verifying that the value input is padronized
      value_dt = stringr::str_to_lower(value)
      if(!(value_dt %in% values_inout)){
        message_me = paste('You must input: in, out, earned, spent, 1 or -1 ')
        show_semaforo(message_me, 2)
        return(NULL)
      }
      
    }
  )
  
  
  ### To Download the data
  output$downloadData <- downloadHandler(
    filename = function() {
      # Use the selected dataset as the suggested file name
      paste0('myData', ".csv")
    },
    content = function(file) {
      # Write the dataset to the `file` that will be downloaded
      if(exists('theFile')){
        if(nrow(theFile$data) == 0){
          show_semaforo('There is no data to save', 1)
          return(NULL)
        }else{
          write.csv(theFile$data, file)
        }
      }else{
        show_semaforo('There is no data to save', 1)
        return(NULL)
      }
    }
  )
  
  
  ### Total value output manipulation
  inout_traductor = function(vec){
    posiv = values_inout[c(1, 3, 5)]
    negativ = values_inout[- c(1, 3, 5)]
    result = ifelse(vec %in% posiv, 1, 
                    ifelse(vec %in% negativ, -1, 0))
  }
  
  totalUsD = eventReactive(input$mySheet_cell_edit,{
    if(exists('theFile')){
      if(nrow(theFile$data) == 0){
        return('0,00 USD')
      }else{
        pivot = theFile$data[['USD_amount']] %>% as.numeric()
        inout = theFile$data[['In_Out']] %>% str_to_lower() %>%
          inout_traductor()
        pivot = pivot * inout

        total = sprintf('%.2f', sum(pivot, na.rm = T))
        total = paste(total, ' USD')
        return(total)
      }
    }else{
      print('esto rueda?')
      return('0,00 USD')
    }
  })
  
  output$totalValue = renderText({
    if(is.null(input$mySheet_cell_edit)){
      return('0,00 USD')
    }else{
      totalUsD()
    }
  })
  
  
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
    con = dbConnect(drv, dbname = 'TheDataLake.db')
    
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

