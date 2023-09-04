library(shiny)
library(DT)
library(stringr)
library(dplyr)

generate_empty = function(NAs = F){
  if(NAs){
    a = data.frame(Date = NA,
                   Amount = NA,
                   Currency = NA,
                   USD_price = NA,
                   USD_amaount = NA,
                   Description = NA,
                   In_Out = NA)
    return(a)
  }
  a = data.frame(Date = character(0),
                Amount = character(0),
                Currency = character(0),
                USD_price = character(0),
                USD_amaount = character(0),
                Description = character(0),
                In_Out = character(0))
  return(a)
}


new_Data = generate_empty()

# Define UI for application that draws a histogram
ui <- fluidPage(
  fluidRow(
    column(8,
           tags$h1('Practical Sheet'),
           column(6,
                  fileInput('inputData', 'Import your Data')),
           column(6,
                  br(),
                  actionButton('newDoc', 'New file'))
           ),
    column(4,
           textOutput('totalValue'))
  ),
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
           actionButton('submitLine', 'Submit Line'))
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  ## prepare new data
  observeEvent(input$newDoc, {
    theFile <<- reactiveValues(data = new_Data)
  })
  
  ## import data
  observeEvent(input$inputData, {
    file_u = read.csv(input$inputData$datapath, sep = ',')
    theFile <<- reactiveValues(data = file_u)
  })
  
  ## preparing the display for the data
  output$mySheet = renderDT({
    if(isTruthy(input$newDoc) | isTruthy(input$inputData)){
      theFile$data
    }
  },
  editable = list(target = 'cell'), 
  server = T,
  selection = 'none',
  class = 'cell-border stripe')
  
  addme_lines = reactive({
    new_line = generate_empty(T)
    
    theFile$data <<- rbind(theFile$data, new_line)
    updateNumericInput(inputId = 'indexToDrop', value = nrow(theFile$data))
  })
  
  ## Adding a new line
  observeEvent(input$addline, {
    if(!exists('theFile')){
      # in case the file doesn't exist, a notification will appear
      showNotification('No data was submited or created yet.')
    }else{
      addme_lines()
      # print(theFile$data)
    }
  })
  
  observeEvent(input$dropline, {
    
    if(!exists('theFile')){
      showNotification('No data was submited or created yet.')
    }else if(is.null(theFile$data) | (nrow(theFile$data) == 0)){
      showNotification('No data was submited or created yet.')
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
    
    # if(c_names == 'In_Out'){
    #   values_inout = c('in', 'out', 'earned', 'spent', 1, -1)
    #   value_dt = stringr::str_to_lower(value_dt)
    #   if(!(value_dt %in% values_inout)){
    #     message_me = paste('You must input: in, out, earned, spent, 1 or -1 ')
    #     showNotification(message_me)
    #     return(NULL)
    #   }
    # }
    
    
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
        showNotification('Date input must be in this numerical format: dd/mm/yyyy')
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
          showNotification('Please input a date after 01/01/2000')
          return(NULL)
        }
        
        ## month data must be between 1 and 12
        if(!(date_num[2] %in% 1:12)){
          showNotification('The month must be between 1 and 12')
          return(NULL)
        }
        
        ## day must be grater or equal to 1
        if(date_num[1] < 1){
          showNotification('The day must be no less than 01.')
        }
        
        ## For the second month in a leap year
        if(date_num[2] == 2 & is_leap_year(date_num[3])){
          ## last day of february must be till 29
          ifelse(date_num[1] > 29, 
                 showNotification('The day must be less than 29'),
                 return(NULL))
          
          ## else, must be till 30 or 31, depending on the month
        }else if(!(date_num[1] <= dayMonth[date_num[2]])){
          nota_date = paste('Input a valid day, must be bellow ', 
                            dayMonth[date_num[2]])
          showNotification(nota_date)
          return(NULL)
        }
      }
    },
    Amount = function(value_list){
      print('hello')
    },
    Currency = function(value_list){},
    USD_price = function(value_list){},
    Description = function(value_list){},
    In_Out = function(value_list){
      value = value_list[[1]]
      c_name = value_list[[2]]
      ## Verifying that the value input is padronized
      values_inout = c('in', 'out', 'earned', 'spent', 1, -1)
      value_dt = stringr::str_to_lower(value)
      if(!(value_dt %in% values_inout)){
        message_me = paste('You must input: in, out, earned, spent, 1 or -1 ')
        showNotification(message_me)
        return(NULL)
      }
      
    }
  )
  
}

# Run the application 
shinyApp(ui = ui, server = server)

## Quiero colocar dateInput() en la columna de data de DT table. Es posible.
## Visitar el siguiente link
# https://stackoverflow.com/questions/55034483/shiny-widgets-in-dt-table
# https://stackoverflow.com/questions/48160173/r-shiny-extract-values-from-numericinput-datatable-column
