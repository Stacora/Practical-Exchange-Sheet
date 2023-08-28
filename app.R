library(shiny)
library(DT)

new_Data = data.frame(Date = character(0),
                      Amount = character(0),
                      Currency = character(0),
                      USD_price = character(0),
                      USD_amaount = character(0),
                      Description = character(0))
# new_Data = data.frame(Date = NA,
#                       Amount = NA,
#                       Currency = NA,
#                       USD_price = NA,
#                       USD_amaount = NA,
#                       Description = NA)

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
    new_line = data.frame(Date = dateInput('date_it', 'Date', value = Sys.Date()),
                          Amount = NA,
                          Currency = NA,
                          USD_price = NA,
                          USD_amaount = NA,
                          Description = NA)
    
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
    
    pivot = theFile$data
    
    pivot[row_dt, col_dt] = value_dt
    theFile$data <<- pivot
    
    if(nrow(pivot) == row_dt){
      addme_lines()
    }
  })
  
  evaluate_data = function(value, column){
    
  }
  
}

# Run the application 
shinyApp(ui = ui, server = server)

## Quiero colocar dateInput() en la columna de data de DT table. Es posible.
## Visitar el siguiente link
# https://stackoverflow.com/questions/55034483/shiny-widgets-in-dt-table
# https://stackoverflow.com/questions/48160173/r-shiny-extract-values-from-numericinput-datatable-column
