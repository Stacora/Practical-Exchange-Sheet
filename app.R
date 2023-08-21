library(shiny)
library(DT)

new_Data = data.frame(Date = character(0),
                      Amount = character(0),
                      Currency = character(0),
                      USD_price = character(0),
                      USD_amaount = character(0),
                      Description = character(0))

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
    column(2,
           br(),
           actionButton('addline', 'Add Line'))
  ),
  textOutput('testando')
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
  editable = list(target = 'row', disable = 0), 
  server = T,
  selection = 'single')
  
  observeEvent(input$addline, {
    if(!exists('theFile')){
      showNotification('No data was submited or created yet.')
    }else{
      new_line = data.frame(Date = NA,
                            Amount = NA,
                            Currency = NA,
                            USD_price = NA,
                            USD_amaount = NA,
                            Description = NA)
      
      theFile$data <<- rbind(theFile$data, new_line)
      updateNumericInput(inputId = 'indexToDrop', value = nrow(theFile$data))
    }
  })
  
  observeEvent(input$dropline, {
    pivot = theFile$data
    pivot = pivot[-input$indexToDrop, ]
    theFile$data <<- pivot
    updateNumericInput(inputId = 'indexToDrop', value = nrow(theFile$data))
  })
  
  observeEvent(input$mySheet_rows_selected, {
    updateNumericInput(inputId = 'indexToDrop', value = input$mySheet_rows_selected)
  })
  
  output$testando = renderPrint({
    input$mySheet_rows_selected
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
