library(shiny)

ui <- fluidPage(
  fileInput("file1", "Choose CSV File",multiple = TRUE,accept = c("text/csv","text/comma-separated-values,text/plain",".csv")),
  tags$hr(),# Horizontal line ----
  # Input: Checkbox if file has header ----
  checkboxInput("header", "Header", TRUE),
  # Input: Select separator ----
  radioButtons("sep", "Separator",choices = c(Comma = ",",Semicolon = ";",Tab = "\t"),selected = ","),
  # Input: Select quotes ----
  radioButtons("quote", "Quote",choices = c(None = "","Double Quote" = '"',"Single Quote" = "'"),selected = '"'),
  tags$hr(), # Horizontal line ----
  # Input: Select number of rows to display ----
  radioButtons("disp", "Display",choices = c(Head = "head",All = "all"),selected = "head"),
  actionButton("submit_data", "Submit")
)

server <- function(input, output){
  v <- reactiveValues(data = NULL)
  
  observeEvent(input$submit_data, {
    v$data <-  read.csv(input$file1$datapath,
                                 header = input$header,
                                 sep = input$sep,
                                 quote = input$quote)
  })
  
  observeEvent(input$reset, {
    v$data <- NULL
  })  
  
  output$plot <- renderPlot({
    if (is.null(v$data)) return()
    print(v$data)
  })
}

shinyApp(ui, server)