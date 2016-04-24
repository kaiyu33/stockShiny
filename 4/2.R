#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#4
#改以輸入數字
library(shiny)
# library(datasets)

# Define UI for application that draws a histogram
ui <- shinyUI(fluidPage(
  
  # Application title
  titlePanel("Taiwan Stock Market"),
  ##############################################################################START UI
  # Sidebar with controls to select a dataset and specify the
  # number of observations to view
  sidebarLayout(
    sidebarPanel(
      textInput("ID", "ID:",placeholder= "Stock Number"),#placeholder IE8 9not support change use label
      
      # selectInput("dataset", "Choose a dataset:",choices = c("2317", "3474", "4938")),
      
      numericInput("obs", "Number of observations to view:", 10)
    ),
    
    # Show a summary of the dataset and an HTML table with the
    # requested number of observations
    mainPanel(
      h3(textOutput("ID", container = span)),#server used input$ID
      
      # verbatimTextOutput("summary"),
      
      tableOutput("view")
    )
  )
  
  ##############################################################################END UI
))

# Define server logic required to draw a histogram
server <- shinyServer(function(input, output) {
  ##############################################################################START server
  
  # Return the requested dataset
  datasetInput <- reactive({
    getStockData(input$ID,"2010-01-01","2016-04-17",autofrom=120)
    get(paste0("TW.",input$ID))
  })
  
  # # Generate a summary of the dataset
  # output$summary <- renderPrint({
  #   dataset <- datasetInput()
  #   summary(dataset)
  # })
  
  output$caption <- renderText({
    input$caption
  })
  
  # Show the first "n" observations
  output$view <- renderTable({
    tail(datasetInput(), n = input$obs)
  })
  
  ##############################################################################END server
})
# Run the application
shinyApp(ui = ui, server = server)
##############################################################################
