#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#3
#解決一直重刷資料的問題
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
      selectInput("dataset", "Choose a dataset:",
                  choices = c("2317", "3474", "4938")),
      
      numericInput("obs", "Number of observations to view:", 10)
    ),
    
    # Show a summary of the dataset and an HTML table with the
    # requested number of observations
    mainPanel(
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
    Symbols.name<-paste0(input$dataset,".TW")
    
    verbose <- FALSE
    tmp <- tempfile()
    on.exit(unlink(tmp))
    beforeDay<-300##########################取距離今天幾天前
    default.from <- as.Date(as.numeric(Sys.Date())+25569-beforeDay, origin = "1899-12-30")
    default.to <- Sys.Date()
    from <- default.from
    to <- default.to
    from.y <- as.numeric(strsplit(as.character(as.Date(from,origin='1970-01-01')),'-',)[[1]][1])
    from.m <- as.numeric(strsplit(as.character(as.Date(from,origin='1970-01-01')),'-',)[[1]][2])-1
    from.d <- as.numeric(strsplit(as.character(as.Date(from,origin='1970-01-01')),'-',)[[1]][3])
    to.y <- as.numeric(strsplit(as.character(as.Date(to,origin='1970-01-01')),'-',)[[1]][1])
    to.m <- as.numeric(strsplit(as.character(as.Date(to,origin='1970-01-01')),'-',)[[1]][2])-1
    to.d <- as.numeric(strsplit(as.character(as.Date(to,origin='1970-01-01')),'-',)[[1]][3])
    yahoo.URL <- "http://ichart.finance.yahoo.com/table.csv?"
    download.file(paste(yahoo.URL,
                        "s=",Symbols.name,
                        "&a=",from.m,
                        "&b=",sprintf('%.2d',from.d),
                        "&c=",from.y,
                        "&d=",to.m,
                        "&e=",sprintf('%.2d',to.d),
                        "&f=",to.y,
                        "&g=d&q=q&y=0",
                        "&z=",Symbols.name,"&x=.csv",
                        sep=''),destfile=tmp,quiet=!verbose)
    tw50_data <- read.csv(tmp,stringsAsFactors=FALSE)
    tw50_data<-mutate(tw50_data,"Open"=as.numeric(Open),"High"=as.numeric(High),"Low"=as.numeric(Low),"Close"=as.numeric(Close),"Volume"=as.numeric(Volume),"Adj"=as.numeric(Adj.Close))
    rownames(tw50_data)<-tw50_data[[1]]
    sample.xts<-select(tw50_data,-7)#顯示日期
    sample.xts <- as.xts(sample.xts, descr='my new xts object')
  })
  
  # # Generate a summary of the dataset
  # output$summary <- renderPrint({
  #   dataset <- datasetInput()
  #   summary(dataset)
  # })
  
  # Show the first "n" observations
  output$view <- renderTable({
    
    
    tail(datasetInput(), n = input$obs)
  })
  
  ##############################################################################END server
})
# Run the application
shinyApp(ui = ui, server = server)
##############################################################################
