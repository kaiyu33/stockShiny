library(shiny)
library(shinydashboard)
library(dygraphs)
library(quantmod)


ui <- dashboardPage(
  #1
  dashboardHeader(title = "我的股票回測系統",
                  dropdownMenuOutput("messageMenu")
                  ),
  #2
  dashboardSidebar(
    sidebarMenu(
      menuItem("回測數據", tabName = "dashboard", icon = icon("dashboard")),
      menuItem("每次交易報酬", tabName = "widgets", icon = icon("th")),
      menuItem("周報酬", tabName = "widgets1", icon = icon("th")),
      menuItem("年報酬", tabName = "widgets2", icon = icon("th"))
    )
  ),
  #3
  dashboardBody(
    tabItems(
      # First tab content
      tabItem(tabName = "dashboard",
              fluidRow(
                box(plotOutput("plot1", height = 250)),
                
                box(
                  title = "Controls",
                  sliderInput("slider", "Number of observations:", 1, 100, 50)
                )
              )
      ),
      tabItem(tabName = "widgets",
              fluidRow(
                box(plotOutput("plot2", height = 250)),
                
                box(
                  title = "Controls",
                  sliderInput("slider1", "Number of observations:", 1, nrow(`2317.TW`), nrow(`2317.TW`)/2)
                )
              )
      ),
      # Second tab content
      tabItem(tabName = "widgets1",
              h2("Widgets tab content"),
              fluidRow(
                box(plotOutput("plot3", height = 250)))
              
              
      ),
      tabItem(tabName = "widgets2",
              h2("Widgets tab content")
      )
      
    )
  )
)

server <- function(input, output) {
  set.seed(122)
  histdata <- rnorm(500)
  
  output$plot1 <- renderPlot({
    data <- histdata[seq_len(input$slider)]
    hist(data)
  })
  
  getSymbols("2317.TW")
  output$plot2 <- renderPlot({
    data2<-`2317.TW`[input$slider1:nrow(`2317.TW`)]
    chartSeries(data2)
  })
 
 
  })
 
  
}

shinyApp(ui, server)