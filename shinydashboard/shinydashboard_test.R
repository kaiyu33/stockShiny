library(shiny)
library(shinydashboard)
library(dygraphs)
library(quantmod)
getSymbols("2317.TW")

header <- dashboardHeader(title = "我的股票回測系統",
                          dropdownMenuOutput("messageMenu")
)

sidebar<- dashboardSidebar(
  selectizeInput(
    'e2', '個股', choices = state.name, multiple = TRUE
  ),
  sidebarMenu(
    menuItem("回測數據"
             , tabName = "dashboard"
             , icon = icon("dashboard")
    ),
    menuItem("報酬率"
             , icon = icon("th")
             ,menuSubItem("周報酬", tabName = "return_week")
             ,menuSubItem("年報酬", tabName = "return_annual")
             ,menuSubItem("每次交易報酬", tabName = "return_each")
    )
    
  )
)

body <- dashboardBody(
  tabItems(
    
    tabItem(tabName = "dashboard",
            fluidRow(
              box(plotOutput("plot2")),
              
              box(
                title = "Controls",
                sliderInput("slider1", "Number of observations:", 1, nrow(`2317.TW`), nrow(`2317.TW`)/2)
              )
            )
    ),
    # Second tab content
    tabItem(tabName = "return_week",
            h2("return.week"),
            fluidRow(
              # box(plotOutput("plot3", height = 250))
            )
    ),
    
    tabItem(tabName = "return_annual",
            h2("return.annual")
    ),
    
    tabItem(tabName = "return_each",
            h2("return.each")
    )
  )
)


ui <- dashboardPage(header,sidebar,body)

server <- function(input, output) {
  set.seed(122)
  histdata <- rnorm(500)
  
  output$plot2 <- renderPlot({
    data2<-`2317.TW`[input$slider1:nrow(`2317.TW`)]
    chartSeries(data2)
  })
  
  
}

shinyApp(ui, server)
