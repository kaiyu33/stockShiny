# library(shiny)
# library(shinydashboard)
# library(dygraphs)
# library(quantmod)
# getSymbols("2317.TW")

runTime<-0

header <- dashboardHeader(title = "我的股票回測系統",
                          dropdownMenuOutput("messageMenu")
)

sidebar<- dashboardSidebar(
  selectizeInput(
    'e2', '個股', choices = state.name, multiple = TRUE
  ),
    shinyUI(fluidPage(
      textOutput("currentTime")
    )),
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
            tabBox(width = 12,
                   title = "First tabBox",
                   # The id lets us use input$tabset1 on the server to find the current tab
                   id = "tabset1", 
                   
                   tabPanel("資料選取"
                            ,verbatimTextOutput("a")
                            # , sidebarLayout(
                            #   sidebarPanel(
                            ,textInput("ID", "ID:",placeholder= "Stock Number")#placeholder IE8 9not support change use label
                            ,hr()
                            ,radioButtons("radio", label = h3("期間 : "),
                                         choices = list("日期" = 1, "全部" = 2, "天數" = 3), 
                                         selected = 1)
                            
                            ,dateRangeInput("daterange", "Date range:",
                                            start  = "2015-01-01",
                                            end    = Sys.Date(),
                                            min    = "1993-01-01",
                                            max    = Sys.Date(),
                                            format = "yyyy/mm/dd",
                                            language = "zh-TW",
                                            separator = " 到 ",
                                            weekstart = 1
                            ),
                            numericInput("days", "天數:", 300,min = 1),
                            br(),
                            actionButton("goButton", "Go!", icon = icon("refresh")),
                            p("Click the button to update the value displayed in the main panel.")
                            #     # selectInput("dataset", "Choose a dataset:",choices = c("2317", "3474", "4938")),
                            #     # numericInput("obs", "Number of observations to view:", 10)
                            #   )
                            # )
                   ),
                   tabPanel("K 線圖"
                            ,  fluidRow(
                              box(width = 12,
                                  plotOutput("plot2")
                              )
                            )
                            ,  fluidRow(
                              box( width = 12,title = "Controls",
                                   sliderInput("slider1","Number of observations:",1,nrow(`2317.TW`),c(1,nrow(`2317.TW`)/2))
                              )
                            )
                   ),
                   tabPanel("詳細資料"
                            # ,  fluidRow(
                            # box(width = 3,
                            # textInput("ID", "ID:",placeholder= "Stock Number")#placeholder IE8 9not support change use label
                            # selectInput("dataset", "Choose a dataset:",choices = c("2317", "3474", "4938")),
                            ,numericInput("obs", "Number of observations to view:", 10)
                            # )
                            # , box(
                            ,tableOutput("view")
                            # )
                            # )
                   ),
                   tabPanel("數據分析",icon = icon("filter"),
                            selectInput("input_type", "Input type",
                                        c("MA","slider", "text", "numeric", "checkbox",
                                          "checkboxGroup", "radioButtons", "selectInput",
                                          "selectInput (multi)", "date", "daterange"
                                        )
                            ),
                            uiOutput("ui"),
                            tags$p("Input type:"),
                            verbatimTextOutput("input_type_text"),
                            tags$p("Dynamic input value:"),
                            verbatimTextOutput("dynamic_value")
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

server <- function(input, output, session) {
  
  output$a<- renderText({
    as.character(input$daterange)
    paste0("\"",as.Date(input$daterange[1]),"\"","\"",as.Date(input$daterange[2]),"\"")
  })
  
  datasetInput <- eventReactive(input$goButton, {
    if(input$radio ==1)getStockData(input$ID,"2010-01-01","2016-04-17",autofrom=120)
    if(input$radio ==2)getStockData(input$ID,all=T)
    if(input$radio ==3)getStockData(input$ID,beforeDay=as.numeric(input$days))
    get(paste0("TW.",input$ID))
  })
  
  output$view <- renderTable({
    tail(datasetInput(), n = input$obs)
  })
  
  output$plot2 <- renderPlot({
    data2<-datasetInput()[input$slider1[1]:input$slider1[2]]
    chartSeries(data2)
  })
  
    output$currentTime <- renderText({
      invalidateLater(1000, session)
      paste(Sys.time())
    })
 
    output$ui <- renderUI({
      if (is.null(input$input_type))
        return()
      
      # Depending on input$input_type, we'll generate a different
      # UI component and send it to the client.
      switch(input$input_type,
             "MA" = numericInput("dynamic", "Dynamic",value = 12),
             "slider" = sliderInput("dynamic", "Dynamic", min = 1, max = 20, value = 10),
             "text" = textInput("dynamic", "Dynamic",value = "starting value"),
             "numeric" =  numericInput("dynamic", "Dynamic",value = 12),
             "checkbox" = checkboxInput("dynamic", "Dynamic",value = TRUE),
             "checkboxGroup" = checkboxGroupInput("dynamic", "Dynamic",choices = c("Option 1" = "option1","Option 2" = "option2"),
                                                  selected = "option2"),
             "radioButtons" = radioButtons("dynamic", "Dynamic",
                                           choices = c("Option 1" = "option1",
                                                       "Option 2" = "option2"),
                                           selected = "option2"
             ),
             "selectInput" = selectInput("dynamic", "Dynamic",
                                         choices = c("Option 1" = "option1",
                                                     "Option 2" = "option2"),
                                         selected = "option2"
             ),
             "selectInput (multi)" = selectInput("dynamic", "Dynamic",
                                                 choices = c("Option 1" = "option1",
                                                             "Option 2" = "option2"),
                                                 selected = c("option1", "option2"),
                                                 multiple = TRUE
             ),
             "date" = dateInput("dynamic", "Dynamic"),
             "daterange" = dateRangeInput("dynamic", "Dynamic")
      )
    })
    
    output$input_type_text <- renderText({
      input$input_type
    })
    
    output$dynamic_value <- renderPrint({
      str(input$dynamic)
    })
    
     
}

shinyApp(ui, server)
