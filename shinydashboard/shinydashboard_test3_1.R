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
                            ,radioButtons("radio", label = h3("期間 : "),inline = T,
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
                            numericInput("daterange_days", "期間的回溯天數:", 120,min = 0),
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
                              box( width = 12,
                                   uiOutput("plot2_range")
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
                            selectInput("input_type", "技術分析",
                                        c("MA","MACD","slider", "text", "numeric", "checkbox",
                                          "checkboxGroup", "radioButtons", "selectInput",
                                          "selectInput (multi)", "date", "daterange"
                                        )
                            ),
                            radioButtons("data_col","選擇資料欄位",
                                         choices = c("價格 收盤價" = "option1",
                                                     "價格 還原除權息" = "option2"),
                                         selected = "option1"
                            ),
                            uiOutput("ui1"),
                            uiOutput("ui2"),
                            uiOutput("ui3"),
                            tags$p("Input type:"),
                            verbatimTextOutput("input_type_text"),
                            tags$p("Dynamic input value:"),
                            verbatimTextOutput("dynamic_value"),
                            selectInput("OperatingRange_type"
                                        ,label = "空頭-多頭 操作比率 (操作金額/成本)",#inline = T,
                                        choices = c("A" , "S", "D"), 
                                        selected = "A"),
                            uiOutput("OperatingRange1"),
                            br(),
                            actionButton("goButton2", "Go!", icon = icon("refresh")),
                            p("Click the button to update the value displayed in the main panel.")
                            
                   )
            )
    ),
    # Second tab content
    tabItem(tabName = "return_week",
            h2("return.week")
            # fluidRow(
            ,box(
            tableOutput("returnWeek")
            )
            # box(plotOutput("plot3", height = 250))
            # )
    ),
    
    tabItem(tabName = "return_annual",
            h2("return.annual")
            ,box(
                          tableOutput("returnAnnual")
            )
    ),
    
    tabItem(tabName = "return_each",
            h2("return.each")
            ,box(
              tableOutput("returnFrequency")
            )
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
    if(input$radio ==1)getStockData(input$ID,as.Date(input$daterange[1]),as.Date(input$daterange[2]),autofrom=input$daterange_days)
    if(input$radio ==2)getStockData(input$ID,all=T)
    if(input$radio ==3)getStockData(input$ID,beforeDay=as.numeric(input$days))
    get(paste0("TW.",input$ID))
  })
  
  output$view <- renderTable({
    tail(datasetInput(), n = input$obs)
  })
  
  output$plot2 <- renderPlot({
    data2<-datasetInput()[input$slider1[1]:input$slider1[2]]
    chartSeries(data2,theme=chartTheme('white'))
  })
  
  output$plot2_range <- renderUI({
    if (is.null(input$ID))
      return()
    sliderInput("slider1","Number of days:",1,nrow(datasetInput()),c((nrow(datasetInput())/2),nrow(datasetInput())),ticks=F,dragRange=T)
  })
  
  output$currentTime <- renderText({
    invalidateLater(1000, session)
    paste(Sys.time())
  })
  
  output$ui1 <- renderUI({
    if (is.null(input$input_type))
      return()
    
    # Depending on input$input_type, we'll generate a different
    # UI component and send it to the client.
    switch(input$input_type,
           "MA" = numericInput("MA_1", "移動平均線 1",value = 20),
           "MACD" = numericInput("dynamic", "Dynamic",value = 12),
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
  
  output$ui2 <- renderUI({
    if (is.null(input$input_type))
      return()
    
    # Depending on input$input_type, we'll generate a different
    # UI component and send it to the client.
    switch(input$input_type,
           "MA" = numericInput("MA_2", "移動平均線 2",value = 20),
           "MACD" = numericInput("dynamic", "Dynamic",value = 26)
    )
  })
  
  output$ui3 <- renderUI({
    if (is.null(input$input_type))
      return()
    
    # Depending on input$input_type, we'll generate a different
    # UI component and send it to the client.
    switch(input$input_type,
           "MA" =NULL,
           "MACD" = numericInput("dynamic", "Dynamic",value = 9)
    )
  })
  
  output$OperatingRange1 <- renderUI({
    if (is.null(input$OperatingRange_type))
      return()
    
    # Depending on input$input_type, we'll generate a different
    # UI component and send it to the client.
    switch(input$OperatingRange_type,
           "A" =numericInput("dynamic", "Dynamic",value = 9),
           "S" = numericInput("dynamic", "Dynamic",value = 9),
           "D" = numericInput("dynamic", "Dynamic",value = 9)
    )
  })
  
  output$input_type_text <- renderText({
    input$input_type
  })
  
  output$dynamic_value <- renderPrint({
    str(input$dynamic)
  })
  
  datasetInput2 <- eventReactive(input$goButton2, {
    getEDxts(input$ID,F)
    # AnalyzingFormula(datasetInput(),EDxts)
  })
  
  datareturnWeek <- eventReactive(input$goButton2, {
      AnalyzingFormula(datasetInput(),datasetInput2())
      WeekRateOfReturn(input$ID,startDate=startDate,endDate=endDate)
  })
  
  output$returnWeek <-  renderTable({
    datareturnWeek()
  })
  
  datareturnAnnual <- eventReactive(input$goButton2, {
    AnalyzingFormula(datasetInput(),datasetInput2())
    AnnualRateOfReturn(input$ID,startDate=startDate,endDate=endDate)
  })
  
  output$returnAnnual <- renderTable({
    datareturnAnnual()
  })
  
  datareturnFrequency <- eventReactive(input$goButton2, {
    AnalyzingFormula(datasetInput(),datasetInput2())
    FrequencyOfReturn(input$ID,startDate=startDate,endDate=endDate)
  })
  
  output$returnFrequency <-renderTable({
    datareturnFrequency()
  })
}

shinyApp(ui, server)
