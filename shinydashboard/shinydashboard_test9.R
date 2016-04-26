library(shiny)
library(shinydashboard)
library(dygraphs)
library(quantmod)
# getSymbols("2317.TW")

tw50<-c(" 3474   華亞科 " = 3474 ," 4938   和碩 " = 4938 ," 3481   群創 " = 3481 ," 2330   台積電 " = 2330 ," 2303   聯電 " = 2303 ," 2882   國泰金 " = 2882 ," 2357   華碩 " = 2357 ," 1303   南亞 " = 1303 ," 2883   開發金 " = 2883 ," 1301   台塑 " = 1301 ," 2002   中鋼 " = 2002 ," 2311   日月光 " = 2311 ," 2317   鴻海 " = 2317 ," 1402   遠東新 " = 1402 ," 2892   第一金 " = 2892 ," 2880   華南金 " = 2880 ," 2801   彰銀 " = 2801 ," 1216   統一 " = 1216 ," 1101   台泥 " = 1101 ," 1102   亞泥 " = 1102 ," 2382   廣達 " = 2382 ," 2308   台達電 " = 2308 ," 1326   台化 " = 1326 ," 2886   兆豐金 " = 2886 ," 2891   中信金 " = 2891 ," 2325   矽品 " = 2325 ," 2105   正新 " = 2105 ," 2395   研華 " = 2395 ," 2408   南科 " = 2408 ," 2412   中華電 " = 2412 ," 2409   友達 " = 2409 ," 1476   儒鴻 " = 1476 ," 2207   和泰車 " = 2207 ," 2301   光寶科 " = 2301 ," 9904   寶成 " = 9904 ," 2912   統一超 " = 2912 ," 2354   鴻準 " = 2354 ," 2474   可成 " = 2474 ," 3045   台灣大 " = 3045 ," 2454   聯發科 " = 2454 ," 2881   富邦金 " = 2881 ," 2887   台新金 " = 2887 ," 4904   遠傳 " = 4904 ," 2885   元大金 " = 2885 ," 3008   大立光 " = 3008 ," 2884   玉山金 " = 2884 ," 2890   永豐金 " = 2890 ," 6505   台塑化 " = 6505 ," 5880   合庫金 " = 5880 ," 2227   裕日車 " = 2227 )

runTime<-0

header <- dashboardHeader(title = "我的股票回測系統",
                          dropdownMenuOutput("messageMenu")
)

sidebar<- dashboardSidebar(
  # selectizeInput(
  #   'e2', '個股', choices = state.name, multiple = TRUE
  # ),
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
                            # , sidebarLayout(
                            #   sidebarPanel(
                            # ,textInput("ID", "ID:",placeholder= "Stock Number")#placeholder IE8 9not support change use label
                            ,selectInput("ID", "個股:",tw50,selected = 2330)
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
                            actionButton("goButton", "Renew Data!", icon = icon("refresh")),
                            p("Click the button to update the value displayed in the main panel.")
                            #     # selectInput("dataset", "Choose a dataset:",choices = c("2317", "3474", "4938")),
                            #     # numericInput("obs", "Number of observations to view:", 10)
                            #   )
                            # )
                   ),
                   tabPanel("K 線圖"
                            ,verbatimTextOutput("a")
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
                                        c("MA","MACD"
                                          # ,"slider", "text", "numeric", "checkbox",
                                          # "checkboxGroup", "radioButtons", "selectInput",
                                          # "selectInput (multi)", "date", "daterange"
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
                            # selectInput("OperatingRange_type"
                            #             ,label = "空頭-多頭 操作比率 (操作金額/成本)",#inline = T,
                            #             choices = c("A" , "S", "D"), 
                            #             selected = "A"),
                            p("多頭-空頭 操作比率 (操作金額/成本)"),
                            numericInput("OperatingRange1", "多頭",value = 1.0,min=0,step=0.1),
                            numericInput("OperatingRange2", "空頭",value = 0,max=0),
                            br(),
                            actionButton("goButton2", "Renew Data!", icon = icon("refresh")),
                            p("Click the button to update the value displayed in the main panel.")
                            
                   )
            )
    ),
    tabItem(tabName = "return_week",
            tabsetPanel(type ="pills",
                        tabPanel("return.week",width = 9,
                                 dataTableOutput("returnWeek")
                        )
            )
    ),
    
    tabItem(tabName = "return_annual",
            tabsetPanel(type ="pills",
                        tabPanel("return.Annual",
                                 dataTableOutput("returnAnnual")
                        )
            )
    ),
    
    tabItem(tabName = "return_each",
            tabsetPanel(type ="pills",
                        tabPanel("return.Frequency",
                                 dataTableOutput("returnFrequency")
                        ),
                        tabPanel("return.Frequency.report",
                                 tableOutput("returnFrequency_Report2"),
                                 hr(),
                                 dataTableOutput("returnFrequency_Report1")
                        )
            )
    )
  )
)



ui <- dashboardPage(header,sidebar,body)

server <- function(input, output, session) {
  
  output$a<- renderText({
   input$ID
  })
  
  datasetInput <- eventReactive(input$goButton, {
    if(input$radio ==1)getStockData(input$ID,as.Date(input$daterange[1]),as.Date(input$daterange[2]),autofrom=input$daterange_days)
    if(input$radio ==2)getStockData(input$ID,all=T)
    if(input$radio ==3)getStockData(input$ID,beforeDay=as.numeric(input$days))
    get(paste0("TW.",input$ID))
  })
  
  output$view <- renderTable({
    as.data.frame(tail(datasetInput(), n = input$obs))
  })
  
  output$plot2 <- renderPlot({
    data<-datasetInput()[input$slider1[1]:input$slider1[2]]
    chartSeries(data,theme=chartTheme('white'))
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
           "MACD" = numericInput("MACD_1", "快速 移動平均線",value = 12)
           # "slider" = sliderInput("dynamic", "Dynamic", min = 1, max = 20, value = 10),
           # "text" = textInput("dynamic", "Dynamic",value = "starting value"),
           # "numeric" =  numericInput("dynamic", "Dynamic",value = 12),
           # "checkbox" = checkboxInput("dynamic", "Dynamic",value = TRUE),
           # "checkboxGroup" = checkboxGroupInput("dynamic", "Dynamic",choices = c("Option 1" = "option1","Option 2" = "option2"),
           #                                      selected = "option2"),
           # "radioButtons" = radioButtons("dynamic", "Dynamic",
           #                               choices = c("Option 1" = "option1",
           #                                           "Option 2" = "option2"),
           #                               selected = "option2"
           # ),
           # "selectInput" = selectInput("dynamic", "Dynamic",
           #                             choices = c("Option 1" = "option1",
           #                                         "Option 2" = "option2"),
           #                             selected = "option2"
           # ),
           # "selectInput (multi)" = selectInput("dynamic", "Dynamic",
           #                                     choices = c("Option 1" = "option1",
           #                                                 "Option 2" = "option2"),
           #                                     selected = c("option1", "option2"),
           #                                     multiple = TRUE
           # ),
           # "date" = dateInput("dynamic", "Dynamic"),
           # "daterange" = dateRangeInput("dynamic", "Dynamic")
    )
  })
  
  output$ui2 <- renderUI({
    if (is.null(input$input_type))
      return()
    
    # Depending on input$input_type, we'll generate a different
    # UI component and send it to the client.
    switch(input$input_type,
           "MA" = numericInput("MA_2", "移動平均線 2",value = 60),
           "MACD" = numericInput("MACD_2", "緩慢 移動平均線",value = 26)
    )
  })
  
  output$ui3 <- renderUI({
    if (is.null(input$input_type))
      return()
    
    # Depending on input$input_type, we'll generate a different
    # UI component and send it to the client.
    switch(input$input_type,
           "MA" =NULL,
           "MACD" = numericInput("MACD_3", "signal moving average",value = 9)
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
  
  choiceDataCol<- eventReactive(input$goButton2, {
    if (is.null(input$input_type))
      return()
    switch(input$data_col,
           "option1" =datasetInput()[,4],
           "option2" = datasetInput()[,6]
    )
  })
  
  AnalyzingFormula_A<- eventReactive(input$goButton2, {
    if (is.null(input$input_type))
      return()
    # choiceDataCol<-if(input$data_col=="option1")datasetInput()[,4]else if(input$data_col=="option2")datasetInput()[,6]
    switch(input$input_type,
           "MA" =runMean(as.numeric(choiceDataCol()),n=input$MA_1),
           "MACD" = MACD(choiceDataCol(), input$MACD_1,input$MACD_2,input$MACD_3, maType="EMA" )[,1]
    )
  })
    
    AnalyzingFormula_B<- eventReactive(input$goButton2, {
      if (is.null(input$input_type))
        return()
      # choiceDataCol<-if(input$data_col=="option1")datasetInput()[,4]else if(input$data_col=="option2")datasetInput()[,6]
      switch(input$input_type,
             "MA" =runMean(as.numeric(choiceDataCol()),n=input$MA_2),
             "MACD" = MACD(choiceDataCol(), input$MACD_1,input$MACD_2,input$MACD_3,  maType="EMA" )[,2]
      )
    })
  
  datareturnWeek <- eventReactive(input$goButton2, {
    # AnalyzingFormula(datasetInput(),datasetInput2())
    AnalyzingFormula(datasetInput(),datasetInput2()
                     ,AnalyzingFormula_A=AnalyzingFormula_A()
                     ,AnalyzingFormula_B=AnalyzingFormula_B()
                     ,Bullish=input$OperatingRange1
                     ,Bearish=input$OperatingRange2
                     ,nameNum=F
    )
    WeekRateOfReturn(input$ID,startDate=startDate,endDate=endDate)
  })
  
  output$returnWeek <-  renderDataTable({
    datareturnWeek()
  })
  
  datareturnAnnual <- eventReactive(input$goButton2, {
    # AnalyzingFormula(datasetInput(),datasetInput2())
    AnalyzingFormula(datasetInput(),datasetInput2()
                     ,AnalyzingFormula_A=AnalyzingFormula_A()
                     ,AnalyzingFormula_B=AnalyzingFormula_B()
                     ,Bullish=input$OperatingRange1
                     ,Bearish=input$OperatingRange2
                     ,nameNum=F
    )
    AnnualRateOfReturn(input$ID,startDate=startDate,endDate=endDate)
  })
  
  output$returnAnnual <- renderDataTable({
    datareturnAnnual()
  })
  
  datareturnFrequency <- eventReactive(input$goButton2, {
    # AnalyzingFormula(datasetInput(),datasetInput2())
    AnalyzingFormula(datasetInput(),datasetInput2()
                     ,AnalyzingFormula_A=AnalyzingFormula_A()
                     ,AnalyzingFormula_B=AnalyzingFormula_B()
                     ,Bullish=input$OperatingRange1
                     ,Bearish=input$OperatingRange2
                     ,nameNum=F
    )
    FrequencyOfReturn(input$ID,startDate=startDate,endDate=endDate)
  })
  
  output$returnFrequency <-renderDataTable({
    datareturnFrequency()
  })
  
  datareturnFrequency_Report <- eventReactive(input$goButton2, {
    # AnalyzingFormula(datasetInput(),datasetInput2())
    AnalyzingFormula(datasetInput(),datasetInput2()
                     ,AnalyzingFormula_A=AnalyzingFormula_A()
                     ,AnalyzingFormula_B=AnalyzingFormula_B()
                     ,Bullish=input$OperatingRange1
                     ,Bearish=input$OperatingRange2
                     ,nameNum=F
    )
    FrequencyOfReturn_Report(get(paste0("FrequencyOfReturn.",input$ID)),Btxts_position=Btxts_position)
    # get("FrequencyOfReturn_Report1")
  })
  
  output$returnFrequency_Report1 <-renderDataTable({
    datareturnFrequency_Report()
    get("FrequencyOfReturn_Report1")
  })
  
  output$returnFrequency_Report2 <-renderTable({
    datareturnFrequency_Report()
    get("FrequencyOfReturn_Report2")
  })
  
}

shinyApp(ui, server)
