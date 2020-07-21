library(shiny)
library(shinydashboard)
library(readxl)
library(writexl)
library(tidyverse)
library(DT)
library(data.table)
library(plotly)
library(leaflet)
library(formattable)


# Load Dataset----
mydata <- read_excel("import_data/serviceperformance.xlsx")
  mydata$OpenDate <- as.Date(mydata$OpenDate)
  mydata$CloseDate <- as.Date(mydata$CloseDate)
  
failuredata <- read_excel("import_data/techreportsummary.xlsx")
  failuredata$OpenDate <- as.Date(failuredata$OpenDate)
  
branchcode <- read_excel("import_data/branchcode.xlsx")
  branchcode$BranchCode <- as.character(branchcode$BranchCode)
  
populationdata <- fread("import_data/datapopulasi.csv")

# User Interface----
ui <- dashboardPage(
  
  # Dashboard Header----
  dashboardHeader(
    title = "Service A&F Dashboard",
    titleWidth = 250
    ),
  
  # Dashboard Sidebar----
  dashboardSidebar(
    width = 250,
    sidebarMenu(
      menuItem("Performance", tabName = "performance", icon = icon("dashboard")),
      menuItem("Valuation", tabName = "valuation", icon = icon("credit-card")),
      menuItem("Outstanding Job", tabName = "outstanding", icon = icon("exclamation")),
      menuItem("Failure Analysis", tabName = "failure", icon = icon("list")),
      menuItem("Unit Population", tabName = "population", icon = icon("globe")),
      menuItem("Dataset", tabName = "dataset", icon = icon("table")),
      menuItem("Source Code", tabName = "sourcecode", icon = icon("code")),
      selectInput(
        inputId = "agency", 
        label = "Select Agency",
        choices = c("All", unique(mydata$Agency)),
        selected = "All"
      ),
      selectInput(
        inputId = "branchname",
        label = "Select Branch",
        choices = c("All", unique(mydata$BranchName)),
        selected = "All"
      ),
      dateRangeInput(
        inputId = "dates",
        label = "Select Date",
        start = min(mydata$OpenDate),
        end = max(mydata$OpenDate),
        min = min(mydata$OpenDate),
        max = max(mydata$OpenDate)
      )
    )
  ),
  
  # Dashboard Body----
  dashboardBody(
    tabItems(
      tabItem("performance",
        fluidRow(
          valueBoxOutput("totalclosedjob"),
          valueBoxOutput("totalclosedinternaljob"),
          valueBoxOutput("totalclosedexternaljob")
        ),
        fluidRow(
          box(title = "Job Type Percentage",
              solidHeader = TRUE,
              width = 4,
              status = "primary",
              plotlyOutput("piechart")),
          box(title = "Service Performance",
              solidHeader = TRUE,
              width = 8,
              align = "center",
              status = "primary",
              plotlyOutput("barchart"))
        )
      ),
      tabItem("valuation",
        fluidRow(
          valueBoxOutput("servicevalue"),
          valueBoxOutput("partsvalue"),
          valueBoxOutput("warrantyvalue")
        ),
        fluidRow(
          box(title = "Internal Job Valuation",
              solidHeader = TRUE,
              width = 6,
              status = "primary",
              plotlyOutput("internalvaluation")),
          box(title = "External Job Valuation",
              solidHeader = TRUE,
              width = 6,
              status = "primary",
              plotlyOutput("externalvaluation"))
        )
      ),
      tabItem("outstanding",
        fluidRow(
          valueBoxOutput("outstandingjobcount"),
          valueBoxOutput("outstandingjobpercentage"),
          valueBoxOutput("outstandingjobvalue")
        ),
        fluidRow(
          box(title = "Outstanding Job",
              solidHeader = TRUE,
              width = 7,
              status = "danger",
              plotlyOutput("outstandingjob")),
          box(title = "Outstanding Job Classification",
              solidHeader = TRUE,
              width = 5,
              status = "danger",
              plotlyOutput("outstandingjobclassification"))
        )
      ),
      tabItem("failure",
        tabsetPanel(
          tabPanel(
            "Failure Analysis Chart",
            column(
              width = 8,
              box(title = "Failure Analysis by Model",
                  width = NULL,
                  height = 500,
                  status = "primary",
                  solidHeader = FALSE,
                  plotlyOutput("failurecategory"))
            ),
            column(
              width = 4,
              valueBoxOutput("failurecount", width = NULL),
              valueBoxOutput("reported", width = NULL)
            )
          ),
          tabPanel(
            "Failure Analysis Dataset",
            DTOutput("failuretable"),
            downloadButton("downloadfailuretable", "Download XLSX")
          )
        )
      ),
      tabItem("population",
        tabsetPanel(
          tabPanel(
            "Population Map",
            column(
              width = 8,
              leafletOutput("population", height = "500px")
            ),
            column(
              width = 4,
              box(
                title = "Population by Model",
                width = NULL,
                solidHeader = TRUE,
                status = "primary",
                align = "center",
                DTOutput("populationdata"),
                style = "height:450px; overflow-y: scroll"
              )
            )
          ),
          tabPanel(
            "Population Table",
            DTOutput("populationtable")
          )
        )
      ),
      tabItem("dataset",
        tabsetPanel(
          tabPanel(
            "Job Performance Dataset",
            DTOutput("table"),
            downloadButton("download", "Download XLSX")
          )
        )
      ),
      tabItem("sourcecode",
        box(
          title = "Source Code",
          width = NULL, status = "primary", solidHeader = TRUE,
          pre(includeText("serviceAFdashboard.R"))
        )
      )
    )
  )
)

server <- function(input, output, session) {
  
  # Reactive Expression to Subset Performance Data----
  mydata_filtered <- reactive({
    if (input$agency != "All") {
      mydata <- mydata %>% 
        filter(Agency == input$agency)
    }
  
    if (input$branchname != "All") {
      mydata <- mydata %>% 
        filter(BranchName == input$branchname)
    }
    
    mydata <- mydata %>% 
      filter(OpenDate >= input$dates[1] & OpenDate <= input$dates[2])

    mydata
  })
  
  # Reactive Expression to Subset Failure Data----
  failuredata <- left_join(failuredata, branchcode, by = "BranchCode")
  
  failuredata_filtered <- reactive({
    if (input$branchname != "All") {
      failuredata <- failuredata %>% 
        filter(OpenDate >= input$dates[1] & OpenDate <= input$dates[2]) %>% 
        filter(BranchName == input$branchname)
    }
    
    failuredata <- failuredata %>% 
      filter(OpenDate >= input$dates[1] & OpenDate <= input$dates[2])
    
    failuredata
  })
  
  #Output Barchart----
  output$barchart <- renderPlotly({
    if (input$branchname != "All") {
      mydata_filtered() %>%
        filter(JobStatus == "Closed") %>%
        count(JobType, JobCategory) %>%
        group_by(JobType) %>% 
        mutate(Total = sum(n)) %>% 
        ungroup() %>% 
        mutate(JobType = fct_reorder(JobType, Total, .desc = TRUE)) %>% 
        plot_ly(x = ~JobType, y = ~n, type = "bar", color = ~JobCategory) %>% 
        layout(
          barmode = "stack",
          legend = list(orientation = "h"),
          xaxis = list(title = "", tickangle = -45),
          yaxis = list(title = ""))
    } else {
      mydata_filtered() %>%
        filter(JobStatus == "Closed") %>%
        count(BranchName, JobCategory) %>%
        group_by(BranchName) %>% 
        mutate(Total = sum(n)) %>% 
        ungroup() %>%
        mutate(BranchName = fct_reorder(BranchName, Total, .desc = TRUE)) %>% 
        plot_ly(x = ~BranchName, y = ~n, type = "bar", color = ~JobCategory) %>% 
        layout(
          barmode = "stack",
          legend = list(orientation = "h"),
          xaxis = list(title = "", tickangle = -45),
          yaxis = list(title = ""))
    }
  })
  
  # Output Piechart----
  output$piechart <- renderPlotly({
    mydata_filtered() %>%
      filter(JobStatus == "Closed") %>%
      count(JobType) %>% 
      plot_ly(labels = ~JobType, values = ~n, type = "pie", 
              textposition = "inside",
              textinfo = "label+percent",
              marker = list(
                line = list(color = "#FFFFFF", width = 1),
                showlegend = FALSE)
              ) %>% 
      layout(legend = list(orientation = "h"))
  })
  
  # Output Total Closed Job----
  output$totalclosedjob <- renderValueBox({
    valueBox(
      mydata_filtered() %>% 
        filter(JobStatus == "Closed") %>% 
        summarize(n()),
      subtitle = "Total Closed Job",
      icon = icon("list"),
      color = "blue"
    )
  })
  
  # Output Total Closed Internal Job----
  output$totalclosedinternaljob <- renderValueBox({
    valueBox(
      mydata_filtered() %>% 
        filter(JobStatus == "Closed", JobCategory == "Internal Job") %>% 
        summarize(n()),
      icon = icon("list"),
      subtitle = "Total Closed Internal Job",
      color = "light-blue"
    )
  })
  
  # Output Total Closed External Job----
  output$totalclosedexternaljob <- renderValueBox({
    valueBox(
      mydata_filtered() %>% 
        filter(JobStatus == "Closed", JobCategory == "External Job") %>% 
        summarize(n()),
      icon = icon("list"),
      subtitle = "Total Closed External Job",
      color = "red"
    )
  })
  
  # Reactive Expression to filter data by Job Category----
  mydata_filtered_internal <- reactive({
    mydata_filtered() %>% 
      filter(JobStatus == "Closed", JobCategory == "Internal Job")
  })
  
  mydata_filtered_external <- reactive({
    mydata_filtered() %>% 
      filter(JobStatus == "Closed", JobCategory == "External Job")
  })
  
  # Output Internal Valuation----
  output$internalvaluation <- renderPlotly({
    if (input$branchname == "All") {
      mydata_filtered_internal() %>%
        group_by(BranchName) %>% 
        summarize(TotalValue = sum(TotalValue)) %>% 
        ungroup() %>% 
        mutate(BranchName = fct_reorder(BranchName, TotalValue)) %>% 
        plot_ly(x = ~TotalValue, y = ~BranchName) %>% 
        add_bars(color = I("#0e1854")) %>% 
        layout(
          xaxis = list(title = ""),
          yaxis = list(title = ""))
    } else {
      mydata_filtered_internal() %>% 
        group_by(JobType) %>% 
        summarize(TotalValue = sum(TotalValue)) %>% 
        ungroup() %>% 
        mutate(JobType = fct_reorder(JobType, TotalValue)) %>%
        plot_ly(x = ~TotalValue, y = ~JobType) %>% 
        add_bars(color = I("#0e1854")) %>% 
        layout(
          xaxis = list(title = ""),
          yaxis = list(title = ""))
    }
  })
  
  # Output External Valuation----
  output$externalvaluation <- renderPlotly({
    if (input$branchname == "All") {
      mydata_filtered_external() %>%
        group_by(BranchName) %>% 
        summarize(TotalValue = sum(TotalValue)) %>% 
        ungroup() %>% 
        mutate(BranchName = fct_reorder(BranchName, TotalValue)) %>% 
        plot_ly(x = ~TotalValue, y = ~BranchName) %>% 
        add_bars(color = I("#610f0f")) %>% 
        layout(
          xaxis = list(title = ""),
          yaxis = list(title = ""))
    } else {
      mydata_filtered_external() %>% 
        group_by(JobType) %>% 
        summarize(TotalValue = sum(TotalValue)) %>% 
        ungroup() %>% 
        mutate(JobType = fct_reorder(JobType, TotalValue)) %>%
        plot_ly(x = ~TotalValue, y = ~JobType) %>% 
        add_bars(color = I("#610f0f")) %>% 
        layout(
          xaxis = list(title = ""),
          yaxis = list(title = ""))
    }
  })
  
  # Output Service Value----
  output$servicevalue <- renderValueBox({
    servicevalue <- mydata_filtered() %>%
      filter(JobStatus == "Closed") %>%
      summarize(sum(Service))
    
    valueBox(
      paste("Rp", accounting(servicevalue, digits = 0L)),
      subtitle = "Service Value",
      color = "blue",
      icon = icon("money")
    )
  })
  
  # Output Parts Value----
  output$partsvalue <- renderValueBox({
    partsvalue <- mydata_filtered() %>%
      filter(JobStatus == "Closed") %>%
      summarize(sum(Parts))
    
    valueBox(
      paste("Rp", accounting(partsvalue, digits = 0L)),
      subtitle = "Parts Value",
      color = "light-blue",
      icon = icon("money")
    )
  })
  
  #Output Warranty Value----
  output$warrantyvalue <- renderValueBox({
    warrantyvalue <- mydata_filtered() %>%
      filter(JobStatus == "Closed", JobType == "IW") %>%
      summarize(sum(TotalValue))
    
    valueBox(
      paste("Rp", accounting(warrantyvalue, digits = 0L)),
      subtitle = "Warranty Value",
      color = "blue",
      icon = icon("money")
    )
  })
  
  #Output Performance Table----
  output$table <- renderDT({
    datatable(
      mydata_filtered() %>% 
        transmute(JobNo, OpenDate = as.Date(OpenDate), CloseDate = as.Date(CloseDate), 
                  JobStatus, Customer, JobDesc, TotalValue),
      colnames = c("Job No", "Open Date", "Close Date", "Job Status", "Customer",
                   "Job Description", "Total Value"),
      class = 'cell-border stripe'
    )
    
  })
  
  # Output Download Performance Table----
  output$download <- downloadHandler(
    filename = function(){
      paste("serviceA&Fperformance-", Sys.Date(), ".xlsx", sep = "")
    },
    content = function(file){
      write_xlsx(mydata_filtered(), file)
    }
  )
  
  # Output Failure Table----
  output$failuretable <- renderDT({
    datatable(
      failuredata_filtered() %>% 
        filter(JobStatus == "CLOSED") %>% 
        select(JobNo, UnitModel, UnitSN, HM, Category, Group),
      colnames = c("Job No", "Unit Model", "Unit SN", "HM", "Category", "Group"),
      class = 'cell-border stripe'
    )
  })
  
  # Output Download Failure Table----
  output$downloadfailuretable <- downloadHandler(
    filename = function(){
      paste("failureanalysis-", Sys.Date(), ".xlsx", sep = "")
    },
    content = function(file){
      write_xlsx(failuredata_filtered(), file)
    }
  )
  
  #Output Outstanding Job Count----
  output$outstandingjobcount <- renderValueBox({
    valueBox(
      mydata_filtered() %>% 
        filter(JobStatus == "Outstanding") %>% 
        summarize(n()),
      subtitle = "Total Outstanding Job",
      color = "red",
      icon = icon("list")
    )
  })
  
  #Output Outstanding Job Percentage----
  output$outstandingjobpercentage <- renderValueBox({
    outstanding_percentage <- mydata_filtered() %>% 
      group_by(JobStatus) %>% 
      summarize(count = n()) %>% 
      ungroup() %>% 
      mutate(percentage = count / sum(count) * 100) %>% 
      filter(JobStatus == "Outstanding") %>% 
      select(percentage)
    
    valueBox(
      paste(round(outstanding_percentage, digits = 0), "%"),
      subtitle = "Outstanding Job Percentage",
      color = "red",
      icon = icon("exclamation")
    )
  })
  
  #Output Outstanding Job Value----
  output$outstandingjobvalue <- renderValueBox({
    outstandingvalue <- mydata_filtered() %>% 
      filter(JobStatus == "Outstanding") %>% 
      summarize(sum(TotalValue))
    
    valueBox(
      paste("Rp", accounting(outstandingvalue, digits = 0L)),
      subtitle = "Outstanding Job Value",
      color = "red",
      icon = icon("credit-card")
    )
  })
  
  #Output Outstanding Job Chart----
  output$outstandingjob <- renderPlotly({
    if (input$branchname != "All") {
      mydata_filtered() %>%
        filter(JobStatus == "Outstanding") %>% 
        count(JobType, JobCategory) %>%
        group_by(JobType) %>% 
        mutate(Total = sum(n)) %>% 
        ungroup() %>% 
        mutate(JobType = fct_reorder(JobType, Total, .desc = TRUE)) %>% 
        plot_ly(x = ~JobType, y = ~n, color = ~JobCategory) %>% 
        add_bars() %>% 
        layout(
          barmode = "stack",
          legend = list(orientation = "h"),
          xaxis = list(title = "", tickangle = -45),
          yaxis = list(title = ""))
    } else {
      mydata_filtered() %>%
        filter(JobStatus == "Outstanding") %>%
        count(BranchName, JobCategory) %>%
        group_by(BranchName) %>% 
        mutate(Total = sum(n)) %>% 
        ungroup() %>%
        mutate(BranchName = fct_reorder(BranchName, Total, .desc = TRUE)) %>% 
        plot_ly(x = ~BranchName, y = ~n, color = ~JobCategory) %>% 
        add_bars() %>% 
        layout(
          barmode = "stack",
          legend = list(orientation = "h"),
          xaxis = list(title = "", tickangle = -45),
          yaxis = list(title = ""))
    }
  })
  
  # Output Outstanding Job Classification----
  output$outstandingjobclassification <- renderPlotly({
    mydata_filtered() %>%
      filter(JobStatus == "Outstanding") %>%
      count(OutstandingJobClass) %>% 
      plot_ly(labels = ~OutstandingJobClass, values = ~n, type = "pie", 
              textposition = "inside",
              textinfo = "label+percent",
              marker = list(
                line = list(color = "#FFFFFF", width = 1),
                showlegend = FALSE)
      ) %>% 
      layout(legend = list(orientation = "h"))
  })
  
  # Failure Chart----
  output$failurecategory <- renderPlotly({
    failuredata_filtered() %>% 
      filter(JobStatus == "CLOSED") %>% 
      count(UnitModel, Group) %>%
      group_by(UnitModel) %>% 
      mutate(Total = sum(n)) %>% 
      ungroup() %>%
      mutate(UnitModel = fct_reorder(UnitModel, Total)) %>% 
      plot_ly(x = ~n, y = ~UnitModel, color = ~Group,
              hoverinfo = "text",
              text = ~paste("Failure Count:", n, "<br>", "Group:", Group)) %>% 
      add_bars() %>% 
      layout(
        barmode = "stack",
        xaxis = list(title = "Failure Count"),
        yaxis = list(title = ""),
        showlegend = FALSE)
  })
  
  # Failure Count----
  output$failurecount <- renderValueBox({
    failurecount <- failuredata_filtered() %>% 
      filter(JobStatus == "CLOSED") %>% 
      summarize(n())
    
    valueBox(
      value = failurecount,
      subtitle = "Failure Count",
      icon = icon("list"),
      color = "blue"
    )
  })
  
  # Report Percentage----
  output$reported <- renderValueBox({
    reported <- failuredata_filtered() %>% 
      filter(JobStatus == "CLOSED") %>% 
      group_by(Reported) %>% 
      summarize(count = n()) %>% 
      ungroup() %>% 
      mutate(percentage = count / sum(count) * 100) %>% 
      filter(Reported == "REPORTED") %>% 
      select(percentage)
    
    valueBox(
      value = paste(round(reported, digits = 1), "%"),
      subtitle = "Technical Help Desk Reported",
      icon = icon("list"),
      color = "light-blue"
    )
  })
  
  # Reactive Expression to Load Maps with Markers----
  populationmap <- reactive({
    leaflet(options = leafletOptions(minZoom = 5)) %>% 
      addTiles() %>% 
      setMaxBounds(lng1 = 94.510561, lat1 = 7.169720, lng2 = 140.952527, lat2 = -10.585397) %>% 
      addCircleMarkers(
        lng = branchcode$lon, 
        lat = branchcode$lat, 
        layerId = branchcode$Branch,
        label = branchcode$Branch,
        labelOptions = labelOptions(textsize = "12px")
      )
  })
  
  # Create Unit Population Maps----
  output$population <- renderLeaflet({
    populationmap()
  })
  
  data_click <- reactiveValues(clickedMarker = NULL)
  observeEvent(input$population_marker_click, {
    data_click$clickedMarker <- input$population_marker_click
  })
  
  # Population Table and Summary----
  summarypopulation <- populationdata %>% 
    mutate(`BRANCH ASS` = str_to_upper(`BRANCH ASS`)) %>% 
    group_by(`BRANCH ASS`, `MODEL`) %>% 
    count() %>% 
    ungroup()
  
  populationtable <- populationdata %>% 
    left_join(branchcode, by = c(`BRANCH ASS` = "Branch"))
  
  # Reactive Expression to Filter Population Table----
  populationtable_filtered <- reactive({
    if (input$branchname != "All") {
      populationtable <- populationtable %>% 
        filter(BranchName == input$branchname)
    }
    populationtable
  })
  
  # Output Population Table based on Clicked Marker----  
  output$populationdata <- renderDT({
    if (is.null(data_click$clickedMarker)) {
      return(NULL)
    }
    datatable(
      summarypopulation %>% 
        filter(`BRANCH ASS` == data_click$clickedMarker$id) %>% 
        select(`MODEL`, `COUNT` = n) %>%
        arrange(desc(`COUNT`)),
      class = 'cell-border stripe',
      caption = paste("Unit Population for Branch ", data_click$clickedMarker$id),
      options = list(
        paging = FALSE
      )
    )
  })
  
  # Output Population Dataset----
  output$populationtable <- renderDT({
    datatable(
      populationtable_filtered() %>% select(1:7, 9),
      colnames = c("Tahun", "Series", "Model", "Unit SN", "Customer", "Delivery Date", 
                   "Branch Ass", "Grade Service Years"),
      class = 'cell-border stripe'
    )
    
  })
  
}

shinyApp(ui, server)