library(shiny)
library(shinydashboard)
library(readxl)
library(writexl)
library(tidyverse)
library(ggplot2)
library(DT)
library(plotly)
library(formattable)


# Load Dataset
mydata <- read_excel("serviceperformance.xlsx")
mydata$OpenDate <- as.Date(mydata$OpenDate)
mydata$CloseDate <- as.Date(mydata$CloseDate)

failuredata <- read_excel("techreportsummary.xlsx")
failuredata$`OPEN DATE` <- as.Date(failuredata$`OPEN DATE`)

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
      menuItem("Dataset", tabName = "dataset", icon = icon("table")),
      menuItem("Failure Analysis", tabName = "failure", icon = icon("list")),
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
          infoBoxOutput("totalclosedjob"),
          infoBoxOutput("totalclosedinternaljob"),
          infoBoxOutput("totalclosedexternaljob")
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
      tabItem("dataset",
        DTOutput("table"),
        downloadButton("download", "Download XLSX")
      ),
      tabItem("failure",
        fluidRow(
          infoBoxOutput("failurecount"),
          infoBoxOutput("reported")
        ),
        fluidRow(
          box(title = "Failure Analysis by Model",
              width = 12,
              height = 480,
              status = "primary",
              solidHeader = TRUE,
              plotlyOutput("failurecategory"))
        )
      )
    )
  )
)

server <- function(input, output, session) {
  
  # Reactive expression to subset data based on user input----
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
  
  failuredata_filtered <- reactive({
    failuredata %>% 
      filter(`OPEN DATE` >= input$dates[1] & `OPEN DATE` <= input$dates[2])
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
        plot_ly(x = ~JobType, y = ~n, color = ~JobCategory) %>% 
        add_bars() %>% 
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
        plot_ly(x = ~BranchName, y = ~n, color = ~JobCategory) %>% 
        add_bars() %>% 
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
  output$totalclosedjob <- renderInfoBox({
    infoBox(
      "Total Closed Job",
      mydata_filtered() %>% 
        filter(JobStatus == "Closed") %>% 
        summarize(n()),
      icon = icon("list"),
      color = "blue"
    )
  })
  
  # Output Total Closed Internal Job----
  output$totalclosedinternaljob <- renderInfoBox({
    infoBox(
      "Total Closed Internal Job",
      mydata_filtered() %>% 
        filter(JobStatus == "Closed", JobCategory == "Internal Job") %>% 
        summarize(n()),
      icon = icon("list"),
      color = "light-blue"
    )
  })
  
  # Output Total Closed External Job----
  output$totalclosedexternaljob <- renderInfoBox({
    infoBox(
      "Total Closed External Job",
      mydata_filtered() %>% 
        filter(JobStatus == "Closed", JobCategory == "External Job") %>% 
        summarize(n()),
      icon = icon("list"),
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
        add_bars(color = I("blue")) %>% 
        layout(
          xaxis = list(title = ""),
          yaxis = list(title = ""))
    } else {
      mydata_filtered_internal() %>% 
        group_by(JobType) %>% 
        summarize(TotalValue = sum(TotalValue)) %>% 
        ungroup() %>% 
        mutate(JobType = fct_reorder(JobType, TotalValue)) %>%
        plot_ly(x = ~TotalValue, y = ~BranchName) %>% 
        add_bars(color = I("blue")) %>% 
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
        add_bars(color = I("red")) %>% 
        layout(
          xaxis = list(title = ""),
          yaxis = list(title = ""))
    } else {
      mydata_filtered_external() %>% 
        group_by(JobType) %>% 
        summarize(TotalValue = sum(TotalValue)) %>% 
        ungroup() %>% 
        mutate(JobType = fct_reorder(JobType, TotalValue)) %>%
        plot_ly(x = ~TotalValue, y = ~BranchName) %>% 
        add_bars(color = I("red")) %>% 
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
      color = "blue",
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
  
  #Output DT Table----
  output$table <- renderDT({
    mydata_filtered() %>% 
      transmute(JobNo, OpenDate = as.Date(OpenDate), CloseDate = as.Date(CloseDate), 
                JobStatus, Customer, JobDesc, TotalValue)
  })
  
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
  
  #Output Outstanding Job Classification----
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
  
  output$download <- downloadHandler(
    filename = function(){
      paste("serviceA&Fperformance-", Sys.Date(), ".xlsx", sep = "")
    },
    content = function(file){
      write_xlsx(mydata_filtered(), file)
    }
  )
  
  # Failure Chart----
  output$failurecategory <- renderPlotly({
    failuredata_filtered() %>% 
      filter(`JOB STATUS` == "CLOSED") %>% 
      count(`UNIT MODEL`, GROUP) %>%
      group_by(`UNIT MODEL`) %>% 
      mutate(Total = sum(n)) %>% 
      ungroup() %>%
      mutate(`UNIT MODEL` = fct_reorder(`UNIT MODEL`, Total)) %>% 
      plot_ly(x = ~n, y = ~`UNIT MODEL`, color = ~GROUP) %>% 
      add_bars() %>% 
      layout(
        barmode = "stack",
        xaxis = list(title = "Failure Count"),
        yaxis = list(title = ""))
  })
  
  # Failure Count----
  output$failurecount <- renderInfoBox({
    failurecount <- failuredata_filtered() %>% 
      filter(`JOB STATUS` == "CLOSED") %>% 
      summarize(n())
    
    infoBox(
      title = "Failure Count",
      value = failurecount,
      icon = icon("list"),
      color = "blue"
    )
  })
  
  # Report Percentage----
  output$reported <- renderInfoBox({
    reported <- failuredata_filtered() %>% 
      filter(`JOB STATUS` == "CLOSED") %>% 
      group_by(REPORTED) %>% 
      summarize(count = n()) %>% 
      ungroup() %>% 
      mutate(percentage = count / sum(count) * 100) %>% 
      filter(REPORTED == "REPORTED") %>% 
      select(percentage)
    
    infoBox(
      title = "Technical Help Desk Reported",
      value = paste(round(reported, digits = 1), "%"),
      icon = icon("list"),
      color = "blue"
    )
  })
  
}

shinyApp(ui, server)