library(shiny)
library(bs4Dash)
library(readxl)
library(writexl)
library(tidyverse)
library(DT)
library(data.table)
library(plotly)
library(leaflet)
library(formattable)
library(highcharter)


# Load Dataset----
mydata <- read_excel("import_data/serviceperformance.xlsx")
mydata$OpenDate <- as.Date(mydata$OpenDate)
mydata$CloseDate <- as.Date(mydata$CloseDate)
mydata$OutstandingJobClass <- factor(
  mydata$OutstandingJobClass, 
  levels = c(
    "0-30 Days",
    "31-60 Days",
    "61-90 Days",
    "91-120 Days",
    "121-180 Days",
    ">180 Days",
    "Closed Job"
  )
)

failuredata <- read_excel("import_data/techreportsummary.xlsx")
failuredata$OpenDate <- as.Date(failuredata$OpenDate)
failuredata$BranchCode <- as.character(failuredata$BranchCode)

branchcode <- read_excel("import_data/branchcode.xlsx")
branchcode$BranchCode <- as.character(branchcode$BranchCode)

populationdata <- fread("import_data/datapopulasi.csv")

shiny::shinyApp(
  ui = bs4DashPage(
    old_school = FALSE,
    sidebar_min = TRUE,
    sidebar_collapsed = TRUE,
    controlbar_collapsed = TRUE,
    controlbar_overlay = TRUE,
    navbar = bs4DashNavbar(
      skin = "dark",
      border = TRUE,
      status = "secondary"
    ),
    sidebar = bs4DashSidebar(
      skin = "dark",
      status = "secondary",
      title = "Service A&F Dashboard",
      brandColor = "secondary",
      bs4SidebarMenu(
        bs4SidebarMenuItem("Performance", tabName = "performance", icon = "dashboard"),
        bs4SidebarMenuItem("Outstanding Job", tabName = "outstanding", icon = "exclamation"),
        bs4SidebarMenuItem("Failure Analysis", tabName = "failure", icon = "list"),
        bs4SidebarMenuItem("Unit Population", tabName = "population", icon = "globe"),
        bs4SidebarMenuItem("Dataset", tabName = "dataset", icon = "table"),
        bs4SidebarMenuItem("Source Code", tabName = "sourcecode", icon = "code")
      )
    ),
    controlbar = bs4DashControlbar(
      skin = "dark",
      title = "Input Parameters",
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
      selectInput(
        inputId = "jobtype",
        label = "Select Job Type",
        choices = c("All", unique(mydata$JobType)),
        selected = "All"
      ),
      selectInput(
        inputId = "unitmodel",
        label = "Select Unit Model",
        choices = c("All", unique(mydata$UnitModel)),
        selected = "All"
      ),
      dateRangeInput(
        inputId = "dates",
        label = "Select Date",
        start = min(mydata$OpenDate),
        end = max(mydata$OpenDate)
      )
    ),
    body = bs4DashBody(
      tags$head(
        tags$link(rel = "stylesheet", type = "text/css", href = "bootstrap.min.sandstone.css")
      ),
      bs4TabItems(
        bs4TabItem(
          tabName = "performance",
          fluidRow(
            bs4InfoBoxOutput("totalclosedjob"),
            bs4InfoBoxOutput("totalclosedinternaljob"),
            bs4InfoBoxOutput("totalclosedexternaljob")
          ),
          fluidRow(
            bs4Card(
              title = "Job Type Percentage",
              solidHeader = TRUE,
              closable = FALSE,
              width = 4,
              status = "info",
              highchartOutput("piechart")
            ),
            bs4Card(
              title = "Service Performance",
              solidHeader = TRUE,
              closable = FALSE,
              width = 8,
              align = "center",
              status = "info",
              highchartOutput("barchart")
            )
          )
        ),
        bs4TabItem(
          tabName = "outstanding",
          fluidRow(
            bs4InfoBoxOutput("outstandingjobcount"),
            bs4InfoBoxOutput("outstandingjobpercentage"),
            bs4InfoBoxOutput("outstandingjobvalue")
          ),
          fluidRow(
            bs4Card(
              title = "Outstanding Job",
              solidHeader = TRUE,
              closable = FALSE,
              width = 7,
              height = 520,
              status = "danger",
              selectInput(
                inputId = "sortbyoutstanding", 
                label = "Outstanding Job Count by", 
                choices = c("By Quantity", "By Value"),
                selected = "By Quantity"
              ),
              highchartOutput("outstandingjob")
            ),
            bs4Card(
              title = "Outstanding Job Classification",
              solidHeader = TRUE,
              closable = FALSE,
              width = 5,
              height = 520,
              status = "danger",
              highchartOutput("outstandingjobclassification")
            )
          )
        ),
        bs4TabItem(
          tabName = "failure",
          bs4TabSetPanel(
            id = "tabfailure",
            side = "left",
            bs4TabPanel(
              tabName = "Failure Analysis Chart",
              fluidRow(  
                column(
                  width = 8,
                  uiOutput("failurechartbox")
                ),
                column(
                  width = 4,
                  bs4InfoBoxOutput("failurecount", width = NULL),
                  bs4InfoBoxOutput("reported", width = NULL)
                )
              ) 
            ),
            bs4TabPanel(
              tabName = "Failure Analysis Dataset",
              DTOutput("failuretable"),
              downloadButton("downloadfailuretable", "Download XLSX")
            ),
            bs4TabPanel(
              tabName = "Count Failed Parts",
              DTOutput("countparts")
            )
          )
        ),
        bs4TabItem(
          tabName = "population",
          bs4TabSetPanel(
            id = "tabpopulation",
            side = "left",
            bs4TabPanel(
              tabName = "Population Map",
              fluidRow(
                column(
                  width = 8,
                  leafletOutput("population", height = "500px")
                ),
                column(
                  width = 4,
                  bs4Card(
                    title = "Population by Model",
                    width = NULL,
                    solidHeader = TRUE,
                    closable = FALSE,
                    status = "primary",
                    align = "center",
                    DTOutput("populationdata"),
                    style = "height:450px; overflow-y: scroll"
                  )
                )
              )
            ),
            bs4TabPanel(
              tabName = "Population Table",
              DTOutput("populationtable")
            )
          )
        ),
        bs4TabItem(
          tabName = "dataset",
          DTOutput("table"),
          downloadButton("download", "Download XLSX")
        ),
        bs4TabItem(
          tabName = "sourcecode",
          bs4Card(
            title = "Source Code",
            width = NULL, 
            status = "primary", 
            solidHeader = TRUE,
            closable = FALSE,
            pre(includeText("app.R"))
          )
        )
      )
    )
  ),
  
  server = function(input, output) {
    
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
      
      if (input$jobtype != "All") {
        mydata <- mydata %>% 
          filter(JobType == input$jobtype)
      }
      
      if (input$unitmodel != "All") {
        mydata <- mydata %>% 
          filter(UnitModel == input$unitmodel)
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
          filter(BranchName == input$branchname)
      }
      
      if (input$unitmodel != "All") {
        failuredata <- failuredata %>% 
          filter(UnitModel == input$unitmodel)
      }
      
      failuredata
    })
    
    #Output Barchart----
    output$barchart <- renderHighchart({
      if (input$branchname != "All") {
        mydata_filtered() %>%
          filter(JobStatus == "Closed") %>%
          count(JobType, JobCategory) %>%
          group_by(JobType) %>% 
          mutate(Total = sum(n)) %>% 
          ungroup() %>% 
          mutate(JobType = fct_reorder(JobType, Total, .desc = TRUE)) %>%
          arrange(desc(Total)) %>% 
          hchart("column", hcaes(x = JobType, y = n, group = JobCategory), stacking = "normal") %>% 
          hc_xAxis(title = list(text = "")) %>%
          hc_yAxis(title = list(text = ""))
      } else {
        mydata_filtered() %>%
          filter(JobStatus == "Closed") %>%
          count(BranchName, JobCategory) %>%
          group_by(BranchName) %>% 
          mutate(Total = sum(n)) %>% 
          ungroup() %>%
          mutate(BranchName = fct_reorder(BranchName, Total, .desc = TRUE)) %>%
          arrange(desc(Total)) %>%
          hchart("column", hcaes(x = BranchName, y = n, group = JobCategory), stacking = "normal") %>% 
          hc_xAxis(title = list(text = "")) %>%
          hc_yAxis(title = list(text = ""))
      }
    })
    
    # Output Piechart----
    output$piechart <- renderHighchart({
      mydata_filtered() %>%
        filter(JobStatus == "Closed") %>%
        count(JobType) %>% 
        hchart("pie", hcaes(x = JobType, y = n))
    })
    
    # Output Total Closed Job----
    output$totalclosedjob <- renderbs4InfoBox({
      bs4InfoBox(
        mydata_filtered() %>% 
          filter(JobStatus == "Closed") %>% 
          summarize(n()),
        title = "Total Closed Job",
        icon = "list",
        status = "info"
      )
    })
    
    # Output Total Closed Internal Job----
    output$totalclosedinternaljob <- renderbs4InfoBox({
      bs4InfoBox(
        mydata_filtered() %>% 
          filter(JobStatus == "Closed", JobCategory == "Internal Job") %>% 
          summarize(n()),
        icon = "list",
        title = "Total Closed Internal Job",
        status = "info"
      )
    })
    
    # Output Total Closed External Job----
    output$totalclosedexternaljob <- renderbs4InfoBox({
      bs4InfoBox(
        mydata_filtered() %>% 
          filter(JobStatus == "Closed", JobCategory == "External Job") %>% 
          summarize(n()),
        icon = "list",
        title = "Total Closed External Job",
        status = "info"
      )
    })
    
    #Output Performance Table----
    output$table <- renderDT({
      datatable(
        mydata_filtered() %>% 
          transmute(JobNo, UnitModel, UnitSN, OpenDate = as.Date(OpenDate), CloseDate = as.Date(CloseDate), 
                    Customer, JobDesc, TotalValue),
        colnames = c("Job No", "Unit Model", "Unit SN", "Open Date", "Close Date", "Customer",
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
          filter(!is.na(Issue)) %>% 
          select(JobNo, UnitModel, UnitSN, HM, AsistNo, Category, Component, PCL),
        colnames = c("Job No", "Unit Model", "Unit SN", "HM", "Asist No", "Category", "Component", "Failed Parts"),
        class = 'cell-border stripe',
        options = list(
          scrollX = TRUE,
          autoWidth = TRUE,
          columnDefs = list(list(width = '120px', targets = 1))
        )
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
    
    # Reactive Expression to Separate PCL----
    failuredata_separate <- reactive({
      failuredata_filtered() %>% 
        separate_rows(PCL, sep = "; ") %>% 
        filter(!is.na(PCL))
    })
    
    # Failure Parts Count----
    output$countparts <- renderDT({
      datatable(
        failuredata_separate() %>%
          group_by(PCL, UnitModel, Component, Category) %>% 
          summarize(Count = n(), min(HM), max(HM)) %>% 
          arrange(desc(Count)),
        colnames = c("Failed Parts", "Unit Model", "Component", "Category", "Count", "Min HM", "Max HM"),
        class = 'cell-border stripe'
      )
    })
    
    #Output Outstanding Job Count----
    output$outstandingjobcount <- renderbs4InfoBox({
      bs4InfoBox(
        mydata_filtered() %>% 
          filter(JobStatus == "Outstanding") %>% 
          summarize(n()),
        title = "Total Outstanding Job",
        status = "danger",
        icon = "list"
      )
    })
    
    #Output Outstanding Job Percentage----
    output$outstandingjobpercentage <- renderbs4InfoBox({
      outstanding_percentage <- mydata_filtered() %>% 
        group_by(JobStatus) %>% 
        summarize(count = n()) %>% 
        ungroup() %>% 
        mutate(percentage = count / sum(count) * 100) %>% 
        filter(JobStatus == "Outstanding") %>% 
        select(percentage)
      
      bs4InfoBox(
        paste(round(outstanding_percentage, digits = 0), "%"),
        title = "Outstanding Job Percentage",
        status = "danger",
        icon = "exclamation"
      )
    })
    
    #Output Outstanding Job Value----
    output$outstandingjobvalue <- renderbs4InfoBox({
      outstandingvalue <- mydata_filtered() %>% 
        filter(JobStatus == "Outstanding") %>% 
        summarize(sum(TotalValue))
      
      bs4InfoBox(
        paste("Rp", accounting(outstandingvalue, digits = 0L)),
        title = "Outstanding Job Value",
        status = "danger",
        icon = "credit-card"
      )
    })
    
    #Output Outstanding Job Chart----
    output$outstandingjob <- renderHighchart({
      if (input$branchname != "All" & input$sortbyoutstanding == "By Quantity") {
        mydata_filtered() %>%
          filter(JobStatus == "Outstanding") %>% 
          count(JobType, OutstandingJobClass) %>%
          group_by(JobType) %>% 
          mutate(Total = sum(n)) %>% 
          ungroup() %>% 
          mutate(JobType = fct_reorder(JobType, Total, .desc = TRUE)) %>% 
          arrange(desc(Total)) %>%
          hchart("column", hcaes(x = JobType, y = n, group = OutstandingJobClass), stacking = "normal") %>% 
          hc_xAxis(title = list(text = "")) %>%
          hc_yAxis(title = list(text = ""))
      } else if (input$branchname == "All" & input$sortbyoutstanding == "By Quantity") {
        mydata_filtered() %>%
          filter(JobStatus == "Outstanding") %>%
          count(BranchName, OutstandingJobClass) %>%
          group_by(BranchName) %>% 
          mutate(Total = sum(n)) %>% 
          ungroup() %>%
          mutate(BranchName = fct_reorder(BranchName, Total, .desc = TRUE)) %>% 
          arrange(desc(Total)) %>%
          hchart("column", hcaes(x = BranchName, y = n, group = OutstandingJobClass), stacking = "normal") %>% 
          hc_xAxis(title = list(text = "")) %>%
          hc_yAxis(title = list(text = ""))
      } else if (input$branchname != "All" & input$sortbyoutstanding == "By Value") {
        mydata_filtered() %>%
          filter(JobStatus == "Outstanding") %>% 
          group_by(JobType, OutstandingJobClass) %>%
          summarize(Value = sum(TotalValue)) %>% 
          group_by(JobType) %>% 
          mutate(TotalValue = sum(Value)) %>% 
          ungroup() %>% 
          mutate(JobType = fct_reorder(JobType, TotalValue, .desc = TRUE)) %>% 
          arrange(desc(TotalValue)) %>%
          hchart("column", hcaes(x = JobType, y = Value, group = OutstandingJobClass), stacking = "normal") %>% 
          hc_xAxis(title = list(text = "")) %>%
          hc_yAxis(title = list(text = ""))
      } else if (input$branchname == "All" & input$sortbyoutstanding == "By Value") {
        mydata_filtered() %>%
          filter(JobStatus == "Outstanding") %>% 
          group_by(BranchName, OutstandingJobClass) %>%
          summarize(Value = sum(TotalValue)) %>% 
          group_by(BranchName) %>% 
          mutate(TotalValue = sum(Value)) %>% 
          ungroup() %>% 
          mutate(BranchName = fct_reorder(BranchName, TotalValue, .desc = TRUE)) %>% 
          arrange(desc(TotalValue)) %>%
          hchart("column", hcaes(x = BranchName, y = Value, group = OutstandingJobClass), stacking = "normal") %>% 
          hc_xAxis(title = list(text = "")) %>%
          hc_yAxis(title = list(text = ""))
      }
    })
    
    # Output Outstanding Job Classification----
    output$outstandingjobclassification <- renderHighchart({
      mydata_filtered() %>%
        filter(JobStatus == "Outstanding") %>%
        count(OutstandingJobClass) %>% 
        hchart("pie", hcaes(x = OutstandingJobClass, y = n))
    })
    
    # Failure Chart Box----
    output$failurechartbox <- renderUI({
      if (input$unitmodel != "All") {
        bs4Card(
          title = paste("Failure Analysis of ", input$unitmodel, " by Category"),
          width = NULL,
          height = 450,
          status = "primary",
          solidHeader = TRUE,
          closable = FALSE,
          plotlyOutput("failurecategory")
        )
      } else {
        bs4Card(
          title = "Failure Analysis by Model and Category",
          width = NULL,
          height = 550,
          status = "primary",
          solidHeader = TRUE,
          closable = FALSE,
          selectInput(
            inputId = "sortbyfailure", 
            label = "Group Failure Count by", 
            choices = c("By Model", "By Category"),
            selected = "By Model"
          ),
          plotlyOutput("failurecategory")
        )
      }
    })
    
    # Failure Chart----
    output$failurecategory <- renderPlotly({
      if (input$unitmodel != "All" | input$sortbyfailure == "By Category") {
        failuredata_filtered() %>% 
          filter(!is.na(Issue)) %>% 
          count(Category, UnitModel) %>%
          group_by(Category) %>% 
          mutate(Total = sum(n)) %>% 
          ungroup() %>%
          mutate(Category = fct_reorder(Category, Total)) %>% 
          plot_ly(x = ~n, y = ~Category, color = ~UnitModel,
                  hoverinfo = "text",
                  text = ~paste("Failure Count:", n, "<br>", "Unit Model:", UnitModel)) %>% 
          add_bars() %>% 
          layout(
            barmode = "stack",
            xaxis = list(title = "Failure Count"),
            yaxis = list(title = ""),
            showlegend = FALSE)
      } else if (input$sortbyfailure == "By Model") {
        failuredata_filtered() %>% 
          filter(!is.na(Issue)) %>% 
          count(UnitModel, Component) %>%
          group_by(UnitModel) %>% 
          mutate(Total = sum(n)) %>% 
          ungroup() %>%
          mutate(UnitModel = fct_reorder(UnitModel, Total)) %>% 
          plot_ly(x = ~n, y = ~UnitModel, color = ~Component,
                  hoverinfo = "text",
                  text = ~paste("Failure Count:", n, "<br>", "Component:", Component)) %>% 
          add_bars() %>% 
          layout(
            barmode = "stack",
            xaxis = list(title = "Failure Count"),
            yaxis = list(title = ""),
            showlegend = FALSE)
      } 
    })
    
    # Failure Count----
    output$failurecount <- renderbs4InfoBox({
      failurecount <- failuredata_filtered() %>% 
        filter(!is.na(Issue)) %>% 
        summarize(n())
      
      bs4InfoBox(
        value = failurecount,
        title = "Failure Count",
        icon = "list",
        status = "primary"
      )
    })
    
    # Report Percentage----
    output$reported <- renderbs4InfoBox({
      reported <- failuredata_filtered() %>% 
        filter(!is.na(Issue)) %>% 
        group_by(Reported) %>% 
        summarize(count = n()) %>% 
        ungroup() %>% 
        mutate(percentage = count / sum(count) * 100) %>% 
        filter(Reported == "REPORTED") %>% 
        select(percentage)
      
      bs4InfoBox(
        value = paste(round(reported, digits = 1), "%"),
        title = "Technical Help Desk Reported",
        icon = "list",
        status = "primary"
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
      
      if (input$unitmodel != "All") {
        populationtable <- populationtable %>% 
          filter(MODEL == input$unitmodel)
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
        populationtable_filtered() %>% select(1, 3:7, 9),
        colnames = c("Tahun", "Model", "Unit SN", "Customer", "Delivery Date", 
                     "Branch Ass", "Grade Service Years"),
        class = 'cell-border stripe'
      )
    })
    
  }
)