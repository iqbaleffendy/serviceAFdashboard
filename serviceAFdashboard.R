library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(readxl)
library(tidyverse)
library(ggplot2)
library(DT)
library(plotly)


# Load Dataset

mydata <- read_excel("serviceperformance.xlsx")

ui <- dashboardPage(

  # Dashboard Header
  dashboardHeader(
    title = "Service A&F Dashboard",
    titleWidth = 250
    ),
  
  # Dashboard Sidebar
  dashboardSidebar(
    width = 250,
    sidebarMenu(
      menuItem("Performance", tabName = "performance", icon = icon("dashboard")),
      menuItem("Valuation", tabName = "valuation", icon = icon("money")),
      menuItem("Outstanding Job", tabName = "outstanding", icon = icon("exclamation")),
      menuItem("Dataset", tabName = "dataset", icon = icon("table")),
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
      )
    )
  ),
  
  # Dashboard Body
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
              plotOutput("piechart")),
          box(title = "Service Performance",
              solidHeader = TRUE,
              width = 8,
              plotOutput("barchart"))
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
              plotOutput("internalvaluation")),
          box(title = "External Job Valuation",
              solidHeader = TRUE,
              width = 6,
              plotOutput("externalvaluation"))
        )
      ),
      tabItem("outstanding",
        column(width = 7,
          box(title = "Outstanding Job",
              solidHeader = TRUE,
              width = NULL,
              height = 550,
              plotOutput("outstandingjob"))
        ),
        column(width = 5,
          valueBoxOutput("outstandingjobpercentage", width = NULL),
          valueBoxOutput("outstandingjobvalue", width = NULL),
          box(title = "Outstanding Job Classification",
              solidHeader = TRUE,
              width = NULL,
              height = 300,
              plotOutput("outstandingjobclassification"))
        )
      ),
      tabItem("dataset",
        box(
          title = "Service A&F Performance Dataset",
          solidHeader = TRUE,
          DTOutput("table")
        )
      )
    )
  )
)

server <- function(input, output, session) {
  
  mydata_filtered <- reactive({
    if (input$agency != "All") {
      mydata <- mydata %>% 
        filter(Agency == input$agency)
    }
  
    if (input$branchname != "All") {
      mydata <- mydata %>% 
        filter(BranchName == input$branchname)
    }
    mydata
  })
  
  output$barchart <- renderPlot({
    if (input$branchname != "All") {
      mydata_filtered() %>%
        filter(JobStatus == "Closed") %>% 
        ggplot(aes(x = fct_infreq(JobType), fill = JobCategory)) +
        geom_bar()
    } else {
      mydata_filtered() %>%
        filter(JobStatus == "Closed") %>%
        ggplot(aes(x = fct_infreq(BranchName), fill = JobCategory)) +
        geom_bar()
    }
  })
  
  output$piechart <- renderPlot({
    mydata_filtered() %>%
      filter(JobStatus == "Closed") %>%
      count(JobType) %>% 
      ggplot(aes(x = "", y = n, fill = JobType)) +
      geom_bar(stat = "identity") +
      coord_polar(theta = "y", start = 0)
  })
  
  output$totalclosedjob <- renderValueBox({
    valueBox(
      mydata_filtered() %>% 
      filter(JobStatus == "Closed") %>% 
      summarize(n()),
    subtitle = "Total Closed Job"
    )
  })
  
  output$totalclosedinternaljob <- renderValueBox({
    valueBox(
      mydata_filtered() %>% 
        filter(JobStatus == "Closed", JobCategory == "Internal Job") %>% 
        summarize(n()),
      subtitle = "Total Closed Internal Job"
    )
  })
  
  output$totalclosedexternaljob <- renderValueBox({
    valueBox(
      mydata_filtered() %>% 
        filter(JobStatus == "Closed", JobCategory == "External Job") %>% 
        summarize(n()),
      subtitle = "Total Closed External Job"
    )
  })
  
  mydata_filtered_internal <- reactive({
    mydata_filtered() %>% 
      filter(JobStatus == "Closed", JobCategory == "Internal Job")
  })
  
  mydata_filtered_external <- reactive({
    mydata_filtered() %>% 
      filter(JobStatus == "Closed", JobCategory == "External Job")
  })
  
  output$internalvaluation <- renderPlot({
    if (input$branchname == "All") {
      mydata_filtered_internal() %>%
        group_by(BranchName) %>% 
        summarize(TotalValue = sum(TotalValue)) %>% 
        ungroup() %>% 
        mutate(BranchName = fct_reorder(BranchName, TotalValue)) %>% 
        ggplot(aes(x = BranchName, y = TotalValue)) +
        geom_col(fill = "blue") +
        coord_flip()
    } else {
      mydata_filtered_internal() %>% 
        group_by(JobType) %>% 
        summarize(TotalValue = sum(TotalValue)) %>% 
        ungroup() %>% 
        mutate(JobType = fct_reorder(JobType, TotalValue)) %>%
        ggplot(aes(x = JobType, y = TotalValue)) +
        geom_col(fill = "blue") +
        coord_flip()
    }
  })
  
  output$externalvaluation <- renderPlot({
    if (input$branchname == "All") {
      mydata_filtered_external() %>%
        group_by(BranchName) %>% 
        summarize(TotalValue = sum(TotalValue)) %>% 
        ungroup() %>% 
        mutate(BranchName = fct_reorder(BranchName, TotalValue)) %>% 
        ggplot(aes(x = BranchName, y = TotalValue)) +
        geom_col(fill = "Red") +
        coord_flip()
    } else {
      mydata_filtered_external() %>% 
        group_by(JobType) %>% 
        summarize(TotalValue = sum(TotalValue)) %>% 
        ungroup() %>% 
        mutate(JobType = fct_reorder(JobType, TotalValue)) %>%
        ggplot(aes(x = JobType, y = TotalValue)) +
        geom_col(fill = "Red") +
        coord_flip()
    }
  })
  
  output$servicevalue <- renderValueBox({
    valueBox(
      mydata_filtered() %>%
        filter(JobStatus == "Closed") %>%
        summarize(sum(Service)),
      subtitle = "Service Value"
    )
  })
  
  output$partsvalue <- renderValueBox({
    valueBox(
      mydata_filtered() %>%
        filter(JobStatus == "Closed") %>%
        summarize(sum(Parts)),
      subtitle = "Parts Value"
    )
  })
  
  output$warrantyvalue <- renderValueBox({
    valueBox(
      mydata_filtered() %>%
        filter(JobStatus == "Closed") %>%
        filter(JobType == "IW") %>% 
        summarize(sum(TotalValue)),
      subtitle = "Warranty Value"
    )
  })
  
  output$table <- renderDT({
    mydata_filtered()
  })
  
  output$outstandingjobpercentage <- renderValueBox({
    outstanding_percentage <- mydata_filtered() %>% 
      group_by(JobStatus) %>% 
      summarize(count = n()) %>% 
      ungroup() %>% 
      mutate(percentage = count / sum(count) * 100) %>% 
      filter(JobStatus == "Outstanding") %>% 
      select(percentage)
    
    valueBox(
      paste(outstanding_percentage, "%"),
      subtitle = "Outstanding Job Percentage"
    )
  })
  
  output$outstandingjobvalue <- renderValueBox({
    valueBox(
      mydata_filtered() %>% 
        filter(JobStatus == "Outstanding") %>% 
        summarize(sum(TotalValue)),
      subtitle = "Outstanding Job Value"
    )
  })
  
  output$outstandingjob <- renderPlot({
    if (input$branchname != "All") {
      mydata_filtered() %>%
        filter(JobStatus == "Outstanding") %>% 
        ggplot(aes(x = fct_infreq(JobType), fill = JobCategory)) +
        geom_bar()
    } else {
      mydata_filtered() %>%
        filter(JobStatus == "Outstanding") %>%
        ggplot(aes(x = fct_infreq(BranchName), fill = JobCategory)) +
        geom_bar()
    }
  })
  
  output$outstandingjobclassification <- renderPlot({
    mydata_filtered() %>%
      filter(JobStatus == "Outstanding") %>%
      count(JobType) %>% 
      ggplot(aes(x = "", y = n, fill = JobType)) +
      geom_bar(stat = "identity") +
      coord_polar(theta = "y", start = 0)
  })
  
}

shinyApp(ui, server)