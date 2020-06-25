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
      tabItem("outstanding"),
      tabItem("dataset",
        DTOutput("table")
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
  
  output$internalvaluation <- renderPlot({
    if (input$branchname == "All") {
      mydata_filtered() %>% 
        filter(JobStatus == "Closed", JobCategory == "Internal Job") %>%
        mutate(BranchName = as.factor(BranchName)) %>% 
        mutate(BranchName = fct_reorder(BranchName, TotalValue)) %>% 
        ggplot(aes(x = BranchName, y = TotalValue), fill = "blue") +
        geom_col() +
        coord_flip()
    } else {
      mydata_filtered() %>% 
        filter(JobStatus == "Closed", JobCategory == "Internal Job") %>%
        mutate(JobType = as.factor(JobType)) %>% 
        mutate(JobType = fct_reorder(JobType, TotalValue)) %>%
        ggplot(aes(x = JobType, y = TotalValue), fill = "blue") +
        geom_col() +
        coord_flip()
    }
  })
  
  output$externalvaluation <- renderPlot({
    if (input$branchname == "All") {
      mydata_filtered() %>% 
        filter(JobStatus == "Closed", JobCategory == "External Job") %>%
        mutate(BranchName = as.factor(BranchName)) %>% 
        mutate(BranchName = fct_reorder(BranchName, TotalValue)) %>% 
        ggplot(aes(x = BranchName, y = TotalValue), fill = "red") +
        geom_col() +
        coord_flip()
    } else {
      mydata_filtered() %>% 
        filter(JobStatus == "Closed", JobCategory == "External Job") %>%
        mutate(JobType = as.factor(JobType)) %>% 
        mutate(JobType = fct_reorder(JobType, TotalValue)) %>%
        ggplot(aes(x = JobType, y = TotalValue), fill = "red") +
        geom_col() +
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
  
}

shinyApp(ui, server)