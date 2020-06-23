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
  dashboardHeader(title = "Service A&F"),
  
  # Dashboard Sidebar
  dashboardSidebar(
    sidebarMenu(
      menuItem("Performance", tabName = "performance"),
      menuItem("Dataset", tabName = "dataset"),
      menuItem("Failure Analysis", tabName = "analysis"),
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
          valueBoxOutput("servicevalue"),
          valueBoxOutput("partsvalue"),
          valueBoxOutput("warrantyvalue")
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
      tabItem("dataset",
        DTOutput("table")
      ),
      tabItem("analysis")
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
        ggplot(aes(x = fct_infreq(JobType))) +
        geom_bar()
    } else {
      mydata_filtered() %>% 
        ggplot(aes(x = fct_infreq(BranchName), fill = JobCategory)) +
        geom_bar()
    }
  })
  
  output$piechart <- renderPlot({
    mydata_filtered() %>%
      count(JobType) %>% 
      ggplot(aes(x = "", y = n, fill = JobType)) +
      geom_bar(stat = "identity") +
      coord_polar(theta = "y", start = 0)
  })
  
  output$servicevalue <- renderValueBox({
    valueBox(
      mydata_filtered() %>% 
        summarize(sum(Service)),
      "Service Value"
    )
  })
  
  output$partsvalue <- renderValueBox({
    valueBox(
      mydata_filtered() %>% 
        summarize(sum(Parts)),
      "Parts Value"
    )
  })
  
  output$warrantyvalue <- renderValueBox({
    valueBox(
      mydata_filtered() %>% 
        filter(JobType == "IW") %>% 
        summarize(sum(TotalValue)),
      "Warranty Value"
    )
  })
  
  output$table <- renderDT({
    mydata_filtered()
  })
  
}

shinyApp(ui, server)