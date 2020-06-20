library(shiny)
library(shinydashboard)
library(readxl)
library(tidyverse)
library(ggplot2)
library(DT)
library(plotly)


# Load dataset

mydata <- read_excel("serviceperformance.xlsx")


ui <- dashboardPage(
  
  # Dashboard Header
  dashboardHeader(title = "Service A&F"),
  
  # Dashboard Sidebar
  dashboardSidebar(
    sidebarMenu(
      menuItem("Performance", tabName = "performance"),
      menuItem("Population", tabName = "population"),
      menuItem("Failure Analysis", tabName = "analysis"),
      selectInput("agency", "Select Agency", choices = unique(mydata$Agency)),
      selectInput("branchname", "Select Branch", choices = unique(mydata$BranchName))
    )
  ),
  
  # Dashboard Body
  dashboardBody(
    tabItems(
      tabItem("performance",
        fluidRow(
          valueBox(value = 10, subtitle = "servicevalue"),
          valueBox(value = 10, subtitle = "partsvalue"),
          valueBox(value = 10, subtitle = "outstandingjob")
        ),
        fluidRow(
          box(title = "Filter",
              solidHeader = TRUE,
              width = 4,
              collapsible = TRUE),
          box(title = "Chart",
              solidHeader = TRUE,
              width = 8)
        )
      ),
      tabItem("population"),
      tabItem("analysis")
    )
  )
)

server <- function(input, output, session) {
  
}

shinyApp(ui, server)