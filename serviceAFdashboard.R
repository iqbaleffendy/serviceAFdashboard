library(shinydashboard)

ui <- dashboardPage(
  dashboardHeader(title = "Service A&F Dashboard"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Performance", tabName = "performance"),
      menuItem("Population", tabName = "population"),
      menuItem("Failure Analysis", tabName = "analysis")
    )
  ),
  dashboardBody(
    tabItems(
      tabItem("performance"),
      tabItem("population"),
      tabItem("analysis")
    )
  )
)

server <- function(input, output, session) {
  
}

shinyApp(ui, server)