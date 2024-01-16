# Gustavo N. Santiago ----
# Small business administration help tool -----
# Version 1

# APPLICATION DESCRIPTION ----
# - Perform CRUD Operations
# - Integrate database at several applications
# - Tool to help a small business to visualize and controll their sales

# Libraries ----
library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(shinythemes)
library(shinyjs)
library(shinyauthr)  # devtools::install_github("business-science/shinyauthr")

# UI ----
ui <- tagList(
  
  ## CSS ----
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "styles.css")
  ),
  
  ## JS ----
  shinyjs::useShinyjs(),
  
  ## Website ----
  uiOutput(outputId = "website")
  
)

# SERVER ----
server <- function(input, output, session) {
  
  ## Render Website ----
  output$website <- renderUI({
    
    dashboardPage(
      dashboardHeader(title = "Bella Pet",
                      tags$li(class = "dropdown",
                              tags$p(class = "greetings", 
                                     "Ola, Usuario"),
                              tags$img(class = "profile_img",
                                       src = "Profile.png"))
                      ),
      dashboardSidebar(
        sidebarMenu(
          menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
          menuItem("Widgets", tabName = "widgets", icon = icon("th"))
        )
      ),
      dashboardBody(
        tabItems(
      # First tab content
      tabItem(tabName = "dashboard",
        fluidRow(
          box(plotOutput("plot1", height = 250)),

          box(
            title = "Controls",
            sliderInput("slider", "Number of observations:", 1, 100, 50)
          )
        )
      ),

      # Second tab content
      tabItem(tabName = "widgets",
        h2("Widgets tab content")
      )
    )
      )
    )
    
  })
  
}

# Run APP ----
shinyApp(ui = ui, server = server)
