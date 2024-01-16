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
library(DT)

# Sources ----
source("pages/inventoryControl.R")

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
          menuInventory(),
          menuItem("Widgets", tabName = "widgets", icon = icon("th"))
        )
      ),
      dashboardBody(
        tabItems(
          ### Inventory ----
          inventoryControlMainPage(tabName = "inventoryControl"),
          tabItem(tabName = "widgets",
            h2("Widgets tab content")
          )
        )
      )
    )
    
  })
  
  ### Add inventory button ----
  observeEvent(input$addInventory, {
    addInventoryModal()
  })
  
  ### Remove inventory button ----
  observeEvent(input$removeInventory, {
    removeInventoryModal()
  })
  
}

# Run APP ----
shinyApp(ui = ui, server = server)
