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

# Databases ----
inventoryPath <- "data/estoque.csv"
buys <- read.csv("data/compra.csv")

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
        ### SideBar Menu ----
        sidebarMenu(
          
          #### Inventory ----
          menuInventory(),
          
          menuItem("Widgets", tabName = "widgets", icon = icon("th"))
          
        )
      ),
      dashboardBody(
        ### Body ----
        tabItems(
          
          #### Inventory ----
          inventoryControlMainPage(tabName = "inventoryControl"),
          
          tabItem(tabName = "widgets",
                  h2("Widgets tab content")
          )
          
        )
      )
    )
    
  })
  
  ## Main Functions ----
  
  ### Inventory table ----
  
  it <- reactiveValues(data = read.csv(inventoryPath), orig = read.csv(inventoryPath))
  
  output$inventoryTable <- renderInventoryTable(it$data)
  
  observeEventsInventory(it, input, inventoryPath)
  
}

# Run APP ----
shinyApp(ui = ui, server = server)
