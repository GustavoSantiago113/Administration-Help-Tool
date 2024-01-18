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
library(dplyr)

# Sources ----
source("pages/inventoryControl.R")

# Databases ----
inventoryPath <- "data/estoque.csv"
buysPath <- "data/compra.csv"

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
        ### Side Bar Menu ----
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
  
  ### Inventory ----
  
  #### Create reactive tables ----
  
  it <- reactiveValues(data = read.csv(inventoryPath), orig = read.csv(inventoryPath))
  bt <- reactiveValues(data = read.csv(buysPath), orig = read.csv(buysPath))
  
  #### Render reactive tables ----
  output$inventoryTable <- render_inventory_table(it$data)
  output$buyTable <- render_inventory_table(bt$data)
  
  #### Edit tables ----
  observeEvent(input$inventoryTable_cell_edit, {edit_inventory(input, it, inventoryPath)})
  observeEvent(input$buyTable_cell_edit, {edit_buy(input, bt, buysPath)})
  
  #### Remove Rows ----
  observeEvent(input$removeInventory, {remove_inventory(input, it, inventoryPath)})
  observeEvent(input$removeBuy, {remove_buy(input, bt, buysPath)})
  
  #### Add data -----
  observeEvent(input$addInventory, {add_inventory_modal()})
  
  observeEvent(input$addBuy, {add_buy_modal()})
  observeEvent(input$buyAdd, {addBuyFunction(name = input$nome,
                                             amount = input$quantidade,
                                             buyValue = input$valorCompra,
                                             sellValue = input$valorVenda,
                                             noteNumber = input$numeroNota,
                                             seller = input$fornecedor,
                                             buyDate = input$dataCompra,
                                             category = input$categoria,
                                             database = bt)
  })
  
  
}

# Run APP ----
shinyApp(ui = ui, server = server)
