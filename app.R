# Gustavo N. Santiago ----
# Small business administration help tool -----
# Version 1

# APPLICATION DESCRIPTION ----
# - Perform CRUD Operations
# - Integrate database at several applications
# - Tool to help a small business to visualize and control their sales

# Libraries ----
library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(shinythemes)
library(shinyjs)
library(DT)
library(dplyr)

# Sources ----
source("pages/inventoryControl.R")
source("pages/calendar.R")
source("pages/simulator.R")

# Databases ----
inventoryPath <- "data/estoque.csv"
buysPath <- "data/compra.csv"
calendarPath <- "data/calendario.csv"

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
          #### Calendar ----
          menuCalendar(),
          #### Simulator ----
          menuSimulator(),
          
          menuItem("Widgets", tabName = "widgets", icon = icon("th"))
          
        )
      ),
      dashboardBody(
        ### Body ----
        tabItems(
          
          #### Inventory ----
          inventoryControlMainPage(tabName = "inventoryControl"),
          #### Calendar ----
          calendarMainPage(tabName = "calendar"),
          #### Simulator ----
          simulatorMainPage(tabName = "simulator"),
          
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
  
  it <- reactiveValues(data = read.csv(inventoryPath), orig = read.csv(inventoryPath)) # Inventory
  bt <- reactiveValues(data = read.csv(buysPath), orig = read.csv(buysPath)) # Buyings
  cl <- reactiveValues(data = read.csv(calendarPath), orig = read.csv(calendarPath)) # Calendar
  counter <- reactiveValues(n = 0) # Track the number of input boxes to render
  # Track all user inputs
  AllInputs <- reactive({
    x <- reactiveValuesToList(input)
  }) 
  
  #### Render reactive tables ----
  output$inventoryTable <- DT::renderDataTable({
    datatable(it$data,
              editable = TRUE)
  })
  output$buyTable <- DT::renderDataTable({
    datatable(bt$data,
              editable = TRUE)
  })
  
  #### Edit tables ----
  observeEvent(input$inventoryTable_cell_edit, {edit_inventory(input, it, inventoryPath)})
  observeEvent(input$buyTable_cell_edit, {edit_buy(input, bt, buysPath)})
  
  #### Remove Rows ----
  observeEvent(input$removeInventory, {remove_inventory(input, it, inventoryPath)})
  observeEvent(input$removeBuy, {remove_buy(input, bt, buysPath)})
  
  #### Add data -----
  observeEvent(input$addInventory, {add_inventory_modal()})
  observeEvent(input$inventoryAdd, {add_inventory(database = it,
                                                  name = input$nomeEstoque,
                                                  amount = input$quantidadeEstoque,
                                                  category = input$categoriaEstoque,
                                                  filePath = inventoryPath)})
  observeEvent(input$addBuy, {add_buy_modal()})
  observeEvent(input$buyAdd, {add_buy(name = input$nomeCompra,
                                      amount = input$quantidadeCompra,
                                      buyValue = input$valorCompra,
                                      sellValue = input$valorVenda,
                                      noteNumber = input$numeroNota,
                                      seller = input$fornecedor,
                                      buyDate = input$dataCompra,
                                      category = input$categoriaCompra,
                                      database = bt,
                                      filePath = buysPath,
                                      inventoryDB = it,
                                      inventoryDBPath = inventoryPath)
  })
  
  ### Calendar ----
  output$my_calendar <- renderCalendar({render_calendar(cl$data)})
  observeEvent(input$addEvent, {add_event_modal()})
  observeEvent(input$eventAdd,{add_calendar(filePath = calendarPath,
                                           database = cl,
                                           calendarName = input$calendarName,
                                           calendarDescription = input$calendarDescription,
                                           calendarStart = input$calendarStart,
                                           calendarEnd = input$calendarEnd,
                                           calendarCategory = input$calendarCategory,
                                           calendarLocation = input$calendarLocation,
                                           calendarColor = input$calendarColor)
  }) # Add
  observeEvent(input$my_calendar_update, {edit_calendar(input, cl, calendarPath)}) # Edit
  observeEvent(input$my_calendar_delete, {remove_calendar(input, cl, calendarPath)}) # Remove
  
  ### Simulator ----
  observeEvent(input$inputsAdd, {add_Input(counter)})
  observeEvent(input$inputsRemove, {remove_Input(counter)})
  textboxes <- reactive({dynamic_Inputs(counter, AllInputs())})
  output$extraInputs <- renderUI({ textboxes() })
  output$simuladorServico <- renderUI({ simulador_servico() })
  output$simuladorProduto <- renderUI({ simulador_produto() })
  
}

# Run APP ----
shinyApp(ui = ui, server = server)
