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
source("pages/cliente_plano.R")
source("pages/cashbook.R")

# Databases ----
inventoryPath <- "data/estoque.csv"
buysPath <- "data/compra.csv"
calendarPath <- "data/calendario.csv"
clientsPath <- "data/clientes.csv"
plansPath <- "data/planos.csv"

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
          #### Cash book ----
          menuCashBook(),
          #### Simulator ----
          menuSimulator(),
          #### Clients and Plans ----
          clientePlano(),
          
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
          #### Cash book ----
          cashbookMainPage(tabName = "cashbook"),
          #### Simulator ----
          simulatorMainPage(tabName = "simulator"),
          #### Clients and Plans ----
          clients_and_plans_MainPage(tabName = "clientePlano"),
          
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
  
  #### Create reactive tables ----
  cl <- reactiveValues(data = read.csv(calendarPath), orig = read.csv(calendarPath))
  
  #### Render reactive calendar ----
  output$my_calendar <- renderCalendar({render_calendar(cl$data)})
  
  #### Add event -----
  observeEvent(input$addEvent, {add_event_modal()})
  observeEvent(input$eventAdd,{add_calendar(filePath = calendarPath,
                                           database = cl,
                                           calendarName = input$calendarName,
                                           calendarDescription = input$calendarDescription,
                                           calendarStart = input$calendarStart,
                                           calendarEnd = input$calendarEnd,
                                           calendarCategory = input$calendarCategory,
                                           calendarLocation = input$calendarLocation,
                                           calendarColor = input$calendarColor,
                                           recurrency = NA,
                                           clientId = NA)
  })
  
  #### Edit event ----
  observeEvent(input$my_calendar_update, {edit_calendar(input, cl, calendarPath)})
  
  #### Remove event ----
  observeEvent(input$my_calendar_delete, {remove_calendar(input, cl, calendarPath)})
  
  ### Simulator ----
  
  #### Creating reactive variables ----
  counter <- reactiveValues(n = 0) # Track the number of input boxes to render
  # Track all user inputs
  AllInputs <- reactive({
    x <- reactiveValuesToList(input)
  })
  
  #### Add inputs ----
  observeEvent(input$inputsAdd, {add_Input(counter)})
  textboxes <- reactive({dynamic_Inputs(counter, AllInputs())})
  output$extraInputs <- renderUI({ textboxes() })
  
  ### Remove inputs ----
  observeEvent(input$inputsRemove, {remove_Input(counter)})
  
  #### Calculating prices ----
  observe({
    preco_final <- soma_preco(counter, input, input$maodeobraSimulador, input$lucroSimulador, input$impostoSimulador)
    output$simuladorServico <- renderUI({output_preco(preco_final)})
    
  })
  output$simuladorProduto <- renderUI({ simulador_produto(input$novoCustoSimulador,
                                                          input$impostoSimulador1,
                                                          input$lucroSimulador1) })
  
  ### Client and plans ----
  
  #### Reactive variables ----
  ct <- reactiveValues(data = read.csv(clientsPath), orig = read.csv(clientsPath))
  pt <- reactiveValues(data = read.csv(plansPath), orig = read.csv(plansPath))
  
  #### Render reactive tables ----
  output$clientsTable <- DT::renderDataTable({
    datatable(ct$data,
              editable = TRUE)
  })
  output$plansTable <- DT::renderDataTable({
    datatable(pt$data,
              editable = TRUE)
  })
  
  #### Add data ----
  observeEvent(input$addClients, {add_client_modal()})
  observeEvent(input$clientAdd, {add_clients(database = ct,
                                             filePath = clientsPath,
                                             nomeAnimal = input$nomeAnimal,
                                             nomeTutor = input$nomeTutor,
                                             aniversarioAnimal = input$aniversarioAnimal,
                                             aniversarioCliente = input$aniversarioCliente,
                                             clienteDesde = input$clienteDesde,
                                             contatoCliente = input$contatoCliente,
                                             porteAnimal = input$porteAnimal,
                                             planoCliente = input$planoCliente,
                                             calendarDB = cl,
                                             calendarFilePath = calendarPath
                                             )
  })
  observeEvent(input$addPlans, {add_plan_modal()})
  observeEvent(input$planAdd, {add_plans(database = pt,
                                         filePath = plansPath,
                                         nomePlano = input$nomePlano,
                                         valorVendaPlano = input$valorVendaPlano,
                                         custoPlano = input$custoPlano,
                                         numeroDeVisitas = input$numeroDeVisitas,
                                         descicaoPlano = input$descicaoPlano)
    })
  
  #### Edit tables ----
  observeEvent(input$clientsTable_cell_edit, {edit_client(input, ct, clientsPath, cl, calendarPath)})
  observeEvent(input$plansTable_cell_edit, {edit_plan(input, pt, plansPath)})
  
  #### Remove Rows ----
  observeEvent(input$removeClients, {remove_client(input, ct, clientsPath, cl, calendarPath)})
  observeEvent(input$removePlans, {remove_plan(input, pt, plansPath)})
  
}

# Run APP ----
shinyApp(ui = ui, server = server)
