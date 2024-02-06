# Author: Gustavo N. Santiago ----
# Small business administration help tool -----
# Version 1

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
source("pages/fiscal_note.R")
source("pages/sells.R")

# Databases ----
inventoryPath <- "data/estoque.csv"
buysPath <- "data/compra.csv"
calendarPath <- "data/calendario.csv"
clientsPath <- "data/clientes.csv"
plansPath <- "data/planos.csv"
sellsProductPath <- "data/venda_produto.csv"
visitPath <- "data/visitas.csv"
sellPlanPath <- "data/venda_plano.csv"

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
      dashboardHeader(title = "Bella Pet", # Title
                      tags$li(class = "dropdown",
                              tags$p(class = "greetings", 
                                     "Ola, Usuario"),
                              tags$img(class = "profile_img",
                                       src = "Profile.png")) #Name and picture in the right corner
                      ), # Header
      dashboardSidebar(
        ### Side Bar Menu ----
        sidebarMenu(
          
          #### Sells ----
          menuSells(),
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
          #### Fiscal Note ----
          fiscalNote(),
          
          menuItem("Widgets", tabName = "widgets", icon = icon("th"))
          
        )
      ),
      dashboardBody(
        ### Body ----
        tabItems(
          
          #### Sells ----
          sellsMainPage(tabName = "sells"),
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
          #### Fiscal Note ----
          fiscal_note_mainPage(tabName = "fiscalNote"),
          
          tabItem(tabName = "widgets",
                  h2("Widgets tab content")
          )
          
        )
      )
    )
    
  })
  
  ## Main Functions ----
  
  ### Sells ----
  
  #### Create reactive tables ----
  cart <- data.frame(Nome=character(),
                     Quantidade = as.numeric(),
                     Categoria = character(),
                     Valor = numeric()) # Cart table
  cartReactive <- reactiveValues(data = cart) # Cart table
  sellsProductTable <- reactiveValues(data = read.csv(sellsProductPath), orig = read.csv(sellsProductPath)) # Sells table
  visitHistorical <- reactiveValues(data = read.csv(visitPath), orig = read.csv(visitPath)) # Visits historical
  planSellHistorical <- reactiveValues(data = read.csv(sellPlanPath), orig = read.csv(sellPlanPath)) #Plan sell historical
  
  #### Render reactive tables ----
  output$inventoryForCart <- DT::renderDataTable({
    datatable(it$data,
              options = list(pageLength = 5, dom = 'ft'),
              selection = "single")
  }) # Products for cart
  output$cart <- DT::renderDataTable({
    datatable(cartReactive$data,
              editable = TRUE,
              options = list(pageLength = 5, dom = 't'))
  }) # Cart
  output$sellsTable <- DT::renderDataTable({
    datatable(sellsProductTable$data,
              editable = TRUE,
              options = list(pageLength = 10))
  })# Sells historic
  output$clientForCheckpoint <- DT::renderDataTable({
    datatable(ct$data,
              options = list(pageLength = 5, dom = 'ft', scrollX = TRUE),
              selection = "single")
  })# Visit checkpoint
  output$visitTable <- DT::renderDataTable({
    datatable(visitHistorical$data,
              editable = TRUE,
              options = list(pageLength = 5, dom = 'ft'),
              selection = "single")
  })# Visit historic
  output$planSellTable <- DT::renderDataTable({
    datatable(planSellHistorical$data,
              editable = TRUE,
              options = list(pageLength = 5, dom = 'ft'),
              selection = "single")
  })# Plan sell historic
  output$planForSell <- DT::renderDataTable({
    datatable(pt$data,
              editable = TRUE,
              options = list(pageLength = 5, dom = 'ft', scrollX = TRUE),
              selection = "single")
  })# Plan table to sell
  output$clientForSell <- DT::renderDataTable({
    datatable(ct$data,
              editable = TRUE,
              options = list(pageLength = 5, dom = 'ft', scrollX = TRUE),
              selection = "single")
  })# Client table to sell
  
  #### Add data -----
  observeEvent(input$sellProduct, {add_product_modal()}) # Modal for product sells
  observeEvent(input$sellVisit, {add_visit_modal()}) # Modal for visits
  observeEvent(input$sellPlan, {add_plan_sell_modal()}) # Modal for sell plan
  observeEvent(input$addCart, {add_to_cart(input, it, cartReactive)}) #Add product to cart
  observeEvent(input$finishSell, {add_to_sell_table(sellProductTable = sellsProductTable,
                                                    cartTable = cartReactive,
                                                    client = input$clientSell,
                                                    sellProductTablePath = sellsProductPath,
                                                    inventoryTable = it)}) #Finish sell
  
  #### Remove Rows ----
  observeEvent(input$removeCart, {remove_from_cart(input, cartReactive)}) #Remove product from cart
  observeEvent(input$unsellProduct, {remove_from_sells(input, sellsProductTable, sellsProductPath)}) #Remove product from sells historical
  
  
  #### Edit tables ----
  observeEvent(input$cart_cell_edit, {edit_cart(input, cartReactive)}) #Edit cart
  observeEvent(input$sellsTable_cell_edit, {edit_sells_table(input, sellsProductTable, sellsProductPath)}) #Edit sells historical
  
  
  ### Inventory ----
  
  #### Create reactive tables ----
  
  it <- reactiveValues(data = read.csv(inventoryPath), orig = read.csv(inventoryPath)) # Inventory
  bt <- reactiveValues(data = read.csv(buysPath), orig = read.csv(buysPath)) # Buying
  
  #### Render reactive tables ----
  output$inventoryTable <- DT::renderDataTable({
    datatable(it$data,
              editable = TRUE)
  }) # Inventory table
  output$buyTable <- DT::renderDataTable({
    datatable(bt$data,
              editable = TRUE)
  }) # Buy table
  
  #### Edit tables ----
  observeEvent(input$inventoryTable_cell_edit, {edit_inventory(input, it, inventoryPath)}) # Edit inventory table
  observeEvent(input$buyTable_cell_edit, {edit_buy(input, bt, buysPath)}) # Edit buy table
  
  #### Remove Rows ----
  observeEvent(input$removeInventory, {remove_inventory(input, it, inventoryPath)}) # Remove inventory rows
  observeEvent(input$removeBuy, {remove_buy(input, bt, buysPath)}) # Remove buy table rows
  
  #### Add data -----
  observeEvent(input$addInventory, {add_inventory_modal()}) # Modal for inventory
  observeEvent(input$inventoryAdd, {add_inventory(database = it,
                                                  name = input$nomeEstoque,
                                                  amount = input$quantidadeEstoque,
                                                  category = input$categoriaEstoque,
                                                  filePath = inventoryPath)}) # Add inventory data
  observeEvent(input$addBuy, {add_buy_modal()}) # Modal for buy table
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
                                      inventoryDBPath = inventoryPath) #Add buying table data
  })
  
  ### Calendar ----
  
  #### Create reactive tables ----
  cl <- reactiveValues(data = read.csv(calendarPath), orig = read.csv(calendarPath)) #Calendar table
  
  #### Render reactive calendar ----
  output$my_calendar <- renderCalendar({render_calendar(cl$data)}) # Display calendar
  
  #### Add event -----
  observeEvent(input$addEvent, {add_event_modal()}) # Modal for calendar
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
                                           clientId = NA) # Add data to calendar widget and table
  })
  
  #### Edit event ----
  observeEvent(input$my_calendar_update, {edit_calendar(input, cl, calendarPath)}) # Edit calendar events widget and table
  
  #### Remove event ----
  observeEvent(input$my_calendar_delete, {remove_calendar(input, cl, calendarPath)}) # Remove calendar events widget and table
  
  ### Simulator ----
  
  #### Creating reactive variables ----
  counter <- reactiveValues(n = 0) # Track the number of input boxes to render
  AllInputs <- reactive({
    x <- reactiveValuesToList(input)
  }) # Track all user inputs
  
  #### Add inputs ----
  observeEvent(input$inputsAdd, {add_Input(counter)}) # Add inputs
  textboxes <- reactive({dynamic_Inputs(counter, AllInputs())}) # Create the dynamic boxes
  output$extraInputs <- renderUI({ textboxes() }) # Render the dynamic boxes
  
  ### Remove inputs ----
  observeEvent(input$inputsRemove, {remove_Input(counter)}) # Remove the inputs
  
  #### Calculating prices ----
  observe({
    preco_final <- soma_preco(counter, input, input$maodeobraSimulador, input$lucroSimulador, input$impostoSimulador)
    output$simuladorServico <- renderUI({output_preco(preco_final)})
    
  }) #Calculate the final prices for services
  output$simuladorProduto <- renderUI({ simulador_produto(input$novoCustoSimulador,
                                                          input$impostoSimulador1,
                                                          input$lucroSimulador1) }) #Calculate final prices for products
  
  ### Client and plans ----
  
  #### Reactive variables ----
  ct <- reactiveValues(data = read.csv(clientsPath), orig = read.csv(clientsPath)) # Client Table
  pt <- reactiveValues(data = read.csv(plansPath), orig = read.csv(plansPath)) # Plan table
  
  #### Render reactive tables ----
  output$clientsTable <- DT::renderDataTable({
    datatable(ct$data,
              editable = TRUE)
  }) # Client table
  output$plansTable <- DT::renderDataTable({
    datatable(pt$data,
              editable = TRUE)
  }) #Plan table
  
  #### Add data ----
  observeEvent(input$addClients, {add_client_modal()}) # Modal for client
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
                                             ) # Add client to database
  })
  observeEvent(input$addPlans, {add_plan_modal()}) # Modal for plan
  observeEvent(input$planAdd, {add_plans(database = pt,
                                         filePath = plansPath,
                                         nomePlano = input$nomePlano,
                                         valorVendaPlano = input$valorVendaPlano,
                                         custoPlano = input$custoPlano,
                                         numeroDeVisitas = input$numeroDeVisitas,
                                         descicaoPlano = input$descicaoPlano)
    })  # Add plan to database
  
  #### Edit tables ----
  observeEvent(input$clientsTable_cell_edit, {edit_client(input, ct, clientsPath, cl, calendarPath)}) # Edit Client table
  observeEvent(input$plansTable_cell_edit, {edit_plan(input, pt, plansPath)}) #Edit plan table
  
  #### Remove Rows ----
  observeEvent(input$removeClients, {remove_client(input, ct, clientsPath, cl, calendarPath)}) # Remove row from client table
  observeEvent(input$removePlans, {remove_plan(input, pt, plansPath)}) # Remove row from plan table
  
  ### Fiscal Note ----
  observeEvent(input$gerar, {generate_report(output)}) # Generate the fiscal note
  
}

# Run APP ----
shinyApp(ui = ui, server = server)
