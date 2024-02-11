# Libraries -----
library(shiny)
library(shinydashboard)
library(tidyverse)

# UI -----

## Menu Item ----
menuSells <- function(){
  menuItem("Vendas", tabName = "sells", icon = icon(name = NULL,
  style = "background: url('sells.svg');
  height: 20px;
  width: 20px;
  display: inline-block;
  background-repeat: no-repeat;")
  )
}

## Main Page ----
sellsMainPage <- function(tabName){
  
  tabItem(
    tabName = tabName,
    tabsetPanel(
      tabPanel(
        title = "Produtos",
        fluidRow(
          column(
            width = 12,
            align = "center",
            tags$h2("Venda de produtos")
          )
        ),
        fluidRow(
          column(
            class = "mainInventoryControl",
            width = 12,
            align = "center",
            actionButton(
              inputId = "sellProduct",
              label = "Novo",
              icon = icon("plus"),
              style = "background-color: #76bfac; color: white; font-family: 'Bahnschrift'; font-size: 20px; height: 50px; margin-left: 10px; margin-right: 10px;"
            ),
            actionButton(
              inputId = "unsellProduct",
              label = "Excluir",
              icon = icon("trash"),
              style = "background-color: red; color: white; font-family: 'Bahnschrift'; font-size: 20px; height: 50px; margin-left: 10px; margin-right: 10px;"
            ),
            br(),
            DTOutput(
              outputId = "sellsTable"
            )
          )
        )
      ),
      tabPanel(
        title = "Visitas",
        fluidRow(
          column(
            width = 12,
            align = "center",
            tags$h2("Visitas")
          )
        ),
        fluidRow(
          column(
            class = "mainInventoryControl",
            width = 12,
            align = "center",
            actionButton(
              inputId = "sellVisit",
              label = "Novo",
              icon = icon("plus"),
              style = "background-color: #76bfac; color: white; font-family: 'Bahnschrift'; font-size: 20px; height: 50px; margin-left: 10px; margin-right: 10px;"
            ),
            actionButton(
              inputId = "unsellVisit",
              label = "Excluir",
              icon = icon("trash"),
              style = "background-color: red; color: white; font-family: 'Bahnschrift'; font-size: 20px; height: 50px; margin-left: 10px; margin-right: 10px;"
            ),
            br(),
            DTOutput(
              outputId = "visitTable"
            )
          )
        )
      ),
      tabPanel(
        title = "Planos",
        fluidRow(
          column(
            width = 12,
            align = "center",
            tags$h2("Venda de planos")
          )
        ),
        fluidRow(
          column(
            class = "mainInventoryControl",
            width = 12,
            align = "center",
            actionButton(
              inputId = "sellPlan",
              label = "Novo",
              icon = icon("plus"),
              style = "background-color: #76bfac; color: white; font-family: 'Bahnschrift'; font-size: 20px; height: 50px; margin-left: 10px; margin-right: 10px;"
            ),
            actionButton(
              inputId = "unsellPlan",
              label = "Excluir",
              icon = icon("trash"),
              style = "background-color: red; color: white; font-family: 'Bahnschrift'; font-size: 20px; height: 50px; margin-left: 10px; margin-right: 10px;"
            ),
            br(),
            DTOutput(
              outputId = "planSellTable"
            )
          )
        )
      )
    )
  )
  
}

## Add Modals ----
add_product_modal <- function(){
  modalDialog(
    title = "Venda de produto",
    size = "m",
    easyClose = TRUE,
    div(
      style="z-index:1002; background-color: white; padding: 10px",
      textInput(
        inputId = "clientSell",
        label = "Adicione o nome do cliente",
        placeholder = "Nome do cliente"
      ),
      br(),
      DTOutput(
        outputId = "inventoryForCart"
      ),
      br(),
      actionButton(
        inputId = "addCart",
        label = "Adicionar ao carrinho",
        icon = icon("plus"),
        style = "background-color: #76bfac; color: white; font-family: 'Bahnschrift'; font-size: 20px; height: 50px; margin-left: 10px; margin-right: 10px;"
      ),
      br(),
      DTOutput(
        outputId = "cart"
      ),
      br(),
      actionButton(
        inputId = "removeCart",
        label = "Remover do carrinho",
        icon = icon("trash"),
        style = "background-color: red; color: white; font-family: 'Bahnschrift'; font-size: 20px; height: 50px; margin-left: 10px; margin-right: 10px;"
      ),
      actionButton(
        inputId = "finishSell",
        label = "Finalizar venda",
        icon = icon("plus"),
        style = "background-color: #76bfac; color: white; font-family: 'Bahnschrift'; font-size: 20px; height: 50px; margin-left: 10px; margin-right: 10px;"
      ),
      br(),
    ),
    footer = NULL
  ) %>% showModal()
} # Modal for selling history

add_visit_modal <- function(){
  modalDialog(
    title = "Visita",
    size = "m",
    easyClose = TRUE,
    div(
      style="z-index:1002; background-color: white; padding: 10px",
      tags$h3("Escolha o cliente"),
      DTOutput(
        outputId = "clientForCheckpoint",
        width = "auto"
      ),
      br(),
      actionButton(
        inputId = "addVisita",
        label = "Confirmar visita",
        icon = icon("plus"),
        style = "background-color: #76bfac; color: white; font-family: 'Bahnschrift'; font-size: 20px; height: 50px; margin-left: 10px; margin-right: 10px;"
      )
    ),
    footer = NULL
  ) %>% showModal()
} # Modal for visit checkpoint history

add_plan_sell_modal <- function(){
  modalDialog(
    title = "Venda de plano",
    size = "m",
    easyClose = TRUE,
    div(
      style="z-index:1002; background-color: white; padding: 10px",
      tags$h3("Escolha um plano"),
      DTOutput(
        outputId = "planForSell",
        width = "auto"
      ),
      br(),
      tags$h3("Escolha o cliente"),
      DTOutput(
        outputId = "clientForSell",
        width = "auto"
      ),
      br(),
      actionButton(
        inputId = "sell_Plan",
        label = "Confirmar venda",
        icon = icon("plus"),
        style = "background-color: #76bfac; color: white; font-family: 'Bahnschrift'; font-size: 20px; height: 50px; margin-left: 10px; margin-right: 10px;"
      )
    ),
    footer = NULL
  ) %>% showModal()
} # Modal for plan selling history

# Server ----

## Add data ----
add_to_cart <- function(input, inventoryTable, cartTable){
  
  row <- input$inventoryForCart_rows_selected # Check the row of select product from modal table
  
  # Create variables to store the name, category and value of the selected product
  name <- inventoryTable$data[row, "Nome"]
  category <- inventoryTable$data[row, "Categoria"]
  value <- inventoryTable$data[row, "Valor"]
  
  cartTable$data[nrow(cartTable$data)+1,] <- c(name,1,category,value) # Append the variables to the cart table
  
} # Add data to the cart table

add_to_sell_table <- function(sellProductTable, cartTable, client, sellProductTablePath, inventoryTable, inventoryTablePath){
  
  # Append the client name, today's date amount to the cart data
  cartTable$data$Data <- format(as.Date(Sys.Date()), "%Y-%m-%d")
  cartTable$data$Cliente <- client
  
  sellProductTable$data <- rbind(cartTable$data, sellProductTable$data)# Combine the cart table with the sells historic table
  
  saveData(data = sellProductTable$data,
           filepath = sellProductTablePath) # Save the combined table
  
  names <- cartTable$data[,1] # Get the names of the products to find them

  newDB <- inventoryTable$data %>%
    filter(Nome %in% names) %>%
    mutate(Quantidade = Quantidade - as.numeric(cartTable$data$Quantidade)) # Find the products and remove the amounts

  # Combine the inventory table with the cart table
  updated_inventoryDB <- dplyr::anti_join(inventoryTable$data, newDB, by = c("Nome"))
  updated_inventoryDB <- dplyr::bind_rows(updated_inventoryDB, newDB)
  updated_inventoryDB <- dplyr::arrange(updated_inventoryDB)
  inventoryTable$data <- updated_inventoryDB

  saveData(data = inventoryTable$data,
           filepath = inventoryTablePath) # Save the updated inventory table

  removeModal() # Close the modal
  
} # Add data to the sells historic data and modify the amount in inventory

add_to_visit <- function(input, clientTable, clientTablePath, visitTable, visitTablePath){
  
  row <- input$clientForCheckpoint_rows_selected # Get the row of select client from modal table
  
  clientTable$data[row, "Visitas_Restantes"] <- as.numeric(clientTable$data[row, "Visitas_Restantes"]) - 1 # Checkpoint in the clients table, removing the visit
  
  saveData(data = clientTable$data,
           filepath = clientTablePath) # Save the clients data
  
  # Create variables to store the name, owner name, date, remaining visits and Id number of the selected client
  name <- clientTable$data[row, "Animal"]
  dono <- clientTable$data[row, "Dono"]
  date <- as.character(Sys.Date())
  remainingVisits <- clientTable$data[row, "Visitas_Restantes"]
  idCliente <- clientTable$data[row, "clienteId"]
  
  visitTable$data[nrow(visitTable$data)+1,] <- c(name, dono, date, remainingVisits, idCliente) # Append the variables to the visits table

  saveData(data = visitTable$data,
           filepath = visitTablePath) # Save the historical visits table
  
  removeModal() # Close the modal
  
} # Add data to historic visits table

add_to_plan <- function(input, clientTable, planTable, clientTablePath, planSellTable, planSellTablePath){
  
  rowPlan <- input$planForSell_rows_selected # Get the row of select plan from modal table
  rowClient <- input$clientForSell_rows_selected # Get the row of select client from modal table
  
  # Create variables to store the name, owner name, date of the selected client
  name <- clientTable$data[rowClient, "Animal"]
  dono <- clientTable$data[rowClient, "Dono"]
  date <- as.character(Sys.Date())
  idCliente <- as.numeric(clientTable$data[rowClient, "clienteId"])
  
  # Create variables to store the value and number of visits of selected plan
  value <- planTable$data[rowPlan, "Venda"]
  num_visitas <- as.numeric(planTable$data[rowPlan, "Visitas"])
  
  planSellTable$data[nrow(planSellTable$data)+1,] <- c(name, dono, value, date, num_visitas, idCliente) # Append the variables to the plan sell table
  
  saveData(data = planSellTable$data,
           filepath = planSellTablePath) # Save the plan sells historical
  
  clientTable$data[rowClient, "Visitas_Restantes"] <- clientTable$data[rowClient, "Visitas_Restantes"] + planTable$data[rowPlan, "Visitas"] # Checkpoint in the clients table, adding the visits
  
  saveData(data = clientTable$data,
           filepath = clientTablePath) # Save the clients table
  
  removeModal() # Close the modal
  
} # Add data to historic plans sells

### Edit Cells ----
edit_cart <- function(input, it){
  
  # Check the row and column of the modified data
  row  <- input$cart_cell_edit$row
  clmn <- input$cart_cell_edit$col
  
  it$data[row, clmn] <- input$cart_cell_edit$value # Change the value on the data
  
} # Edit the cart table

edit_sells_table <- function(input, sellsProductTable, tablePath, inventoryTable, inventoryTablePath){
  
  # Check the row and column of the modified data
  row  <- input$sellsTable_cell_edit$row
  clmn <- input$sellsTable_cell_edit$col
  
  if(clmn == 2){

    new_value <- as.numeric(input$sellsTable_cell_edit$value) - as.numeric(sellsProductTable$data[row, clmn]) # Getting the difference of the stock
    name <- sellsProductTable$data[row, "Nome"] # Getting the product name
    
    newDB <- inventoryTable$data %>%
      filter(Nome == name) %>%
      mutate(Quantidade = Quantidade - new_value) # Find the products and add the amounts

    # Combine the inventory table with the cart table
    updated_inventoryDB <- dplyr::anti_join(inventoryTable$data, newDB, by = c("Nome"))
    updated_inventoryDB <- dplyr::bind_rows(updated_inventoryDB, newDB)
    updated_inventoryDB <- dplyr::arrange(updated_inventoryDB)
    inventoryTable$data <- updated_inventoryDB

    saveData(data = inventoryTable$data,
             filepath = inventoryTablePath) # Save the updated inventory table
    
  }
  
  sellsProductTable$data[row, clmn] <- input$sellsTable_cell_edit$value # Change the value on the data
  
  saveData(data = sellsProductTable$data,
           filepath = tablePath) # Save the changes
  
} # Edit the sells historic table and modify the amount in the inventory

edit_visits_table <- function(input, visitsHistoricalTable, tablePath){
  
  # Check the row and column of the modified data
  row  <- input$visitTable_cell_edit$row
  clmn <- input$visitTable_cell_edit$col
  
  visitsHistoricalTable$data[row, clmn] <- input$visitTable_cell_edit$value # Change the value on the data
  
  saveData(data = visitsHistoricalTable$data,
           filepath = tablePath) # Save the changes
  
} # Edit the checkpoint visit historic table and modify the amount in the inventory

edit_plans_table <- function(input, planHistoricalTable, tablePath, clientTable, clientTablePath){
  
  # Check the row and column of the modified data
  row  <- input$planSellTable_cell_edit$row
  clmn <- input$planSellTable_cell_edit$col
  
  if(clmn == 5){
    
    new_value <- input$planSellTable_cell_edit$value - planHistoricalTable$data[row, clmn]
    
    idcliente <- planHistoricalTable$data[row, "idCliente"] # Getting the product name
    
    newDB <- clientTable$data %>%
      filter(clienteId == idcliente) %>%
      mutate(Visitas_Restantes = Visitas_Restantes + new_value) # Find the products and add the amounts
    
    # Combine the inventory table with the cart table
    updated_inventoryDB <- dplyr::anti_join(clientTable$data, newDB, by = c("clienteId"))
    updated_inventoryDB <- dplyr::bind_rows(updated_inventoryDB, newDB)
    updated_inventoryDB <- dplyr::arrange(updated_inventoryDB)
    clientTable$data <- updated_inventoryDB
    
    saveData(data = clientTable$data,
             filepath = clientTablePath) # Save the updated inventory table
    
  }
  
  planHistoricalTable$data[row, clmn] <- input$planSellTable_cell_edit$value # Change the value on the data
  
  saveData(data = planHistoricalTable$data,
           filepath = tablePath) # Save the changes
  
} # Edit the plan sells historic table and modify the amount in the inventory

### Remove rows ----

remove_from_cart <- function(input, it){
  if (!is.null(input$cart_rows_selected)) {
    it$data <- it$data[-as.numeric(input$cart_rows_selected),]
  }
} # Remove rows from the cart table

remove_from_sells <- function(input, it, tablePath, inventoryTable, inventoryTablePath){
  if (!is.null(input$sellsTable_rows_selected)) {
    
    rows <- input$sellsTable_rows_selected # Getting the rows of the selected products
    
    # Getting the names and amounts of the selected products
    quantidades <- as.numeric(it$data[rows, "Quantidade"])
    names <- it$data[rows, "Nome"]
    
    newDB <- inventoryTable$data %>%
      filter(Nome %in% names) %>%
      mutate(Quantidade = Quantidade + quantidades) # Find the products and add the amounts

    # Combine the inventory table with the cart table
    updated_inventoryDB <- dplyr::anti_join(inventoryTable$data, newDB, by = c("Nome"))
    updated_inventoryDB <- dplyr::bind_rows(updated_inventoryDB, newDB)
    updated_inventoryDB <- dplyr::arrange(updated_inventoryDB)
    inventoryTable$data <- updated_inventoryDB
    
    saveData(data = inventoryTable$data,
             filepath = inventoryTablePath) # Save the updated inventory table
    
    it$data <- it$data[-as.numeric(input$sellsTable_rows_selected),] # Remove the rows from sells history
    saveData(data = it$data,
             filepath = tablePath) # Save the changes
  }
} # Remove rows from the sells historic and modify the amount in the inventory

remove_from_visits <- function(input, it, tablePath, clientTable, clientTablePath){
  if (!is.null(input$visitTable_rows_selected)) {
    
    row <- input$visitTable_rows_selected # Getting the rows of the selected visit
    
    # Getting the names and amounts of the selected products
    clientId <- as.numeric(it$data[row, "clienteId"])
    
    newDB <- clientTable$data %>%
      filter(clienteId == clientId) %>%
      mutate(Visitas_Restantes = Visitas_Restantes + 1)

    # Combine the inventory table with the cart table
    updated_inventoryDB <- dplyr::anti_join(clientTable$data, newDB, by = c("clienteId"))
    updated_inventoryDB <- dplyr::bind_rows(updated_inventoryDB, newDB)
    updated_inventoryDB <- dplyr::arrange(updated_inventoryDB)
    clientTable$data <- updated_inventoryDB

    saveData(data = clientTable$data,
             filepath = clientTablePath) # Save the updated inventory table

    it$data <- it$data[-as.numeric(input$visitTable_rows_selected),]
    saveData(data = it$data,
             filepath = tablePath) # Save the changes
  }
} # Remove rows from the sells historic and modify the amount in the inventory

remove_from_plans <- function(input, it, tablePath, clientTable, clientTablePath){
  if (!is.null(input$planSellTable_rows_selected)) {
    
    row <- input$planSellTable_rows_selected # Getting the rows of the selected visit
    
    # Getting the names and amounts of the selected products
    clientId <- it$data[row, "idCliente"]
    
    numb_of_visits <- as.numeric(it$data[row, "Numero_Visitas"])
    
    newDB <- clientTable$data %>%
      filter(clienteId == clientId) %>%
      mutate(Visitas_Restantes = Visitas_Restantes - numb_of_visits)
    
    # Combine the inventory table with the cart table
    updated_inventoryDB <- dplyr::anti_join(clientTable$data, newDB, by = c("clienteId"))
    updated_inventoryDB <- dplyr::bind_rows(updated_inventoryDB, newDB)
    updated_inventoryDB <- dplyr::arrange(updated_inventoryDB)
    clientTable$data <- updated_inventoryDB
    
    saveData(data = clientTable$data,
             filepath = clientTablePath) # Save the updated inventory table
    
    it$data <- it$data[-as.numeric(input$planSellTable_rows_selected),]
    saveData(data = it$data,
             filepath = tablePath) # Save the changes
  }
} # Remove rows from the sells historic and modify the amount in the inventory


## Save edits to Data ----
saveData <- function(data, filepath) {
  write.csv(data, filepath, row.names = FALSE)
}