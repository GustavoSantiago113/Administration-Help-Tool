# Libraries -----
library(shiny)
library(shinydashboard)
library(dplyr)

# UI -----

## Menu Item ----
menuInventory <- function(){
  menuItem("Controle de Estoque", tabName = "inventoryControl", icon = icon(name = NULL,
                                                                            style = "background: url('inventory.svg');
                                                                                    height: 20px;
                                                                                    width: 20px;
                                                                                    display: inline-block;
                                                                                    background-repeat: no-repeat;")
  )
}

## Main Page ----
inventoryControlMainPage <- function(tabName){
  
  tabItem(
    tabName = tabName,
    tabsetPanel(
      tabPanel(
        title = "Compras",
        fluidRow(
          column(
            width = 12,
            align = "center",
            tags$h2("Controle de compras")
          )
        ),
        fluidRow(
          column(
            class = "mainInventoryControl",
            width = 12,
            align = "center",
            actionButton(
              inputId = "addBuy",
              label = "Novo",
              icon = icon("plus"),
              style = "background-color: #76bfac; color: white; font-family: 'Bahnschrift'; font-size: 20px; height: 50px; margin-left: 10px; margin-right: 10px;"
            ),
            actionButton(
              inputId = "removeBuy",
              label = "Excluir",
              icon = icon("trash"),
              style = "background-color: red; color: white; font-family: 'Bahnschrift'; font-size: 20px; height: 50px; margin-left: 10px; margin-right: 10px;"
            ),
            br(),
            DTOutput(
              outputId = "buyTable",
              width = "auto"
            )
          )
        )
      ),
      tabPanel(
        title = "Estoque",
        fluidRow(
          column(
            width = 12,
            align = "center",
            tags$h2("Controle de Estoque")
          )
        ),
        fluidRow(
          column(
            class = "mainInventoryControl",
            width = 12,
            align = "center",
            actionButton(
              inputId = "addInventory",
              label = "Novo",
              icon = icon("plus"),
              style = "background-color: #76bfac; color: white; font-family: 'Bahnschrift'; font-size: 20px; height: 50px; margin-left: 10px; margin-right: 10px;"
            ),
            actionButton(
              inputId = "removeInventory",
              label = "Excluir",
              icon = icon("trash"),
              style = "background-color: red; color: white; font-family: 'Bahnschrift'; font-size: 20px; height: 50px; margin-left: 10px; margin-right: 10px;"
            ),
            br(),
            DTOutput(
              outputId = "inventoryTable",
              width = "auto"
            )
          )
        )
      )
    )
  )
  
}

## Add Modals ----
# Modal for buying
add_buy_modal <- function(inventoryDB){
  
  my_autocomplete_list <- inventoryDB$data[,"Nome"]
  modalDialog(
    title = "Adicionar Produto",
    size = "m",
    easyClose = TRUE,
    div(
      column(
        width = 12,
        style="z-index:1002; margin-bottom: 15px; padding-left: 0px;",
        selectizeInput(
          inputId = 'nomeCompra',
          label = 'Nome',
          choices = my_autocomplete_list,
          selected = NULL,
          multiple = FALSE,
          options = list(create = TRUE)
        )
      ),
      numericInput(
        inputId = "quantidadeCompra",
        label = "Quantidade",
        min = 0,
        value = 0
      ),
      numericInput(
        inputId = "valorCompra",
        label = "Valor da Compra",
        min = 0,
        value = 0
      ),
      numericInput(
        inputId = "valorVenda",
        label = "Valor da Venda",
        min = 0,
        value = 0
      ),
      textInput(
        inputId = "numeroNota",
        label = NULL,
        placeholder = "Numero da Nota Fiscal"
      ),
      textInput(
        inputId = "fornecedor",
        label = NULL,
        placeholder = "Nome do Fornecedor"
      ),
      dateInput(
        inputId = "dataCompra",
        label = "Data de compra",
        language = "pt-BR",
        format = "dd-mm-yyyy",
      ),
      selectInput(
        inputId = "categoriaCompra",
        label = NULL,
        choices = c("Racao" = "Racao",
                    "Banho e Tosa" = "Banho e Tosa",
                    "Roupinha" = "Roupinha")
      ),
      actionButton(inputId = "buyAdd", 
                     label   = "Adicionar a compras", 
                     style = "background-color: #76bfac; color: white; font-family: 'Bahnschrift'; font-size: 20px; height: 50px; margin-left: 10px; margin-right: 10px;")
    ),
    footer = NULL
  ) %>% showModal()
}

# Modal for inventory
add_inventory_modal <- function(input){
  modalDialog(
    title = "Adicionar Produto",
    size = "m",
    easyClose = TRUE,
    div(
      style="z-index:1002",
      textInput(
        inputId = "nomeEstoque",
        label = NULL,
        placeholder = "Nome"
      ),
      numericInput(
        inputId = "quantidadeEstoque",
        label = "Quantidade",
        value = 0,
        min = 0
      ),
      numericInput(
        inputId = "valorEstoque",
        label = "Valor de venda",
        value = 0,
        min = 0
      ),
      selectInput(
        inputId = "categoriaEstoque",
        label = NULL,
        choices = c("Racao" = "Racao",
                    "Banho e Tosa" = "Banho e Tosa",
                    "Roupinha" = "Roupinha")
      ),
      actionButton(inputId = "inventoryAdd", 
                     label   = "Adicionar ao estoque", 
                     style = "background-color: #76bfac; color: white; font-family: 'Bahnschrift'; font-size: 20px; height: 50px; margin-left: 10px; margin-right: 10px;")
    ),
    footer = NULL
  ) %>% showModal()
  
}

# Server ----

## Observe Events ----

### Edit Cells ----

# Edit inventory
edit_inventory <- function(input, it, tablePath){
  
  row  <- input$inventoryTable_cell_edit$row # Check the row of selected cell
  clmn <- input$inventoryTable_cell_edit$col # Check the column number of selected cell
  it$data[row, clmn] <- input$inventoryTable_cell_edit$value # Edit and updates the selected cell
  saveData(data = it$data,
           filepath = tablePath) # Saves the updates
  
}

# Edit buy
edit_buy <- function(input, bt, tablePath, inventoryTable, inventoryTablePath){
  
  row  <- input$buyTable_cell_edit$row # Check the row of selected cell
  clmn <- input$buyTable_cell_edit$col # Check the column number of selected cell
  
  # If the column is in amount, modify it in the inventory
  if(clmn == 2){
    product <- bt$data$Nome[[row]] # Check product name
    amount <- as.numeric(bt$data[row, clmn]) - as.numeric(input$buyTable_cell_edit$value) # Get the difference between what was and the new value
    
    # Find the products and remove the amounts
    newDB <- inventoryTable$data %>%
      filter(Nome == product) %>%
      mutate(Quantidade = Quantidade - as.numeric(amount)) 
    
    # Combine the inventory table with the cart table
    updated_inventoryDB <- dplyr::anti_join(inventoryTable$data, newDB, by = c("Nome"))
    updated_inventoryDB <- dplyr::bind_rows(updated_inventoryDB, newDB)
    updated_inventoryDB <- dplyr::arrange(updated_inventoryDB)
    inventoryTable$data <- updated_inventoryDB
    
    saveData(data = inventoryTable$data,
             filepath = inventoryTablePath) # Saves the updates
    
  }
  
  bt$data[row, clmn] <- input$buyTable_cell_edit$value # Edit and updates the selected cell
  
  saveData(data = bt$data,
           filepath = tablePath) # Saves the updates
  
}

### Remove rows ----
# Remove from inventory
remove_inventory <- function(input, it, tablePath){
  if (!is.null(input$inventoryTable_rows_selected)) {
    it$data <- it$data[-as.numeric(input$inventoryTable_rows_selected),] # Remove the selected row from the table
    saveData(data = it$data,
             filepath = tablePath) # Saves the updates
  }
}

# Remove from buy
remove_buy <- function(input, bt, tablePath, inventoryTable, inventoryTablePath){
  if (!is.null(input$buyTable_rows_selected)) {
    
    row <- input$buyTable_rows_selected # Check the row number of the selected row
    product <- bt$data$Nome[[row]] # Get the product name
    amount <- bt$data$Quantidade[[row]] # Get the amount of the product
    
    # Find the products and remove the amounts
    newDB <- inventoryTable$data %>%
      filter(Nome == product) %>%
      mutate(Quantidade = Quantidade - as.numeric(amount)) 
    
    # Combine the inventory table with the cart table
    updated_inventoryDB <- dplyr::anti_join(inventoryTable$data, newDB, by = c("Nome"))
    updated_inventoryDB <- dplyr::bind_rows(updated_inventoryDB, newDB)
    updated_inventoryDB <- dplyr::arrange(updated_inventoryDB)
    inventoryTable$data <- updated_inventoryDB
    
    saveData(data = inventoryTable$data,
             filepath = inventoryTablePath) # Saves the updates
    
    bt$data <- bt$data[-as.numeric(input$buyTable_rows_selected),] # Remove the selected row from the table
    saveData(data = bt$data,
             filepath = tablePath) # Saves the updates
  }
}

### Add data ----
# Add to inventory
add_inventory <- function(database, name, amount, category, sellValue, filePath){
  
  database$data[nrow(database$data)+1,] <- c(name,
                                             as.numeric(amount),
                                             category,
                                             as.numeric(sellValue)) # Add to the data the input data in the last row
  
  saveData(data = database$data,
           filepath = filePath) # Save the updates
  
  removeModal() # Close the modal
  
}
# Add to buy
add_buy <- function(database, name, amount, buyValue, sellValue, noteNumber, seller, buyDate, category, filePath, inventoryDB, inventoryDBPath){
  
  database$data[nrow(database$data)+1,] <- c(name,
                                             as.numeric(amount),
                                             as.numeric(buyValue),
                                             as.numeric(sellValue),
                                             format(as.Date(buyDate), "%Y-%m-%d"),
                                             category,
                                             noteNumber,
                                             seller) # Add to the data the input data in the last row
  saveData(data = database$data,
           filepath = filePath) # Save the updates

  row_index <- which(inventoryDB$data$Nome == name) # Get the row number of the product add which was already in the database

  # If the product was already in the inventory database, add the bought amount to the inventory, otherwise, add the new product to the inventory
  if (length(row_index) > 0) {
    inventoryDB$data$Quantidade[row_index] <- inventoryDB$data$Quantidade[row_index] + as.numeric(amount)
  } else {
    inventoryDB$data[nrow(inventoryDB$data)+1,] <- c(name,
                                                     as.numeric(amount),
                                                     category,
                                                     as.numeric(sellValue))
  }

  saveData(data = inventoryDB$data,
           filepath = inventoryDBPath) # Save the updates

  removeModal() # Close the modal
}

## Save edits to Data ----

saveData <- function(data, filepath) {
  write.csv(data, filepath, row.names = FALSE)
}