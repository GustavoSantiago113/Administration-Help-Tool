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
              outputId = "buyTable"
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
              outputId = "inventoryTable"
            )
          )
        )
      )
    )
  )
  
}

## Add Modals ----
add_buy_modal <- function(){
  modalDialog(
    title = "Adicionar Produto",
    size = "m",
    easyClose = TRUE,
    div(
      style="z-index:1002",
      textInput(
        inputId = "nomeCompra",
        label = NULL,
        placeholder = "Nome"
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
                   class   = "btn-success")
    ),
    footer = NULL
  ) %>% showModal()
}
add_inventory_modal <- function(){
  
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
                   class   = "btn-success")
    ),
    footer = NULL
  ) %>% showModal()
  
}

# Server ----

## Observe Events ----

### Edit Cells ----
edit_inventory <- function(input, it, tablePath){
  
  row  <- input$inventoryTable_cell_edit$row
  clmn <- input$inventoryTable_cell_edit$col
  it$data[row, clmn] <- input$inventoryTable_cell_edit$value
  saveData(data = it$data,
           filepath = tablePath)
  
}

edit_buy <- function(input, bt, tablePath){
  
  row  <- input$buyTable_cell_edit$row
  clmn <- input$buyTable_cell_edit$col
  bt$data[row, clmn] <- input$buyTable_cell_edit$value
  saveData(data = bt$data,
           filepath = tablePath)
  
}

### Remove rows ----

remove_inventory <- function(input, it, tablePath){
  if (!is.null(input$inventoryTable_rows_selected)) {
    it$data <- it$data[-as.numeric(input$inventoryTable_rows_selected),]
    saveData(data = it$data,
             filepath = tablePath)
  }
}

remove_buy <- function(input, bt, tablePath){
  if (!is.null(input$buyTable_rows_selected)) {
    bt$data <- bt$data[-as.numeric(input$buyTable_rows_selected),]
    saveData(data = bt$data,
             filepath = tablePath)
  }
}

### Add data ----

add_inventory <- function(database, name, amount, category, sellValue, filePath){
  
  database$data[nrow(database$data)+1,] <- c(name,
                                             as.numeric(amount),
                                             category,
                                             as.numeric(sellValue))
  
  saveData(data = database$data,
           filepath = filePath)
  
  removeModal()
  
}

add_buy <- function(database, name, amount, buyValue, sellValue, noteNumber, seller, buyDate, category, filePath, inventoryDB, inventoryDBPath){
  
  database$data[nrow(database$data)+1,] <- c(name,
                                             as.numeric(amount),
                                             as.numeric(buyValue),
                                             as.numeric(sellValue),
                                             format(as.Date(buyDate), "%Y-%m-%d"),
                                             category,
                                             noteNumber,
                                             seller)
  saveData(data = database$data,
           filepath = filePath)

  row_index <- which(inventoryDB$Nome == name)

  if (length(row_index) > 0) {
    inventoryDB$data$Quantidade[row_index] <- inventoryDB$data$Quantidade[row_index] + as.numeric(amount)
  } else {
    inventoryDB$data[nrow(inventoryDB$data)+1,] <- c(name,
                                                     as.numeric(amount),
                                                     category,
                                                     as.numeric(sellValue))
  }

  saveData(data = inventoryDB$data,
           filepath = inventoryDBPath)
  
  removeModal()
}

## Save edits to Data ----

saveData <- function(data, filepath) {
  write.csv(data, filepath, row.names = FALSE)
}