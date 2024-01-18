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
    tabName = "inventoryControl",
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
        inputId = "nome",
        label = NULL,
        placeholder = "Nome"
      ),
      textInput(
        inputId = "quantidade",
        label = NULL,
        placeholder = "Quantidade"
      ),
      textInput(
        inputId = "valorCompra",
        label = NULL,
        placeholder = "Valor da Compra"
      ),
      textInput(
        inputId = "valorVenda",
        label = NULL,
        placeholder = "Valor da Venda"
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
        language = "pt-BR"
      ),
      selectInput(
        inputId = "categoria",
        label = NULL,
        choices = c("Racao", "Banho e Tosa", "Roupinha")
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
        inputId = "nome",
        label = NULL,
        placeholder = "Nome"
      ),
      textInput(
        inputId = "quantidade",
        label = NULL,
        placeholder = "Quantidade"
      ),
      selectInput(
        inputId = "categoria",
        label = NULL,
        choices = c("Racao", "Banho e Tosa", "Roupinha")
      ),
      actionButton(inputId = "inventoryAdd", 
                   label   = "Adicionar ao estoque", 
                   class   = "btn-success")
    ),
    footer = NULL
  ) %>% showModal()
  
}

# Server ----

## Render Tables ----
render_inventory_table <- function(data){
  DT::renderDataTable({
    datatable(data,
              editable = TRUE)
  })
}

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

addBuyFunction <- function(database, name, amount, buyValue, sellValue, noteNumber, seller, buyDate, category){
  
  d <- c(Nome = name,
         Quantidade = as.numeric(amount),
         Valor.de.Compra = as.numeric(buyValue),
         Valor.de.Venda = as.numeric(sellValue),
         Data = buyDate,
         Categoria = category,
         Nota = as.numeric(noteNumber),
         Fornecedor = seller)
  
  database$data <- rbind(database$data, d)
  
}

## Save edits to Data ----

saveData <- function(data, filepath) {
  write.csv(data, filepath, row.names = FALSE)
}