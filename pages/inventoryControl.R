# Libraries -----
library(shiny)
library(shinydashboard)

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
  
}

## Add Modal ----
addInventoryModal <- function(){
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
        inputId = "forcencedor",
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
      actionButton(inputId = "inventoryAdd", 
                   label   = "Adicionar ao estoque", 
                   class   = "btn-success")
    ),
    footer = NULL
  ) %>% showModal()
}

## Render Table ----
renderInventory <- function(database){
  
  
  
}

# Server ----

## Render Table ----
renderInventoryTable <- function(data){
  DT::renderDataTable({
    datatable(data,
              editable = TRUE)
  })
}

## Observe Events ----

observeEventsInventory <- function(it, input, inventoryPath){
  
  ### Edit Row ----
  observeEvent(input$inventoryTable_cell_edit, {
    row  <- input$inventoryTable_cell_edit$row
    clmn <- input$inventoryTable_cell_edit$col
    it$data[row, clmn] <- input$inventoryTable_cell_edit$value
    saveData(data = it$data,
             filepath = inventoryPath)
  })
  
  ### Delete row ----
  observeEvent(input$removeInventory,{
    if (!is.null(input$inventoryTable_rows_selected)) {
      it$data <- it$data[-as.numeric(input$inventoryTable_rows_selected),]
      saveData(data = it$data,
               filepath = inventoryPath)
    }
  })
  
  ### Add inventory button ----
  observeEvent(input$addInventory, {
    addInventoryModal()
  })
  
}

## Save Data ----
saveData <- function(data, filepath) {
  write.csv(data, filepath, row.names = FALSE)
}