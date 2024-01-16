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
        tags$div(
          class = "controleEstoqueDiv",
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
          actionButton(
            inputId = "editInventory",
            label = "Editar",
            icon = icon("edit"),
            style = "background-color: #76bfac; color: white; font-family: 'Bahnschrift'; font-size: 20px; height: 50px; margin-left: 10px; margin-right: 10px;"
          ),
          searchInput(
            inputId = "searchInventory",
            label = "",
            placeholder = "Pesquisar...",
            btnSearch = icon("magnifying-glass"),
            width = "300px",
          )
        ),
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

## Remove modal ----
removeInventoryModal <- function(){
  modalDialog(
    title = "Remover Produto",
    size = "m",
    easyClose = TRUE,
    div(
      searchInput(
        inputId = "searchDeleteInventory",
        label = "",
        placeholder = "Pesquisar...",
        btnSearch = icon("magnifying-glass"),
        width = "300px",
      ),
      lapply(1:10, function(i) {
        listItems(i, paste("Product", i))
      }),
      actionButton(inputId = "inventoryAdd", 
                   label   = "Remover produtos do estoque", 
                   class   = "btn-danger")
    ),
    
    footer = NULL
  ) %>% showModal()
}

### List of Itens to exclude ----
listItems <- function(itemNumber, productName){
  tags$div(
    class = "listItems",
    checkboxInput(
      inputId = paste("item", str(itemNumber)),
      label = productName
    ),
    numericInput(
      inputId = paste("item", str(itemNumber), "amount"),
      label = NULL,
      value = 0,
      width = 80,
      min = 0
    )
  )
}
