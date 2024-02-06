# Libraries -----
library(shiny)
library(shinydashboard)
library(dplyr)

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
}

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
}

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
}

# Server ----

## Add data ----
add_to_cart <- function(input, inventoryTable, cartTable){
  
  row <- input$inventoryForCart_rows_selected
  
  name <- inventoryTable$data[row, "Nome"]
  category <- inventoryTable$data[row, "Categoria"]
  value <- inventoryTable$data[row, "Valor"]
  
  cartTable$data[nrow(cartTable$data)+1,] <- c(name,1,category,value)
}

add_to_sell_table <- function(sellProductTable, cartTable, client, sellProductTablePath, inventoryTable){
  
  sellProductTable$data <- rbind(cartTable$data, sellProductTable$data)
  
  sellProductTable$data$Data <- as.character(Sys.Date())
  sellProductTable$data$Cliente <- client
  
  saveData(data = sellProductTable$data,
           filepath = sellProductTablePath)
  
  avector <- dplyr::pull(cartTable$data, Nome)

  inventoryTable$data <- inventoryTable$data %>%
    filter(!Nome %in% avector) %>%
  
  removeModal()
  
}

### Edit Cells ----
edit_cart <- function(input, it){
  
  row  <- input$cart_cell_edit$row
  clmn <- input$cart_cell_edit$col
  it$data[row, clmn] <- input$cart_cell_edit$value
  
}

edit_sells_table <- function(input, sellsProductTable, tablePath){
  
  row  <- input$sellsTable_cell_edit$row
  clmn <- input$sellsTable_cell_edit$col
  sellsProductTable$data[row, clmn] <- input$sellsTable_cell_edit$value
  saveData(data = sellsProductTable$data,
           filepath = tablePath)
  
}

### Remove rows ----

remove_from_cart <- function(input, it){
  if (!is.null(input$cart_rows_selected)) {
    it$data <- it$data[-as.numeric(input$cart_rows_selected),]
  }
}

remove_from_sells <- function(input, it, tablePath){
  if (!is.null(input$sellsTable_rows_selected)) {
    it$data <- it$data[-as.numeric(input$sellsTable_rows_selected),]
    saveData(data = it$data,
             filepath = tablePath)
  }
}



## Save edits to Data ----

saveData <- function(data, filepath) {
  write.csv(data, filepath, row.names = FALSE)
}