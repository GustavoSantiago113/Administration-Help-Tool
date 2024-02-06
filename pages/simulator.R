# Libraries -----
library(shiny)
library(shinydashboard)

# UI -----

## Menu Item ----
menuSimulator <- function(){
  menuItem("Simulador",
           tabName = "simulator",
           icon = icon(name = NULL,
           style = "background: url('simulator.svg');
           height: 20px;
           width: 20px;
           display: inline-block;
           background-repeat: no-repeat;")
  )
}

## Main Page ----
simulatorMainPage <- function(tabName){
  
  tabItem(
    tabName = tabName,
    fluidRow(
      column(
        width = 12,
        align = "center",
        tags$h2("Simulador")
      )
    ),
    fluidRow(
      column(
        width = 1
      ),
      column(
        width = 4,
        class = "simulador",
        tags$h3("Servico"),
        br(),
        numericInput(
          inputId = "maodeobraSimulador",
          label = "Mao de obra:",
          value = 0,
          min = 0,
        ),
        numericInput(
          inputId = "impostoSimulador",
          label = "Imposto (%):",
          value = 0,
          min = 0,
        ),
        numericInput(
          inputId = "lucroSimulador",
          label = "Lucro desejado (%):",
          value = 0,
          min = 0,
        ),
        tags$h3("Produtos"),
        uiOutput("extraInputs"),
        actionButton(inputId = "inputsAdd", 
                     label   = "Adicionar produto", 
                     class   = "btn-success"),
        actionButton(inputId = "inputsRemove", 
                     label   = "Remover produto", 
                     class   = "btn-danger"),
        br(),
        uiOutput("simuladorServico")
      ),
      column(
        width = 4,
        class = "simulador",
        br(),
        numericInput(
          inputId = "novoCustoSimulador",
          label = "Novo custo do produto:",
          value = 0,
          min = 0,
        ),
        numericInput(
          inputId = "impostoSimulador1",
          label = "Imposto (%):",
          value = 0,
          min = 0,
        ),
        numericInput(
          inputId = "lucroSimulador1",
          label = "Lucro desejado (%):",
          value = 0,
          min = 0,
        ),
        uiOutput("simuladorProduto")
      ),
    )
  )
  
}

# Server ----

## Buttons ----

### Add ----
add_Input <- function(counter){
  counter$n <- counter$n + 1
}

### Remove ----
remove_Input <- function(counter){
  if (counter$n > 0) counter$n <- counter$n - 1
}

## Dynamic inputs ----
dynamic_Inputs <- function(counter, AllInputs){
  
  n <- counter$n # Map the number of desired inputs
  
  if (n > 0) {
    isolate({
      lapply(seq_len(n), function(i) {
        tags$div(
          tags$h4(paste("Produto", i)),
          numericInput(inputId = paste0("precoLista", i),
                       label = "Preco por quantidade",
                       min = 0,
                       value = AllInputs[[paste0("precoLista", i)]]),
          numericInput(inputId = paste0("quantidadeLista", i),
                       label = "Quantidade",
                       min = 0,
                       value = AllInputs[[paste0("quantidadeLista", i)]])
        )
      })
    })
  } # Create the number inputs boxes
  
}

## Dynamic outputs -----
soma_preco <- function(counter, input, maodeobraSimulador, lucroSimulador, impostoSimulador){
  
  inputValues <- lapply(seq_len(counter$n), function(i) {
    
    precoLista = input[[paste0("precoLista", i)]]
    quantidadeLista = input[[paste0("quantidadeLista", i)]]
    
    list(
      precoCusto <- precoLista*quantidadeLista
    )
  }) # Create a list to store the input values
  
  total_cost <- sum(unlist(inputValues)) + maodeobraSimulador # Calculate cost
  lucro <- total_cost + (total_cost * (lucroSimulador/100)) # Calculate profit
  preco_final <- lucro + (lucro * (impostoSimulador/100)) # Include taxes
  return(preco_final)
  
}
output_preco <- function(preco_final){
  if(is.na(preco_final)){
    tags$p(
      class = "simuladorOutput",
      "Não há dados suficientes para calcular o valor de venda."
    )
  }
  else{
    tags$p(
      class = "simuladorOutput",
      paste("Valor de venda:", preco_final)
    )
  }
}

simulador_produto <- function(novoCustoSimulador, impostoSimulador1, lucroSimulador1){
  
  valorDeVenda <- ( novoCustoSimulador + (novoCustoSimulador * (lucroSimulador1/100) ) ) # Calculate selling price
  
  valorDeVenda_imposto <- valorDeVenda + ( valorDeVenda * (impostoSimulador1/100) ) # Include taxes
  
  tags$p(
    class = "simuladorOutput",
    paste("Valor de venda:", valorDeVenda_imposto)
  )
}
