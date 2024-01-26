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
        )
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
        )
      ),
    )
  )
  
}