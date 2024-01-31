# Libraries -----
library(shiny)
library(shinydashboard)
library(dplyr)
source("pages/calendar.R")

# UI -----

## Menu Item ----
clientePlano <- function(){
  menuItem("Clientes & Planos", tabName = "clientePlano", icon = icon(name = NULL,
  style = "background: url('client_plan.svg');
  height: 20px;
  width: 20px;
  display: inline-block;
  background-repeat: no-repeat;")
  )
}

## Main Page ----
clients_and_plans_MainPage <- function(tabName){
  
  tabItem(
    tabName = tabName,
    tabsetPanel(
      tabPanel(
        title = "Clientes",
        fluidRow(
          column(
            width = 12,
            align = "center",
            tags$h2("Clientes")
          )
        ),
        fluidRow(
          column(
            class = "mainInventoryControl",
            width = 12,
            align = "center",
            actionButton(
              inputId = "addClients",
              label = "Novo",
              icon = icon("plus"),
              style = "background-color: #76bfac; color: white; font-family: 'Bahnschrift'; font-size: 20px; height: 50px; margin-left: 10px; margin-right: 10px;"
            ),
            actionButton(
              inputId = "removeClients",
              label = "Excluir",
              icon = icon("trash"),
              style = "background-color: red; color: white; font-family: 'Bahnschrift'; font-size: 20px; height: 50px; margin-left: 10px; margin-right: 10px;"
            ),
            br(),
            DTOutput(
              outputId = "clientsTable"
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
            tags$h2("Planos")
          )
        ),
        fluidRow(
          column(
            class = "mainInventoryControl",
            width = 12,
            align = "center",
            actionButton(
              inputId = "addPlans",
              label = "Novo",
              icon = icon("plus"),
              style = "background-color: #76bfac; color: white; font-family: 'Bahnschrift'; font-size: 20px; height: 50px; margin-left: 10px; margin-right: 10px;"
            ),
            actionButton(
              inputId = "removePlans",
              label = "Excluir",
              icon = icon("trash"),
              style = "background-color: red; color: white; font-family: 'Bahnschrift'; font-size: 20px; height: 50px; margin-left: 10px; margin-right: 10px;"
            ),
            br(),
            DTOutput(
              outputId = "plansTable"
            )
          )
        )
      )
    )
  )
  
}

## Add Modals ----
add_client_modal <- function(){
  modalDialog(
    title = "Adicionar Cliente",
    size = "m",
    easyClose = TRUE,
    div(
      style="z-index:1002",
      textInput(
        inputId = "nomeAnimal",
        label = NULL,
        placeholder = "Nome"
      ),
      textInput(
        inputId = "nomeTutor",
        label = NULL,
        placeholder = "Nome do tutor"
      ),
      dateInput(
        inputId = "aniversarioAnimal",
        label = "Aniversario",
        format = "dd-mm-yyyy",
        language = "pt-BR"
      ),
      dateInput(
        inputId = "aniversarioCliente",
        label = "Aniversario do Tutor",
        format = "dd-mm-yyyy",
        language = "pt-BR"
      ),
      dateInput(
        inputId = "clienteDesde",
        label = "Cliente desde...",
        format = "dd-mm-yyyy",
        language = "pt-BR"
      ),
      textInput(
        inputId = "contatoCliente",
        label = NULL,
        placeholder = "Contato"
      ),
      textInput(
        inputId = "planoCliente",
        label = NULL,
        placeholder = "Plano escolhido"
      ),
      selectInput(
        inputId = "porteAnimal",
        label = NULL,
        choices = c("Pequeno" = "Pequeno",
                    "Medio" = "Medio",
                    "Grande" = "Grande")
      ),
      actionButton(inputId = "clientAdd", 
                   label   = "Adicionar a clientes", 
                   class   = "btn-success")
    ),
    footer = NULL
  ) %>% showModal()
}

add_plan_modal <- function(){
  modalDialog(
    title = "Adicionar Plano",
    size = "m",
    easyClose = TRUE,
    div(
      style="z-index:1002",
      textInput(
        inputId = "nomePlano",
        label = NULL,
        placeholder = "Nome"
      ),
      numericInput(
        inputId = "valorVendaPlano",
        label = "Valor de venda",
        min = 0,
        value = 0
      ),
      numericInput(
        inputId = "custoPlano",
        label = "Custo do servico",
        min = 0,
        value = 0
      ),
      numericInput(
        inputId = "numeroDeVisitas",
        label = "Numero de Visitas",
        min = 0,
        value = 0
      ),
      textInput(
        inputId = "descicaoPlano",
        label = NULL,
        placeholder = "Descricao do plano"
      ),
      actionButton(inputId = "planAdd", 
                   label   = "Adicionar a planos", 
                   class   = "btn-success")
    ),
    footer = NULL
  ) %>% showModal()
}

# Server ----

## Observe Events ----

### Edit Cells ----
edit_client <- function(input, ct, tablePath){
  
  row  <- input$clientsTable_cell_edit$row
  clmn <- input$clientsTable_cell_edit$col
  ct$data[row, clmn] <- input$clientsTable_cell_edit$value
  saveData(data = ct$data,
           filepath = tablePath)
  
}

edit_plan <- function(input, pt, tablePath){
  
  row  <- input$plansTable_cell_edit$row
  clmn <- input$plansTable_cell_edit$col
  pt$data[row, clmn] <- input$plansTable_cell_edit$value
  saveData(data = pt$data,
           filepath = tablePath)
  
}

### Remove rows ----

remove_client <- function(input, ct, tablePath){
  if (!is.null(input$clientsTable_rows_selected)) {
    ct$data <- ct$data[-as.numeric(input$clientsTable_rows_selected),]
    saveData(data = ct$data,
             filepath = tablePath)
  }
}

remove_plan <- function(input, pt, tablePath){
  if (!is.null(input$plansTable_rows_selected)) {
    pt$data <- pt$data[-as.numeric(input$plansTable_rows_selected),]
    saveData(data = pt$data,
             filepath = tablePath)
  }
}

### Add data ----

add_clients <- function(database, filePath, nomeAnimal, nomeTutor, aniversarioAnimal, aniversarioCliente, clienteDesde, contatoCliente, porteAnimal, planoCliente, calendarDB, calendarFilePath){
  
  d <- c(Dono = nomeTutor,
         Animal = nomeAnimal,
         Aniversario_Animal = format(as.Date(aniversarioAnimal), "%d/%m/%Y"),
         Aniversario_Dono = format(as.Date(aniversarioCliente), "%d/%m/%Y"),
         Plano = planoCliente,
         Data_Inicio = format(as.Date(clienteDesde), "%d/%m/%Y"),
         Porte = porteAnimal,
         Contato = contatoCliente,
         Visitas_Restantes = 0)
  
  database$data <- rbind(d, database$data)
  
  saveData(data = database$data,
           filepath = filePath)
  
  add_calendar(filePath = calendarFilePath,
               database = calendarDB,
               calendarName = paste("Aniversario do", nomeAnimal),
               calendarDescription = "Aniversario do pet",
               calendarStart = format(as.Date(aniversarioAnimal), "%Y-%m-%d"),
               calendarEnd = format(as.Date(aniversarioAnimal), "%Y-%m-%d"),
               calendarCategory = "TRUE",
               calendarLocation = "PetShop",
               calendarColor = "#a83291",
               recurrency = "Every year")
  add_calendar(filePath = calendarFilePath,
               database = calendarDB,
               calendarName = paste("Aniversario do", nomeTutor),
               calendarDescription = "Aniversario do tutor",
               calendarStart = format(as.Date(aniversarioCliente), "%Y-%m-%d"),
               calendarEnd = format(as.Date(aniversarioCliente), "%Y-%m-%d"),
               calendarCategory = "TRUE",
               calendarLocation = "PetShop",
               calendarColor = "#a83291",
               recurrency = "Every year")
  add_calendar(filePath = calendarFilePath,
               database = calendarDB,
               calendarName = paste("Aniversario de cliente do", nomeAnimal),
               calendarDescription = "Aniversario de fidelizacao",
               calendarStart = format(as.Date(clienteDesde), "%Y-%m-%d"),
               calendarEnd = format(as.Date(clienteDesde), "%Y-%m-%d"),
               calendarCategory = "TRUE",
               calendarLocation = "PetShop",
               calendarColor = "#a83291",
               recurrency = "Every year")
  
  removeModal()
  
}

add_plans <- function(database, filePath, nomePlano, valorVendaPlano, custoPlano, numeroDeVisitas, descicaoPlano){
  
  d <- c(Nome = nomePlano,
         Venda = valorVendaPlano,
         Custo = custoPlano,
         Visitas = numeroDeVisitas,
         Descricao = descicaoPlano)
  
  database$data <- rbind(d, database$data)
  
  saveData(data = database$data,
           filepath = filePath)
  
  removeModal()
}

## Save edits to Data ----

saveData <- function(data, filepath) {
  write.csv(data, filepath, row.names = FALSE)
}