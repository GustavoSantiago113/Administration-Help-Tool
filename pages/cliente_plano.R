# Libraries -----
library(shiny)
library(shinydashboard)
library(tidyverse)
library(lubridate)
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
edit_client <- function(input, ct, tablePath, calendarDB, calendarFilePath){
  
  row  <- input$clientsTable_cell_edit$row
  clmn <- input$clientsTable_cell_edit$col
  
  if(clmn == 3 || clmn == 4 || clmn == 6){
    ID <- ct$data[["clienteId"]][[row]]
    columnName <- names(ct$data[clmn])
    if(columnName == "Aniversario_Animal"){
      rowValue = "Aniversario do pet"
    }
    if(columnName == "Aniversario_Dono"){
      rowValue = "Aniversario do tutor"
    }
    if(columnName == "Data_Inicio"){
      rowValue = "Aniversario de fidelizacao"
    }
    
    date_obj <- dmy(input$clientsTable_cell_edit$value)
    
    date_list <- lapply(seq(0, 10), function(i) {
      new_date <- date_obj + years(i)
      formatted_date <- format(new_date, "%Y-%m-%d")
      return(formatted_date)
    })
    
    newDB <- calendarDB$data %>%
      filter(idCliente == as.character(ID),
             body == rowValue)
    
    newDB$start <- unlist(date_list)
    newDB$end <- unlist(date_list)
    
    updated_calendarDB <- dplyr::anti_join(calendarDB$data, newDB, by = c("idCliente"))
    
    updated_calendarDB <- dplyr::bind_rows(updated_calendarDB, newDB)
    
    updated_calendarDB <- dplyr::arrange(updated_calendarDB, calendarId)
    
    calendarDB$data <- updated_calendarDB
    
    saveData(data = calendarDB$data,
             filepath = calendarFilePath)
  }
  
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

remove_client <- function(input, ct, tablePath, calendarDB, calendarFilePath){
  if (!is.null(input$clientsTable_rows_selected)) {
    
    row <- input$clientsTable_rows_selected
    ID <- ct$data[["clienteId"]][[row]]
    
    newDB <- calendarDB$data %>%
      filter(idCliente != as.character(ID))
    
    ct$data <- ct$data[-as.numeric(input$clientsTable_rows_selected),]
    saveData(data = ct$data,
             filepath = tablePath)
    saveData(data = newDB,
             filepath = calendarFilePath)
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
  
  if (length(database$data[["clienteId"]]) > 0) {
    # Get the last value of the specified column
    last_value <- tail(database$data[["clienteId"]], 1)
    
    # Create a new variable based on the condition
    identifier <- ifelse(is.na(last_value), 0, last_value)
    
    identity <- identifier + 1
  } else {
    # If the column is empty, set id to 1 (or any other initial value)
    identity <- 1
  }
  
  database$data[nrow(database$data)+1,] <- c(nomeTutor,
                                             nomeAnimal,
                                             format(as.Date(aniversarioAnimal), "%d/%m/%Y"),
                                             format(as.Date(aniversarioCliente), "%d/%m/%Y"),
                                             planoCliente,
                                             format(as.Date(clienteDesde), "%d/%m/%Y"),
                                             porteAnimal,
                                             contatoCliente,
                                             0,
                                             identity)
  
  saveData(data = database$data,
           filepath = filePath)
  
  additional_years <- 10
  
  for (i in 0:additional_years) {

    new_date_Pet <- as.Date(as.character(aniversarioAnimal), "%Y-%m-%d") + years(i)
    add_calendar(filePath = calendarFilePath,
                 database = calendarDB,
                 calendarName = paste("Aniversario de", nomeAnimal),
                 calendarDescription = paste("Aniversario do pet, contato:", contatoCliente),
                 calendarStart = new_date_Pet,
                 calendarEnd = new_date_Pet,
                 calendarCategory = "TRUE",
                 calendarLocation = "PetShop",
                 calendarColor = "#a83291",
                 recurrency = "Todo ano",
                 clientId = identity)
    
    new_date_Cliente <- as.Date(as.character(aniversarioCliente), "%Y-%m-%d") + years(i)
    add_calendar(filePath = calendarFilePath,
                 database = calendarDB,
                 calendarName = paste("Aniversario de", nomeTutor),
                 calendarDescription = paste("Aniversario de Tutor, contato:", contatoCliente),
                 calendarStart = new_date_Cliente,
                 calendarEnd = new_date_Cliente,
                 calendarCategory = "TRUE",
                 calendarLocation = "PetShop",
                 calendarColor = "#a83291",
                 recurrency = "Todo ano",
                 clientId = identity)
    
    new_date_Fidel <- as.Date(as.character(clienteDesde), "%Y-%m-%d") + years(i)
    add_calendar(filePath = calendarFilePath,
                 database = calendarDB,
                 calendarName = paste("Aniversario de cliente de", nomeAnimal),
                 calendarDescription = paste("Aniversario de Fidelizacao, contato:", contatoCliente),
                 calendarStart = new_date_Fidel,
                 calendarEnd = new_date_Fidel,
                 calendarCategory = "TRUE",
                 calendarLocation = "PetShop",
                 calendarColor = "#a83291",
                 recurrency = "Todo ano",
                 clientId = identity)
  }
  
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