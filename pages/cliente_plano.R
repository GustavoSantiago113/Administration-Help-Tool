# Libraries -----
library(shiny)
library(shinydashboard)
library(dplyr)

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