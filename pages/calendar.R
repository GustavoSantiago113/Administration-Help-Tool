# Libraries -----
library(shiny)
library(shinydashboard)
library(toastui)
library(shinyDatetimePickers)
library(colourpicker)

# UI -----

## Menu Item ----
menuCalendar <- function(){
  menuItem("Calendario", tabName = "calendar", icon = icon(name = NULL,
  style = "background: url('calendar.svg'); height: 20px; width: 20px; display: inline-block; background-repeat: no-repeat;")
  )
}

## Main Page ----
calendarMainPage <- function(tabName){
  
  tabItem(
    tabName = tabName,
    fluidRow(
      column(
        width = 12,
        align = "center",
        tags$h2("Calendario")
      )
    ),
    fluidRow(
      column(
        width = 12,
        align = "center",
        actionButton(
          inputId = "addEvent",
          label = "Novo",
          icon = icon("plus"),
          style = "background-color: #76bfac; color: white; font-family: 'Bahnschrift'; font-size: 20px; height: 50px; margin-left: 10px; margin-right: 10px;"
        ),
      )
    ),
    calendar(read.csv("data/calendario.csv"),
             view = "week",
             navigation = TRUE,
             defaultDate = Sys.Date(),
             useCreationPopup = TRUE,
             isReadOnly = FALSE,
             navOpts = navigation_options(
               fmt_date = "DD/MM/YYYY",
               sep_date = " - "
             )) %>%
      cal_week_options(
        startDayOfWeek = 1,
        workweek = FALSE,
        daynames = c("Dom", "Seg", "Ter", "Quar", "Quin", "Sex", "Sab")
      )
  )
  
}

## Add Modals ----
add_event_modal <- function(){
  modalDialog(
    title = "Adicionar Evento",
    size = "m",
    easyClose = TRUE,
    div(
      textInput(
        inputId = "calendarName",
        label = NULL,
        placeholder = "Nome do evento"
      ),
      textInput(
        inputId = "calendarDescription",
        label = NULL,
        placeholder = "Descricao do evento"
      ),
      textInput(
        inputId = "calendarLocation",
        label = NULL,
        placeholder = "Localizacao do evento"
      ),
      datetimeMaterialPickerInput(
        inputId = "calendarStart",
        label = "Data e hora de inicio",
        disablePast = TRUE,
        value = Sys.time()
      ),
      br(),
      datetimeMaterialPickerInput(
        inputId = "calendarEnd",
        label = "Data e hora de fim",
        value = Sys.time() + 60*60
      ),
      br(),
      fluidRow(
        column(
          width = 12,
          style = "z-index:1002;",
          colourpicker::colourInput("calendarColor",
                                    "Cor do cartao",
                                    "green",
                                    palette = "limited",
                                    showColour = "background")
        )
      ),
      selectInput(
        inputId = "calendarCategory",
        label = "Dia todo?",
        choices = c("Nao" = "time",
                    "Sim" = "allday")
      ),
      actionButton(inputId = "eventAdd", 
                   label   = "Criar evento", 
                   class   = "btn-success")
    ),
    footer = NULL
  ) %>% showModal()
}