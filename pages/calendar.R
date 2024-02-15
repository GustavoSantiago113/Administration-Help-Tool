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
    calendarOutput("my_calendar")
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
        choices = c("Nao" = "FALSE",
                    "Sim" = "TRUE")
      ),
      actionButton(inputId = "eventAdd", 
                   label   = "Criar evento", 
                   class   = "btn-success")
    ),
    footer = NULL
  ) %>% showModal()
}

# Server ----

## Render Calendar ----
render_calendar <- function(calendarData){
  cal <- toastui::calendar(calendarData,
                  view = "week",
                  navigation = TRUE,
                  defaultDate = Sys.Date(),
                  useCreationPopup = TRUE,
                  isReadOnly = FALSE,
                  navOpts = toastui::navigation_options(
                    fmt_date = "DD/MM/YYYY",
                    sep_date = " - ",
                    today_label = "Hoje"
                  )
                  ) %>%
    toastui::cal_week_options(
      startDayOfWeek = 1,
      workweek = FALSE,
      daynames = c("Dom", "Seg", "Ter", "Quar", "Quin", "Sex", "Sab")
    )
  return(cal)
}

## Observe Events ----

### Add event ----
add_calendar <- function(filePath, database, calendarName, calendarDescription, calendarStart, calendarEnd, calendarCategory, calendarLocation, calendarColor, recurrency, clientId) {
  
  if (length(database$data[["calendarId"]]) > 0) {
    # Get the last value of the specified column
    last_value <- as.numeric(tail(database$data[["calendarId"]], 1))
    
    # Create a new variable based on the condition
    identifier <- ifelse(is.na(last_value), 0, last_value)
    
    identity <- identifier + 1
    
  } else {
    # If the column is empty, set id to 1 (or any other initial value)
    identity <- 1
  }

  database$data[nrow(database$data)+1,] <- c(identity,
                                             calendarName,
                                             calendarDescription,
                                             as.character(calendarStart),
                                             as.character(calendarEnd),
                                             calendarCategory,
                                             calendarLocation,
                                             calendarColor,
                                             "#FFF",
                                             recurrency,
                                             clientId) # Add to the data the input data in the last row
  
  saveData(data = database$data,
           filepath = filePath) # Save the changes
  
  removeModal() # Close the modal
}

### Edit event ----
edit_calendar <- function(input, database, filePath){
  
  cal_proxy_update("my_calendar", input$my_calendar_update) # Update data in the widget
  
  values <- input$my_calendar_update$schedule # Get what was changed
  id <- values$calendarId # Get the Id of what was changed
  row_to_update <- which(database$data$calendarId == id) # Get the row number of the event changed
  
  changes <- input$my_calendar_update$changes # Create a variable to store what was changed
  
  # Verify if the event exists, if so, change the value
  if (length(row_to_update) > 0) {
    
    database$data[row_to_update, "title"] <- ifelse(exists("title", changes), changes$title, database$data[row_to_update, "title"])
    database$data[row_to_update, "start"] <- ifelse(exists("start", changes), changes$start, database$data[row_to_update, "start"])
    database$data[row_to_update, "end"] <- ifelse(exists("end", changes), changes$end, database$data[row_to_update, "end"])
    database$data[row_to_update, "isAllday"] <- ifelse(exists("isAllday", changes), changes$isAllday, database$data[row_to_update, "isAllday"])
    database$data[row_to_update, "location"] <- ifelse(exists("location", changes), changes$location, database$data[row_to_update, "location"])
    
    saveData(database$data, filePath) # Save the changes
  } else {
    print("Row not found.")
  }
}

### Remove event ----
remove_calendar <- function(input, database, filePath){
  cal_proxy_delete("my_calendar", input$my_calendar_delete) # Update the removal in the widget
  
  values <- input$my_calendar_delete # Get what was deleted
  id <- values$calendarId # Get the id of the deleted event
  row_to_delete <- which(database$data$calendarId == id) # Delete the row of deleted event
  database$data <- database$data[-row_to_delete,] # Delete the row from the database

  saveData(database$data, filePath) # Save the changes
}

## Save edits to Data ----

saveData <- function(data, filepath) {
  write.csv(data, filepath, row.names = FALSE)
}