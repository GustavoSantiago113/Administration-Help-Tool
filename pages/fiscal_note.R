# Libraries ----
library(shinycssloaders)
library(rmarkdown)

# UI ----

## Menu Item ----
fiscalNote <- function(){
  
  menuItem("Nota Fiscal", tabName = "fiscalNote", icon = icon(name = NULL,
  style = "background: url('fiscalNote.svg');
  height: 20px;
  width: 20px;
  display: inline-block;
  background-repeat: no-repeat;")
  )
  
}

## Main Page ----
fiscal_note_mainPage <- function(tabName){
  
  tabItem(
    tabName = tabName,
    fluidRow(
      column(
        width = 12,
        align = "center",
        tags$h2("Nota Fiscal")
      )
    ),
    br(),
    fluidRow(
      column(
        width = 12,
        style = "display:flex",
        align = "center",
        tags$h3("Gerar nota fiscal para o ano de: "),
        numericInput(
          inputId = "anoDesejado",
          label = NULL,
          min = 2023,
          value = 2023
        ),
        actionButton(
          inputId = "gerar",
          label = "Gerar",
          style = "background-color: #76bfac; color: white; font-family: 'Bahnschrift'; font-size: 20px; height: 50px; margin-left: 10px; margin-right: 10px;"
        )
      )
    ),
    br(),
    br(),
    fluidRow(
      column(
        width = 12,
        align = "center",
        uiOutput("downloadReport"),
      )
    )
  )
  
}

# Server ----

## Generate report ----
generate_report <- function(output){
  output$downloadReport <- renderUI({
    withSpinner(uiOutput("downloadReportButton"), size = 0.5)
  })
  
  output$downloadReportButton <- renderUI({
    Sys.sleep(1.5)
    downloadButton("downloadPDF",
                   "Baixar",
                   style = "background-color: #76bfac; color: white; font-family: 'Bahnschrift'; font-size: 20px; height: 50px; margin-left: 10px; margin-right: 10px;"
                   )
  })
}