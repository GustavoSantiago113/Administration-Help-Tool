# Libraries -----
library(shiny)
library(shinydashboard)
library(lubridate)
library(tidyverse)

# UI -----

## Menu Item ----
menuCashBook <- function(){
  menuItem("Livro-Caixa",
           tabName = "cashbook",
           icon = icon(name = NULL,
                       style = "background: url('livrocaixa.svg');
           height: 20px;
           width: 20px;
           display: inline-block;
           background-repeat: no-repeat;")
  )
}

## Main Page ----
cashbookMainPage <- function(tabName){
  
  tabItem(
    tabName = tabName,
    fluidRow(
      column(
        width = 12,
        align = "center",
        tags$h2("Livro-Caixa")
      )
    ),
    fluidRow(
      column(
        width = 3
      ),
      column(
        width = 6,
        align = "center",
        valueBox(value = "R$ 1200",
                 subtitle = paste("Lucro de", month(ymd(Sys.Date()), 
                                                    label = TRUE, 
                                                    locale = "pt_BR",
                                                    abbr = FALSE)),
                 color = "navy",
                 width = NULL,
                 icon = icon(name = NULL,
                             style = "background: url('income.svg');
                             height: 70px;
                             width: 70px;
                             display: inline-block;
                             background-repeat: no-repeat;")
                 )
      ),
      column(
        width = 3
      )
    ),
    fluidRow(
      column(
        width = 4,
        align = "center",
        valueBox(value = "R$ 4620",
                 subtitle = "Valor em estoque",
                 color = "teal",
                 width = NULL,
                 icon = icon(name = NULL,
                             style = "background: url('stock.svg');
                             height: 60px;
                             width: 60px;
                             display: inline-block;
                             background-repeat: no-repeat;")
                 )
      ),
      column(
        width = 4,
        align = "center",
        valueBox(value = "R$ 15620",
                 subtitle = "Poupanca",
                 color = "teal",
                 width = NULL,
                 icon = icon(name = NULL,
                             style = "background: url('savings.svg');
                             height: 60px;
                             width: 60px;
                             display: inline-block;
                             background-repeat: no-repeat;")
                 )
      ),
      column(
        width = 4,
        align = "center",
        valueBox(value = "R$ 620",
                 subtitle = paste("Gastos de", month(ymd(Sys.Date()), 
                                                    label = TRUE, 
                                                    locale = "pt_BR",
                                                    abbr = FALSE)),
                 color = "teal",
                 width = NULL,
                 icon = icon(name = NULL,
                             style = "background: url('payment.svg');
                             height: 60px;
                             width: 60px;
                             display: inline-block;
                             background-repeat: no-repeat;")
                 )
      ),
    ),
    fluidRow(
      column(
        width = 6,
        tags$h2("Grafico de pizza")
      ),
      column(
        width = 6,
        tags$h2("Grafico de linha")
      )
    )
  )
  
}

# # Server ----
# 
# ## Monthly profit ----
# monthly_profit < function(sellingProductDB, sellingPlanDB){
#   
#   month <- month(ymd(Sys.Date()))
#   
#   filteredProductDB <- sellingProductDB %>%
#     filter()
#   
#   filteredPlanDB <- sellingPlanDB %>%
#     filter()
#   
# }
# 
# ## Stock value ----
# stock_value < function(stockDB){
#   
# }
# 
# ## Savings ----
# savings < function(sellingDB, buyingDB){
#   
# }
# 
# ## Monthly spents ----
# monthly_spents < function(buyingDB){
#   
# }
# 
# ## Donuts graph ----
# donuts_graph < function(sellingDB){
#   
# }
# 
# ## Line graph ----
# line_graph < function(sellingDB, buyingDB){
#   
# }