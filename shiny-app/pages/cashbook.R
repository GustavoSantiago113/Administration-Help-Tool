# Libraries -----
library(shiny)
library(shinydashboard)
library(lubridate)
library(tidyverse)
library(ggplot2)

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
cashbookMainPage <- function(tabName, sellingProductDB, sellingPlanDB, inventoryDB, buyingDB){
  
  sellingProductDB <- read.csv(sellingProductDB)
  sellingPlanDB <- read.csv(sellingPlanDB)
  inventoryDB <- read.csv(inventoryDB)
  buyingDB <- read.csv(buyingDB)
  
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
        valueBox(value = monthly_profit(sellingProductDB, sellingPlanDB),
                 subtitle = paste("Entrada do mes de", month(ymd(Sys.Date()),
                                                             label = TRUE,
                                                             locale = Sys.getlocale("LC_TIME"),
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
        valueBox(value = stock_value(inventoryDB),
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
        valueBox(value = savings(sellingProductDB, sellingPlanDB, buyingDB),
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
        valueBox(value = monthly_spents(buyingDB),
                 subtitle = paste("Gastos de", month(ymd(Sys.Date()), 
                                                    label = TRUE, 
                                                    locale = Sys.getlocale("LC_TIME"),
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
        uiOutput("donutPlot", height = "270px")
      ),
      column(
        width = 6,
        uiOutput("linePlot", height = "270px")
      )
    )
  )
  
}

# Server ----

## Monthly profit ----
monthly_profit <- function(sellingProductDB, sellingPlanDB){
  
  ano <- year(ymd(Sys.Date()))
  mes <- month(ymd(Sys.Date()))
  
  sellingProductDB$Data.Fim <- as.Date(sellingProductDB$Data.Fim)
  sellingPlanDB$Data.Fim <- as.Date(sellingPlanDB$Data.Fim) 

  filteredProductDB <- sellingProductDB %>%
    filter(year(Data.Fim) >= ano,
           month(Data.Fim) >= mes)
  
  totalProduct <- sum(as.numeric(filteredProductDB$Valor) * as.numeric(filteredProductDB$Quantidade)) / as.numeric(filteredProductDB$Parcelas)

  filteredPlanDB <- sellingPlanDB %>%
    filter(year(Data.Fim) >= ano,
           month(Data.Fim) >= mes)
  
  totalPlan <- sum(filteredPlanDB$Valor) / as.numeric(filteredPlanDB$Parcelas)
  
  return(totalProduct+totalPlan)

}

## Stock value ----
stock_value <- function(inventoryDB){
  
  stock_value <- sum(as.numeric(inventoryDB$Valor) * as.numeric(inventoryDB$Quantidade))
  
  return(stock_value)
  
}

## Savings ----
savings <- function(sellingProductDB, sellingPlanDB, buyingDB){
  
  totalProduct <- sum(as.numeric(sellingProductDB$Valor) * as.numeric(sellingProductDB$Quantidade))
  
  totalPlan <- sum(as.numeric(sellingPlanDB$Valor))
  
  totalCompra <- sum(as.numeric(buyingDB$Valor.de.Compra) * as.numeric(buyingDB$Quantidade))
  
  return(totalProduct+totalPlan-totalCompra)
  
}

## Monthly spent ----
monthly_spents <- function(buyingDB){

  year <- year(ymd(Sys.Date()))
  month <- month(ymd(Sys.Date()))
  
  buyingDB$Data <- as.Date(buyingDB$Data, format = "%Y-%m-%d")
  
  filteredbuyingDB <- buyingDB %>%
    filter(year(Data) == year,
           month(Data) == month)
  
  totalProduct <- sum(as.numeric(filteredbuyingDB$Valor.de.Compra) * as.numeric(filteredbuyingDB$Quantidade))
  
  return(totalProduct)
  
}

## Donuts graph ----
donuts_graph <- function(output, sellingProductDB){
  
  output$donutPlot <- renderUI({
    withSpinner(plotOutput("donutMaking"), size = 0.4)
  })
  
  output$donutMaking <- renderPlot({
    
    sellingProductDB$data$Quantidade <- as.numeric(sellingProductDB$data$Quantidade)
    
    data <- sellingProductDB$data %>%
      dplyr::select(Categoria, Quantidade)
    
    graph <- ggplot(data, aes(x = '', y = Quantidade, fill=Categoria)) +
      ggtitle("Qual produto vende mais?")+
      theme(axis.text = element_rect(fill = "#ecf3f1"),
            axis.title = element_rect(fill = "#ecf3f1"),
            axis.ticks = element_rect(fill = "#ecf3f1"),
            panel.grid  = element_rect(fill = "#ecf3f1"),
            legend.position="none",
            line = element_rect(fill = "#ecf3f1")
      )+
      geom_bar(stat = "identity", width = 1) +
      coord_polar(theta = "y") +
      scale_fill_brewer(palette = "Dark2") +
      theme_void()
    
    return(graph)
    
    
  })
  
  
}

## Line graph ----
line_graph <- function(output, sellingProductDB, sellingPlanDB, buyingDB){
  
  output$linePlot <- renderUI({
    withSpinner(plotOutput("lineMaking"), size = 0.4)
  })
  
  output$lineMaking <- renderPlot({
  
    sellingProductDB$data$Data <- as.Date(sellingProductDB$data$Data)
    
    sellingProductDB <- sellingProductDB$data %>%
      select(Quantidade,Valor,Data)
    
    # sellingPlanDB$data$Data <- as.Date(sellingPlanDB$data$Data_Inicio)
    # 
    # sellingPlanDB <- sellingPlanDB$data %>%
    #   select(Valor,Data)
    # 
    # sellingPlanDB$Quantidade <- 1
    
    buyingDB$data$Data <- as.Date(buyingDB$data$Data, format = "%Y-%m-%d")
    
    buyingDB <- buyingDB$data %>%
      select(Quantidade,Valor.de.Compra,Data) %>%
      rename(Valor = Valor.de.Compra)
    
    buyingDB$Valor <- as.numeric(buyingDB$Valor)
    buyingDB$Quantidade <- as.numeric(buyingDB$Quantidade)
    
    # sellingPlanDB$Valor <- as.numeric(sellingPlanDB$Valor)
    # sellingPlanDB$Quantidade <- as.numeric(sellingPlanDB$Quantidade)
    
    sellingProductDB$Valor <- as.numeric(sellingProductDB$Valor)
    sellingProductDB$Quantidade <- as.numeric(sellingProductDB$Quantidade)
    
    combined_data <- bind_rows(
      mutate(buyingDB, Type = "Compra"),
      #mutate(sellingPlanDB, Type = "Venda"),
      mutate(sellingProductDB, Type = "Venda")
    )
    
    combined_data <- combined_data %>%
      mutate(Month = format(Data, "%Y/%m"),
             MonthlyAmountValue = Quantidade * Valor) %>%
      group_by(Type, Month) %>%
      summarise(MeanAmountValue = mean(MonthlyAmountValue))
    
    # Create Line Graph
    graph <- ggplot(combined_data, aes(x = Month, y = MeanAmountValue, color = Type, group = Type)) +
      geom_line() +
      geom_point() +
      labs(title = "Compra x Venda",
           x = "Data",
           y = "Valor",
           color = "Tipo") +
      theme_minimal()
    
    return(graph)
  })
  
}