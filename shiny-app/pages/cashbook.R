# Libraries -----
library(shiny)
library(shinydashboard)
library(lubridate)
library(tidyverse)
library(ggplot2)
library(plotly)
library(zoo)

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
  growth <- growthing_rate(sellingProductDB, sellingPlanDB)
  
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
        valueBox(value = round(monthly_profit(sellingProductDB, sellingPlanDB),2),
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
        if(growth >= 0){
          valueBox(value = paste(round(growth, 1), "%", sep = " "),
                   subtitle = "Comparacao mes anterior",
                   color = "teal",
                   width = NULL,
                   icon = icon("arrow-up"))
        }else{
          valueBox(value = paste(round(growth, 1), "%", sep = " "),
                   subtitle = "Comparacao mes anterior",
                   color = "teal",
                   width = NULL,
                   icon = icon("arrow-down"))
        }
        
      ),
      column(
        width = 4,
        align = "center",
        valueBox(value = round(savings(sellingProductDB, sellingPlanDB, buyingDB),2),
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
        valueBox(value = round(monthly_spents(buyingDB),2),
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
  
  totalProduct <- sum(as.numeric(filteredProductDB$Valor) * as.numeric(filteredProductDB$Quantidade) / as.numeric(filteredProductDB$Parcelas))

  filteredPlanDB <- sellingPlanDB %>%
    filter(year(Data.Fim) >= ano,
           month(Data.Fim) >= mes)
  
  totalPlan <- sum(as.numeric(filteredPlanDB$Valor) / as.numeric(filteredPlanDB$Parcelas))
  
  return(totalProduct+totalPlan)

}

## Stock value ----
growthing_rate <- function(sellingProductDB, sellingPlanDB){
  
  sellingProductDB$Data.Fim <- as.Date(sellingProductDB$Data.Fim)
  sellingPlanDB$Data.Fim <- as.Date(sellingPlanDB$Data.Fim)
  sellingPlanDB$Data_Inicio <- as.Date(sellingPlanDB$Data_Inicio)
  sellingProductDB$Data <- as.Date(sellingProductDB$Data)
  
  # This month
  ano <- year(ymd(Sys.Date()))
  mes <- month(ymd(Sys.Date()))
  
  filteredProductDB <- sellingProductDB %>%
    filter(year(Data.Fim) >= ano,
           month(Data.Fim) >= mes)
  
  totalProduct <- sum(as.numeric(filteredProductDB$Valor) * as.numeric(filteredProductDB$Quantidade) / as.numeric(filteredProductDB$Parcelas))
  
  filteredPlanDB <- sellingPlanDB %>%
    filter(year(Data.Fim) >= ano,
           month(Data.Fim) >= mes)
  
  totalPlan <- sum(as.numeric(filteredPlanDB$Valor) / as.numeric(filteredPlanDB$Parcelas))
  
  totalSellsThisMonth <- totalPlan + totalProduct
  
  # Last Month
  lastMonth <- as.Date(Sys.Date()) - months(1)
  lastMonth <- format(as.Date(lastMonth), "%Y-%m")
  
  filteredProductDBLM <- sellingProductDB %>%
    filter(format(as.Date(Data), "%Y-%m") == lastMonth)
  
  totalProductLM <- sum(as.numeric(filteredProductDBLM$Valor) * as.numeric(filteredProductDBLM$Quantidade) / as.numeric(filteredProductDBLM$Parcelas))
  
  filteredPlanDBLM <- sellingPlanDB %>%
    filter(format(as.Date(Data_Inicio), "%Y-%m") == lastMonth)
  
  totalPlanLM <- sum(as.numeric(filteredPlanDBLM$Valor) / as.numeric(filteredPlanDBLM$Parcelas))
  
  totalSellsLastMonth <- totalPlanLM + totalProductLM
  
  # Calculation
  growth <- (totalSellsThisMonth - totalSellsLastMonth) / totalSellsLastMonth * 100
  
  return(growth)
  
}

## Savings ----
savings <- function(sellingProductDB, sellingPlanDB, buyingDB){
  
  ano <- year(ymd(Sys.Date()))
  mes <- month(ymd(Sys.Date()))
  
  filteredProductDB <- sellingProductDB %>%
    filter(year(Data.Fim) >= ano,
           month(Data.Fim) >= mes)
  
  totalProduct <- sum(as.numeric(filteredProductDB$Valor) * as.numeric(filteredProductDB$Quantidade) / as.numeric(filteredProductDB$Parcelas))
  
  filteredPlanDB <- sellingPlanDB %>%
    filter(year(Data.Fim) >= ano,
           month(Data.Fim) >= mes)
  
  totalPlan <- sum(as.numeric(filteredPlanDB$Valor) / as.numeric(filteredPlanDB$Parcelas))
  
  filteredBuyingDB <- buyingDB %>%
    filter(year(Data.Fim) >= ano,
           month(Data.Fim) >= mes)
  
  totalCompra <- sum(as.numeric(filteredBuyingDB$Valor.de.Compra) * as.numeric(filteredBuyingDB$Quantidade) / as.numeric(filteredBuyingDB$Parcelas))
  
  return(totalProduct+totalPlan-totalCompra)
  
}

## Monthly spent ----
monthly_spents <- function(buyingDB){

  year <- year(ymd(Sys.Date()))
  month <- month(ymd(Sys.Date()))
  
  buyingDB$Data <- as.Date(buyingDB$Data, format = "%Y-%m-%d")
  
  filteredBuyingDB <- buyingDB %>%
    filter(year(Data) >= year,
           month(Data) >= month)
  
  totalProduct <- sum(as.numeric(filteredBuyingDB$Valor.de.Compra) * as.numeric(filteredBuyingDB$Quantidade) / as.numeric(filteredBuyingDB$Parcelas))
  
  return(totalProduct)
  
}

## Donuts graph ----
donuts_graph <- function(output, sellingProductDB){
  
  output$donutPlot <- renderUI({
    withSpinner(plotlyOutput("donutMaking"), size = 0.4)
  })
  
  output$donutMaking <- renderPlotly({
    
    sellingProductDB$data$Quantidade <- as.numeric(sellingProductDB$data$Quantidade)
    
    data <- sellingProductDB$data %>%
      dplyr::select(Categoria, Quantidade)
    
    categoria_totals <- aggregate(Quantidade ~ Categoria, data = data, FUN = sum)
    
    plot <- plot_ly(labels = categoria_totals$Categoria, 
                    values = categoria_totals$Quantidade, 
                    type = 'pie', 
                    hole = 0.6) %>%
      layout(title = "Qual vende mais?",
             showlegend = TRUE,
             xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
             yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
             paper_bgcolor = "#ecf0f5",  # Background color of the entire plot area
             plot_bgcolor = "#ecf0f5")
    
    plot
    
  })
  
  
}

## Line graph ----
line_graph <- function(output, sellingProductDB, sellingPlanDB, buyingDB){
  
  output$linePlot <- renderUI({
    withSpinner(plotlyOutput("lineMaking"), size = 0.4)
  })
  
  output$lineMaking <- renderPlotly({
    
    # Product
    sellingProductDB$data$Data <- as.Date(sellingProductDB$data$Data)
    
    sellingProduct <- sellingProductDB$data %>%
      mutate(iD = row_number()) %>%
      group_by(iD) %>%
      slice(rep(row_number(), time=Parcelas)) %>%
      mutate(Data = Data + months(row_number() - 1),
             Valor = Valor / Parcelas * Quantidade) %>%
      ungroup()
    
    # Plan
    sellingPlanDB$data$Data <- as.Date(sellingPlanDB$data$Data_Inicio)
    
    sellingPlan <- sellingPlanDB$data %>%
      mutate(Quantidade = 1) %>%
      mutate(iD = row_number()) %>%
      group_by(iD) %>%
      slice(rep(row_number(), time=Parcelas)) %>%
      mutate(Data = Data + months(row_number() - 1),
             Valor = Valor / Parcelas * Quantidade) %>%
      ungroup()
    
    # Buying
    buyingDB$data$Data <- as.Date(buyingDB$data$Data)
    
    buying <- buyingDB$data %>%
      mutate(iD = row_number()) %>%
      group_by(iD) %>%
      slice(rep(row_number(), time=Parcelas)) %>%
      mutate(Data = Data + months(row_number() - 1),
             Valor = Valor.de.Compra / Parcelas * Quantidade) %>%
      ungroup()
    
    combined_data <- bind_rows(
      mutate(buying, Type = "Compra"),
      mutate(sellingPlan, Type = "Venda"),
      mutate(sellingProduct, Type = "Venda")
    )
    
    yearMonths <- c(as.Date(Sys.Date()) - months(5), as.Date(Sys.Date()) + months(5))
    Months <- as.Date(paste(yearMonths, "-01", sep=""))
    
    combined_data <- combined_data %>%
      mutate(Month = format(Data, "%Y-%m")) %>%
      group_by(Type, Month) %>%
      summarise(Valor = sum(Valor)) %>%
      filter(as.Date(paste(Month, "-01", sep="")) >= Months[1],
             as.Date(paste(Month, "-01", sep="")) <= Months[2])
    
    # Create Line Graph
    graph <- ggplot(combined_data, aes(x = Month, y = Valor, color = Type, group = Type)) +
      geom_line() +
      geom_point() +
      labs(title = "Compra x Venda",
           x = "Data",
           y = "Valor",
           color = "Tipo") +
      theme_minimal()+
      theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
            plot.background = element_rect(fill = "#ecf0f5"),
            panel.background = element_rect(fill = "#ecf0f5",
                                            colour = "#ecf0f5",
                                            size = 0.5, linetype = "solid"))
    
    return(ggplotly(graph))
  })
  
}