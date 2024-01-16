# Functions (Back-End) ---------

## Libraries ----
library(shiny)
library(shinyauthr)  # devtools::install_github("business-science/shinyauthr")

## Login -----

validate_pwd <- function(input, output, session, data, user_col, pwd_col){
  
  user <- data %>% pull(!! enquo(user_col))
  pwd <- data %>% pull(!! enquo(pwd_col))
  
  eventReactive(input$login_button, {
    
    validate <- FALSE
    
    if(input$user_name == user && input$password == pwd){
      validate <- TRUE
    }
    
    if(validate) shinyjs::hide(id = "login")
    
    validate
    
  })
  
}