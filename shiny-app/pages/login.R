# UI -----
login_ui <- function(id, title){
  
  ns <- NS(id)
  
  div(
    id = ns("login"),
    style = "width: 500px;
    max-width: 100%;
    margin: 0 auto;
    padding: 20px;
    padding-top: 0px;
    padding-left: 10px;
    padding-right: 10px;
    padding-bottom: 10px;
    padding-top: 5px;
    background-color: #505f75;
    border-radius: 10px;",
    column(
      width = 12,
      align = "center",
      h2(title, style = "color: #FFF !important"),
      textInput(inputId = ns("user_name"),
                label = tagList(icon("user"), ""),
                placeholder = "Nome de usuario"),
      passwordInput(inputId = ns("password"),
                    label = tagList(icon("unlock-alt"), ""),
                    placeholder = "Senha"),
      actionButton(inputId = ns("login_button"),
                     "Log in",
                     class = "btn-primary", style = "background-color: #76bfac;
                     color: white;
                     font-family: 'Bahnschrift';
                     font-size: 20px;
                     height: 50px;
                     margin-left: 10px;
                     margin-right: 10px;
                     border-radius: 10px;
                     border-color: #76bfac;")
    )
  )
}



# Server -----
validate_pwd <- function(input, output, session, data, user_col, pwd_col){
  
  eventReactive(input$login_button, {
    
    validate <- FALSE
    
    dataFiltered <- data %>%
      filter(Usuario == input$user_name)
    
    if(input$password == dataFiltered$Senha){
      validate <- TRUE
      
      shinyjs::hide(id = "login")
      
      return(c(validate = validate, table = dataFiltered))
    }
    
    
  })
  
}

