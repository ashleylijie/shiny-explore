

output$uiLogin <- renderUI({
  
    if (!USER$Logged) {
      
      bootstrapPage(
        titlePanel("用户登录"),
        
        textInput("userName", "Username : "),
        passwordInput("passwd", "Password : "),
        br(),
        actionButton("Login", "Log in"))
        
    }
})

output$pass <- renderText({  
    if (!USER$Logged) {
        USER$pass
    }  
})

# Login info during session ----
output$userPanel <- renderUI({
    if (USER$Logged) {
        fluidRow(
            column(2,
                   "User: ", USER$name
            ),
            column(1, actionLink("logout", "Logout"))
        )
    }  
})

# control login
observeEvent(input$Login , {
    Username <- isolate(input$userName)
    Password <- isolate(input$passwd)
    
    user_info <- user_verify(Username, Password)
    
    if (nrow(user_info) > 0) {
      
      USER$Logged <- TRUE
      USER$name <- Username      

      } else {
        
        USER$pass <- "User or Password verification failed."
      
    }
})

# control logout
observeEvent(input$logout , {
    USER$Logged <- FALSE
    USER$pass <- ""
})