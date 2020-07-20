
output$pageStub <- renderUI({
 useShinyalert()  # Set up shinyalert
 x = rv$limn
 # cat(paste0("Rendering ", webpage$name, " v.", rv$limn, "\n"))
 fillPage(
 tags$img(src = "Login-BOCATrack Graphics-TOP.png", width = "100%"),
                tags$br(),
                tags$br(), tags$br(),
                tags$br(),
  column(
  width = 2, offset=5,
  h3("Login"),
  textInput("username", label = "Username"),
  passwordInput("password", label = "Password"),
  actionButton("login_btn",label="Login"),
  tags$br(),
  tags$br(),
  tags$br(),
  
  tags$img(
  src = "BOCALogo Graphic.png",
  align = "center", 
  height = "65%",
  width = "100%")),
 tags$br(),
 tags$br(),
 tags$br(),
 tags$br(),tags$br(),
 tags$br(),
 tags$br(),
 tags$br(), tags$br(),
 tags$br(),
 tags$br(),
 tags$br(),tags$br(),
 tags$br(),
 tags$br(),
 tags$br(),
 tags$footer(
   tags$img(src = "Login-BOCATrack Graphics-BOT.png", width = "102%", align ="center"), style = "
              position:absolute;
              bottom:0;
              width:102%;
              height:110px;   /* Height of the footer */
              color: white;
              z-index: 1000;"
 )
   
 )

 
 
 })
value <- F
observeEvent(input$resumeBtn,{value <- T 
removeModal()
if (session$userData$servicing.dt$Type == 1)
{source(here::here("call_back_stage1.R"),local=T)}
else
{source(here::here("preventative_maintenance.R"),local=T)}
})
observeEvent(input$newBtn,{value <- F 
removeModal()
session$userData$servicing.dt[1,] <- NA
source(here::here("type_choice.R"), local=T)
})
observeEvent(input$login_btn,{
  session$userData$user <<- as.character(input$username)
  session$userData$pass <<- as.character(input$password)
  session$userData$users.dt <<- dbGetQuery(connect_to_db(), 
                                           paste("SELECT * FROM users WHERE Username = '",
                                                 session$userData$user,
                                                 # 'testdummy',
                                                 "' AND Password = '", 
                                                 session$userData$pass,
                                                 # 'desk',
                                                 "'", sep = ""))

  session$userData$clientID <<- session$userData$users.dt$ID_Client
  session$userData$locations <<- dbGetQuery(connect_to_db(), paste("SELECT Address, ID_Building FROM buildings WHERE ID_Client = '", as.character(session$userData$clientID),"'", sep = ""))
  session$userData$phone_num <<- dbGetQuery(connect_to_db(), paste("SELECT Phone FROM client WHERE ID_Client = '", as.character(session$userData$clientID),"'", sep = ""))$Phone
  # cat(session$userData$phone_num)
  session$userData$ID_Service <<- generate_id()
  # print(session$userData$users.dt)
  session$userData$ID_Building <<- unique(session$userData$users.dt$ID_Building)
  # print(session$userData$ID_Building)
  session$userData$elevators <<- dbGetQuery(connect_to_db(), paste("SELECT Dev_Des, ID_Building FROM elevators WHERE ID_Building = '", 
                                                      as.character(session$userData$ID_Building),"'", sep = ""))

  
  session$userData$servicing.dt <<-dbGetQuery(connect_to_db(), 
                                              paste("SELECT * FROM servicing WHERE ID_Building = '",
                                                    session$userData$ID_Building,
                                                    # 'testdummy',
                                                    "' AND Incomplete = 1", sep = ""))[1,]
  print(session$userData$elevators)
  print(session$userData$servicing.dt)
  print(nrow(session$userData$servicing.dt))
  
  if (!is.na(session$userData$servicing.dt$ID_Service[1]))
      {showModal(modalDialog( h3("You have an unsubmitted request. Would you like to resume your previous session?"), 
                         fluidRow(actionButton("resumeBtn", "Resume"), actionButton("newBtn", "Start new session"), align = "center"), footer = NULL))}
  
             if (nrow(session$userData$users.dt) == 1){loggedIn <- T
              if (session$userData$users.dt$Security ==1) {
               cat('Rendering Dashboard')
               source(here::here('dashboard.R'),local=T)}
              else if (value) 
              {print(value)
                if (session$userData$servicing.dt$Type == 1)
                  {source(here::here("call_back_stage1.R"),local=T)}
                  else
                    {source(here::here("preventative_maintenance.R"),local=T)}
                }
                  else
                      {source(here::here("type_choice.R"), local=T)}
             } else {
              shinyalert(title = "Login Unsuccessful",text = "Username or Password incorrect, please try again.", type = 'error')
             }
             })
# shinyalert(title = "You have an unsubmitted request. Would you like to resume your previous session?", inputType = "radio", choices = "Yes", "No")
# if (choices = "Yes"){LINES 79-83} else{84}
             
# LINE 78 if user selects continue previous session in shinyalert would you like to restore your previous session from "DATE"?//new line then run line 80 if not then line 85


# observeEvent(input$login_btn,{
#   session$userData$user <<- as.character(input$username)
#   session$userData$pass <<- as.character(input$password)
#   users.dt <- dbGetQuery(cn, paste("SELECT * 
#                                               FROM users 
#                                               WHERE Username = '",session$userData$user,"' 
#                                               AND Password = '", session$userData$pass, "'", sep = ""))
#   # users.dt <- dbGetQuery(cn, "SELECT * FROM users WHERE Username = 'testdummy' AND Password = 'desk'")
#   session$userData$ClientId <<- as.character(users.dt$ID_Client)
#   if (nrow(users.dt) == 1){loggedIn <- T
#   if (users.dt$Security ==1) {
#     # cat('Rendering Dashboard')
#     source(here::here('dashboard.R'),local=T)}
#   else {source(here::here("type_choice.R"), local=T)}
#   } else {
#     shinyalert(title = "Login Unsuccessful",text = "Username or Password incorrect, please try again.", type = 'error')
#   }
# })


