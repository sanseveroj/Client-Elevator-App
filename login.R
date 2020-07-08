library(shinyWidgets)

# Database Variables
host <- "boca-2.cg55foopexne.us-east-1.rds.amazonaws.com"
port <- 3306
dbname <- "BOCA_2"
user <- "JoeSans"
password <- "Joe5933547"

killDbConnections()
my_db <- src_mysql(dbname = dbname,host = host, port = port, user=user,password = password )
cn <- dbConnect(drv = RMySQL::MySQL(), username = user, password= password, host = host, dbname = dbname, port = port)

output$pageStub <- renderUI({
 useShinyalert()  # Set up shinyalert
 x = rv$limn
 cat(paste0("Rendering ", webpage$name, " v.", rv$limn, "\n"))
 fluidPage(
 verticalLayout(tags$img(src = "Login-BOCATrack Graphics-TOP.png", width = "100%", height = "100%"),
                tags$br(),
                tags$br(), tags$br(),
                tags$br(),
  column(
  width = 4, offset=5,
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
  width = "450")),
 tags$br(),
 tags$br(),
 tags$br(),
 tags$br(),tags$br(),
 tags$br(),
 tags$br(),
 tags$br(),
 tags$img(src = "Login-BOCATrack Graphics-BOT.png", width = "100%", height = "110", align = "center")
 
 ))
 
 })


observeEvent(input$login_btn,{
  session$userData$user <<- as.character(input$username)
  session$userData$pass <<- as.character(input$password)
  session$userData$users.dt <<- dbGetQuery(cn, 
                                           paste("SELECT * FROM users WHERE Username = '",
                                                 session$userData$user,
                                                 # 'testdummy',
                                                 "' AND Password = '", 
                                                 session$userData$pass,
                                                 # 'desk',
                                                 "'", sep = ""))

  session$userData$clientID <<- session$userData$users.dt$ID_Client
  session$userData$locations <<- dbGetQuery(cn, paste("SELECT Address, ID_Building FROM buildings WHERE ID_Client = '", as.character(session$userData$clientID),"'", sep = ""))
  session$userData$phone_num <<- dbGetQuery(cn, paste("SELECT Phone FROM client WHERE ID_Client = '", as.character(session$userData$clientID),"'", sep = ""))$Phone
  # cat(session$userData$phone_num)
  session$userData$ID_Service <<- generate_id()
  # print(session$userData$users.dt)
  session$userData$ID_Building <<- unique(session$userData$users.dt$ID_Building)
  # print(session$userData$ID_Building)
  session$userData$elevators <<- dbGetQuery(cn, paste("SELECT Dev_Des, ID_Building FROM elevators WHERE ID_Building = '", 
                                                      as.character(session$userData$ID_Building),"'", sep = ""))

  
  session$userData$servicing.dt <<-dbGetQuery(cn, 
                                              paste("SELECT * FROM servicing WHERE ID_Building = '",
                                                    session$userData$ID_Building,
                                                    # 'testdummy',
                                                    "' AND Date >= '", 
                                                    lubridate::as_date(Sys.Date()) - 7
                                                    ,
                                                    # 'desk',
                                                    "' AND Incomplete = 1", sep = ""))[1,]
  print(session$userData$elevators)
  print(session$userData$servicing.dt)
  print(nrow(session$userData$servicing.dt))

             if (nrow(session$userData$users.dt) == 1){loggedIn <- T
              if (session$userData$users.dt$Security ==1) {
               cat('Rendering Dashboard')
               source(here::here('dashboard.R'),local=T)}
              else if (!is.na(session$userData$servicing.dt$ID_Service[1]))
              {
                if (session$userData$servicing.dt$Type == 1)
                  {source(here::here("call_back_stage1.R"),local=T)}
                  else
                    {source(here::here("preventative_maintenance.R"),local=T)}
                }else
                      {source(here::here("type_choice.R"), local=T)}
             } else {
              shinyalert(title = "Login Unsuccessful",text = "Username or Password incorrect, please try again.", type = 'error')
             }
             })

             




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


