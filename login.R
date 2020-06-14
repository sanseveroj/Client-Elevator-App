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
 fluidRow(column(
  width = 4, offset=4,
  h3("Login"),
  textInput("username", label = "Username"),
  passwordInput("password", label = "Password"),
  actionButton("login_btn",label="Login")
 ))
)
})

observeEvent(input$login_btn,{
             session$userData$user <<- as.character(input$username)
             session$userData$pass <<- as.character(input$password)
             users.dt <- dbGetQuery(cn, paste("SELECT * 
                                              FROM users 
                                              WHERE Username = '",session$userData$user,"' 
                                              AND Password = '", session$userData$pass, "'", sep = ""))
             # users.dt <- dbGetQuery(cn, "SELECT * FROM users WHERE Username = 'testdummy' AND Password = 'desk'")
             session$userData$ClientId <<- as.character(users.dt$ID_Client)
             if (nrow(users.dt) == 1){loggedIn <- T
              if (users.dt$Security ==1) {
               # cat('Rendering Dashboard')
               source(here::here('dashboard.R'),local=T)}
              else {source(here::here("type_choice.R"), local=T)}
             } else {
              shinyalert(title = "Login Unsuccessful",text = "Username or Password incorrect, please try again.", type = 'error')
             }
             })

             

