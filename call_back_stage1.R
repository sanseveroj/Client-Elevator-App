# #Database Variables
library(dplyr)
library(dbplyr)
#Database Variables
host <- "boca-2.cg55foopexne.us-east-1.rds.amazonaws.com"
port <- 3306
dbname <- "BOCA_2"
user <- "JoeSans"
password <- "Joe5933547"

# killDbConnections()
my_db <- src_mysql(dbname = dbname,host = host, port = port, user=user,password = password )
cn <- dbConnect(drv = RMySQL::MySQL(), username = user, password= password, host = host, dbname = dbname, port = port)

users.dt <- dbGetQuery(cn, 
                       paste("SELECT * FROM users WHERE Username = '",
                             session$userData$user,
                             # 'testdummy',
                             "' AND Password = '", 
                             session$userData$pass,
                             # 'desk',
                             "'", sep = ""))
clientID <- unique(users.dt$ID_Client)
locations <- dbGetQuery(cn, paste("SELECT Address, ID_Building FROM buildings WHERE ID_Client = '", as.character(clientID),"'", sep = ""))
phone_num <- dbGetQuery(cn, paste("SELECT Phone FROM client WHERE ID_Client = '", as.character(clientID),"'", sep = ""))$Phone
session$userData$ID_Service <- generate_id()
ID_Building <- unique(users.dt$ID_Building)
locations <- dbGetQuery(cn, paste("SELECT Dev_Des, ID_Building FROM elevators WHERE ID_Building = '", as.character(ID_Building),"'", sep = ""))

output$pageStub <- renderUI(
 #  cat("Rendering CallBack"),
 fluidPage(
 fluidRow(
  h1('Step 1 - Mechanic Request'),
  # column(width = 5,
         # selectInput('selAddress','Building',choices= locations$Address,
         #             tags$head(tags$style(
         #  HTML(".shiny-split-layout > div {overflow: visible;}")))))
         #    ),
 # # #Client desc of Call back (dropdown)
 fluidRow(column(
  width = 5,
  selectInput(
   "ClientDesc",
   label = "Reason for call",
   choices = c(
    "Entrapment",
    "Noise",
    "Shutdown",
    "Vibration",
    "Other"
   ),
   selected = "Shutdown"
  )
 )),
 div(class="header", checked=NA,
     h5("Contractor number:", style= 'font-weight: 700; font-family: "Helvetica Neue",Helvetica,Arial,sans-serif;
        font-size: 14px;
        line-height: 1.42857143;
        color: #333;'),
     a(paste("tel:", phone_num), href=phone_num,style= 'font-size:20px;')
     ),
 br(),
 fluidRow(
  column(width = 6,
         splitLayout(
          selectInput(
           "inp_callBack",
           label='Time call was placed?',
           choices =
            #timelist                                ====
           list(
            "05:00 AM",
            "05:15 AM",
            "05:30 AM",
            "05:45 AM",
            "06:00 AM",
            "06:15 AM",
            "06:30 AM",
            "06:45 AM",
            "07:00 AM",
            "07:15 AM",
            "07:30 AM",
            "07:45 AM",
            "08:00 AM",
            "08:15 AM",
            "08:30 AM",
            "08:45 AM",
            "09:00 AM",
            "09:15 AM",
            "09:30 AM",
            "09:45 AM",
            "10:00 AM",
            "10:15 AM",
            "10:30 AM",
            "10:45 AM",
            "11:00 AM",
            "11:15 AM",
            "11:30 AM",
            "11:45 AM",
            "12:00 PM",
            "12:15 PM",
            "12:30 PM",
            "12:45 PM",
            "01:00 PM",
            "01:15 PM",
            "01:30 PM",
            "01:45 PM",
            "02:00 PM",
            "02:15 PM",
            "02:30 PM",
            "02:45 PM",
            "03:00 PM",
            "03:15 PM",
            "03:30 PM",
            "03:45 PM",
            "04:00 PM",
            "04:15 PM",
            "04:30 PM",
            "04:45 PM",
            "05:00 PM",
            "05:15 PM",
            "05:30 PM",
            "05:45 PM",
            "06:00 PM",
            "06:15 PM",
            "06:30 PM",
            "06:45 PM",
            "07:00 PM",
            "07:15 PM",
            "07:30 PM",
            "07:45 PM",
            "08:00 PM",
            "08:15 PM",
            "08:30 PM",
            "08:45 PM",
            "09:00 PM"
           )
           #end time list                            ----
           ), column(width=1,br(),
                 actionButton("btnCallBack",'Now'),
                 tags$style(type='text/css',"#inp_callBack {
                       margin-top: 5px;}"))
         )),tags$head(tags$style(HTML("
                   .shiny-split-layout > div {overflow: visible;}")))),
 actionButton('submitMechRequest', 'Next')
 
)))
 
observeEvent(input$submitMechRequest,{
 session$userData$ID_Service    <- generate_id()
 session$userData$ID_Building   <- users.dt$ID_Building
 session$userData$ID_Client     <- clientID
 session$userData$Type          <- 1
 session$userData$Description   <- "CB"
 session$userData$Caller        <- users.dt$ID_User
 session$userData$Call_Placed   <- lubridate::ymd_hm(paste(Sys.Date(),input$inp_callBack,sep="-"))
 session$userData$Call_Reason   <- input$ClientDesc
 
 
 cat(paste(input$CallBack,
           session$userData$Call_Return_Time))
 
 source(here::here("call_back_stage2.R"), local=T)
})

observeEvent(input$btnCallBack, {
 currTime <-   format(x = lubridate::round_date(Sys.time(), '15 minutes'),
                      format = '%I:%M %p')
 updateSelectInput(session, 'inp_callBack', selected = as.character(currTime))
})
observeEvent(input$nowCallBack, {
 currTime <-   format(x = lubridate::round_date(Sys.time(), '15 minutes'),
                      format = '%I:%M %p')
 updateSelectInput(session, 'CallBack', selected = as.character(currTime))
})
