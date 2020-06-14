#Libraries
library(dplyr)
library(dbplyr)
#Database Variables
host <- "boca-2.cg55foopexne.us-east-1.rds.amazonaws.com"
port <- 3306
dbname <- "BOCA_2"
user <- "JoeSans"
password <- "Joe5933547"

killDbConnections()
my_db <- src_mysql(dbname = dbname,host = host, port = port, user=user,password = password )
cn <- dbConnect(drv = RMySQL::MySQL(), username = user, password= password, host = host, dbname = dbname, port = port)

users.dt <- dbGetQuery(cn, 
       paste("SELECT * FROM users WHERE Username = '",
             # session$userData$user,
             'testdummy',
             "' AND Password = '", 
             # session$userData$pass,
             'desk',
             "'", sep = ""))
clientID <- unique(users.dt$ID_Client) #removes dupes
locations <- dbGetQuery(cn, paste("SELECT Address, ID_Building FROM buildings WHERE ID_Client = '", clientID, "'", sep = ""))
session$userData$ID_Service <- generate_id()

output$pageStub <- renderUI({
useShinyalert()
cat("Rendering Prev_Maint")
fluidPage(h1('Preventative Maintenance'),
fluidRow(
 column(width = 5,
        selectInput('selAddress','Building',choices= locations$Address)),tags$head(tags$style(HTML("
                   .shiny-split-layout > div {overflow: visible;}")))),
 fluidRow(
  column(width = 5,
         splitLayout(
          selectInput(
           "mArrival2",
           label='Mechanic Arrival',
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
            "12:15 AM",
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
          ),
          column(width=1,br(),
                 actionButton("nowArrival2",'Now'),
                 tags$style(type='text/css',"#nowArrival2 {
                            margin-top: 5px;}"))
                 ))),
 h3("Mechanic Departure"),
 fluidRow(column(width = 5,
                 splitLayout(
                  selectInput(
                   "mCheckout2",
                   label = "Mechanic Departure",
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
                    "12:15 AM",
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
                   ),
                   #end time list                            ----
                   selected = 1
                  ),column(width = 1,
                           br(),
                           actionButton("nowDeparture2","Now"),
                           tags$style(type='text/css',"#nowDeparture2 {
                       margin-top: 5px;}"))
                 )),tags$head(tags$style(HTML("
                      .shiny-split-layout > div {overflow: visible;}")))
 ),actionButton('departBtn', 'Submit')
         )})

observeEvent(input$departBtn,{ 
            dataRow   <- data.frame(
              ID_Service    = session$userData$ID_Service,
              ID_Building   = locations$ID_Building[locations$Address==input$selAddress],
              ID_Client     = clientID,
              Type          = 0,
              Description   = 'PM',
              Caller        = users.dt$ID_User,
              Component     = NA,
              Call_Reason   = NA,
              Call_Placed   = NA,
              Call_Returned = NA,
              Arrival       = lubridate::ymd_hm(paste(Sys.Date(),input$mArrival2,sep="-")),
              Departure     = lubridate::ymd_hm(paste(Sys.Date(),input$mCheckout2,sep="-")),
              Date          = Sys.time())

             tryCatch({dbWriteTable(cn, name = 'servicing', value = dataRow, append = T, row.names = F)},
                      warning = function(w) {
                       killDbConnections()
                       cn <- dbConnect(drv = RMySQL::MySQL(), username = user, password= password, host = host, dbname = dbname, port = port)
                       dbWriteTable(cn, name = 'servicing', value = dataRow, append = T, row.names = F)
                      cat('write warning table reconnected')
                       },
                      error = function(e) {
                       killDbConnections()
                       cn <- dbConnect(drv = RMySQL::MySQL(), username = user, password= password, host = host, dbname = dbname, port = port)
                       dbWriteTable(cn, name = 'servicing', value = dataRow, append = T, row.names = F)
                       cat('write error table reconnected')
                       })

             # cat('alert incoming')

             shinyalert(title = "Submission Successful!",
                        text = 'Click to Continue',
                        type = 'success',
                        closeOnClickOutside = T,
                        callbackR = function(){
                         if(length(dbListConnections(MySQL()))>10){
                          killDbConnections()
                          cn <- dbConnect(drv = RMySQL::MySQL(), username = user, password= password, host = host, dbname = dbname, port = port)
                         }
                         js$redirect("?login")
                        })
             })

observeEvent(input$nowArrival2, {
 currTime <-   format(x = lubridate::round_date(Sys.time(), '15 minutes'),
                      format = '%I:%M %p')
 updateSelectInput(session, 'mArrival2', selected = as.character(currTime))
})
observeEvent(input$nowDeparture2, {
 currTime <-   format(x = lubridate::round_date(Sys.time(), '15 minutes'),
                      format = '%I:%M %p')
 updateSelectInput(session, "mCheckout2", selected = as.character(currTime))
})