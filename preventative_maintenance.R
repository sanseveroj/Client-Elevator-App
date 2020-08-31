#Libraries
library(dplyr)
library(dbplyr)
#Database Variables
print(nrow(session$userData$users.dt))
if (nrow(session$userData$servicing.dt) > 0){
  cat("inside if")
selected_Dev_Des  <- session$userData$servicing.dt$Dev_Des[1]
selected_Arrival <- session$userData$servicing.dt$Arrival[1]
selected_Departure <- session$userData$servicing.dt$Departure[1]

}else{selected_Dev_Des  <- session$userData$elevators$Dev_Des[1]
      selected_Arrival <- NULL
      selected_Departure <- NULL
}

print(selected_Dev_Des)
selected_Dev_Des <- as.character(selected_Dev_Des)
output$pageStub <- renderUI({
useShinyalert()
cat("Rendering Prev_Maint")
fluidPage(h1('Preventative Maintenance'),
fluidRow(
  column(width = 5,
         selectInput('selDesignation','Device Designation',
                     choices= session$userData$elevators$Dev_Des,
                     selected = selected_Dev_Des))
),
 fluidRow(
  column(width = 5,
         splitLayout(
          selectInput(
           "mArrival2",
           label='Mechanic arrival',
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
         ,selected = selected_Arrival
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
                   label = "Mechanic departure",
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
                   selected = selected_Departure
                  ),column(width = 1,
                           br(),
                           actionButton("nowDeparture2","Now"),
                           tags$style(type='text/css',"#nowDeparture2 {
                       margin-top: 5px;}"))
                 )),tags$head(tags$style(HTML("
                      .shiny-split-layout > div {overflow: visible;}")))
 ),actionButton('departBtn', 'Submit'), actionButton('saveBtn', 'Save'),
fluidRow(column(width = 3, offset = 9, style = "margin-top: 365px; margin-right: -50px", tags$img(
           src = "BOCALogo Graphic.png",
           align = "right", 
           height = "65%",
           width = "450")))
         )})

observeEvent(input$departBtn,{ 
  if (!is.na(session$userData$servicing.dt$ID_Service[1])) {session$userData$my_ID <- session$userData$servicing.dt$ID_Service[1]} else {session$userData$my_ID <- generate_id()}
  
            dataRow   <- data.frame(
              ID_Service    = session$userData$ID_Service,
              ID_Building   = session$userData$users.dt$ID_Building,
              Dev_Des       = input$selDesignation,
              ID_Client     = session$userData$clientID,
              Type          = 0,
              Description   = 'PM',
              Caller        = session$userData$users.dt$ID_User,
              Component     = NA,
              Call_Reason   = NA,
              Call_Placed   = NA,
              Call_Returned = NA,
              Arrival       = paste(str_trunc(input$mArrival2, width = 5, side = "right", ellipsis = ""), ":00" , sep = ""),
              Departure     = paste(str_trunc(input$mCheckout2, width = 5, side = "right", ellipsis = ""), ":00" , sep = ""),
              Date          = Sys.Date(),
              Incomplete    = 0,
              OtherCR       = NA,
              OtherComp     = NA
            )
              
            dataRow$Date <- as.character(dataRow$Date)
            servicing.db <- dbGetQuery(connect_to_db(), "SELECT * FROM servicing")
            if (!is.na(session$userData$servicing.dt$ID_Service[1])) {my_Row <- which(servicing.db$ID_Service == session$userData$my_ID)} else {my_Row <- nrow(servicing.db) + 1}
            servicing.db[my_Row, ] <- dataRow
            
            dbWriteTable(connect_to_db(), name='servicing',value = servicing.db, overwrite = T, row.names = F)

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
print(selected_Arrival)
print(selected_Departure)
observeEvent(input$saveBtn,{ 
  if (!is.na(session$userData$servicing.dt$ID_Service[1])) {my_ID <- session$userData$servicing.dt$ID_Service[1]} else {my_ID <- generate_id()}
  dataRow   <- data.frame(
    ID_Service    = my_ID,
    ID_Building   = session$userData$users.dt$ID_Building,
    Dev_Des       = input$selDesignation,
    ID_Client     = session$userData$clientID,
    Type          = 0,
    Description   = 'PM',
    Caller        = session$userData$users.dt$ID_User,
    Component     = NA,
    Call_Reason   = NA,
    Call_Placed   = NA,
    Call_Returned = NA,
    Arrival       = input$mArrival2,
    Departure     = input$mCheckout2,
    Date          = Sys.Date(),
    Incomplete    = 1,
    OtherCR       = NA,
    OtherComp     = NA
  )
  dataRow$Date <- as.character(dataRow$Date)
  servicing.db <- dbGetQuery(connect_to_db(), "SELECT * FROM servicing")
  if (!is.na(session$userData$servicing.dt$ID_Service[1])) {my_Row <- which(servicing.db$ID_Service == my_ID)} else {my_Row <- nrow(servicing.db) + 1}
  servicing.db[my_Row, ] <- dataRow
  
  dbWriteTable(connect_to_db(), name='servicing',value = servicing.db, overwrite = T, row.names = F)
  
  # cat('alert incoming')
  
  shinyalert(title = "Save Successful!",
             text = 'Return to login',
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