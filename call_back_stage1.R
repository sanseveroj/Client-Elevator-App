# # #Database Variables
# library(dplyr)
# library(dbplyr)
# #Database Variables
# host <- "boca-2.cg55foopexne.us-east-1.rds.amazonaws.com"
# port <- 3306
# dbname <- "BOCA_2"
# user <- "JoeSans"
# password <- "Joe5933547"
# 
# # killDbConnections()
# my_db <- src_mysql(dbname = dbname,host = host, port = port, user=user,password = password )
# cn <- dbConnect(drv = RMySQL::MySQL(), username = user, password= password, host = host, dbname = dbname, port = port)

if (nrow(session$userData$servicing.dt) > 0){
  cat("inside if")

  selected_Dev_Des  <- session$userData$servicing.dt$Dev_Des[1]
  selected_call_reason <- session$userData$servicing.dt$Call_Reason[1]
  selected_call_placed <- session$userData$servicing.dt$Call_Placed[1]
  print(selected_call_placed)
}else
  {
  selected_Dev_Des  <- session$userData$elevators$Dev_Des[1]
selected_call_reason <- 1
selected_call_placed <- NULL

}


output$pageStub <- renderUI(
 #  cat("Rendering CallBack"),
 fluidPage(
 fluidRow(
  h1('Step 1 - Mechanic Request'),
  column(width = 5,
  selectInput('selDesignation','Device Designation',
              choices= session$userData$elevators$Dev_Des,
              selected = selected_Dev_Des
             ))
     ),
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
   selected = selected_call_reason
  ),
  conditionalPanel(
    condition = "input.ClientDesc == 'Other'",
    textInput("OtherCR","Other Call Reason", width = '180px')
 ))),

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
           ,selected = selected_call_placed
           ), column(width=1,br(),
                 actionButton("btnCallBack",'Now'),
                 tags$style(type='text/css',"#inp_callBack {
                       margin-top: 5px;}"))
         )),tags$head(tags$style(HTML("
                   .shiny-split-layout > div {overflow: visible;}")))),
 actionButton('submitMechRequest', 'Next'),
 actionButton('saveMechRequest', 'Save')
 ,column(width = 3, offset = 9, style = "margin-top: 400px;", tags$img(
   src = "BOCALogo Graphic.png",
   align = "right", 
   height = "65%",
   width = "450"))))
 
observeEvent(input$submitMechRequest, ignoreInit = T, {
  if (!is.na(session$userData$servicing.dt$ID_Service[1])) {session$userData$my_ID <- session$userData$servicing.dt$ID_Service[1]} else {session$userData$my_ID <- generate_id()}
 session$userData$ID_Service    <- session$userData$my_ID
 session$userData$ID_Building   <- session$userData$users.dt$ID_Building
 session$userData$ID_Client     <- session$userData$clientID
 session$userData$Type          <- 1
 session$userData$Description   <- "CB"
 session$userData$Caller        <- session$userData$users.dt$ID_User
 session$userData$Call_Placed   <- lubridate::ymd_hm(paste(Sys.Date(),input$inp_callBack,sep="-"))
 session$userData$Call_Reason   <- input$ClientDesc
 session$userData$Dev_Des       <- input$selDesignation
if (is.null(input$OtherCR)){session$userData$OtherCR <- input$OtherCR} else { session$userData$OtherCR <- NA}
 # cat(paste(input$CallBack,
           # session$userData$Call_Return_Time))

 source(here::here("call_back_stage2.R"), local=environment())
})

observeEvent(input$saveMechRequest,{
  sender <- "kyle.ciantar77@gmail.com"
  recepients <- c("kyle.ciantar@uconn.edu", "kyle.ciantar77@gmail.com")
  
  if (is.na(session$userData$servicing.dt$ID_Service[1]) && input$ClientDesc %in% c("Shutdown", "Entrapment")){
    
    send.mail(from= sender,
              to= recepients,
              subject="Shutdown",
              body="There has been a elevator shutdown",
              html=T,
              smtp=list(host.name = "smtp.gmail.com",
                        port = 465,
                        user.name = "kyle.ciantar77",
                        passwd = "Heartdog7",
                        ssl = T),
              authenticate=T, 
              send = TRUE)
    

  }
  print("Start Save")
  # print(session$userData$servicing.dt$ID_Service[1])
  # print(input$ClientDesc)
  if (!is.na(session$userData$servicing.dt$ID_Service[1])) {session$userData$my_ID <- session$userData$servicing.dt$ID_Service[1]} else {session$userData$my_ID <- generate_id()}
  dataRow   <- data.frame(
    ID_Service    = session$userData$my_ID,
    ID_Building   = session$userData$users.dt$ID_Building,
    Dev_Des       = input$selDesignation,
    ID_Client     = session$userData$clientID,
    Type          = 1,
    Description   = 'CB',
    Caller        = session$userData$users.dt$ID_User,
    Component     = NA,
    Call_Reason   = input$ClientDesc,
    Call_Placed   = input$inp_callBack,
    Call_Returned = NA,
    Arrival       = NA,
    Departure     = NA,
    Date          = Sys.time(),
    Incomplete    = 1,
    OtherCR       = input$OtherCR,
    OtherComp     = NA
  )
 dataRow$Date <- as.character(dataRow$Date)
 # print(dataRow)
  servicing.db <- dbGetQuery(connect_to_db(), "SELECT * FROM servicing")
  if (!is.na(session$userData$servicing.dt$ID_Service[1])) {my_Row <- which(servicing.db$ID_Service == session$userData$my_ID)} 
  else {my_Row <- nrow(servicing.db) + 1}
  servicing.db <- servicing.db[-my_Row,]
  servicing.db <- rbind.data.frame(servicing.db, dataRow)
  # print(servicing.db[nrow(servicing.db),])
  # servicing.db[my_Row, 'Date'] <-lubridate::as_date(servicing.db[my_Row, "Date"])
  # print(dataRow)
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
