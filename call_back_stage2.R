# Lists for Drop down ####
comp_list <- c( 
   "Door Hall",
   "Door Car",
   "Door Operator",
   "Door Restrictor",
   "Door Safety Edge",
   "Door Software",
   "Door Track",
   "Drive Fault",
   "Unknown",
   "Software/Controller",
   "Brake",
   "Slack cable switch",
   "Fascia",
   "Fuse",
   "Governor Cable",
   "Hall Riser",
   "Hydro oil cold",
   "Relay",
   "Safety switch",
   "Communication",
   "Emergency light",
   "Hoistway",
   "Load weighing",
   "Machine",
   "Power fault failure",
   "Switch",
   "Door saddle debris",
   "FDNY",
   "Water leak",
   "Independent Service",
   "Escalator - Step drive system",
   "Escalator - Handrail system",
   "Escalator - Balustrade",
   "Escalator - Safety switches",
   "Other"
)
comp_list <-sort(comp_list)
#### End List ####


# print(session$userData$Call_Placed)

if (session$userData$resumeFlag) 
   { selected_comp_reason <- session$userData$servicing.dt$Component[1]
     selected_Arrival <-   session$userData$servicing.dt$Arrival[1]
     selected_Departure <- session$userData$servicing.dt$Departure[1]
} else {selected_comp_reason <- 1
       selected_Arrival <- NULL
       selected_Departure <- NULL}

output$pageStub <- renderUI(fluidPage(
 h1('Step 2 - Mechanic Arrival'),
 fluidRow(
  column(width = 5,
         splitLayout(
          selectInput(
           "mArrival",
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
          , selected = selected_Arrival
           ),
          column(width=1,br(),
                 shiny::actionButton("nowArrival1",'Now'),
                 tags$style(type='text/css',"#nowArrival1 {
                       margin-top: 5px;}"))
         ))),
 h3("Mechanic Departure"),
 fluidRow(column(width = 5,
                 splitLayout(
                  selectInput(
                   "mDepart",
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
 ),
 # # #Elevator unit/# (dropdown, populated from database)
 # fluidRow(column(width = 5,
 #                 selectInput(
 #                  "DevDesig",
 #                  label = "Device designation",
 #                  choices = list(1, 2, 3, 4, 5),
 #                  selected = 1
 #                 ))),
 # # #Mechanic Explanation of component associated w/ shutdown
 fluidRow(column(width=6,selectInput(
  "Component",
  label="Component inspected by mechanic",
  choices = comp_list,
 
  selected = selected_comp_reason,
  

  ),
  conditionalPanel(
     condition = "input.Component == 'Other'",
     textInput("otherComp","Other Comp", width = '180px')
 ))),

 
 
 fluidRow(actionButton('prevBtn','Previous'),actionButton('departBtn', 'Submit'),actionButton('saveBtn', 'Save')
          ,column(width = 3, offset = 9, style = "margin-top: 365px; margin-right: -50px", tags$img(
             src = "BOCALogo Graphic.png",
             align = "right", 
             height = "65%",
             width = "450")
))
))

observeEvent(input$departBtn,{ 
 cat(paste(session$userData$ID_Service,
 session$userData$ID_Building,
 session$userData$ID_Client,
 session$userData$Caller,
 input$Component,
 session$userData$Call_Reason,
 session$userData$Call_Placed,
 lubridate::ymd_hm(paste(Sys.Date(),input$mArrival,sep="-")),
 lubridate::ymd_hm(paste(Sys.Date(),input$mDepart,sep="-"))))
 # print(session$userData$OtherCR)
 
 if (is.null(input$OtherComp)) {my_other_comp <- NA} else{my_other_comp <-input$OtherComp}
 
 dataRow   <- data.frame(
  ID_Service    = session$userData$my_ID,
  ID_Building   = session$userData$ID_Building,
  Dev_Des       = session$userData$Dev_Des,
  ID_Client     = session$userData$ID_Client,
  Type          = 1,
  Description   = 'CB',
  Caller        = session$userData$Caller,
  Component     = input$Component,
  Call_Reason   = session$userData$Call_Reason,
  Call_Placed   = str_trunc(as.character(session$userData$Call_Placed), width = 8, side = "left", ellipsis = ""),           
  Call_Returned = NA,
  Arrival       = str_trunc(input$mArrival, width = 8, side = "left", ellipsis = ""),
  Departure     = str_trunc(input$mDepart, width = 8, side = "left", ellipsis = ""),
  Date          = Sys.Date(),
  Incomplete    = 0,
  OtherCR       = session$userData$OtherCR,
  OtherComp     = my_other_comp
  )
 dataRow$Date <- as.character(dataRow$Date)
 servicing.db <- dbGetQuery(connect_to_db(), "SELECT * FROM servicing")
 if (session$userData$resumeFlag) {servicing.db[which(servicing.db$ID_Service == session$userData$my_ID),] <- dataRow} 
 else {servicing.db <- rbind.data.frame(servicing.db, dataRow)}
 
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
print(session$userData$my_ID)

observeEvent(input$saveBtn,{ 
   print(session$userData$Call_Placed)
   
   cat(paste(session$userData$ID_Service,
   session$userData$ID_Building,
   session$userData$ID_Client,
   session$userData$Caller,
   input$Component,
   session$userData$Call_Reason,
   session$userData$Call_Placed,
   Sys.time(),
   session$userData$OtherCR,
   is.null(input$OtherComp
   )))
 
   # print(input$mArrival)
   # print(input$mDepart)
   if (is.null(input$OtherComp)) {my_other_comp <- NA} else{my_other_comp <-input$OtherComp}
print(as.character(session$userData$Call_Placed))
   dataRow   <- data.frame(
      ID_Service    = session$userData$my_ID,
      ID_Building   = session$userData$ID_Building,
      Dev_Des       = session$userData$Dev_Des,
      ID_Client     = session$userData$ID_Client,
      Type          = 1,
      Description   = 'CB',
      Caller        = session$userData$Caller,
      Component     = input$Component,
      Call_Reason   = session$userData$Call_Reason,
      Call_Placed   = str_trunc(as.character(session$userData$Call_Placed), width = 10, side = "right", ellipsis = ""),
      Call_Returned = NA,
      Arrival       = input$mArrival,
      Departure     = input$mDepart,
      Date          = Sys.Date(),
      Incomplete    = 1,
      OtherCR       = session$userData$OtherCR,
      OtherComp     = my_other_comp
   )
   
   dataRow$Date <- as.character(dataRow$Date)
   servicing.db <- dbGetQuery(connect_to_db(), "SELECT * FROM servicing")
   if (session$userData$resumeFlag) {my_Row <- which(servicing.db$ID_Service == session$userData$my_ID)} else {my_Row <- nrow(servicing.db) + 1}
   servicing.db[my_Row, ] <- dataRow
   print(servicing.db)
   print(str(servicing.db))
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


   
observeEvent(input$nowArrival1, {
 currTime <-   format(x = lubridate::round_date(Sys.time(), '15 minutes'),
                      format = '%I:%M %p')
 updateSelectInput(session, 'mArrival', selected = as.character(currTime))
})
observeEvent(input$nowDeparture2, {
 currTime <-   format(x = lubridate::round_date(Sys.time(), '15 minutes'),
                      format = '%I:%M %p')
 updateSelectInput(session, 'mDepart', selected = as.character(currTime))
 })
observeEvent(input$prevBtn, ignoreInit = T, {
   
   (source(here::here("call_back_stage1.R"),local=environment())[1])
   # js$redirect("?call_back_stage1")
})