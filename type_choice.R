# session$userData$users.dt <<- dbGetQuery(cn, 
#                        paste("SELECT * FROM users WHERE Username = '",
#                              session$userData$user,
#                              # 'testdummy',
#                              "' AND Password = '", 
#                              session$userData$pass,
#                              # 'desk',
#                              "'", sep = ""))
# session$userData$clientID <<- unique(session$userData$users.dt$ID_Client)
# session$userData$locations <<- dbGetQuery(cn, paste("SELECT Address, ID_Building FROM buildings WHERE ID_Client = '", as.character(session$userData$clientID),"'", sep = ""))
# session$userData$phone_num <<- dbGetQuery(cn, paste("SELECT Phone FROM client WHERE ID_Client = '", as.character(session$userData$clientID),"'", sep = ""))$Phone
# session$userData$ID_Service <<- generate_id()
# session$userData$ID_Building <<- unique(session$userData$users.dt$ID_Building)
# session$userData$elevators <<- dbGetQuery(cn, paste("SELECT Dev_Des, ID_Building FROM elevators WHERE ID_Building = '", as.character(session$userData$ID_Building),"'", sep = ""))
# 
# session$userData$servicing.dt <<-dbGetQuery(cn, 
#                                         paste("SELECT * FROM servicing WHERE ID_Building = '",
#                                               session$userData$ID_Building,
#                                               # 'testdummy',
#                                               "' AND Date >= '", 
#                                               lubridate::as_date(Sys.Date()) - 7
#                                               ,
#                                               # 'desk',
#                                               "' AND Incomplete = 1", sep = ""))[1,]
# cat(exists('session$userData$servicing.dt'))
# if (nrow(session$userData$servicing.dt) > 0)
# {print(session$userData$servicing.dt)
# if (session$userData$servicing.dt$Type[1] == 1) {js$redirect("?call_back_stage1")} 
# else {js$redirect("?preventative_maintenance")}}


output$pageStub <- renderUI({
 cat("Rendering Type_Choice")
 # cat(paste0("Rendering ", webpage$name, " v.", rv$limn, "\n"))
 fluidRow(column(width = 3,offset = 3,align='right',
  actionButton('CallBackBtn', 'Call Back',icon("phone")),
  tags$style(type='text/css', "#CallBackBtn 
           { margin-top: 275px; 
             height: 170px; 
             width: 275px;
             font-size: 30px;
             box-shadow: 5px 5px 15px 5px rgba(0,0,0,0.5);}")
 ),column(width = 3,align= 'left',
        actionButton('PrevMainBtn', HTML('Preventative <br/>   Maintenance'), icon('wrench')),
        tags$style(type='text/css', "#PrevMainBtn {
                   height: 170px; 
                   width:  275px;
                   margin-top: 275px;
                   font-size: 30px;
                   box-shadow: 5px 5px 15px 5px rgba(0,0,0,0.5);}")
 ),column(width = 3, style = "margin-top: 800px;", tags$img(
   src = "BOCALogo Graphic.png",
   align = "right", 
   height = "65%",
   width = "450")))

})
 
observeEvent(input$CallBackBtn, {
  
  
  
  
  
  
  source(here::here("call_back_stage1.R"), local=T)})
observeEvent(input$PrevMainBtn, {source(here::here("preventative_maintenance.R"),local=T)})