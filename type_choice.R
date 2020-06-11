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
 ))
})
 
observeEvent(input$CallBackBtn, source(here("call_back_stage1.R"), local=T))
observeEvent(input$PrevMainBtn, source(here("preventative_maintenance.R"),local=T))