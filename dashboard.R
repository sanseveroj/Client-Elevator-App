library(shiny)
library(dplyr)
library(plotly)
library(data.table)
library(shinydashboard)
library(dbplyr)
library(RMySQL)
library(DBI)
library(tidyquant)
host <- "boca-2.cg55foopexne.us-east-1.rds.amazonaws.com"
port <- 3306
dbname <- "BOCA_2"
user <- "JoeSans"
password <- "Joe5933547"

# killDbConnections()
my_db <- src_mysql(dbname = dbname,host = host, port = port, user=user,password = password )
cn <- dbConnect(drv = RMySQL::MySQL(), username = user, password= password, host = host, dbname = dbname, port = port)
servicing.db <- dbGetQuery(cn, "SELECT * FROM servicing")

#Formatting Date strings
servicing.db$Call_Placed <- lubridate::ymd_hms(servicing.db$Call_Placed)
servicing.db$Call_Returned <- lubridate::ymd_hms(servicing.db$Call_Returned)

servicing.db <- servicing.db %>% 
  mutate(month = lubridate::month(Date, abbr=T, label= T),
         mechTime = difftime(Arrival, Call_Placed, units = 'mins'))
servicing.db$Component[servicing.db$Component == ""] <- "Other"
servicing.db$Component[servicing.db$Component == "NA"] <- "Other"
servicing.db$Component[is.na(servicing.db$Component)] <- "Other"

servicing.db$LateCB <- 0
servicing.db$LateCB[servicing.db$mechTime >30] <- 1

# client_id <- session$userData$ClientId
client_id <- "oQ0CaHqIq1LiGcMR"
buildings <- dbGetQuery(cn, paste("SELECT * FROM buildings WHERE ID_Client = '",client_id,"'", sep = ''))

servicing.db <- merge(
  x= servicing.db,
  y= buildings[,c('ID_Building','Address', 'PM.ReqHrs')],
  by= 'ID_Building'
)
servicing.db$MechDur <- round(difftime(servicing.db$Departure,servicing.db$Arrival,units='hours'),1)

monthList <- data.frame(month = lubridate::month(seq.Date(ymd('2019/1/1'),ymd('2019/12/1'),'month'), 
                                                 abbr=T, label=T))
fullService <-merge(
  x = merge(
    x = monthList,
    y= servicing.db %>% 
      group_by(month, Address)%>%
      filter(Type == 0) %>%
      summarise(PM.PerfHrs = sum(MechDur)),
    by = 'month',
    all.x = T
  ),
  y = merge(
    x=monthList,
    y=buildings
  ),
  by = c('month','Address'),
  all.y = T
)

fullService$PM.PerfHrs[is.na(fullService$PM.PerfHrs)] <- 0
fullService <- fullService %>% ungroup()
names(fullService)[1] <- "Month"
names(servicing.db)[16] <- "Month"
# UI ----
output$pageStub <- renderUI(fluidPage(
  tags$style(type="text/css",
             ".shiny-output-error { visibility: hidden; }",
             ".shiny-output-error:before { visibility: hidden; }"
  ),
 if(loggedIn) {actionButton('logoutBtn','Logout', style= 
                             'position: fixed; right: 10px;top: 5px;')},
  title = 'BOCA',
  sidebarLayout(sidebarPanel(width = 2,
   selectInput('Month',label= 'Select Month',
     c("All",as.character(monthList$month))),
   selectInput('Address','Select Address', c("All",buildings$Address))
   ),
  mainPanel(
   tabsetPanel(
    
    tabPanel('Preventative Maintenance',
             br(),
     fluidRow(
      box(width = 12,
          plotlyOutput('Pmaint')
         )
      ),
     fluidRow(
       box(width = 12,
           DT::dataTableOutput('servicing')
                  )
     )
    ),
    
    tabPanel('Call Back',
             br(),
            try(fluidRow(box(width = 6,plotlyOutput("Calls")))),
            fluidRow(box(width = 6,plotlyOutput("Components"),solidHeader = T,
                tags$style(type='text/css', "#Components {margin-top: 25px;}")))
           )
   )
  )
 )
)
)
# Server ----
#Prev Maintenance Plots ----
rServicing <- reactive({
 if(input$Address== "All" & input$Month== "All") {
  fullService %>%
   group_by(Month) %>%
   summarise(PM.ReqHrs = sum(PM.ReqHrs, na.rm=T),
             PM.PerfHrs = sum(PM.PerfHrs, na.rm = T))
 }
  else if (input$Address == "All") {
   fullService %>% filter(Month == input$Month)
    }
  else if (input$Month == "All") {
   fullService %>% filter(Address == input$Address)
    }
  else {
   fullService %>%filter(Address == input$Address,
                         Month == input$Month)%>%
    mutate(PM.MissHrs = PM.ReqHrs - PM.PerfHrs)
    }
 })

output$Pmaint <- renderPlotly({
  if(input$Address== "All" & input$Month== "All") {
   rServicing() %>%
    plot_ly(x=~Month, 
            y= ~PM.ReqHrs,
            type= 'scatter', 
            mode = 'lines',
            name = 'Required Hours') %>%
    add_trace(y=~PM.PerfHrs, 
              mode = 'lines',
              name = 'Performed Hours') %>%
    layout(
     title = 'Preventative Maintenance',
     yaxis = list(title = 'Hours'),
     xaxis = list(title = 'Month'),
     colorway = c('#FF0000','#00cc00')
    )
   } else if (input$Address == "All") {
    rServicing() %>%
     plot_ly(type = 'bar',
             y = ~Address,
             x = ~PM.ReqHrs,
             name = 'Required Hours' 
            ) %>%
    add_trace(x= ~PM.PerfHrs, 
              name = 'Recorded Hours'
              ) %>%
    layout(title = ~Month,
           yaxis = list(title = 'Hours'),
           xaxis = list(title = 'Address'),
           barmode = 'overlay',
           colorway = c('#FF0000','#00cc00')
          )
   }else if (input$Month == "All") {
    rServicing() %>%
     plot_ly(x=~Month, 
             y= ~PM.ReqHrs,
             type= 'scatter', 
             mode = 'lines',
             name = 'Required Hours') %>%
     add_trace(y=~PM.PerfHrs, 
               mode = 'lines',
               name = 'Performed Hours')%>%
     layout(
      title = ~Address,
      yaxis = list(title = 'Hours'),
      xaxis = list(title = 'Month'),
      colorway = c('#FF0000','#00cc00'))
   }
  else {
   rServicing() %>%
   plot_ly(labels = c('Recorded Hours', 'Required Hours'),values = c(~PM.PerfHrs, ~PM.MissHrs)) %>%
    add_pie(hole = 0.6) %>%
    layout(title =~Address,  showlegend = T,
           # xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
           # yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE)),
          colorway = c('#FF0000','#00cc00'))
     }
   
 })

#Preventative Maintenance Table ----
output$servicing <- DT::renderDataTable(
 DT::datatable(rServicing(), options = list(pageLength = 25, buttons = c('copy', 'csv', 'excel', 'pdf', 'print')))
)
 
#Call Back Plots ---- 
 late_month <- reactive({
  if(input$Address== "All" & input$Month== "All") {
   temp <- servicing.db %>%
    group_by(Month) %>%
    count(LateCB) %>%
    tidyr::spread(LateCB, n)
   temp[is.na(temp)] <-0
   if(ncol(temp) <3) {
     if("0" %in% names(temp)) temp$`1` <- 0
     if("1" %in% names(temp)) temp$`0` <- 0
   }
   temp
  }
  else if (input$Address == "All") {
   temp <- servicing.db %>%
    group_by(Month) %>%
    filter(Month == 'Jan') %>%
    # filter(Month == input$Month)%>%
    count(LateCB) %>%
    tidyr::spread(LateCB, n)
   temp[is.na(temp)] <-0
   if(ncol(temp) <3) {
     if("0" %in% names(temp)) temp$`1` <- 0
     if("1" %in% names(temp)) temp$`0` <- 0
   }
   temp
   }
   else if (input$Month == "All") {
   temp <- servicing.db %>%
    group_by(Month) %>%
    filter(Address == input$Address)%>%
    count(LateCB) %>%
    tidyr::spread(LateCB, n)
   
    temp[is.na(temp)] <-0
    if(ncol(temp) <3) {
      if("0" %in% names(temp)) temp$`1` <- 0
      if("1" %in% names(temp)) temp$`0` <- 0
    }
    temp}
  else {
   temp <- servicing.db %>%
    group_by(Month) %>%
    filter(Address == input$Address,
           Month == input$Month)%>%
    count(LateCB) %>%
    tidyr::spread(LateCB, n)
   temp[is.na(temp)] <-0
   if(ncol(temp) <3) {
     if("0" %in% names(temp)) temp$`1` <- 0
     if("1" %in% names(temp)) temp$`0` <- 0
   }
   temp
   
  }
  
 })
 
 rComponents <-reactive({
  if(input$Address== "All" & input$Month== "All") {
   components <- servicing.db %>%
    group_by(Component) %>%
    summarise(compCount = n())
  }
  else if (input$Address == "All") {
   components <- servicing.db %>%
   group_by(Month,Component) %>%
   filter(Month == input$Month)%>%
   summarise(compCount = n())}
  else if (input$Month == "All") {
   components <- servicing.db %>%
    group_by(Address,Component) %>%
    filter(Address == input$Address)%>%
    summarise(compCount = n())}
  else {
   components <- servicing.db %>%
    group_by(Month,Address,Component) %>%
    filter(Address == input$Address,
           Month == input$Month)%>%
    summarise(compCount = n())}
 })
 
 output$Components <- renderPlotly({
  plot_ly(
   data = rComponents(),
   type = 'pie',
   labels = ~Component,
   values = ~compCount
   )%>% layout(title = 'Components')
 })
 
 output$Calls <- renderPlotly({
  temp <- late_month()
  
  validate(
    need( nrow(temp) > 0, "Data insufficient for plot")
  )
  
  temp %>% 
    plot_ly(
   type = 'bar',
   x = ~Month,
   y = ~`0`,
   name = 'On-Time' 
  ) %>%
   add_trace(y= ~`1`, name = 'Late') %>%
   layout(
    title = 'Service Calls per Month',
    yaxis = list(title = 'Calls'),
    xaxis = list(title = 'Month'),
    barmode = 'stack',
    colorway = c('#00cc00','#FF0000')
   )
 
 })


