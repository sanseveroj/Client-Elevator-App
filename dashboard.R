library(shiny)
library(dplyr)
library(plotly)
library(data.table)
library(shinydashboard)
library(dbplyr)
library(RMySQL)
library(DBI)


servicing.db <- dbGetQuery(connect_to_db(), "SELECT * FROM servicing")

#Formatting Date strings
servicing.db <- servicing.db  %>% mutate(Call_Placed = lubridate::as_datetime(Call_Placed), 
                                         Call_Returned = lubridate::as_datetime(Call_Returned),
                                         Arrival = lubridate::as_datetime(Arrival),
                                         Departure = lubridate::as_datetime(Departure)) %>% 
 mutate(month = lubridate::month(lubridate::as_date(Date), abbr=T, label= T),
         mechTime = difftime(Arrival, Call_Placed, units = 'mins'))
servicing.db$Component[servicing.db$Component == ""] <- "Other"
servicing.db$Component[servicing.db$Component == "NA"] <- "Other"
servicing.db$Component[is.na(servicing.db$Component)] <- "Other"

servicing.db$LateCB <- 0
servicing.db$LateCB[servicing.db$mechTime >150] <- 1

servicing.db$Entrapments <- 0
servicing.db$Entrapments[servicing.db$Call_Reason == "Entrapment"] <- 1

client_id <- session$userData$clientID
print(client_id)
# client_id <- "OmKcuNXfR7iOziUe"
buildings <- dbGetQuery(connect_to_db(), paste("SELECT * FROM buildings WHERE ID_Client = '",client_id,"'", sep = ''))

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
names(servicing.db)[which(names(servicing.db)=='month')] <- "Month"

# UI ----
output$pageStub <- renderUI(fluidPage(
  tags$style(type="text/css",
             ".shiny-output-error { visibility: hidden; }",
             ".shiny-output-error:before { visibility: hidden; }"
  ),
  tags$head(tags$script('
                        var dimension = [0, 0];
                        $(document).on("shiny:connected", function(e) {
                        dimension[0] = window.innerWidth;
                        dimension[1] = window.innerHeight;
                        Shiny.onInputChange("dimension", dimension);
                        });
                        $(window).resize(function(e) {
                        dimension[0] = window.innerWidth;
                        dimension[1] = window.innerHeight;
                        Shiny.onInputChange("dimension", dimension);
                        });
                        ')),
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
            flowLayout(box(width = 6,plotlyOutput("Calls"))),
            flowLayout(column(width = 6, plotlyOutput("Components"),
                tags$style(type='text/css', "#Components {margin-top: 25px; margin-left: -65px;}")), 
                column(width = 6, offset = 12, plotlyOutput("Entrapments")),
                     tags$style(type='text/css', "#Entrapments {margin-top: 25px;margin-left: 75px;}")))
                  
                
            
           
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

observeEvent(input$dimension,{
  output$Pmaint <- renderPlotly({
  if(input$Address== "All" & input$Month== "All") {
   rServicing() %>%
    # temp %>% 
    plot_ly(width = 0.60*as.numeric(input$dimension[1]), 
            height = 0.37*as.numeric(input$dimension[2]),
            x=~Month, 
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
             width = (0.60*as.numeric(input$dimension[1])),
             height = 0.37*as.numeric(input$dimension[2]),
             y = ~Address,
             x = ~PM.ReqHrs,
             name = 'Required Hours' 
            ) %>%
    add_trace(x= ~PM.PerfHrs, 
              name = 'Recorded Hours'
              ) %>%
    layout(title = ~Month,
           xaxis = list(title = 'Hours'),
           yaxis = list(title = 'Address'),
           barmode = 'overlay',
           colorway = c('#FF0000','#00cc00')
          )
   }else if (input$Month == "All") {
    rServicing() %>%
     plot_ly(width = (0.60*as.numeric(input$dimension[1])),
             height = 0.37*as.numeric(input$dimension[2]),
            x=~Month, 
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
   plot_ly(width = (0.32*as.numeric(input$dimension[1])), 
           height = 0.32*as.numeric(input$dimension[2]),
     labels = c('Recorded Hours', 'Required Hours'),values = c(~PM.PerfHrs, ~PM.MissHrs)) %>%
    add_pie(hole = 0.5) %>%
    layout(title =~Address,  showlegend = T,
           # xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
           # yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE)),
          colorway = c('#FF0000','#00cc00'))
  }
  
   
 })
})
#Preventative Maintenance Table ----
output$servicing <- DT::renderDataTable(
 DT::datatable(rServicing(),
               selection = 'single',
               filter = 'bottom',
               extensions = c('Buttons', 'ColReorder', 'FixedHeader', 'Scroller'),
               rownames = F,
               options = list(
                 pageLength = 25,
                 dom = 'Bfrtip',
                 searching = T,
                 colReorder = T,
                 fixedHeader = T,
                 filter = 'top',
                 paging = T,
                 deferRender = T,
                 scroller = T,
                 searchHighlight = T,
                 scrollX = T,
                 scrollY = T,
                 buttons = c('copy', 'csv', 'excel', 'pdf', 'print')))
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
    # filter(Month == 'Jan') %>%
    filter(Month == input$Month)%>%
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
 
 rEntrapments <-reactive({
   if(input$Address== "All" & input$Month== "All") {
     entrapments <- servicing.db %>% 
       dplyr::select(Month, Entrapments) %>%
     group_by(Month) %>%
       summarise(Entrapments = sum(Entrapments), 
                 serviceCount = n()
                 ) %>%
       mutate(nonEntrapment = serviceCount - Entrapments)
     # temp <- servicing.db %>%
     #   dplyr::select(Month, Entrapments) %>%
     # group_by(Month) %>%
     #   summarise(Entrapments = sum(Entrapments),
     #             serviceCount = n()) %>%
     #   mutate(nonEntrapment = serviceCount - Entrapments)
     } 
   else if (input$Address == "All") {
     entrapments <- servicing.db %>% 
       dplyr::select(Month, Entrapments) %>%
       filter(Month == input$Month)%>%
       group_by(Month) %>%
       summarise(Entrapments = sum(Entrapments), serviceCount = n()) %>%
     mutate(nonEntrapment = serviceCount - Entrapments)} 
   else if (input$Month == "All") {
     entrapments <- servicing.db %>%
     dplyr::select(Address, Entrapments) %>%
     filter(Address == input$Address)%>%
     group_by(Address) %>%
     summarise(Entrapments = sum(Entrapments), serviceCount = n()) %>%
       mutate(nonEntrapment = serviceCount - Entrapments)}

   else {
     entrapments <- servicing.db %>%
     dplyr::select(Month, Address, Entrapments) %>%
     filter(Month == input$Month, Address == input$Address)%>%
       group_by(Month, Address) %>%
       summarise(Entrapments = sum(Entrapments), serviceCount = n()) %>%
       mutate(nonEntrapment = serviceCount - Entrapments)}
 })
 
 output$Components <- renderPlotly({
  plot_ly(
   data = rComponents(),
   type = 'pie',
   hole = 0.35,
   width = 0.35*as.numeric(input$dimension[1]), 
   height = 0.45*as.numeric(input$dimension[2]),
   labels = ~Component,
   textinfo = "none",
   values = ~compCount
   )%>% layout(title = 'Components', showlegend = FALSE)
 })
 
 output$Calls <- renderPlotly({
  temp <- late_month()
  
  validate(
    need( nrow(temp) > 0, "Data insufficient for plot")
  )
  
  temp %>% 
    plot_ly(
   type = 'bar',
   width = 0.8*as.numeric(input$dimension[1]), 
   height = 0.35*as.numeric(input$dimension[2]),
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
 output$Entrapments <- renderPlotly({
  temp <- rEntrapments()

  validate(
    need( nrow(temp) > 0, "Data insufficient for plot")
     )
  temp %>%
   plot_ly(
     type = 'bar',
     width = 0.40*as.numeric(input$dimension[1]),
     height = 0.45*as.numeric(input$dimension[2]),
     x = ~Month,
     y= ~Entrapments,
     name = 'Entrapments'
   ) %>%
  add_bars(y = ~nonEntrapment, name = 'Other Shutdowns', x = ~Month) %>%
     layout(
       title = 'Shutdowns vs Entrapments',
       yaxis = list(title = 'Shutdowns'),
       xaxis = list(title = 'Month'),
       barmode = 'stack',
       colorway = c('#00cc00','#FF0000'),
       showlegend = FALSE
       )
 })
 

# Load data into temp by running servicing.db run 232-241 to get temp.

