library(shiny)
library(dplyr)
library(plotly)
library(data.table)
library(shinydashboard)
library(dbplyr)
library(RMySQL)
library(DBI)
library(tidyr)

servicing.db <- dbGetQuery(connect_to_db(), "SELECT * FROM servicing")

#Formatting Date strings
servicing.db <- servicing.db  %>% slice(-1) %>%
  mutate(Call_Placed = hms::as_hms(Call_Placed), 
                                         Call_Returned = hms::as_hms(Call_Returned),
                                         Arrival = hms::as_hms(Arrival),
                                         Departure = hms::as_hms(Departure)) %>% 
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
names(servicing.db)[which(names(servicing.db)=='month')] <- "Month"
servicing.db <- merge(
  x= servicing.db,
  y= buildings[,c('ID_Building','Address', 'PM.ReqHrs')],
  by= 'ID_Building'
)
servicing.db$MechDur <- round(difftime(servicing.db$Departure,servicing.db$Arrival,units='hours'),1)

monthList <- data.frame(Month = lubridate::month(seq.Date(ymd('2019/1/1'),ymd('2019/12/1'),'month'), 
                                                 abbr=T, label=T))
fullService <-merge(
  x = merge(
    x = monthList,
    y= servicing.db %>% 
      group_by(Month, Address)%>%
      filter(Type == 0) %>%
      summarise(PM.PerfHrs = sum(MechDur, na.rm = T)),
    by = 'Month',
    all.x = T
  ),
  y = merge(
    x=monthList,
    y=buildings
  ),
  by = c('Month','Address'),
  all.y = T
)


fullService$PM.PerfHrs[is.na(fullService$PM.PerfHrs)] <- 0
fullService <- fullService %>% ungroup()
names(fullService)[1] <- "Month"


# UI ----
output$pageStub <- renderUI(fluidPage(
  style = "max-height: 100vh; overflow-y: auto;" ,
  tags$style(type="text/css",
             ".shiny-output-error { visibility: hidden; }",
             ".shiny-output-error:before { visibility: hidden; }"
  ),
  tags$head(tags$script('
                        var dimension = [0, 0];
                        $(document).on("shiny:sessioninitialized", function(e) {
                        dimension[0] = window.innerWidth;
                        dimension[1] = window.innerHeight;
                        Shiny.onInputChange("dimension", dimension);
                        });
                        $(window).resize(function(e) {
                        dimension[0] = window.innerWidth;
                        dimension[1] = window.innerHeight;
                        Shiny.onInputChange("dimension", dimension);
                        });')
                       ),
 if(loggedIn) {actionButton('logoutBtn','Logout', style= 
                             'position: fixed; right: 10px;top: 5px;')},
  title = 'BOCA',
  sidebarLayout(sidebarPanel(width = 2,
   selectInput('Month',label= 'Select Month',
     c("All",as.character(monthList$Month))),
   selectInput('Address','Select Address', c("All",buildings$Address))
   ),
  mainPanel(
   tabsetPanel(
    
    tabPanel('Preventive Maintenance',
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
            fluidRow(column(width = 12,plotlyOutput("Calls"))),
            fluidRow(column(width = 6, plotlyOutput("Entrapments")),
                     # tags$style(type='text/css', "#Entrapments {margin-top: 25px;margin-left: 105px;}"), 
                     column(width = 6, plotlyOutput("Components"),
                # tags$style(type='text/css', "#Components {margin-top: 25px; margin-left: -125px;}")
                )))
    ,
    tabPanel('Device Designation',
             br(),
             fluidRow(width = 12, plotlyOutput("rEntrapmentsElev"))
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
SiteHeight <- ifelse(is.null(input$dimension[2]), 1080, ifelse(abs(SiteHeight - input$dimension[2]) < 300,SiteHeight,input$dimension[2]))
SiteWidth <- ifelse(is.null(input$dimension[1]), 1920, ifelse(abs(SiteWidth - input$dimension[1]) < 20,SiteWidth,input$dimension[1]))
# observeEvent(input$dimension,{
  output$Pmaint <- renderPlotly({
    SiteHeight <- ifelse(is.null(input$dimension[2]), 1080, ifelse(abs(SiteHeight - input$dimension[2]) < 300,SiteHeight,input$dimension[2]))
    SiteWidth <- ifelse(is.null(input$dimension[1]), 1920, ifelse(abs(SiteWidth - input$dimension[1]) < 20,SiteWidth,input$dimension[1]))
  if(input$Address== "All" & input$Month== "All") {

    print(input$dimension[1])
    print(input$dimension[2])
    print("test here")
    print(SiteWidth)
    print(SiteHeight)
    
   rServicing() %>%
    # temp %>% 
    plot_ly(
      # width = 0.70*as.numeric(SiteWidth), 
            # height = 0.37*as.numeric(SiteHeight),
            x=~Month, 
            y= ~PM.ReqHrs,
            type= 'scatter', 
            mode = 'lines',
            name = 'Required Hours') %>%
    add_trace(y=~PM.PerfHrs, 
              mode = 'lines',
              name = 'Performed Hours') %>%
    layout(
     title = 'Preventive Maintenance',
     yaxis = list(title = 'Hours'),
     xaxis = list(title = 'Month'),
     colorway = c('#FF0000','#00cc00')
    )
   } else if (input$Address == "All") {
    rServicing() %>%
     plot_ly(type = 'bar',
             # width = (0.60*as.numeric(SiteWidth)),
             # height = 0.37*as.numeric(SiteHeight),
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
     plot_ly(
       # width = (0.60*as.numeric(SiteWidth)),
             # height = 0.37*as.numeric(SiteHeight),
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
   plot_ly(
     # width = (0.32*as.numeric(SiteWidth)), 
           # height = 0.32*as.numeric(SiteHeight),
     labels = c('Recorded Hours', 'Required Hours'),values = c(~PM.PerfHrs, ~PM.MissHrs)) %>%
    add_pie(hole = 0.5) %>%
    layout(title =~Address,  showlegend = T,
           # xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
           # yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE)),
          colorway = c('#FF0000','#00cc00'))
  }
  
   
 })
# })
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
    filter(Type == 1) %>%
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
    group_by(Month, Address) %>%
    # filter(Month == 'Jan') %>%
    filter(Month == input$Month, Type == 1)%>%
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
    filter(Address == input$Address, Type == 1)%>%
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
           Month == input$Month, Type == 1)%>%
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
     filter(Type == 1) %>%
    group_by(Component) %>%
    summarise(compCount = n())
  }
  else if (input$Address == "All") {
   components <- servicing.db %>%
   group_by(Month,Component) %>%
   filter(Month == input$Month, Type == 1)%>%
   summarise(compCount = n())}
  else if (input$Month == "All") {
   components <- servicing.db %>%
    group_by(Address,Component) %>%
    filter(Address == input$Address, Type == 1)%>%
    summarise(compCount = n())}
  else {
   components <- servicing.db %>%
    group_by(Month,Address,Component) %>%
    filter(Address == input$Address,
           Month == input$Month, Type == 1)%>%
    summarise(compCount = n())}
 })
 
 rEntrapments <-reactive({
   if(input$Address== "All" & input$Month== "All") {
     entrapments <- servicing.db %>% 
       filter(Type == 1) %>%
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
       filter(Month == input$Month, Type == 1)%>%
       dplyr::select(Address, Month, Entrapments) %>%
       # filter(Month == "Jan")%>%
       group_by(Address, Month) %>%
       summarise(Entrapments = sum(Entrapments), serviceCount = n()) %>%
     mutate(nonEntrapment = serviceCount - Entrapments)
     } 
   else if (input$Month == "All") {
     entrapments <- servicing.db %>%
       filter(Address == input$Address, Type == 1)%>%
     dplyr::select(Month, Address, Entrapments) %>%
     group_by(Address, Month) %>%
     summarise(Entrapments = sum(Entrapments), serviceCount = n()) %>%
       mutate(nonEntrapment = serviceCount - Entrapments)}

   else {
     entrapments <- servicing.db %>%
       filter(Month == input$Month, Address == input$Address, Type == 1)%>%
     dplyr::select(Month, Address, Entrapments) %>%
       group_by(Month, Address) %>%
       summarise(Entrapments = sum(Entrapments), serviceCount = n()) %>%
       mutate(nonEntrapment = serviceCount - Entrapments)}
 })
 
 
 output$Components <- renderPlotly({
   SiteHeight <- ifelse(is.null(input$dimension[2]), 1080, ifelse(abs(SiteHeight - input$dimension[2]) < 300,SiteHeight,input$dimension[2]))
   SiteWidth <- ifelse(is.null(input$dimension[1]), 1920, ifelse(abs(SiteWidth - input$dimension[1]) < 20,SiteWidth,input$dimension[1]))
  plot_ly(
   data = rComponents(),
   type = 'pie',
   hole = 0.35,
   textposition = 'inside',
   textinfo = 'label+percent',
   # width = 0.35*as.numeric(SiteWidth), 
   # height = 0.45*as.numeric(SiteHeight),
   labels = ~Component,
   textinfo = "none",
   values = ~compCount
   
   )%>% layout(title = 'Components', showlegend = FALSE)
 })
 
 output$Calls <- renderPlotly({
   SiteHeight <- ifelse(is.null(input$dimension[2]), 1080, ifelse(abs(SiteHeight - input$dimension[2]) < 300,SiteHeight,input$dimension[2]))
   SiteWidth <- ifelse(is.null(input$dimension[1]), 1920, ifelse(abs(SiteWidth - input$dimension[1]) < 20,SiteWidth,input$dimension[1]))
   temp <- late_month()
   
   validate(
     need( nrow(temp) > 0, "Data insufficient for plot")
   )
   if (input$Address == "All"& input$Month != 'All') {
     temp %>% 
       plot_ly(
         type = 'bar',
         # width = 0.8*as.numeric(SiteWidth),
         # height = 0.35*as.numeric(SiteHeight),
         x = ~`0`,
         y = ~Address,
         name = 'On-Time' 
       ) %>%
       add_trace(x= ~`1`, name = 'Late') %>%
       layout(
         title = 'Service Calls per Month',
         yaxis = list(title = 'Address'),
         xaxis = list(title = 'Calls'),
         barmode = 'stack',
         colorway = c('#00cc00','#FF0000')
       )
   }else {
     temp %>% 
       plot_ly(
         type = 'bar',
         # width = 0.8*as.numeric(SiteWidth),
         # height = 0.35*as.numeric(SiteHeight),
         x = ~Month,
         y = ~`0`,
         name = 'On-Time' 
       ) %>%
       add_trace(y= ~`1`, name = 'Late') %>%
       layout(
         title = 'Service Calls per Month YTD 2020',
         yaxis = list(title = 'Calls'),
         xaxis = list(title = 'Month'),
         barmode = 'stack',
         colorway = c('#00cc00','#FF0000')
       )
     
   }
 })
 output$Entrapments <- renderPlotly({
   SiteHeight <- ifelse(is.null(input$dimension[2]), 1080, ifelse(abs(SiteHeight - input$dimension[2]) < 300,SiteHeight,input$dimension[2]))
   SiteWidth <- ifelse(is.null(input$dimension[1]), 1920, ifelse(abs(SiteWidth - input$dimension[1]) < 20,SiteWidth,input$dimension[1]))
  temp <- rEntrapments()

  validate(
    need( nrow(temp) > 0, "Data insufficient for plot")
     )
  if (input$Address == "All" & input$Month != 'All') {
    temp %>% 
      plot_ly(
        type = 'bar',
        # width = 0.55*as.numeric(SiteWidth),
        # height = 0.35*as.numeric(SiteHeight),
        x = ~Entrapments,
        y = ~Address,
        name = 'Entrapments' 
      ) %>%
      add_trace(x= ~nonEntrapment, name = 'Other Shutdowns') %>%
      layout(
        title = 'Entrapments and Shutdowns 2020 YTD',
        yaxis = list(title = 'Address'),
        xaxis = list(title = 'Calls'),
        barmode = 'stack',
        colorway = c('#FF0000','#FFFF00'),
        showlegend = TRUE
        )
    
    
  }else{temp %>%
   plot_ly(
     type = 'bar',
     # width = 0.40*as.numeric(SiteWidth),
     # height = 0.40*as.numeric(SiteHeight),
     x = ~Month,
     y= ~Entrapments,
     name = 'Entrapments'
   ) %>%
  add_bars(y = ~nonEntrapment, name = 'Other Shutdowns', x = ~Month) %>%
     layout(
       title = 'Entrapments and Shutdowns 2020 YTD',
       yaxis = list(title = 'Event'),
       xaxis = list(title = 'Month'),
       barmode = 'stack',
       colorway = c('#FF0000','#FFFF00'),
       showlegend = TRUE
       )
  }
 })
 
 #Device Designation Plots
 
 rEntrapmentsElev <-reactive({

   validate(
     need(input$Address != "All", message = "Must select address")
   )
    
   if (input$Month == "All" & input$Address != "All") {
     EntrapmentsElev <-
       servicing.db %>%
       filter(Address == input$Address, Type == 1)%>%
       dplyr::select(Dev_Des, Call_Reason, Address) %>%
       mutate(Call_Reason = ifelse(Call_Reason != "Entrapment", "Shutdown", "Entrapment")) %>%
       # filter(Address == "3 CHRYSLER ROAD")%>%
       group_by(Dev_Des, Call_Reason) %>%
       summarise(Call_Reason_Count = n()) %>%
       spread(key = Call_Reason, value = Call_Reason_Count, fill = 0)
   }


   
   
   else if (input$Month != "All" & input$Address != "All"){
     EntrapmentsElev <-
     servicing.db %>%
     filter(Address == input$Address, Month == input$Month, Type == 1)%>%
     dplyr::select(Dev_Des, Call_Reason, Address, Month) %>%
     mutate(Call_Reason = ifelse(Call_Reason != "Entrapment", "Shutdown", "Entrapment")) %>%
     # filter(Address == "3 CHRYSLER ROAD" & Month == "Jan")%>%
     group_by(Dev_Des, Call_Reason, Month) %>%
     summarise(Call_Reason_Count = n()) %>%
     spread(key = Call_Reason, value = Call_Reason_Count, fill = 0)
     }
   
     })
output$rEntrapmentsElev <- renderPlotly({

 elevplot <- rEntrapmentsElev()


if (input$Month == "All" & input$Address != "All") {
  elevplot %>%
     plot_ly(
       type = 'bar',
       # width = 0.40*as.numeric(SiteWidth),
       # height = 0.45*as.numeric(SiteHeight),
       x = ~Dev_Des,
       y= ~Entrapment,
       name = 'Entrapments'
     ) %>%
     add_bars(y = ~Shutdown, name = 'Other Shutdowns') %>%
     layout(
       title = 'Shutdowns and Entrapments 2020 YTD',
       yaxis = list(title = 'Event'),
       xaxis = list(title = 'Elevator'),
      
       barmode = 'stack',
       colorway = c('#FF0000','#FFFF00'),
       showlegend = TRUE
     )}
 
 else if (input$Month != "All" & input$Address != "All") {
   elevplot %>%
     plot_ly(
       type = 'bar',
       # width = 0.40*as.numeric(SiteWidth),
       # height = 0.45*as.numeric(SiteHeight),
       x = ~Dev_Des,
       y= ~Entrapment,
       name = 'Entrapments'
     ) %>%
     add_bars(y = ~Shutdown, name = 'Other Shutdowns') %>%
     layout(
       title = 'Shutdowns and Entrapments',
       yaxis = list(title = 'Event'),
       xaxis = list(title = 'Elevator'),
       barmode = 'stack',
       colorway = c('#FF0000','#FFFF00'),
       showlegend = TRUE
     )}
   
   
   
 
 })

# rshutdowntime <- reactive(
#   round(difftime(servicing.db$Departure,servicing.db$Call_Placed,units='hours'),1)
#   totalservicehours <-
#     servicing.db %>%
#     dplyr::select(Dev_Des, Call_Placed, Address, Departure) %>%
#     # filter(Address == input$Address)%>%
#     filter(Address == "3 CHRYSLER ROAD")%>%
#     group_by(Dev_Des)
# )
 
# Load data into temp by running servicing.db run 232-241 to get temp.

