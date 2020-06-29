library(markdown)
library(shiny)
library(dplyr)
library(dbplyr)
library(RMySQL)
library(DBI)
library(shinyalert)
library(here) 
library(shinythemes)

#Global Variables ----
site_pages <- tibble(name="login",sp=0)     # in terms of pages, it's the amount of user sp
site_pages <- rbind(site_pages, tibble(name="preventative_maintenance",sp=0))     #    required to open the page.
site_pages <- rbind(site_pages, tibble(name="dashboard",sp=500))
site_pages <- rbind(site_pages, tibble(name="type_choice",sp=0))
site_pages <- rbind(site_pages, tibble(name="call_back_stage1",sp=0))
site_pages <- rbind(site_pages, tibble(name="call_back_stage2",sp=0))
page_debug_on <- F
# setwd("C:/Users/Joseph/Dropbox/Boca/Client Elevator Managment/Application/V3")
killDbConnections <- function () {
 
 all_cons <- dbListConnections(MySQL())
 
 for(con in all_cons)
 {try(dbDisconnect(con))}
 
}

#Database Variables
host <- "boca-2.cg55foopexne.us-east-1.rds.amazonaws.com"
port <- 3306
dbname <- "BOCA_2"
user <- "JoeSans"
password <- "Joe5933547"

# killDbConnections()
my_db <- src_mysql(dbname = dbname,host = host, port = port, user=user,password = password )
cn <- dbConnect(drv = RMySQL::MySQL(), username = user, password= password, host = host, dbname = dbname, port = port)
idList <- list()
Sys.setenv(TZ='America/New_York')
#Global Functions ----
pageGet <- function(webpage) {
 p <- site_pages[webpage==site_pages$name,]
 if(nrow(p)==0) {                                # if the page doesn't exist, p will be an empty tibble,
  p <- tibble(name="", sp=0)                   #    but return a tibble with 1 row, with name blank.
 }
 return(p)
}

generate_id <- function() {
  newID <- paste(collapse = '', sample(x = c(letters, LETTERS, 0:9), size = 16, replace = TRUE))
  return(newID)
}

# style= 'position: relative; right: 10px;top: 5px;'
#service call tracker ----
ui <- fluidPage(theme = shinytheme('sandstone'),
 useShinyalert(),
 actionButton('logoutBtn','Logout', style= 
  'position: fixed; right: 10px;top: 5px;'),
 title="Servicing Client Tracking",
 tagList(
  tags$head(
   tags$script(src="js.cookie.js"),
   tags$script(HTML("
                    Shiny.addCustomMessageHandler('redirect', function(url) {
                    window.location = url;
                    });
                    Shiny.addCustomMessageHandler('setCookie', function(pList) {
                    if(pList.days>0) {
                    Cookies.set(pList.cookieType, escape(pList.cookie), { expires: pList.days });
                    } else {
                    Cookies.set(pList.cookieType, escape(pList.cookie));
                    }
                    });
                    Shiny.addCustomMessageHandler('getCookie', function(pList) {
                    var cookie = Cookies.get(pList.cookieType);
                    if (typeof cookie == 'undefined') { cookie = ''; }
                    Shiny.onInputChange('js.'.concat(pList.cookieType), cookie);
                    });
                    Shiny.addCustomMessageHandler('removeCookie', function(pList) {
                    Cookies.remove(pList.cookieType);
                    });
                    ")),
   uiOutput("uiStub")                   # the actual page will get attached here
   )
  ))

#Server ----
server <- function(input, output, session) {

 # if(page_debug_on) {
 #  cat("Session started.\n")
 #  onSessionEnded(function() {cat("Session ended.\n\n\n")})
 # }
 onSessionEnded(function() {
   # killDbConnections()
   })
 ### Plain old functions that are used by multiple pages go above the server function, so they only load once.
 ###    Reactives used by multiple pages, however, have to be inside the server function and load every session.
 
 rv <- reactiveValues()      # session reactive values
 # for non-reactive variables, use the session$userData environment
 
 rv$limn <- 1                # render/re-render page buzzer
 rv$cookies_baked <- 0       # render menu buzzer
 rv$logout <- 0              # needed for logout.R page
 # rv$modal_warning <- 0       # used with an observer below to bring up modal warning dialogs
 
 
 # Functions for running javascript on the browser
 #   Because these communicate using the session object, they have to be in the server.
 #   In general, the first parameter is the name of the function and the second is a list of named parameters
 #   The javascript code is loaded by the UI
 #   All these functions are stored in the js$ global
 js = list()

 js$redirect = function(url) {
  session$sendCustomMessage("redirect", url)  # This one expects a string, not a list
 }

 js$setCookie = function(cookieType, cookie, daysTillExpire=0) {
  session$sendCustomMessage("setCookie", list(cookieType=cookieType, cookie=cookie, days=daysTillExpire))
 }

 js$getCookie = function(cookieType) {
  session$sendCustomMessage("getCookie", list(cookieType=cookieType))
 }

 js$removeCookie = function(cookieType) {
  session$sendCustomMessage("removeCookie", list(cookieType=cookieType))
 }

 ### All the action starts here! ###
 
 # Wait until javascript files have finished loading before asking for cookie
 # observeEvent(session$clientData$url_port, {  # This is just a hack, but now it's safe
 #  js$getCookie("sessionID")                 #    to request the sessionID from the user's browser
 # })   # Cookie observer to determine login status

 # an observer to send modal warnings
 # to call:
 # session$userData$modal_title <- ""
 # session$userData$modal_text <- ""   embedded HTML is ok
 # rv$modal_warning <- rv$modal_warning + 1
 # observeEvent(rv$modal_warning, {
 #  if(rv$modal_warning>0) {                           # skip initialization
 #   showModal(modalDialog(
 #    title = HTML("<h4>", session$userData$modal_title, "</h4>"),
 #    HTML(session$userData$modal_text),
 #    footer = modalButton("Ok")
 #   ))
 #  }
 # })
 observeEvent(input$logoutBtn, {
  session$userData$user <<- NA
  session$userData$pass <<- NA
  # killDbConnections()
  js$redirect("?login")
 })
 # additional ui for what's the same on all webpages
 output$uiStub <- renderUI(tagList(
  # fluidRow(
  #  column(4,
  #         HTML("<h5>", "site_name", "</h5>")
  # ),
  uiOutput("pageStub")
 ))
 
 # This section shows how to build a menu that's sensitive to whether the user is logged in (user superpower > 0).
 #   Note that you can present various menu options based on the user's superpower level, as with the Admin menu here.
 #   Also note that the code that makes sure this doesn't run until after the cookie observer has finished, because
 #      until then, session$userData$user will be null.
 
 ### End of common reactives, now load the reactives for the page specified in the URL
 #     Note, this cannot be inside a reactive, because it ends by loading source code, which needs to be
 #        in the environment of the server, not inside an observer/reactive
 
 webpage <- isolate(session$clientData$url_search)     # isolate() to deal with reactive context
 if(is.null(webpage)) {                                # report if session$ wasn't ready yet...
  webpage <- "?login"                                 #    ...null means issues to solve
  }
 if(webpage=="") { webpage <- "?login" }                # blank means home page
 webpage <- substr(webpage, 2, nchar(webpage))         # remove leading "?", add ".R"
 p <- pageGet(webpage)
 if(p$name != ""){                                     # is that one of our files?
  webpage <- p                                       # note that this is a tibble
 }
 source(here::here(paste0(webpage$name, ".R")), local=environment())        # load and run server code for this page
} # end of server                                        #    in the server environment

# Run the application
shinyApp(ui = ui, server = server)
