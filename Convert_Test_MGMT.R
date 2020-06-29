library(tidyverse)
library(readxl)
library(dplyr)
client <- read_excel("MockData.xlsx", sheet = 1)
buildings <- read_excel("MockData.xlsx", sheet = 2)
users <- read_excel("MockData.xlsx", sheet = 3)
servicing <- read_excel("MockData.xlsx", sheet = 4)
elevators <- read_excel("MockData.xlsx", sheet = 5)

TestMgmt <- read_excel('ElevatorData.xlsx', trim_ws = T)

generate_id <- function() {
 newID <- paste(collapse = '', 
                sample(x = c(letters, LETTERS, 0:9), size = 16, replace = TRUE))
 return(newID)
}

#Get new client ####
 my_clients <- TestMgmt %>%
 filter(str_detect(`Client Name`,"AVALON")) %>%
 select(`Client Name`, `Property Manager Phone`,`Property Manager Name`) %>%
 separate(`Client Name`, into = c('Client Name', 'Region'), sep = "-") %>%
 mutate(ID_Client = generate_id()) %>% 
 group_by(`Client Name`) %>% 
  unique() %>%
  slice(1) %>%
 select(-Region) %>%
  ungroup() 
 
 # my_clients <- rbind.data.frame(my_clients, 
 #                                cbind.data.frame(
 #                                  `Client Name` = 'AVALON BAY',
 #                                  `Property Manager Phone` = 860999333,
 #                                  `Property Manager Name` = 'Avalon Master',
 #                                  ID_Client = generate_id()
 #                                   )
 #                                )
names(my_clients) <- c('Client', 'Phone', 'Contact','ID_Client')
my_clients <- my_clients %>% select(ID_Client, Client, Phone, Contact)

client <- rbind.data.frame(client, my_clients)

#Get new Buildings ####
my_buildings <- TestMgmt %>%
filter(str_detect(`Client Name`,"AVALON")) %>%
group_by(`Street Address`) %>%
select(`Street Address`, `DeviceCount`, `Client Name`) %>%
separate(`Client Name`, into = c('Client Name', 'Region'), sep = "-") %>% 
select(-`Client Name`) %>% 
mutate(ID_Building = generate_id()) %>%
mutate(ID_Client = generate_id()) %>%
mutate(PM.ReqHrs = sample(10:40, size = 1, replace = TRUE))%>%
unique() %>%
  slice(1) %>%
  ungroup()

buildings$Region <- NA

names(buildings)
names(my_buildings) <- c('Address', 'Elevators','Region', 'ID_Building', 'ID_Client', 'PM.ReqHrs')
my_buildings <- my_buildings %>% select(ID_Building, ID_Client, Address, Elevators, PM.ReqHrs, Region)

buildings <- rbind.data.frame(buildings, my_buildings)

#Get new users ####
my_rows <- seq(1, length.out = nrow(my_buildings), by = 1)
my_funk <- function(x){
  return(paste("testdummy", x, sep = ""))
  
}
my_users <- unlist(lapply(my_rows, my_funk))
my_pass <- "password"
my_security <- "0"
my_ID_Client <- my_clients$ID_Client
my_ID_Building <- my_buildings$ID_Building

my_ID_User <- c()
for (i in my_rows) {my_ID_User <- c(my_ID_User, generate_id())
  
}


users <-cbind.data.frame(
  ID_User = my_ID_User, 
  ID_Client = my_ID_Client, 
  Username = my_users, 
  Password = my_pass, 
  Security = my_security, 
  ID_Building = my_ID_Building)


#Get new Elevators ####




my_elevators <- merge(
  x = TestMgmt,
  y = my_buildings, 
  by.x = "Street Address",
  by.y = "Address"
) %>%
filter(str_detect(`Client Name`,"AVALON")) %>%
select(`Building Car Number`, ID_Client, ID_Building) %>%
unique()
  

names(my_elevators)
names(my_elevators) <- c('Dev_Des', 'ID_Client', 'ID_Building')
  
elevators <- my_elevators
