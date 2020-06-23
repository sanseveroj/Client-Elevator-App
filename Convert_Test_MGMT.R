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
 filter(str_detect(`Client Name`,"AVALON")) %>% #select(`Client Name`) %>% unique()
 select(`Client Name`, `Property Manager Phone`,`Property Manager Name`) %>%
 group_by(`Client Name`) %>%
 mutate(ID_Client = generate_id()) %>%
 unique() %>%
  slice(1) %>%
  ungroup()
 
 my_clients <- rbind.data.frame(my_clients, 
                                cbind.data.frame(
                                  `Client Name` = 'AVALON BAY',
                                  `Property Manager Phone` = 860999333,
                                  `Property Manager Name` = 'Avalon Master',
                                  ID_Client = generate_id()
                                   )
                                )
names(my_clients) <- c('Client', 'Phone', 'Contact','ID_Client')
my_clients <- my_clients %>% select(ID_Client, Client, Phone, Contact)

client <- rbind.data.frame(client, my_clients)

#Get new Buildings ####
my_buildings <- TestMgmt %>%
filter(str_detect(`Client Name`,"AVALON")) %>%
group_by(`Street Address`) %>%
select(`Street Address`, `DeviceCount`) %>%
mutate(ID_Building = generate_id()) %>%
mutate(ID_Client = generate_id()) %>%
mutate(PM.ReqHrs = sample(10:40, size = 1, replace = TRUE))
unique() %>%
  slice(1) %>%
  ungroup()

my_buildings <- rbind.data.frame(my_buildings, cbind.data.frame(
                                              `Street Address` = 'Master Address',
                                              `DeviceCount` = 'Master',
                                              `PM.ReqHrs` = 0,
                                              ID_Building = generate_id(),
                                              ID_Client = generate_id()
))


names(my_buildings) <- c('Address', 'Elevators', 'ID_Building', 'ID_Client', 'PM.ReqHrs')
my_buildings <- my_buildings %>% select(ID_Building, ID_Client, Address, Elevators, PM.ReqHrs)

buildings <- rbind.data.frame(buildings, my_buildings)

  
