####Libraries####
library(easypackages)
libs<-c("tidyverse", "ProPublicaR", "peRspective", "tidyjson")
libraries(libs)
#### Testing Some Stuff Out ####
dat<-get_house_senate_floor_actions_by_date("house", 2020, 02, 05, 
                                       myAPI_Key = "QPmDQTwRyLD5zbEB3WzurHSXXzY3xHDeiQLSbjRe")
dat$results %>% spread_all

actions<-dat$results %>% enter_object(floor_actions) %>% gather_array %>% spread_all
actions$description

get_statement_subjects(myAPI_Key = "QPmDQTwRyLD5zbEB3WzurHSXXzY3xHDeiQLSbjRe")
get_congressional_statement_by_subjects(subject = "greenhouse-gases", 
                                        myAPI_Key = "QPmDQTwRyLD5zbEB3WzurHSXXzY3xHDeiQLSbjRe")
lists_of_committees(116, "house", myAPI_Key = "QPmDQTwRyLD5zbEB3WzurHSXXzY3xHDeiQLSbjRe")
