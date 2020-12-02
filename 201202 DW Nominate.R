####Libraries####
library(easypackages)
libs<-c("tidyverse", "ProPublicaR", "peRspective", "stringr", "tidytext", "lubridate", 
        "topicmodels", "tm")
libraries(libs)

#### Bringing in DW-Nominate Data and Run analyses ####

dw.nominate <- read_csv("DW-Nom Chamber-Congress.csv")
house.dw <- dw.nominate[dw.nominate$chamber=="House" & dw.nominate$score=="party.mean.diff.d1", ]
senate.dw <- dw.nominate[dw.nominate$chamber=="Senate" & dw.nominate$score=="party.mean.diff.d1",]

house <- house %>%
  full_join(house.dw, by="congress")

senate <- senate %>%
  full_join(senate.dw, by="congress")
# Make some Figures

ggplot(aes(x=value, y=tox), data=house)+
  geom_point()+
  geom_smooth(method=lm, color='red')+
  xlab("Party Polarization in the House (DW-Nominate)")+
  ylab("Toxicity of Speech in the House")+
  ggtitle("Party Polarization and Toxicity in the House")+
  theme_bw()

ggplot(aes(x=value, y=tox), data=senate)+
  geom_point()+
  geom_smooth(method=lm, color='red')+
  xlab("Party Polarization in the Senate (DW-Nominate)")+
  ylab("Toxicity of Speech in the Senate")+
  ggtitle("Party Polarization and Toxicity in the Senate")+
  theme_bw()



