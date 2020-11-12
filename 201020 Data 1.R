####Libraries####
library(easypackages)
libs<-c("tidyverse", "ProPublicaR", "peRspective", "tidyjson", "jsonlite", "stringr")
libraries(libs)

####Getting in the Data####
setwd("~/Google Drive/UPenn/Coding/R Programs/Personal Research--Penn/Congressional-Record")
index<-list.files("json")

setwd("~/Google Drive/UPenn/Coding/R Programs/Personal Research--Penn/Congressional-Record/json")

output.fun<-function(x) {
  vect<-unlist(fromJSON(x)$content$text)
  x<-length(vect)
  print(vect)
  vect[(x-1)]
}

dat<-data.frame(unlist(lapply(index, FUN=output.fun)))
dat$index<-c(1:nrow(dat))
colnames(dat)<-c("text", "index")

dat$text<-str_replace_all(dat$text, "[\n]", "")

text_scores<-matrix(NA, nrow = length(nrow(dat)), ncol=1)
for(i in 1:nrow(dat)){
text_scores[i] <- prsp_score(
  text = dat$text[i], 
  languages = "en",
  score_model = "TOXICITY"
)
print(i)
}
dat$toxicity <- unlist(text_scores)






####Tried and not used####
index[1] %>% gather_object %>% json_types %>% count(name, type)
index[1] %>% enter_object(content) %>% gather_array %>% spread_all %>% gather_object %>% json_types %>% count(name, type)
dat <- index[8] %>% enter_object(content) %>% gather_array %>% spread_all %>% select(text)
dat$index<-c(1:length(dat$text))
dat<- dat %>%
  select(text, index) 
