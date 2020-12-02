#### Libraries ####
library(easypackages)
libs<-c("tidyverse", "ProPublicaR", "peRspective", "stringr", "tidytext", "lubridate", 
        "topicmodels", "tm")
libraries(libs)

#### Functions ####

# Get the toxicity score for a comment
toxicity <- function(x){
  if(is.null(x)){NA}
  else{prsp_score(
    text = x, 
    languages = "en",
    score_model = "TOXICITY"
  )}
}

# Take a vector of comments and get a vector of toxicity scores
tox.scores <- function(vect){
  scores <- sapply(vect, FUN=toxicity)
  return(unlist(scores))
}

# Take a dataframe with columns "speech" and "id"; get out the vector of tox 
#scores for those comments, then binds this vector to original dataframe
toxicity.function <- function(df){
  data <- df
  vect <- strtrim(df$speech, 20479)
  tox <- tox.scores(vect=vect)
  data$toxicity <- tox
  return(data)
}


#dat <- dat[!is.na(dat$speech_id),]
#test <- dat %>%
#  prsp_stream(text=speech,
#              text_id = speech_id,
#              score_model="TOXICITY",
#              safe_output = T,
#              verbose=T)

#Take a file name, get out the text data, search the matching descriptive file and merge 
#in metadata; create column of comment length; trim strings to max length; 
#filter data based on chamber provided as input;
#get toxicity scores for sample n comments using dependent functions above; 
#return a list of the congress number and a vector of toxicity scores of length n
sample.n <- function(file, chamber=NA, n=NA){
  
  # Get the descriptors
  num <- str_sub(file, -7, -5)
  desc.file <- paste("descr_", num, ".txt", sep="")
  desc <- read.delim(file=desc.file, sep="|", skipNul = T, fileEncoding = "ISO-8859-1", quote="")
  
  #Get the data and merge in descriptors
  dat <- read.delim(file=file, sep="|", skipNul = T, fileEncoding = "ISO-8859-1", quote = "")
  dat$speech_id <- as.character(dat$speech_id)
  desc$speech_id <- as.character(desc$speech_id)
  dat <- left_join(dat, desc, by='speech_id')
  
  # Trim strings to maximum length
  dat <- dat %>%
    mutate(length=nchar(speech, type = 'chars'))
  dat <- dat[dat$length<20480,]
  
  # Drop strings shorter than the mean (bottom 50% of data)
  mean.words <- mean(dat$word_count, na.r=T)
  dat <- dat[dat$word_count > mean.words,]
  
  # Drop if text_id==NA
  dat <- dat[!is.na(dat$speech_id),]
  
  # If chamber provided, filter by chamber
  
  if(is.na(chamber)) {
    dat <- dat
  } else {dat <- dat[dat$chamber==chamber,]}
  
  # If n=NA, get tox scores for all comments, else take a sample size=n
  
  if(is.na(n)){
    dat.out <- dat %>%
      prsp_stream(text=speech,
                  text_id = speech_id,
                  score_model="TOXICITY",
                  safe_output = T,
                  verbose=T)
  }
  else{
  set.seed(num)
  dat.out <- dat[sample(1:nrow(dat), n),] %>%
    prsp_stream(text=speech,
                text_id = speech_id,
                score_model="TOXICITY",
                safe_output = T,
                verbose=T)
  }
  return(list(num, dat.out))
  
}

get.chamber.congress <- function(file, chamber=NA) {
  
  # Get the descriptors
  num <- str_sub(file, -7, -5)
  desc.file <- paste("descr_", num, ".txt", sep="")
  desc <- read.delim(file=desc.file, sep="|", skipNul = T, fileEncoding = "ISO-8859-1", quote="")
  
  #Get the data and merge in descriptors
  dat <- read.delim(file=file, sep="|", skipNul = T, fileEncoding = "ISO-8859-1", quote = "")
  dat$speech_id <- as.character(dat$speech_id)
  desc$speech_id <- as.character(desc$speech_id)
  dat <- left_join(dat, desc, by='speech_id')
  
  # Drop if text_id==NA
  dat <- dat[!is.na(dat$speech_id),]
  
  # If chamber provided, filter by chamber
  
  if(is.na(chamber)) {
    dat <- dat
  } else {dat <- dat[dat$chamber==chamber,]}
  
  return(dat)
}

#### Data ####

setwd("~/Google Drive/UPenn/Coding/R Programs/Personal Research--Penn/Congressional-Record")

file.index <- list.files("hein-daily")
file.index <- file.index[grep("speeches_", file.index)]

setwd("~//Google Drive//UPenn//Coding//R Programs//Personal Research--Penn//Congressional-Record//hein-daily")


# Sample Size of 2000 

h97.2k <- sample.n(file.index[1], chamber = 'H', 2000)[[2]]
h98.2k <- sample.n(file.index[2], chamber = 'H', 2000)[[2]]

h.2000 <- sapply(file.index, FUN=function(x) sample.n(x, chamber='H', 2000))
s.2000 <- sapply(file.index, FUN=function(x) sample.n(x, chamber='S', 2000))

# House and Senate, Samples of 100
h.100 <- sapply(file.index, FUN=function(x) sample.n(x, chamber='H', 100))
s.100 <- sapply(file.index, FUN=function(x) sample.n(x, chamber='S', 100))

setwd("~/Google Drive/UPenn/Coding/R Programs/Personal Research--Penn/Congressional-Record")
save(h.100, file=paste(str_sub(year(today()), 3, 4), month(today()), day(today()), 
                       " House Sample 100", sep = ""))
save(s.100, file=paste(str_sub(year(today()), 3, 4), month(today()), day(today()), 
                       " Senate Sample 100", sep = ""))
load("201129 House Sample 100")
load("201129 Senate Sample 100")


means.h100 <- sapply(h.100[2,], FUN=function(x) mean(x$TOXICITY, na.rm=T))
sds.h100 <- sapply(h.100[2,], FUN=function(x) sd(x$TOXICITY, na.rm=T))
tox.time.h100 <- data.frame(time=97:114, mean=means.h100, std=sds.h100)

ggplot(aes(x=time, y=mean), data=tox.time.h100)+
  geom_point()+
  geom_smooth(method=lm, color='red')+
  ylab("Toxicity of Discourse")+
  xlab("Congress")+
  labs(title="Average Toxicity of Discourse in the House")+
  theme_bw()+
  theme(plot.title = element_text(size=18, hjust=0.5),
        plot.caption = element_text(size=14, hjust = .5), 
        axis.title = element_text(size=16), 
        axis.text = element_text(size=16))

means.s100 <- sapply(s.100[2,], FUN=function(x) mean(x$TOXICITY, na.rm=T))
sds.s100 <- sapply(s.100[2,], FUN=function(x) sd(x$TOXICITY, na.rm=T))
tox.time.s100 <- data.frame(time=97:114, mean=means.s100, std=sds.s100)

ggplot(aes(x=time, y=mean), data=tox.time.s100)+
  geom_point()+
  geom_smooth(method=lm, color='red')+
  ylab("Toxicity of Discourse")+
  xlab("Congress")+
  labs(title="Average Toxicity of Discourse in the Senate")+
  theme_bw()+
  theme(plot.title = element_text(size=18, hjust=0.5),
        plot.caption = element_text(size=14, hjust = .5), 
        axis.title = element_text(size=16), 
        axis.text = element_text(size=16))

write_csv(tox.time.h100, file=paste(str_sub(year(today()), 3, 4), month(today()), day(today()), 
                                         " House Means.csv", sep = ""))
write_csv(tox.time.s100, file=paste(str_sub(year(today()), 3, 4), month(today()), day(today()), 
                                    " Senate Means.csv", sep = ""))

#### Some Extra Code ####
# Sample size of 100 (1.75 hours)

h.100 <- sapply(file.index, FUN=function(x) sample.n(x, chamber='H', 100))

means <- sapply(test.2[2,], FUN=mean)
sds <- sapply(test.2[2,], FUN=sd)

log.means <- sapply(means, FUN=log)

tox.time <- data.frame(time=97:114, mean=means, std=sds)

plot(tox.time$time, tox.time$mean, ylab = "Toxicity of Discourse", xlab="Congress", 
     main="Average Toxicity of Discourse in each Congress (House and Senate Combined)",
     sub="
     
     Figure shows the average toxicity of a random sample of 100 speeches made on the floor 
     of each Congress. Even with a relatively small smaple size, a slight increase in toxicity is 
     apparent as we move through time. Toxicity is our dependent variable, and we generally expect 
     it to increase over time, so seeing a slight increase in our data suggests we are on the 
     right track. It's also reassuring to see that there is variation in our DV.")
abline(lm(mean~time, data=tox.time), col='red')

ggplot(aes(x=time, y=mean), data=tox.time)+
  geom_point()+
  geom_smooth(method=lm, color='red')+
  ylab("Toxicity of Discourse")+
  xlab("Congress")+
  labs(title="Average Toxicity of Discourse in each Congress (House and Senate Combined)",
       caption = str_wrap("Figure shows the average toxicity of a random sample of 100 
                             speeches made on the floor of each Congress. Even with a relatively 
                             small smaple size, a slight increase in toxicity is apparent as we 
                             move through time. Toxicity is our dependent variable, and we 
                             generally expect it to increase over time, so seeing a slight 
                             increase in our data suggests we are on the right track. It's 
                             also reassuring to see that there is variation in our DV.", 125))+
  theme_bw()+
  theme(plot.title = element_text(size=18, hjust=0.5),
        plot.caption = element_text(size=14, hjust = .5), 
        axis.title = element_text(size=16), 
        axis.text = element_text(size=16))

# Working with Tidy Data
dat <- get.chamber.congress(file.index[2], chamber = 'H')

dat.tidy <- dat %>%
  select(speech_id, speech) %>%
  unnest_tokens(word, speech) %>%
  anti_join(stop_words) %>%
  count(speech_id, word) %>%
  cast_dtm(speech_id, word, n)

tidy.LDA <- LDA(dat.tidy, k=3, control= list(alpha=.1, seed = 1234))
tidy.doc.topics <- tidy(tidy.LDA, matrix='gamma')
tidy.word.topics <- tidy(tidy.LDA, matrix='beta')

doc.topics <- tidy.doc.topics %>%
  pivot_wider( id_cols = document, names_from=topic, values_from=gamma, names_prefix="topic")

topic <- apply(doc.topics[,-1], 1, function(x) which(x==max(x)))

word.topics <- tidy.word.topics %>%
  pivot_wider( id_cols = document, names_from=topic, values_from=beta, names_prefix="topic_")

top10 <- tidy.word.topics %>%
  group_by(topic) %>%
  top_n(15, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)

# Playing with bootstrapping
h97.100 <- h.100[[2]]
test.boot <- lapply(h.100[2,], 
                    function(y) boot(data=y$TOXICITY, statistic = function(x, i) mean(x[i]), R=10000))
lapply(test.boot, FUN=plot)

test.ci.boot <- boot.ci(test.boot)




