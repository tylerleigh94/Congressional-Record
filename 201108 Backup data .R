#### Libraries ####
library(easypackages)
libs<-c("tidyverse", "ProPublicaR", "peRspective", "stringr")
libraries(libs)

#### Data ####
file.index <- list.files("hein-daily")
file.index <- file.index[grep("speeches_", file.index)]

toxicity <- function(x){
  if(is.null(x)){NA}
    else{prsp_score(
      text = x, 
      languages = "en",
      score_model = "TOXICITY"
  )}
}

tox.scores <- function(vect){
  scores <- sapply(vect, FUN=toxicity)
  return(unlist(scores))
}

toxicity.function <- function(df){
  data <- df
  vect <- strtrim(df$speech, 20479)
  tox <- tox.scores(vect=vect)
  data$toxicity <- tox
  return(data)
  }


setwd("~//Google Drive//UPenn//Coding//R Programs//Personal Research--Penn//Congressional-Record//hein-daily")

sample.n <- function(file, chamber=NA, n){
  
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
  
  # If chamber provided, filter by chamber
  
  if(is.na(chamber)) {
    dat <- dat
  } else {dat <- dat[dat$chamber==chamber,]}
  
  dat.out <- toxicity.function(dat[sample(1:nrow(dat), n),])
  
  return(list(num, dat.out$toxicity))
  
}

# Sample size of 100 (1.75 hours)

test.2 <- sapply(file.index, FUN=function(x) sample.n(x, chamber='H', 100))

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

# Sample Size of 1000 (9:30pm-)

test.3 <- sapply(file.index, FUN=function(x) sample.n(x, 1000))


# Replicating errors with House
set.seed(11345)
et.1 <- sample.n(file.index[2], chamber='H', n=100)

set.seed(6788601)
et.2 <- sample.n(file.index[2], chamber='H', n=100)

set.seed(770663)
et.2 <- sample.n(file.index[2], chamber='H', n=100)

# House and Senate, Samples of 100
h.100 <- sapply(file.index, FUN=function(x) sample.n(x, chamber='H', 100))
s.100 <- sapply(file.index, FUN=function(x) sample.n(x, chamber='S', 100))

means.h100 <- sapply(h.100[2,], FUN=mean)
sds.h100 <- sapply(h.100[2,], FUN=sd)
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

means.s100 <- sapply(s.100[2,], FUN=mean)
sds.s100 <- sapply(s.100[2,], FUN=sd)
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




