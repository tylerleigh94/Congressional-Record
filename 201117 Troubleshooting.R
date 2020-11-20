#Refer to 201108 Backup data.R
#this is troubleshooting of the functions in that file

##Try to figure out the errors
set.seed(1123546) # this produces an error at file.index[4]
for(i in 1:length(file.index)){
  sample.n(file.index[i], chamber='H', 1000)
  print(file.index[i])
}

dat.test <- read.delim(file=file.index[4], sep="|", skipNul = T, 
                       fileEncoding = "ISO-8859-1", quote = "")

set.seed(112378)
dat.sample <- dat.test[sample(1:nrow(dat.test), 1000),]
dat.tox.test <- toxicity.function(dat.sample) # the error does not appear to replicate down the chain


for(i in 1:nrow(dat)){
  print(i)
  test<- toxicity.function(dat[i,])
}

