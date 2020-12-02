#### Libraries ####
library(easypackages)
libs<-c("tidyverse", "ProPublicaR", "peRspective", "stringr", "readxl", "igraph")
libraries(libs)

setwd("~/Google Drive/UPenn/Coding/R Programs/Personal Research--Penn/Congressional-Record")

# Get list of files
house.list <- paste("H", c(97:114), sep="")
senate.list <- paste("S", c(97:114), sep="")

####Function to get the data####

get.network <- function(sheet) {
  # Get the graph and data (first column is names, so exclude it)
  
  num <- str_sub(sheet, 2)
  dat <- suppressMessages(read_excel("a_sign_of_the_times.xlsx", sheet=sheet, col_names = F))
  dat <- as.matrix(dat)
  graph <- graph_from_adjacency_matrix(dat[,-1], mode=c("upper"))
  V(graph)$name <- dat[,1]
  dat <- as_tibble(dat)
  colnames(dat)[1] <- "name"
  
  # Include Congress number
  V(graph)$congress <- num
  
  # Get out Partisanship
  
  dat <- dat %>%
    mutate(PID=str_sub(name, -2, -2))
  
  dat$party <- ifelse(dat$PID=="D", 1,
                         ifelse(dat$PID=="R", 2, 3))
  
  ## Connect partisanship to graph
  V(graph)$party <- dat$party
  
  return(graph)
}

## Function to get out LCC
get.lcc <- function(graph) {
  # Get out largest connected component
  comps <- components(graph)
  decomp <- decompose(graph)
  LCC.index <- sapply(decomp, function(x) vcount(x)==max(comps$csize))
  LCC <- decomp[LCC.index]
  LCC <- LCC[[1]]
  return(LCC)
}

## Function to get out assortativity by party and community
get.assort <- function(LCC) {
  
  # Detect communities
  LCC.s <- igraph::simplify(LCC, remove.loops = T)
  wt <- cluster_walktrap(LCC.s, modularity = F)
  
  # Assign membership to community
  V(LCC.s)$community <- wt$membership
  
  # Assortativity by party and community
  community <- assortativity.nominal(LCC.s, V(LCC.s)$community, directed = F)
  party <- assortativity.nominal(LCC.s, V(LCC.s)$party, directed = F)
  
  return(c(party, community))
}

## Wrap it all up together
get.it.all <- function(sheets){
  output<-matrix(NA, ncol=4, nrow=length(sheets))
  for(x in 1:length(sheets)){
    network <- get.network(sheets[x])
    LCC <- get.lcc(network)
    assort <- get.assort(LCC)
    assort <- as.numeric(assort)
    output[x, 1] <- assort[1]
    output[x, 2] <- assort[2]
    output[x, 3] <- length(V(LCC))
    output[x, 4] <- length(V(LCC))/length(V(network))
  }
  output <- as_tibble(output)
  colnames(output)<-c("party", "community", "LCC.size", "LCC.perc")
  return(output)
}

## Get some plots out
get.plots <- function(sheet){
  graph <- get.network(sheet = sheet)
  lcc <- get.lcc(graph = graph)
  coords <- layout_with_fr(lcc)
  V(lcc)$color <- ifelse(V(lcc)$party==1, 'blue', 
                              ifelse(V(lcc)$party==2, 'red', 'green'))
  
  par(mar = c(.1,.1,1,.1)) 
  plot <- plot(lcc, layout=coords, vertex.color=V(lcc)$color, 
         vertex.label=NA, vertex.size=5, main=sheet)
  
  return(plot)
  
}

# Run my analyses

house <- get.it.all(house.list)
senate <- get.it.all(senate.list)

house$congress <- c(97:114)
senate$congress <- c(97:114)

house$tox <- means.h100
senate$tox <- means.s100

m.house<-lm(tox~party, data=house)
m.senate<-lm(tox~party, data=senate)

plot(house$party, house$tox)
plot(senate$party, senate$tox)

ggplot(aes(x=party, y=tox), data=house)+
  geom_point()+
  geom_smooth(method=lm, color='red')+
  xlab("Party Polarization in the House (Network Assortativity)")+
  ylab("Toxicity of Speech in the House")+
  ggtitle("Party Polarization and Toxicity in the House")+
  theme_bw()

ggplot(aes(x=party, y=tox), data=senate)+
  geom_point()+
  geom_smooth(method=lm, color='red')+
  xlab("Party Polarization in the Senate (Network Assortativity)")+
  ylab("Toxicity of Speech in the Senate")+
  ggtitle("Party Polarization and Toxicity in the Senate")+
  theme_bw()

# Polarization Figures over time

sapply(house.list, get.plots)
sapply(senate.list, get.plots)

dat.h.network <- lapply(house.list, function(x) get.lcc(get.network(x)))

do.call(union, dat.h.network)
####Things I'm not using ####


# data in and read as graph object; names attached
dat.97 <- read_excel("a_sign_of_the_times.xlsx", sheet="H97", col_names = F)
dat.97 <- as.matrix(dat.97)
graph.97 <- graph_from_adjacency_matrix(dat.97[,-1], mode=c("upper"))
V(graph.97)$names <- dat.97[,1]
dat.97 <- as_tibble(dat.97)
colnames(dat.97)[1] <- "names"

# Get out Partisanship

dat.97 <- dat.97 %>%
  mutate(PID=str_sub(names, -2, -2))

dat.97$party <- ifelse(dat.97$PID=="D", 1,
                       ifelse(dat.97$PID=="R", 2, 3))

## Connect partisanship to graph
V(graph.97)$party <- dat.97$party

# Get out largest connected component
comps.97 <- components(graph.97)
decomp.97 <- decompose(graph.97)
LCC.97.index <- sapply(decomp.97, function(x) vcount(x)==max(comps.97$csize))
LCC.97 <- decomp.97[LCC.97.index]
LCC.97 <- LCC.97[[1]]


## Plot LCC
coords.lcc97 <- layout_with_fr(LCC.97)
plot(LCC.97, layout=coords.lcc97, vertex.label=V(LCC.97)$names, vertex.size=1)

## Detect communities
LCC.97.s <- igraph::simplify(LCC.97, remove.loops = T)
wt.97 <- cluster_walktrap(LCC.97.s, modularity = F)

## Compare communities to partisanship
assortativity.nominal(LCC.97.s, V(LCC.97.s)$party, directed = F)

V(LCC.97.s)$community <- wt.97$membership
assortativity.nominal(LCC.97.s, V(LCC.97.s)$community, directed = F)

## Color by partisanship
V(LCC.97.s)$color <- ifelse(V(LCC.97.s)$party==1, 'blue', 
                            ifelse(V(LCC.97.s)$party==2, 'red', 'green'))
plot(LCC.97.s, layout=coords.lcc97, vertex.color=V(LCC.97.s)$color, 
     vertex.label=NA, vertex.size=5)



