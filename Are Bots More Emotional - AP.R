# UC San Diego
# Text as Data, Molly Roberts
# Final Project, Hernan Picatto, Anton Prokopyev

# Clear all
rm(list = ls())

#### Install packages ####
library(RSocrata)
library(tm)
library(stm)
library(NLP)
library(SnowballC)
library(plyr)
library(stringr)
library(base)
library(syuzhet)
library(quanteda)
library(wordcloud)

#### Read in data ####
primary <- read.csv("Final_Project/data_merged.csv")
primary[c("uscore")][is.na(primary[c("uscore")])] <- 9999
primary$botornot <- ifelse(primary$uscore == 9999, "unknown", ifelse(primary$uscore < 0.5, "human", "bot"))

#### Process text data ####
processed <- textProcessor(primary$Contents, primary)
names(processed)
out <- prepDocuments(processed$documents, processed$vocab, 
                     processed$meta)

#### Run Structural Topic Modelling Algorithm ####
out$meta$date <- as.Date(out$meta$Date, format="%m/%d/%y")
stm.out <- stm(out$documents, out$vocab, K=30,
              prevalence = ~ Party + s(date),
              data=out$meta, init.type="Spectral")
save(stm.out, file="STMDebates.RData")

#### Review Output Topics ####
table(primary$Party)
load("STMDebates.RData")
labelTopics(stm.out)
plot.STM(stm.out, n=10)
findThoughts(stm.out, out$meta$Text, topics=11, n=10)
findThoughts(stm.out, out$meta$Text, topics=22, n=10)

out$meta$date <- as.numeric(out$meta$date)
prep <- estimateEffect(c(1:30) ~ Party + s(date), stm.out, out$meta)
plot.estimateEffect(prep, "Party", method="difference",
                    cov.value1="Democratic", cov.value2="Republican",
                    verbose.labels=F, labeltype = "frex", model=stm.out,
                    topics=1:10)
library(igraph)
mod.out.corr <- topicCorr(stm.out)
plot(mod.out.corr)

#### Visualize Topics Using STM Browser ####
library(devtools)
install_github("mroberts/stmBrowser",dependencies=TRUE)
library(stmBrowser)
out$meta$date <- as.Date(out$meta$Date, format="%m/%d/%y")
stmBrowser(stm.out, data=out$meta, c("Party", "date"), text="Text", n=nrow(out$meta))
#With a content covariate
##stm.out <- stm(out$documents, out$vocab, K=30, 
#               prevalence = ~ Party + s(date), content= ~Party, 
#               data=out$meta, init.type="Spectral")
#save(stm.out, file="STMDebates_Content.RData")
load("STMDebates_Content.RData")
sageLabels(stm.out)
plot(stm.out, "perspectives", topics=20)
plot(stm.out, "perspectives", topics=4)