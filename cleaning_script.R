#Check for/install necessary packages

##create function to load needed packages 
#load_install<-function(lib){
#  if(! require(lib, character.only=TRUE)) {
#    install.packages(lib, character.only=TRUE) }
#  library(lib, character.only=TRUE)
#}
##the required packages
#theLib<-c("plyr", "ggplot2", "scales", "gdata", "chron", "reshape2", "grid", "maps", "mapdata")
##apply
#lapply(theLib, load_install)

#Load packages
library(ggplot2)
library(scales)

#Load data

setwd("/Users/florenceclee/Desktop/qmssviz-project/") #edit to reflect your directory
rowca <- read.csv("ebola-dat-rowca-updated.csv", stringsAsFactors=FALSE)

#Explore

##function to check for NAs
check <- function(data) {
  NAs <- sum(is.na(data))
  print(paste("NAs:", NAs))     # count NA's
  if (NAs > 0) {
    cc <- complete.cases(data)  # logical for each case (row)
    print(paste("Complete Cases:", all(cc)))  
    data[which(!complete.cases(data)),]
  }
}

rowca$Date <- as.Date(rowca$Date, "%m/%d/%y")
##value
numNoCom <- function(df, col) {
  as.numeric(gsub(",", "", df[,col]))
}
rowca$Value <- numNoCom(rowca, "Value")

rowca <- rowca[order(rowca$Country, rowca$Localite, rowca$Date),]
##Cases = Confirmed cases + New cases + Probable cases + Suspected cases
##okay to use Cases and Deaths only

rowcaSub <- subset(rowca, rowca$Category %in% c("Cases", "Deaths"))

#Clean Liberia data

liberia <- subset(rowcaSub, rowcaSub$Country == "Liberia", select = c(Country, Localite, Category, Value, Date))
##get rid of redundant County
liberia$Localite <- gsub("(?:County)","", liberia$Localite)
##function to trim trailing spaces
trim <- function(x) {
  #gets rid of trailing spaces
  gsub("^\\s+|\\s+$", "", x)
}
##trim trailing spaces
liberia$Localite <- trim(liberia$Localite)

#Order by Date
liberia <- liberia[order(liberia$Country, liberia$Date),]


#Clean Sierra Leone dataset 
sierraLeone <- subset(rowcaSub, rowcaSub$Country == "Sierra Leone", select = c(Country, Localite, Category, Value, Date))


##function to capitalize first letter of each word
simpleCap <- function(x) {
  s <- strsplit(x, " ")[[1]]
  paste(toupper(substring(s, 1,1)), substring(s, 2),
        sep="", collapse=" ")
}

##fix capitalization and trailing space
sierraLeone$Localite <- trim(sapply(sierraLeone$Localite, simpleCap))


##fix 'port'
for(i in which(sierraLeone$Localite=="Port")) {
  sierraLeone$Localite[i] <- "Port Loko"
}


##PROBLEM
##"Western area rural" and "Western area urban" are collapsed into one category for
##June data. It's not a big deal, since it's only two points... but maybe we can 
##collapse all of them?

#Order by Date
sierraLeone <- sierraLeone[order(sierraLeone$Country, sierraLeone$Date),]


#Clean Guinea dataset 
guinea <- subset(rowcaSub, rowcaSub$Country == "Guinea", select = c(Country, Localite, Category, Value, Date))

guinea$Localite <- trim(guinea$Localite)


##fix weird accent translation
for (i in which(guinea$Localite=="For\xe9cariah")) {
  guinea$Localite[i] <- "Forecariah"
}

##noticed some have multiple Localite names
##let's just get rid of those rows

guinea <- guinea[grep("and", guinea$Localite, invert=TRUE),]

#Order by Date
guinea <- guinea[order(guinea$Country, guinea$Date),]


ebolaData <- as.data.frame(rbind(guinea, sierraLeone, liberia))

