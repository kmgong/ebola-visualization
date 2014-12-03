#Check for/Install Necessary Packages

##create function
load_install<-function(lib){
  if(! require(lib, character.only=TRUE)) install.packages(lib, character.only=TRUE)
  library(lib, character.only=TRUE)
}

##the required libraries (e.g. packages)
theLib<-c("plyr", "ggplot2", "scales", "gdata", "chron", "reshape2", "grid", "maps", "mapdata")

##apply function
lapply(theLib, load_install)


#Create inital data frame

setwd("/Users/florenceclee/Desktop/qmssviz-project/") #edit to reflect your directory
rowca <- read.csv("ebola-dat-rowca-updated.csv", stringsAsFactors=FALSE)
who <- read.csv("ebola-dat-who.csv", stringsAsFactors=FALSE)

#Use ROWCA Data set

##general structure
str(rowca)
names(rowca)

##For NAs
check <- function(data) {
  NAs <- sum(is.na(data))
  print(paste("NAs:", NAs))  # count NA's
  if (NAs > 0) {
    cc <- complete.cases(data)  # logical for each case (row)
    print(paste("Complete Cases:", all(cc)))  
    data[which(!complete.cases(data)),]
  }
}
check(rowca)

# Subsetting data frame

##General dataset
data_use <- subset(rowca, select=c(Country, Localite, Category, Value, Date))
head(data_use)

##Liberia dataset
liberia_sub <- subset(rowca, rowca$Country == "Liberia", select = c(Country, Localite, Category, Value, Date))
head(liberia_sub)
liberia_sub <- liberia_sub[order(liberia_sub$Country, liberia_sub$Localite, liberia_sub$Date),]
#Cases = Confirmed cases + New cases + Probable cases + Suspected cases

##Change Date into appropriate data type
liberia_sub$Date <- as.Date(liberia_sub$Date, "%m/%d/%y")
#liberia_sub$Value <- as.numeric(liberia_sub$Value)
str(liberia_sub)

##Checking unique levels of Localite
unique(liberia_sub$Localite)
###some Localites have trailing spaces!

##Create function to trim trailing spaces
trim <- function(df, col) {
  #gets rid of trailing spaces
  gsub("^\\s+|\\s+$", "", df[,col])
}

##Apply function
liberia_sub$Localite <- trim(liberia_sub, "Localite")
unique(liberia_sub$Localite)
###Fixed!

unique(liberia_sub$Category)
###No problem with category

##Subsetting down to only "Cases" and "Deaths"
liberia_reduced <- subset(liberia_sub, liberia_sub$Category %in% c("Cases", "Deaths"))
check(liberia_reduced)

##Checking for Values that are not numbers (ie blanks)
numblank <- function(df, col) {
  blanks <- sum(df[,col]=="")
  print(paste("Number blank:", blanks))
  if (blanks > 0) {
    df[which(df[, col]==""),]
  }
}

numblank(liberia_reduced, "Value")
#16 blanks in Values column (looks like all deaths)

##Changing blanks in Value column to 0's
for (i in 1:nrow(liberia_reduced)) {
  if(liberia_reduced$Value[i] == "") {
    liberia_reduced$Value[i] <- 0
  }}

##Re-checking for Value that are blanks
numblank(liberia_reduced, "Value")
#no blanks in Values column

##Change Value into appropriate data type
##deal with commas
numNoCom <- function(df, col) {
  as.numeric(gsub(",", "", df[,col]))
}

liberia_reduced$Value <- numNoCom(liberia_reduced, "Value")
