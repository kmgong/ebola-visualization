##Check for/Install Necessary Packages

load_install<-function(lib){
  if(! require(lib, character.only=TRUE)) install.packages(lib, character.only=TRUE)
  library(lib, character.only=TRUE)
}

## the required libraries (e.g. packages)
theLib<-c("plyr", "ggplot2", "scales", "gdata", "chron", "reshape2", "grid", "maps", "mapdata")

## apply the function
lapply(theLib, load_install)

#Create inital data frame
setwd("/Users/florenceclee/Desktop/qmssviz-project/") #edit to reflect your directory
rowca <- read.csv("ebola-dat-rowca.csv", stringsAsFactors=FALSE)
who <- read.csv("ebola-dat-who.csv", stringsAsFactors=FALSE)

# ROWCA Data set

##General structure
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

##Subsetting data frame

liberia_sub <- subset(rowca, rowca$Country == "Liberia", select = c(Country, Localite, Category, Value, Date))

#Checking unique levels of Localite
unique(liberia_sub$Localite)
#some Localites have trailing spaces!

liberia_sub$Localite <- gsub(" $","", liberia_sub$Localite, perl=T)
unique(liberia_sub$Localite)
#Fixed!

unique(liberia_sub$Category)
#no problem with category

##Subsetting down to only "Cases" and "Deaths" (based only on my own thoughts of how to approach all the data, see notes at bottom)
liberia_reduced <- subset(liberia_sub, liberia_sub$Category %in% c("Cases", "Deaths"))

##Checking for Values that are not numbers (ie blanks)
blanks<-liberia_reduced$Value==""
sum(blanks==TRUE)
#16 blanks in Values column (looks like all deaths)

##Changing blanks in Values column to 0's
for (i in 1:nrow(liberia_reduced)){
  if(liberia_reduced$Value[i] == ""){
    liberia_reduced$Value[i] <- 0
  }}

##Re-checking for Values that are blanks
blanks<-liberia_reduced$Value==""
sum(blanks==TRUE)
#no blanks in Values column

##Notes
#Originally used only confirmed cases, but there were almost always more deaths than confirmed cases, so switch to cases instead
#Going forward: creating a function to do this?
#What countries to map? I'm thinking narrow it down to the three hardest hit: Liberia, Guinea, and Sierra Leone
