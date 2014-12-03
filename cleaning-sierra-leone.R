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

##Sierra Leone dataset
sierraLeone <- subset(rowca, rowca$Country == "Sierra Leone", select = c(Country, Localite, Category, Value, Date))
sierraLeone <- sierraLeone[order(sierraLeone$Country, sierraLeone$Localite, sierraLeone$Date),]
#Cases = Confirmed cases + New cases + Probable cases + Suspected cases

##Change Date into appropriate data type
sierraLeone$Date <- as.Date(sierraLeone$Date, "%m/%d/%y")
#liberia_sub$Value <- as.numeric(liberia_sub$Value)
str(sierraLeone)

##Checking unique levels of Localite
unique(sierraLeone$Localite)
###some Localites have trailing spaces!

##Create function to trim trailing spaces
trim <- function(df, col) {
  #gets rid of trailing spaces
  gsub("^\\s+|\\s+$", "", df[,col])
}

##Apply function
sierraLeone$Localite <- trim(sierraLeone, "Localite")
unique(sierraLeone$Localite)
###Fixed!

##Make all localites lowercase
sierraLeone$Localite <- tolower(sierraLeone$Localite)
unique(sierraLeone$Localite)

##Fix localities
for(i in which(sierraLeone$Localite=="port")) {
  sierraLeone$Localite[i] <- "port loko"
}
unique(sierraLeone$Localite)

##PROBLEM
##"Western area rural" and "Western area urban" are collapsed into one category for
##June data. It's not a big deal, since it's only two points... but maybe we can 
##collapse all of them?

unique(sierraLeone$Category)
###No problem with category

##Subsetting down to only "Cases" and "Deaths"
sierraLeone_reduced <- subset(sierraLeone, sierraLeone$Category %in% c("Cases", "Deaths"))
check(sierraLeone_reduced)

##Checking for Values that are not numbers (ie blanks)
numblank <- function(df, col) {
  blanks <- sum(df[,col]=="")
  print(paste("Number blank:", blanks))
  if (blanks > 0) {
    df[which(df[, col]==""),]
  }
}

numblank(sierraLeone_reduced, "Value")
#28 blanks in Value column (looks like all deaths)

##Changing blanks in Value column to 0's
for (i in 1:length(sierraLeone_reduced$Value)) {
  if(sierraLeone_reduced$Value[i]=="") {
    sierraLeone_reduced$Value[i] <- 0
  }
}
  
##Re-checking for Value that are blanks
numblank(sierraLeone_reduced, "Value")
#no blanks in Values column

##Change Value into appropriate data type
##deal with commas
numNoCom <- function(df, col) {
  as.numeric(gsub(",", "", df[,col]))
}

sierraLeone_reduced$Value <- numNoCom(sierraLeone_reduced, "Value")
str(sierraLeone_reduced)
unique(sierraLeone_reduced$Localite)
