### Script defines (and then runs) a function that can be used to
###de-cumulate (scientific term) the levels in "Category" column.

##Ideally should use country specific data frames, because I was too lazy to
## have the function subset within country. However, it does subset by
## localites, so assuming all localites are unique it should work on the
## combined data set.

###Defining function to do above for each country

get_diff <- function(df){
  final_df <- data.frame() #empty data frame that will be added to
  localites <- unique(df$Localite) #list of localites in df
  categories <- unique(df$Category) #unique(df$Category) #list of category levels in df
  
  #By all levels in "Category"
  for (j in (1:length(categories))){
  #De-cumulate across all localites
  for (i in (1:length(localites))){
    localite <- subset(df, df$Localite == localites[i]) #subset to a localite
    category <- subset(localite, localite$Category == categories[j]) #select only one category
    
    #NA's to 0 (Needed for next step)
    for (i in (1:length(category$Value))){
     if (is.na(category$Value[i])){
       category$Value[i] <- 0
     }}
    
    category$Value <- c(category$Value[1], diff(category$Value)) #take differences, first row remains the same
    
    #Negatives to NA
    for (i in (1:length(category$Value))){
      if (category$Value[i] < 0){
        category$Value[i] <- NA
      }}
    
    #Update final data frame with diff'd values
    final_df <- as.data.frame(rbind(final_df, category))
      }}
  
  #Order by Country, localite and Date
  final_df <- final_df[order(final_df$Country, final_df$Localite, final_df$Date),]
  
  return(final_df)
  }

##Running function for each country
liberia_byday <- get_diff(liberia)
sierraLeone_byday <- get_diff(sierraLeone)
guinea_byday <- get_diff(guinea)

#Combining all countries into one
ebolaData_byday <- as.data.frame(rbind(guinea_byday, sierraLeone_byday, liberia_byday))


#histogram of data frame with negative values
ggplot(data=cat, aes(x=Date, y=Value))+geom_bar(stat="identity")+ggtitle(paste(unique(cat$Country), ":", unique(cat$Localite)))
#histogram of data frame with NA instead of negative values
ggplot(data=cat_NA, aes(x=Date, y=Value))+geom_bar(stat="identity")+ggtitle(paste(unique(cat$Country), ":", unique(cat$Localite)))