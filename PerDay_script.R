### Script to create categories with cases/deaths by day (not cummulative)
##Use data frames from cleaning_script.R

##Only started testing my steps to do this, but look at how this
#localite works out:

#Liberia

loc <- subset(liberia, liberia$Localite == "Bong") #bong localite
cat <- subset(loc, loc$Category == "Cases") #cases
cat_orig <- cat #copy of df for comparison purpose
cat$Value <- c(cat$Value[1], diff(cat$Value)) #new df, with each row
#substracted from the following row (row2 value - row1 value). First row
#keeps its value

cat[which(cat$Value < 0),]
#5 rows/days have negative values - not cumulative?!
