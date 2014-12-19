library(shiny)
library(shinyapps)
library(scales)
library(ggplot2)
source("cleaning_script.R")

#draw outline of national data; static
#ebolaUse <- subset(ebolaData, ebolaData$Localite %in% "National" & ebolaData$Category %in% "Cases")
#data <- ggplot(ebolaUse, aes(Date, Value))
#ebGph <- geom_line(aes(colour=factor(Country)), size=1, alpha=0.3)
#dateScale <- scale_x_date(breaks=date_breaks("months"), labels=date_format("%m/%d/%y"))
#labels <- labs(title="Ebola Cases in Africa Over Time", x="Date", y="Number of Cases", colour="Country")
#cols <- scale_colour_manual(values = c("#11887F", "#FC1325", "#670155"))
#format <- theme(panel.background = element_rect(fill="white"),
#                panel.grid.major = element_line(colour="#CFCFCF", linetype=2),
#                axis.line = element_line(size=0.25, colour="black"),
##                axis.title = element_text(family="Open Sans", size=14),
#                plot.title = element_text(family="Open Sans", size=18),
#                axis.text = element_text(family="Open Sans", size=12, colour="black"),
#                axis.ticks = element_line(size=0.25, colour="black"))  

shinyServer(function(input, output) {  
  output$ebPlot <- renderPlot({
    var <- switch(input$loc, 
                  "National"="National",
                  "Conakry"="Conakry",
                  "Gueckedou"="Gueckedou", 
                  "Kissidougou"="Kissidougou", 
                  "Macenta"="Macenta", 
                  "Dabola"="Dabola", 
                  "Dinguiraye"="Dinguiraye", 
                  "Telimele"="Telimele", 
                  "Boffa"="Boffa", 
                  "Kouroussa"="Kouroussa",   
                  "Dubreka"="Dubreka", 
                  "Nzerekore"="Nzerekore", 
                  "Pita"="Pita", 
                  "Siguiri"="Siguiri", 
                  "Yomou"="Yomou", 
                  "Forecariah"="Forecariah", 
                  "Kerouane"="Kerouane", 
                  "Coyah"="Coyah", 
                  "Dalaba"="Dalaba", 
                  "Kindia"="Kindia", 
                  "Beyla"="Beyla", 
                  "Lola"="Lola", 
                  "Faranah"="Faranah", 
                  "Boke"="Boke", 
                  "Mamou"="Mamou", 
                  "Kankan"="Kankan", 
                  "Kailahun"="Kailahun", 
                  "Bo"="Bo", 
                  "Kenema"="Kenema", 
                  "Koinadugu"="Koinadugu", 
                  "Moyamba"="Moyamba", 
                  "Bombali"="Bombali",
                  "Freetown"="Freetown", 
                  "Kambia"="Kambia", 
                  "Kono"="Kono", 
                  "Port Loko"="Port Loko", 
                  "Western Area"="Western Area", 
                  "Bonthe"="Bonthe", 
                  "Pujehun"="Pujehun", 
                  "Tonkolili"="Tonkolili", 
                  "Western Area Rural"="Western Area Rural", 
                  "Western Area Urban"="Western Area Urban", 
                  "Bong"="Bong", 
                  "Grand Cape Mount"="Grand Cape Mount", 
                  "Lofa"="Lofa", 
                  "Margibi"="Margibi", 
                  "Montserrado"="Montserrado", 
                  "Nimba"="Nimba", 
                  "Grand Gedeh"="Grand Gedeh", 
                  "Bomi"="Bomi", 
                  "Grand Bassa"="Grand Bassa", 
                  "River Gee"="River Gee", 
                  "RiverCess"="RiverCess", 
                  "Sinoe"="Sinoe", 
                  "Gbarpolu"="Gbarpolu",          
                  "Maryland"="Maryland", 
                  "Grand Kru"="Grand Kru"       
                  )
    ebolaUse <- subset(ebolaData, ebolaData$Localite %in% "National" & ebolaData$Category %in% "Cases")
    ebolaUse2 <- subset(ebolaData, ebolaData$Localite %in% var & ebolaData$Category %in% "Cases")
    data <- ggplot(ebolaUse, aes(Date, Value))
    ebGph <- geom_line(aes(colour=factor(Country)), size=1, alpha=0.25)
    locGph <- geom_line(data = ebolaUse2, aes(Date, Value, colour=factor(Country)), size=1)  
    dateScale <- scale_x_date(breaks=date_breaks("months"), labels=date_format("%m/%d/%y"))
    labels <- labs(title="Ebola Cases in Africa Over Time", x="Date", y="Number of Cases", colour="Country")
    cols <- scale_colour_manual(values = c("#11887F", "#FC1325", "#670155"))
    format <- theme(panel.background = element_rect(fill="white"),
                    panel.grid.major = element_line(colour="#CFCFCF", linetype=2),
                    axis.line = element_line(size=0.25, colour="black"),
                    axis.title = element_text(family="Open Sans", size=14),
                    plot.title = element_text(family="Open Sans", size=18),
                    axis.text = element_text(family="Open Sans", size=12, colour="black"),
                    axis.ticks = element_line(size=0.25, colour="black"))  
    data + ebGph + locGph + cols + dateScale + labels  + format 
  })
})