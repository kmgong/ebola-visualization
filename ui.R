library(shiny)

# Define UI for application that draws a histogram
shinyUI(fluidPage(
  
  # Application title
  titlePanel("Ebola Outbreak in Africa"),
  
  sidebarLayout(
    selectInput("loc", 
                label = "Localite",
                choices = c("National", "Conakry", "Gueckedou", "Kissidougou", "Macenta", 
                            "Dabola", "Dinguiraye", "Telimele", "Boffa", "Kouroussa", 
                            "Dubreka", "Nzerekore", "Pita", "Siguiri", "Yomou", 
                            "Forecariah", "Kerouane", "Coyah", "Dalaba", "Kindia", 
                            "Beyla", "Lola", "Faranah", "Boke", "Mamou", 
                            "Kankan", "Kailahun", "Bo", "Kenema", "Koinadugu", 
                            "Moyamba", "Bombali", "Freetown", "Kambia", "Kono", 
                            "Port Loko", "Western Area", "Bonthe", "Pujehun", "Tonkolili", 
                            "Western Area Rural", "Western Area Urban", "Bong", "Grand Cape Mount", "Lofa", 
                            "Margibi", "Montserrado", "Nimba", "Grand Gedeh", "Bomi", 
                            "Grand Bassa", "River Gee", "RiverCess", "Sinoe", "Gbarpolu",          
                            "Maryland", "Grand Kru"       
                ),
                selected = "National"),

    
    # Show a plot of the generated distribution
    mainPanel(
      plotOutput("ebPlot"))
  )
))