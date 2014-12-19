source("cleaning_script.R")
#let's look only at national-level data for Cases
ebolaUse <- subset(ebolaData, ebolaData$Localite %in% "National" & ebolaData$Category %in% "Cases")
#using ggplot
data <- ggplot(ebolaUse, aes(Date, Value))
ebGph <- geom_line(aes(colour=factor(Country)), size=1)
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

data + ebGph + cols + dateScale + labels  + format

#using rcharts
#install.packages("devtools")
#require(devtools)
#install_github("rCharts", "ramnathv")
#library(rCharts)
#ebolaUse$Date <- as.character(ebolaUse$Date)
#head(ebolaUse$Date)
#ebGph2 <- rPlot(Value ~ Date, data=ebolaUse, type="line",
#                color="Country", tooltip=mytooltip)
#ebGph2
#ebGph2$print("chart1")
#ebGph2$publish("my chart", host="rpubs")
