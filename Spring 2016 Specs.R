#libraries
library(ggplot2)
library(dplyr)

#Set working directory
setwd("/Users/Shelly/Desktop/2016-17_CrabExp_WaterChem/data/all_Spec_data/")

#List all spec files in directory
file.names <- dir(getwd(),pattern=".csv")

#Create empty output file for new files to go into
Spec_pH <- data.frame()

#Create loop to go through all files and remove unnecessary columns
for (i in 1:length(file.names)){
  SpecTemp <- read.csv(file.names[i], stringsAsFactors = FALSE)
  SpecTemp2 <- SpecTemp[-1,-c(1,4,6:8,10:30)]
  
  Spec_pH <- rbind(Spec_pH,SpecTemp2)
}


#Put MOATS in sequence
moatsNames <- as.character(seq(1,13))

#Rename columns
colnames(Spec_pH)[1:4]<-c("MOATS","Jar","Date","pH")

#Make MOATS a factor, and pH numeric
Spec_pH$MOATS<-factor(Spec_pH$MOATS, moatsNames)
Spec_pH$pH<-as.numeric(Spec_pH$pH)
levels(Spec_pH$MOATS)

#remove empty rows
Spec_pH <- subset(Spec_pH, !is.na(pH))

#Arrange columns by number 
Spec_pH <- arrange(Spec_pH,MOATS)

#Change DateTime format so R is happy
str(Spec_pH)
Spec_pH$Date <- as.POSIXct(strptime(Spec_pH$Date, "%e-%b-%y"))

#Add treatment column
lowMOATS <- c(1,5,6,8,9,11)
Spec_pH$Treatment <- "High"
Spec_pH$Treatment[Spec_pH$MOATS %in%lowMOATS] <- "Low"
View(Spec_pH)

#Calcuate mean and standard deviation for each MOATS
MOATS_Avg <- tapply(Spec_pH$pH, (Spec_pH$MOATS), mean)
MOATS_Std <- tapply(Spec_pH$pH, (Spec_pH$MOATS), sd)
MOATS_Stats <- cbind(MOATS_Avg, MOATS_Std)
colnames(MOATS_Stats) [1:2] <-c("Average pH", "Standard Deviation")
View(MOATS_Stats)

#Optional subout to look at only some bits of data set
#subout <- subset(Spring_2016_Specs, MOATS==2)
#View(subout)

#Pretty scatter plot of all MOATS
p <- ggplot(Spring_2016_Specs, aes(NewDate, pH))
p + ggtitle("All MOATS") + xlab("Time") + geom_point(aes(colour =MOATS))

#Make graph function 
graphf <- function(df,moats){
  print(ggplot(subset(df,MOATS == moats), aes(NewDate, pH)) + geom_point(aes(colour = pH)) + 
          scale_colour_gradient(low = "blue") + ggtitle(paste("Moats",moats )) + xlab("Date"))
}
moatslist <- levels(Spring_2016_Specs$MOATS)

#Loop through all MOATS to create all 13 graphs
for(i in 1:length(moatslist)){
  graphf(Spring_2016_Specs,moatslist[i])
}

#Cool facetwrap graph
p <- ggplot(Spring_2016_Specs, aes(NewDate, pH, colour = pH)) + 
  geom_point() + ggtitle("All MOATS") + xlab("Time") +
  facet_wrap(~ MOATS)


#Scatter plot of individual MOATS from subout function
#p <- ggplot(subout, aes(NewDate, pH))
#p + geom_point(aes(colour = pH)) + scale_colour_gradient(low = "blue") + ggtitle("MOATS 2") + xlab("Date")

#Basic box and whisker plot
p <- ggplot(Spring_2016_Specs, aes(MOATS, pH))
p + geom_boxplot()

#Colorful box and whisker plot
p <- ggplot(Spring_2016_Specs, aes(MOATS, pH))
p + geom_boxplot(aes(colour = MOATS))

# function for computing mean, DS, max and min values
min.mean.sd.max <- function(x) {
  r <- c(min(x), mean(x) - sd(x), mean(x), mean(x) + sd(x), max(x))
  names(r) <- c("ymin", "lower", "middle", "upper", "ymax")
  r
}

# ggplot code
p1 <- ggplot(aes(y = pH, x = factor(MOATS)), data = Spring_2016_Specs)
p1 <- p1 + stat_summary(fun.data = min.mean.sd.max, geom = "boxplot") + geom_jitter(position=position_jitter(width=.2), size=3) + 
  ggtitle("pH of all MOATS with Standard Deviation") + xlab("MOATS") + ylab("pH")
p1

#Create a loop to go through all MOATS data and put outliers into separate table
mLevels <- levels(Spring_2016_Specs$MOATS)

theAnswer<-data.frame(NULL)

for(i in 1:length(mLevels)){
  dsub <- subset(Spring_2016_Specs, MOATS == mLevels[i])
  subout.Outlr<-boxplot.stats(dsub$pH)$out
  dSubout <- subset(dsub, pH %in% subout.Outlr)
  theAnswer <- rbind(theAnswer, dSubout)
}
View(theAnswer)

#Export data to Excel file
write.csv(Spring_2016_Specs, "C:/Users/Danielle.Perez/Documents/2016 crabs/Zoea/Spring_2016_Specs.csv")
write.csv((MOATS_Stats), "C:/Users/Danielle.Perez/Documents/2016 crabs/Zoea/MOATS_Stats.csv" )
write.csv(theAnswer, "C:/Users/Danielle.Perez/Documents/2016 crabs/Zoea/MOATS Outliers.csv")


