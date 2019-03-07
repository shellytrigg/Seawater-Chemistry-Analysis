#libraries
library(ggplot2)
library(gtools)
library(dplyr)

#Set working directory
setwd("C:/Users/Danielle.Perez/Documents/2016 crabs/Megalopae-Juvenile/Spec pH CSV")

#List all files in that directory
file.names <- dir(getwd(),pattern=".csv")
file.names

#Create empty output file for new files to go into
Summer_2016_Specs <- NULL

#Create loop to go through all files and remove unnecessary columns
for (i in 1:length(file.names)){
  SpecTemp <- read.csv(file.names[i], stringsAsFactors = FALSE)
  SpecTemp2 <- SpecTemp[-1,-c(1,4,6:8,10:30)]
  
  Summer_2016_Specs <- smartbind(Summer_2016_Specs, SpecTemp2)
}

#Remove random fifth column added when using smartbind
Summer_2016_Specs <- Summer_2016_Specs[ -c(5) ]

#Put MOATS in sequence
moatsNames <- as.character(seq(1,13))

#Rename columns
colnames(Summer_2016_Specs)[1:4]<-c("MOATS","Jar","Date","pH")

#Make MOATS a factor, and pH numeric
Summer_2016_Specs$MOATS<-factor(Summer_2016_Specs$MOATS, moatsNames)
Summer_2016_Specs$pH<-as.numeric(Summer_2016_Specs$pH) 
levels(Summer_2016_Specs$MOATS)

#Arrange columns by number (need to be resorted to go in numerical order??)
Summer_2016_Specs <- arrange(Summer_2016_Specs,MOATS)

#Remove empty rows
Summer_2016_Specs[Summer_2016_Specs==""] <- NA
Summer_2016_Specs <- na.omit(Summer_2016_Specs)

#Change DateTime format so R is happy
str(Summer_2016_Specs)
Summer_2016_Specs$NewDate <- as.POSIXct(strptime(Summer_2016_Specs$Date, "%e-%b-%y"))

#Add treatment column
lowMOATS <- c(1,2,3,6,9,13)
Summer_2016_Specs$Treatment <- "High"
Summer_2016_Specs$Treatment[Summer_2016_Specs$MOATS %in%lowMOATS] <- "Low"
View(Summer_2016_Specs)

#Calcuate mean and standard deviation for each MOATS
MOATS_Avg <- tapply(Summer_2016_Specs$pH, (Summer_2016_Specs$MOATS), mean)
MOATS_Std <- tapply(Summer_2016_Specs$pH, (Summer_2016_Specs$MOATS), sd)
MOATS_Stats <- cbind(MOATS_Avg, MOATS_Std)
colnames(MOATS_Stats) [1:2] <-c("Average pH", "Standard Deviation")
View(MOATS_Stats)

#Pretty scatter plot of all MOATS
p <- ggplot(Summer_2016_Specs, aes(NewDate, pH))
p + ggtitle("All MOATS") + xlab("Time") + geom_point(aes(colour =MOATS))

#Make graph function 
graphf <- function(df,moats){
  print(ggplot(subset(df,MOATS == moats), aes(NewDate, pH)) + geom_point(aes(colour = pH)) + 
          scale_colour_gradient(low = "blue") + ggtitle(paste("Moats",moats )) + xlab("Date"))
}
moatslist <- levels(Summer_2016_Specs$MOATS)

#Loop through all MOATS to create all 13 graphs
for(i in 1:length(moatslist)){
  graphf(Summer_2016_Specs,moatslist[i])
}

#Cool facetwrap graph
p <- ggplot(Summer_2016_Specs, aes(NewDate, pH, colour = pH)) + 
  geom_point() + ggtitle("All MOATS") + xlab("Time") +
  facet_wrap(~ MOATS)
p

#Basic box and whisker plot
p <- ggplot(Summer_2016_Specs, aes(MOATS, pH))
p + geom_boxplot()

#Colorful box and whisker plot
p <- ggplot(Summer_2016_Specs, aes(MOATS, pH))
p + geom_boxplot(aes(colour = MOATS))

#Function for computing mean, DS, max and min values
min.mean.sd.max <- function(x) {
  r <- c(min(x), mean(x) - sd(x), mean(x), mean(x) + sd(x), max(x))
  names(r) <- c("ymin", "lower", "middle", "upper", "ymax")
  r
}

# ggplot code
p1 <- ggplot(aes(y = pH, x = factor(MOATS)), data = Summer_2016_Specs)
p1 <- p1 + stat_summary(fun.data = min.mean.sd.max, geom = "boxplot") + geom_jitter(position=position_jitter(width=.2), size=3) + 
  ggtitle("pH of all MOATS with Standard Deviation") + xlab("MOATS") + ylab("pH")
p1

#Create a loop to go through all MOATS data and put outliers into separate table
mLevels <- levels(Summer_2016_Specs$MOATS)

theAnswer<-data.frame(NULL)

for(i in 1:length(mLevels)){
  dsub <- subset(Summer_2016_Specs, MOATS == mLevels[i])
  subout.Outlr<-boxplot.stats(dsub$pH)$out
  dSubout <- subset(dsub, pH %in% subout.Outlr)
  theAnswer <- rbind(theAnswer, dSubout)
}
View(theAnswer)

#Export data to Excel file
write.csv(Summer_2016_Specs, "C:/Users/Danielle.Perez/Documents/2016 crabs/Megalopae-Juvenile/Summer_2016_Specs.csv")
write.csv(MOATS_Stats), "C:/Users/Danielle.Perez/Documents/2016 crabs/Megalopae-Juvenile/MOATS_Stats.csv" )
write.csv(theAnswer, "C:/Users/Danielle.Perez/Documents/2016 crabs/Megalopae-Juvenile/MOATS Outliers.csv")
