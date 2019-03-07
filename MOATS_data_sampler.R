#set working directory to the correct folder
setwd("C:/Michael/experiments/pteropod/Summer2016/data")

#load libraries
library("pastecs", lib.loc="~/R/win-library/3.2")
library("xlsx", lib.loc="~/R/win-library/3.2")
library("ggplot2", lib.loc="~/R/win-library/3.2")
library("stringr", lib.loc="~/R/win-library/3.2")

#create empty dataframe
d <- NULL

#get the folder list from the wd
folder.list <- dir(getwd())

#this sets the window length (number of samples to average for subsampled dataset)
winLen <- 100

#this will get the file names in each folder
for(j in 1:length(folder.list)){
  file.names <- dir(paste(getwd(), folder.list[j], sep = "/"), pattern = ".lvm")
   for(i in 1:length(file.names)){
    #start pulling in the data.  Skip 9 rows 
    dtemp <- read.table(paste(getwd(),folder.list[j], file.names[i],sep="/"), sep="\t", skip = 9, nrows=2, header=FALSE, stringsAsFactors=FALSE)
    #pull the date and time from these cell
    datetime <- paste(dtemp[1,2],dtemp[2,2],sep = "_")
    #convert to posix 
    MastDat <- as.POSIXct(strptime(datetime, "%Y/%m/%d_%H:%M:%S"))
    dtemp <- read.table(paste(getwd(),folder.list[j], file.names[i],sep="/"), sep="\t", skip = 22, header=FALSE)
    dtemp$V3 = dtemp$V4 = dtemp$V6 = dtemp$V7 = dtemp$V9 = dtemp$V10 = dtemp$V12 = dtemp$V13 = dtemp$V14 = dtemp$V15 = NULL
    #make new datetime field equal start date plus these seconds from the first field
    dtemp$datetime = MastDat + dtemp$V1
    dtemp$moats <- as.factor(folder.list[j]) 
    
    #rename remaining columns
    names(dtemp)[names(dtemp)=="V1"] <- "Secs"
    names(dtemp)[names(dtemp)=="V2"] <- "pH"
    names(dtemp)[names(dtemp)=="V5"] <- "OL"
    names(dtemp)[names(dtemp)=="V8"] <- "O"
    names(dtemp)[names(dtemp)=="V11"] <- "DO" 
    
    #this next loop creates the short dataset, averaging over winlen number of records
    dLen <- nrow(dtemp)
    sLen <- floor(dLen/winLen)
    #define rows then columns
    dShort <-dtemp[1:sLen, ]
    dIndex <- 1
    for (k in 1:sLen){
      dShort[k, ]<- dtemp[dIndex, ]
      dShort$pH[k] <- mean(dtemp$pH[dIndex:(dIndex + winLen - 1)])
      dShort$Secs[k] <- mean(dtemp$Secs[dIndex:(dIndex + winLen - 1)])
      dShort$OL[k] <- mean(dtemp$OL[dIndex:(dIndex + winLen - 1)])
      dShort$O[k] <- mean(dtemp$O[dIndex:(dIndex + winLen - 1)])
      dShort$DO[k] <- mean(dtemp$DO[dIndex:(dIndex + winLen - 1)])
      dIndex <- dIndex + winLen
    }
    d <- rbind(d, dShort)
  }
}

str(d)
View(d)
nrow(d)

#write.xlsx(d[1:100, ], "C:/Michael/experiments/crabs/Jan2016/data/data/d1.100.xlsx")
#write.xlsx(dShort[1:10, ], "C:/Michael/experiments/crabs/Jan2016/data/data/dshort.xlsx")

#clean up data by start and end dates of experiments
dDate <- subset(d, d$datetime > as.POSIXct("2016-11-21 00:00:00") & d$datetime < as.POSIXct("2016-11-30 16:00:00") & d$pH > 0)
#make new column with moats as just a number
##dDate$m <- str_sub(dDate$moats, 2)

#make subsets for all the individual moats
m1 <- subset(dDate, moats == "m1") 
pH1 <- round(mean(m1$pH), digits = 2)
OL1 <- round(mean(m1$OL), digits = 2)
DO1 <- round(mean(m1$DO), digits = 2)

m2 <- subset(dDate, moats == "m2")
pH2 <- round(mean(m2$pH), digits = 2)
OL2 <- round(mean(m2$OL), digits = 2)
DO2 <- round(mean(m2$DO), digits = 2)

m3 <- subset(dDate, moats == "m3")
pH3 <- round(mean(m3$pH), digits = 2)
OL3 <- round(mean(m3$OL), digits = 2)
DO3 <- round(mean(m3$DO), digits = 2)

m4 <- subset(dDate, moats == "m4")
pH4 <- round(mean(m4$pH), digits = 2)
OL4 <- round(mean(m4$OL), digits = 2)
DO4 <- round(mean(m4$DO), digits = 2)

m5 <- subset(dDate, moats == "m5")
pH5 <- round(mean(m5$pH), digits = 2)
OL5 <- round(mean(m5$OL), digits = 2)
DO5 <- round(mean(m5$DO), digits = 2)

m6 <- subset(dDate, moats == "m6")
pH6 <- round(mean(m6$pH), digits = 2)
OL6 <- round(mean(m6$OL), digits = 2)
DO6 <- round(mean(m6$DO), digits = 2)

m7 <- subset(dDate, moats == "m7")
pH7 <- round(mean(m7$pH), digits = 2)
OL7 <- round(mean(m7$OL), digits = 2)
DO7 <- round(mean(m7$DO), digits = 2)

m8 <- subset(dDate, moats == "m8")
pH8 <- round(mean(m9$pH), digits = 2)
OL8 <- round(mean(m9$OL), digits = 2)
DO8 <- round(mean(m9$DO), digits = 2)

m9 <- subset(dDate, moats == "m9")
pH9 <- round(mean(m9$pH), digits = 2)
OL9 <- round(mean(m9$OL), digits = 2)
DO9 <- round(mean(m9$DO), digits = 2)

m10 <- subset(dDate, moats == "m10")
pH10 <- round(mean(m10$pH), digits = 2)
OL10 <- round(mean(m10$OL), digits = 2)
DO10 <- round(mean(m10$DO), digits = 2)

m11 <- subset(dDate, moats == "m11")
pH11 <- round(mean(m11$pH), digits = 2)
OL11 <- round(mean(m11$OL), digits = 2)
DO11 <- round(mean(m11$DO), digits = 2)

m12 <- subset(dDate, moats == "m12")
pH12 <- round(mean(m12$pH), digits = 2)
OL12 <- round(mean(m12$OL), digits = 2)
DO12 <- round(mean(m12$DO), digits = 2)

m13 <- subset(dDate, moats == "m13")
pH13 <- round(mean(m13$pH), digits = 2)
OL13 <- round(mean(m13$OL), digits = 2)
DO13 <- round(mean(m13$DO), digits = 2)

#moats2
p1 <- ggplot(m2) + geom_point(aes(x = datetime, y = pH),colour = 'red', size = 0.1) + xlab("") + ggtitle("MOATS 2 pH") + geom_text(aes(x = as.POSIXct("2016-11-30 00:00:00"), y = 7.87),label = pH2, colour = 'blue', size = 3)
p2 <- ggplot(m2) + geom_point(aes(x = datetime, y = OL),colour = 'blue', size = 0.1) + xlab("") + ylab("DegC") + ggtitle("MOATS 2 Temp") + geom_text(aes(x = as.POSIXct("2016-11-28 00:00:00"), y = 13),label = OL2, colour = 'blue', size = 3)
p3 <- ggplot(m2) + geom_point(aes(x = datetime, y = DO),colour = 'green', size = 0.1) + xlab("") + ggtitle("MOATS 2 DO") + geom_text(aes(x = as.POSIXct("2016-11-30 00:00:00"), y = 5.2),label = DO2, colour = 'blue', size = 3)

multiplot(p1, p2, p3, cols=2)

#moats3
p1 <- ggplot(m3) + geom_point(aes(x = datetime, y = pH),colour = 'red', size = 0.1) + xlab("") + ggtitle("MOATS 3 pH") + geom_text(aes(x = as.POSIXct("2016-11-30 00:00:00"), y = 7.87),label = pH3, colour = 'blue', size = 3)
p2 <- ggplot(m3) + geom_point(aes(x = datetime, y = OL),colour = 'blue', size = 0.1) + xlab("") + ylab("DegC") + ggtitle("MOATS 3 Temp") + geom_text(aes(x = as.POSIXct("2016-11-28 00:00:00"), y = 10.8),label = OL3, colour = 'blue', size = 3)
p3 <- ggplot(m3) + geom_point(aes(x = datetime, y = DO),colour = 'green', size = 0.1) + xlab("") + ggtitle("MOATS 3 DO") + geom_text(aes(x = as.POSIXct("2016-11-30 00:00:00"), y = 9.2),label = DO3, colour = 'blue', size = 3)

multiplot(p1, p2, p3, cols=2)

#moats4
p1 <- ggplot(m4) + geom_point(aes(x = datetime, y = pH),colour = 'red', size = 0.1) + xlab("") + ggtitle("MOATS 4 pH") + geom_text(aes(x = as.POSIXct("2016-11-30 00:00:00"), y = 7.67),label = pH4, colour = 'blue', size = 3)
p2 <- ggplot(m4) + geom_point(aes(x = datetime, y = OL),colour = 'blue', size = 0.1) + xlab("") + ylab("DegC") + ggtitle("MOATS 4 Temp") + geom_text(aes(x = as.POSIXct("2016-11-28 00:00:00"), y = 10.8),label = OL4, colour = 'blue', size = 3)
p3 <- ggplot(m4) + geom_point(aes(x = datetime, y = DO),colour = 'green', size = 0.1) + xlab("") + ggtitle("MOATS 4 DO") + geom_text(aes(x = as.POSIXct("2016-11-30 00:00:00"), y = 5.2),label = DO4, colour = 'blue', size = 3)

multiplot(p1, p2, p3, cols=2)

#moats5
p1 <- ggplot(m5) + geom_point(aes(x = datetime, y = pH),colour = 'red', size = 0.1) + xlab("") + ggtitle("MOATS 5 pH") + geom_text(aes(x = as.POSIXct("2016-11-30 00:00:00"), y = 7.61),label = pH5, colour = 'blue', size = 3)
p2 <- ggplot(m5) + geom_point(aes(x = datetime, y = OL),colour = 'blue', size = 0.1) + xlab("") + ylab("DegC") + ggtitle("MOATS 5 Temp") + geom_text(aes(x = as.POSIXct("2016-11-28 00:00:00"), y = 10.7),label = OL5, colour = 'blue', size = 3)
p3 <- ggplot(m5) + geom_point(aes(x = datetime, y = DO),colour = 'green', size = 0.1) + xlab("") + ggtitle("MOATS 5 DO") + geom_text(aes(x = as.POSIXct("2016-11-30 00:00:00"), y = 9.1),label = DO5, colour = 'blue', size = 3)

multiplot(p1, p2, p3, cols=2)

#moats6
p1 <- ggplot(m6) + geom_point(aes(x = datetime, y = pH),colour = 'red', size = 0.1) + xlab("") + ggtitle("MOATS 6 pH") + geom_text(aes(x = as.POSIXct("2016-11-30 00:00:00"), y = 7.9),label = pH6, colour = 'blue', size = 3)
p2 <- ggplot(m6) + geom_point(aes(x = datetime, y = OL),colour = 'blue', size = 0.1) + xlab("") + ylab("DegC") + ggtitle("MOATS 6 Temp") + geom_text(aes(x = as.POSIXct("2016-11-28 00:00:00"), y = 10.8),label = OL6, colour = 'blue', size = 3)
p3 <- ggplot(m6) + geom_point(aes(x = datetime, y = DO),colour = 'green', size = 0.1) + xlab("") + ggtitle("MOATS 6 DO") + geom_text(aes(x = as.POSIXct("2016-11-30 00:00:00"), y = 4.3),label = DO6, colour = 'blue', size = 3)

multiplot(p1, p2, p3, cols=2)

#moats7
p1 <- ggplot(m7) + geom_point(aes(x = datetime, y = pH),colour = 'red', size = 0.1) + xlab("") + ggtitle("MOATS 7 pH") + geom_text(aes(x = as.POSIXct("2016-11-30 00:00:00"), y = 7.9),label = pH7, colour = 'blue', size = 3)
p2 <- ggplot(m7) + geom_point(aes(x = datetime, y = OL),colour = 'blue', size = 0.1) + xlab("") + ylab("DegC") + ggtitle("MOATS 7 Temp") + geom_text(aes(x = as.POSIXct("2016-11-28 00:00:00"), y = 10.8),label = OL7, colour = 'blue', size = 3)
p3 <- ggplot(m7) + geom_point(aes(x = datetime, y = DO),colour = 'green', size = 0.1) + xlab("") + ggtitle("MOATS 7 DO") + geom_text(aes(x = as.POSIXct("2016-11-30 00:00:00"), y = 9),label = DO7, colour = 'blue', size = 3)

multiplot(p1, p2, p3, cols=2)

#moats8
p1 <- ggplot(m8) + geom_point(aes(x = datetime, y = pH),colour = 'red', size = 0.1) + xlab("") + ggtitle("MOATS 8 pH") + geom_text(aes(x = as.POSIXct("2016-11-30 00:00:00"), y = 7.7),label = pH8, colour = 'blue', size = 3)
p2 <- ggplot(m8) + geom_point(aes(x = datetime, y = OL),colour = 'blue', size = 0.1) + xlab("") + ylab("DegC") + ggtitle("MOATS 8 Temp") + geom_text(aes(x = as.POSIXct("2016-11-28 00:00:00"), y = 11.2),label = OL8, colour = 'blue', size = 3)
p3 <- ggplot(m8) + geom_point(aes(x = datetime, y = DO),colour = 'green', size = 0.1) + xlab("") + ggtitle("MOATS 8 DO") + geom_text(aes(x = as.POSIXct("2016-11-30 00:00:00"), y = 9),label = DO8, colour = 'blue', size = 3)

multiplot(p1, p2, p3, cols=2)

#moats9
p1 <- ggplot(m9) + geom_point(aes(x = datetime, y = pH),colour = 'red', size = 0.1) + xlab("") + ggtitle("MOATS 9 pH") + geom_text(aes(x = as.POSIXct("2016-11-30 00:00:00"), y = 7.7),label = pH9, colour = 'blue', size = 3)
p2 <- ggplot(m9) + geom_point(aes(x = datetime, y = OL),colour = 'blue', size = 0.1) + xlab("") + ylab("DegC") + ggtitle("MOATS 9 Temp") + geom_text(aes(x = as.POSIXct("2016-11-30 00:00:00"), y = 25),label = OL9, colour = 'blue', size = 3)
p3 <- ggplot(m9) + geom_point(aes(x = datetime, y = DO),colour = 'green', size = 0.1) + xlab("") + ggtitle("MOATS 9 DO") + geom_text(aes(x = as.POSIXct("2016-11-30 00:00:00"), y = 9),label = DO9, colour = 'blue', size = 3)

multiplot(p1, p2, p3, cols=2)

#moats10
p1 <- ggplot(m10) + geom_point(aes(x = datetime, y = pH),colour = 'red', size = 0.1) + xlab("") + ggtitle("MOATS 10 pH") + geom_text(aes(x = as.POSIXct("2016-11-30 00:00:00"), y = 7.65),label = pH10, colour = 'blue', size = 3)
p2 <- ggplot(m10) + geom_point(aes(x = datetime, y = OL),colour = 'blue', size = 0.1) + xlab("") + ylab("DegC") + ggtitle("MOATS 10 Temp") + geom_text(aes(x = as.POSIXct("2016-11-28 00:00:00"), y = 11),label = OL10, colour = 'blue', size = 3)
p3 <- ggplot(m10) + geom_point(aes(x = datetime, y = DO),colour = 'green', size = 0.1) + xlab("") + ggtitle("MOATS 10 DO") + geom_text(aes(x = as.POSIXct("2016-11-30 00:00:00"), y = 9.2),label = DO10, colour = 'blue', size = 3)

multiplot(p1, p2, p3, cols=2)

#moats11
p1 <- ggplot(m11) + geom_point(aes(x = datetime, y = pH),colour = 'red', size = 0.1) + xlab("") + ggtitle("MOATS 11 pH") + geom_text(aes(x = as.POSIXct("2016-11-28 00:00:00"), y = 7.625),label = pH11, colour = 'blue', size = 3)
p2 <- ggplot(m11) + geom_point(aes(x = datetime, y = OL),colour = 'blue', size = 0.1) + xlab("") + ylab("DegC") + ggtitle("MOATS 11 Temp") + geom_text(aes(x = as.POSIXct("2016-11-28 00:00:00"), y = 10.75),label = OL11, colour = 'blue', size = 3)
p3 <- ggplot(m11) + geom_point(aes(x = datetime, y = DO),colour = 'green', size = 0.1) + xlab("") + ggtitle("MOATS 11 DO") + geom_text(aes(x = as.POSIXct("2016-11-30 00:00:00"), y = 4),label = DO11, colour = 'blue', size = 3)

multiplot(p1, p2, p3, cols=2)

#moats12
p1 <- ggplot(m12) + geom_point(aes(x = datetime, y = pH),colour = 'red', size = 0.1) + xlab("") + ggtitle("MOATS 12 pH") + geom_text(aes(x = as.POSIXct("2016-11-28 00:00:00"), y = 7.90),label = pH12, colour = 'blue', size = 3)
p2 <- ggplot(m12) + geom_point(aes(x = datetime, y = OL),colour = 'blue', size = 0.1) + xlab("") + ylab("DegC") + ggtitle("MOATS 12 Temp") + geom_text(aes(x = as.POSIXct("2016-11-28 00:00:00"), y = -20),label = OL12, colour = 'blue', size = 3)
p3 <- ggplot(m12) + geom_point(aes(x = datetime, y = DO),colour = 'green', size = 0.1) + xlab("") + ggtitle("MOATS 12 DO") + geom_text(aes(x = as.POSIXct("2016-11-30 00:00:00"), y = 4.2),label = DO12, colour = 'blue', size = 3)

multiplot(p1, p2, p3, cols=2)

#moats13
p1 <- ggplot(m13) + geom_point(aes(x = datetime, y = pH),colour = 'red', size = 0.1) + xlab("") + ggtitle("MOATS 13 pH") + geom_text(aes(x = as.POSIXct("2016-11-28 00:00:00"), y = 7.9),label = pH13, colour = 'blue', size = 3)
p2 <- ggplot(m13) + geom_point(aes(x = datetime, y = OL),colour = 'blue', size = 0.1) + xlab("") + ylab("DegC") + ggtitle("MOATS 13 Temp") + geom_text(aes(x = as.POSIXct("2016-11-28 00:00:00"), y = 11.2),label = OL13, colour = 'blue', size = 3)
p3 <- ggplot(m13) + geom_point(aes(x = datetime, y = DO),colour = 'green', size = 0.1) + xlab("") + ggtitle("MOATS 13 DO") + geom_text(aes(x = as.POSIXct("2016-11-30 00:00:00"), y = 8.8),label = DO13, colour = 'blue', size = 3)

multiplot(p1, p2, p3, cols=2)

########################
###aggregate stats######

mlist



# Multiple plot function
#
# ggplot objects can be passed in ..., or to plotlist (as a list of ggplot objects)
# - cols:   Number of columns in layout
# - layout: A matrix specifying the layout. If present, 'cols' is ignored.
#
# If the layout is something like matrix(c(1,2,3,3), nrow=2, byrow=TRUE),
# then plot 1 will go in the upper left, 2 will go in the upper right, and
# 3 will go all the way across the bottom.
#
multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  library(grid)
  
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if (numPlots==1) {
    print(plots[[1]])
    
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}
