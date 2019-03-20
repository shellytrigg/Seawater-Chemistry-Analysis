#libraries
library(ggplot2)
library(dplyr)
library(tidyr)
library(readxl)
library("seacarb", lib.loc = "/Library/Frameworks/R.framework/Versions/3.2/Resources/")
library(plotrix)



###############
###Salinity####
###############

# #make a table of all salinity data from salinity logs to use for cross-checking the salinity entered in the Spec pH files
# setwd("/Users/Shelly/Documents/GitHub/Seawater-Chemistry-Analysis/2016-17_PteropodExp_WaterChem/PteropodWaterChem/")
# salinity1 <- read.csv("Salinity Log 2016.csv", stringsAsFactors = FALSE)
# 
# #convert date column
# salinity1$Date <- as.POSIXct(strptime(salinity1$Date, "%y-%m-%d"))
# 
# salinity1 <- salinity1[,which(names(salinity1) %in% c("Date", "Salinity"))]
# #remove rows where salinity is NA
# salinity1 <- subset(salinity1, !is.na(salinity1))

#####################
#######Spec pH ######
#####################

#define salinity data path
spec_path <- "~/Documents/GitHub/Seawater-Chemistry-Analysis/2016-17_PteropodExp_WaterChem/PteropodWaterChem/specs_w_correction/"
#salinity is recorded as 29.62 for all dates
#PMEL measured salinity 29.61, so I'll just go with the 29.62 (I don't think 0.01 is going to make a difference)



#List all spec files in directory
file.names <- dir(spec_path, pattern = ".csv", full.names = TRUE)

#Create empty output file for new files to go into
Spec_pH <- data.frame()

#Create loop to go through all files and remove unnecessary columns
for (i in 1:length(file.names)){
  SpecTemp <- read.csv(file.names[i], stringsAsFactors = FALSE)
  SpecTemp2 <- SpecTemp[-1,-c(1,4,6,8,10:30)]
  
  Spec_pH <- rbind(Spec_pH,SpecTemp2)
}

#Make a list of MOATS numbers in order
moatsNames <- c(1,3,4,5,6,7,8,10,11,12,13)

#Rename columns
colnames(Spec_pH)[1:5]<-c("MOATS","Jar","Date","Salinity","pH")

#Make MOATS a factor, and pH numeric
Spec_pH$MOATS<-as.numeric(Spec_pH$MOATS, moatsNames)
Spec_pH$Salinity<- as.numeric(Spec_pH$Salinity)
Spec_pH$pH<-as.numeric(Spec_pH$pH)

#remove empty rows
Spec_pH <- subset(Spec_pH, !is.na(pH))

#order rows by MOATS number 
Spec_pH <- Spec_pH[order(Spec_pH$MOATS),]

#Change DateTime format so R is happy
str(Spec_pH)
Spec_pH$Date <- as.POSIXct(strptime(Spec_pH$Date, "%e-%b-%y"))

#Add treatment column
lowMOATS <- c(1,4,5,8,10,11)
Spec_pH$Treatment <- "High"
Spec_pH$Treatment[Spec_pH$MOATS %in%lowMOATS] <- "Low"
colnames(Spec_pH)[5] <- "pH_at_25C"
Spec_pH$Data_Type <- "Spec"


#Code to verify Spec_pH salinity values were correctly entered 
#verify_spec_sal <- merge(unique(Master_data[which(Master_data$Salinity != "NA"),c(1,2,4)]), unique(Spec_pH[,3:4]), by = "Date", all = TRUE)
#write.csv(verify_spec_sal[which(verify_spec_sal$Salinity.x != verify_spec_sal$Salinity.y | 
# is.na(verify_spec_sal$Salinity.x) | is.na(verify_spec_sal$Salinity.y)),], "~/Desktop/2016-17_CrabExp_WaterChem/data/Spec_inconsistencies2016-2017.csv", quote = FALSE, row.names = FALSE)

#II_verify_spec_sal <- merge(unique(Master_data[which(Master_data$Salinity != "NA"),c(1,2,4)]), unique(Spec_pH[,3:4]), by = "Date", all = TRUE)
#View(II_verify_spec_sal[which(II_verify_spec_sal$Salinity.x != II_verify_spec_sal$Salinity.y |is.na(II_verify_spec_sal$Salinity.x) | is.na(II_verify_spec_sal$Salinity.y)),])
##This looks good after corrections


############
###MOATS####
############

##some MOATS data (2017 MOATS 11 8/11/2017-8/14/2017) have an incorrect date listed: 1903. Need to determine what to correct this date and time to
#last date and time in  M11 8/10/2017 data file
as.POSIXct(strptime("2018-06-09 16:42:29.8751721382141113281", "%Y-%m-%d %H:%M:%S"))+5432872.164942
#[1] "2018-08-11 13:50:21 PDT"
#[1] "2018-08-12 16:42:27 PDT"

#8/12/17
#[1] "2018-08-12 16:42:33 PDT"
#[1] "2018-08-13 16:42:27 PDT"

#8/13/2017
#[1] "2018-08-13 16:42:33 PDT
#[1] "2018-08-14 16:42:27 PDT"

#first and last date time in M11 8/14/2017 data file if we assume the date and time that should be listed is 2018-06-09 and 16:42:29.875172138214111328
#[1] "2018-08-14 16:42:33 PDT"
#[1] "2018-08-15 14:03:57 PDT"

#first date and time in M11 8/15/2017 data file
as.POSIXct(strptime("2018-08-15 14:04:22.7403888702392578125", "%Y-%m-%d %H:%M:%S"))+0
#[1] "2018-08-15 14:04:22 PDT"


#define path to master MOATS data folder containing all individual MOATS folders (i.e. M1-M13) each with a set of .lvm files
lvm_path <- "~/Documents/GitHub/Seawater-Chemistry-Analysis/2016-17_PteropodExp_WaterChem/PteropodWaterChem/MOATS_data"
#create empty dataframe
d <- data.frame()
#get the folder list from the wd
folder.list <- dir(lvm_path, full.names = TRUE)
#this sets the window length (number of samples to average for subsampled dataset)
winLen <- 100

#this will get the file names in each folder
for(i in 1:length(folder.list)){
  file.names <- dir(folder.list[i], pattern = ".lvm")
  for(j in 1:length(file.names)){
    print(paste(folder.list[i],file.names[j], sep = "/"))
    #start pulling in the data.  
    #First count the max number of columns in the file
    ncols <- max(count.fields(paste(folder.list[i],file.names[j],sep="/"), sep = "\t"))
    #Read in data to get date and time data were collected
    dtemp <- read.table(paste(folder.list[i],file.names[j],sep="/"), sep="\t", skip = 9, nrows=2, header=FALSE, col.names = paste0("V", seq_len(ncols)), stringsAsFactors=FALSE, fill = TRUE)
    #fill = TRUE adds NAs to empty fields,col.names = paste0("V", seq_len(ncols))  makes the function consider all columns for adding NAs to
    
    #pull the date and time from these cell
    datetime = paste(dtemp[1,2],dtemp[2,2],sep = "_")
    #convert to posix 
    datetime <- as.POSIXct(strptime(datetime, "%Y/%m/%d_%H:%M:%S"), tz = "GMT")
    #now read in the rest of the data
    dtemp <- read.table(paste(folder.list[i],file.names[j],sep="/"), sep="\t", skip = 22, header=FALSE, col.names = paste0("V", seq_len(ncols)), stringsAsFactors=FALSE, fill = TRUE,skipNul = TRUE)
    #only include data for seconds, pH, tempLowBox, tempHiBox, DO
    dtemp <- dtemp[,c(1,2,5,8,11)]
    #create MOATS column; extracts MOATS number from folder name (i.e. '1' gets extracted from M1 Data')
    dtemp$moats <- substr(folder.list[i],nchar(folder.list[i])-1,nchar(folder.list[i]))
    dtemp$moats <- gsub("m","",dtemp$moats)
    dtemp$moats <- as.numeric(dtemp$moats)
    #rename remaining columns
    colnames(dtemp) <- c("Secs","pH", "OL", "O","DO","MOATS")
    #create a column for date 
    dtemp$datetime <- datetime + dtemp$Secs
    #this next loop creates the short dataset, averaging over winlen number of records; take the total number of data rows, divide by winLen, and round to nearest integer.
    sLen <- as.integer(nrow(dtemp)/winLen)
    #create shortened data frame
    dShort <-dtemp[1:sLen, ]
    dStartIndex <- 1
    dEndIndex <- winLen
    for (l in 1:sLen){
      dShort[l, ]<- dtemp[dStartIndex, ]
      #for seconds column, print the range of seconds the average is taken over
      #dShort[l,1]<- paste(as.integer(dtemp[dStartIndex,1]),as.integer(dtemp[dEndIndex,1]), sep = "-")
      dShort[l,c(2:5)] <- apply(dtemp[dStartIndex:dEndIndex,c(2:5)],2,mean)
      dStartIndex <- dStartIndex + winLen
      dEndIndex <- dEndIndex + winLen
    }
    d <- rbind(d, dShort)
  }
}
 
###restrict data to dates of the experiment
start_date <- as.POSIXct("2016/11/21_17:00:00", "%Y/%m/%d_%H:%M:%S", tz = "GMT")
end_date <- as.POSIXct("2016/11/30", "%Y/%m/%d", tz = "GMT")

d <- d[which(d$datetime >= start_date & d$datetime <= end_date),]

#plot MOATS pH data 
ggplot(d,aes(datetime, pH)) + geom_point() + facet_wrap(~MOATS)
#blip on MOATS 1 between Nov 24-26; but could be real

#plot temp data for all MOATS
ggplot(d,aes(datetime, OL)) + geom_point() + facet_wrap(~MOATS)
#MOATS 12 temp probe wonky, don't include that data
#plot temp data without 12
ggplot(d[which(d$MOATS != 12),],aes(datetime, OL)) + geom_point() + facet_wrap(~MOATS)
#all MOATS are a little spikey, suggesting these spikes are real

ggplot(d,aes(datetime, DO)) + geom_point() + facet_wrap(~MOATS)
#MOATS 12 and MOATS 1 DO probes have weird blips between nov 24-26 and nov 25-27, respectively


#Paul suggested creating flags for outlier data
#i.e. data outside a variety of thresholds for temp, pH, etc.
#But I haven't done this yet.
#For this first pass, I used the following to exclude outlier data

#remove data with pH vals < 0 because these are from probe errors   
#d_good_pH <- subset(d, d$pH > 6 & d$pH < 8.2)
#remove data with temp values that don't make sense; temps > 25 are not real
#d_good_pH <- subset(d_good_pH, d_good_pH$OL < 25)
#d_good_pH <- subset(d_good_pH, d_good_pH$O < 25)
#make a column with only the time (this is for merging with other data files later)
#d_good_pH$Date <- as.POSIXct(strptime(substr(d_good_pH$datetime,1,10), "%Y-%m-%d"))
#keep all MOATS data
d_unfiltered <- d
d_unfiltered$Date <- as.POSIXct(strptime(substr(d_unfiltered$datetime,1,10), "%Y-%m-%d"))
#make a column with only the date (this is for merging with other data files later)
#d_good_pH$Time <-substr(d_good_pH$datetime,12,19)
d_unfiltered$Time <-substr(d_unfiltered$datetime, 12,19)
#add data type column
#d_good_pH$Data_Type <- "MOATS"
d_unfiltered$Data_Type <- "MOATS"
#remove datetime column
#d_good_pH$datetime = NULL
d_unfiltered$datetime = NULL
#subset data for experiment run time
#for zoea 2016
#exp_Start <- as.POSIXct("2016-03-02 00:00:00")
#exp_End <- as.POSIXct("2016-03-26 00:00:00")
#d$datetime > subset(d_good_pH,d_good_pH$Date_Time > as.POSIXct("2016-03-02 00:00:00") & d_good_pH$Date_Time < as.POSIXct("2016-03-26 24:00:00"))

#QC check for funky temperature readings
#m13Sept2016<- data.frame(d_good_pH[which(d_good_pH$MOATS == "13" & d_good_pH$Date > as.POSIXct(strptime("2016-08-31", format = "%Y-%m-%d")) & d_good_pH$Date < as.POSIXct(strptime("2016-09-30", format = "%Y-%m-%d")) ),])
#m13Sept2016$datetime <- paste(m13Sept2016$Date,m13Sept2016$Time, sep = " ")
#m13Sept2016$datetime <- as.POSIXct(m13Sept2016$datetime)
#ggplot(m13Sept2016, aes(datetime, OL)) +geom_point(aes(colour =MOATS))
#not sure what this means; if it's real or not, so will leave it in



##############
###Code from Mega_2016_data_only.R, this is how to convert spec pH to pH at 12
######

#calculate mean temp
mean(d[which(d$MOATS != 12),"OL"])
#[1] 10.29312
sd(d[which(d$MOATS != 12),"OL"])
#[1] 0.165076
meanT <- mean(d[which(d$MOATS != 12),"OL"])


#convert pH at 25C to pH at 12C
for (i in 1:nrow(Spec_pH)){
  print(i)
  Sal <- 29.62
  #calculate TA ('alk') using the slope and intercept determined by 'Alkalinity v Salinity_KD_2018.R' script. This script compared Fassbender et al 2016 data to data from Muk (Chase's and our data from the past two years). This way, we were able to figure out what our offset is here at Muk.
  TAcalc <- ((50.946 * Sal)+522.506)/1000000
  pHspec <- Spec_pH$pH_at_25C[i]
  Temp <- meanT
  Spec_pH$carbSpecDIC[i] <- carb(flag = 8, pHspec, TAcalc, Sal,T=25)$DIC
  Spec_pH$carbSpecpH[i] <- carb(flag = 15, TAcalc,as.numeric(Spec_pH$carbSpecDIC[i]), Sal,Temp)$pH
}

class(Spec_pH$carbSpecpH) <- "numeric"
class(Spec_pH$carbSpecDIC) <- "numeric"
Spec_pH$carbSpecDIC <- Spec_pH$carbSpecDIC *1000000
#write.csv(data, "~/Desktop/2016-17_CrabExp_WaterChem/data.csv", quote = FALSE, row.names = FALSE)
data <- data[which(data$pH > 0),]


####
#read in target treatment info so that MOATS can be separated out and averaged by treatment
####

treat <- read.csv("~/Documents/GitHub/Seawater-Chemistry-Analysis/2016-17_PteropodExp_WaterChem/PteropodWaterChem/Treatments.csv", stringsAsFactors = FALSE)
#merge treatment info with MOATS info
dtreat <- merge(d, treat, by = "MOATS", all.x = TRUE)


#########################
####Calculating avg pH and DO for each treatment
##########################

#DO first; I'm leaving in the blips in MOATS 1 and 12 because I don't know if they are real or not

mean(dtreat[grep("HH", dtreat$Treatment_abbv),"DO"])
sd(dtreat[grep("HH", dtreat$Treatment_abbv),"DO"])

mean(dtreat[grep("HL", dtreat$Treatment_abbv),"DO"])
sd(dtreat[grep("HL", dtreat$Treatment_abbv),"DO"])

mean(dtreat[grep("LH", dtreat$Treatment_abbv),"DO"])
sd(dtreat[grep("LH", dtreat$Treatment_abbv),"DO"])

mean(dtreat[grep("LL", dtreat$Treatment_abbv),"DO"])
sd(dtreat[grep("LL", dtreat$Treatment_abbv),"DO"])

ggplot(dtreat) + geom_boxplot(aes(Target_Treatment,DO)) 

####Now calculate for pH; seems like there is probe drift on day 22 in MOATS 8 and day 29 in MOATS 10, and possibly day 22 MOATS 4. but it only appears to be a few data points, so I'm not removing them for now. 

mean(dtreat[grep("HH", dtreat$Treatment_abbv),"pH"])
sd(dtreat[grep("HH", dtreat$Treatment_abbv),"pH"])

mean(dtreat[grep("HL", dtreat$Treatment_abbv),"pH"])
sd(dtreat[grep("HL", dtreat$Treatment_abbv),"pH"])

mean(dtreat[grep("LH", dtreat$Treatment_abbv),"pH"])
sd(dtreat[grep("LH", dtreat$Treatment_abbv),"pH"])

mean(dtreat[grep("LL", dtreat$Treatment_abbv),"pH"])
sd(dtreat[grep("LL", dtreat$Treatment_abbv),"pH"])

ggplot(dtreat) + geom_boxplot(aes(Target_Treatment,pH)) 

#### 
#make supplementary table
#add column for jar so that Spec pH will stay in a separate row from MOATS data
dtreat$Jar <- "NA"
#make a column for time only
dtreat$time <- as.character(dtreat$datetime)
dtreat$time <- substr(dtreat$time,12,19)

#merge MOATS data with Spec data
all_chem <- merge(dtreat, Spec_pH[,-grep("Treatment|Salinity|Data_Type", colnames(Spec_pH))], by = c("Date","MOATS", "Jar"), all = TRUE)
#remove uninformative columns
all_chem<- all_chem[,-c(4,7,9,11)]

#reorder columns
all_chem <- all_chem[,c(7,2,3,1,8,4,5,6,9)]
#rename columns
colnames(all_chem)[5:9] <- c("Time", "pH_logger", "Temp_logger", "DO_logger", "Spec_pH")


############
###Presens####
############
#Set working directory to where presens data is
setwd("/Users/Shelly/Desktop/2016-17_CrabExp_WaterChem/data/")
#read in data files

presens1 <- read.csv("Moats-Presens DO Comparison 2016 Zoea.csv", stringsAsFactors = FALSE)
presens2 <- read.csv("Moats-Presens DO Comparison 2016 Mega.csv", stringsAsFactors = FALSE)
#combine data files into one data frame
presens <- rbind(presens1,presens2)
#exclude data that is not a PreSens reading
presens <- presens[grep("PreSens", presens$Sensor),]
#rename columns
colnames(presens) <- c("Date", "MOATS", "Data_Type","presensDO")
#convert date column to R date format
presens$Date <- as.POSIXct(strptime(presens$Date, "%m/%d/%Y"))


###############################
##Salinity vs. Total Alkalinity#
##############################

#read in PMEL data
PMELsal_1 <- read_xlsx("PMEL/Paul_McElhany_1.13.17_DIC_TA_Clean.xlsx")
PMELsal_2 <- read_xlsx("PMEL/NWFSC_Mar-Sept_2017_DIC_TA (1).xlsx")
PMELsal_3 <- read_xlsx("PMEL/Chase_Williams_July_Aug_2017_DIC_TA (1).xlsx")
PMELsal_4 <- read_xlsx("PMEL/Chase_Williams_logsheet_DIC_TA.xlsx", sheet = 2)
PMELsal_5 <- read_xlsx("PMEL/Chase_Williams_12-6through2-17_2017_DIC_TA.xlsx", sheet = 2)

#change column names to be consistent among all PMEL files
colnames(PMELsal_1)[4] <- "Salinity"
colnames(PMELsal_1)[7] <- "Salinity from DIC"
colnames(PMELsal_1)[8] <- "DIC"
colnames(PMELsal_1)[11] <- "TA"
PMELsal_1$Source <- "OAlab2016-2017"

colnames(PMELsal_2)[1] <- "ID"
colnames(PMELsal_2)[6] <- "DIC"
colnames(PMELsal_2)[9] <- "TA"
PMELsal_2$SalDIC <- "NA"
colnames(PMELsal_2)[13] <- "Salinity from DIC"
PMELsal_2$Source <- "OAlabMarToSept17"


colnames(PMELsal_3)[1] <- "ID"
colnames(PMELsal_3)[6] <- "DIC"
colnames(PMELsal_3)[9] <- "TA"
PMELsal_3$SalDIC <- "NA"
colnames(PMELsal_3)[13] <- "Salinity from DIC"
PMELsal_3$Source <- "ChaseJulToAug17"

colnames(PMELsal_4)[2] <- "ID"
colnames(PMELsal_4)[4] <- "Salinity"
colnames(PMELsal_4)[5] <- "DIC"
colnames(PMELsal_4)[8] <- "TA"
#remove 3's from data (flagged 3 = poor sample)
PMELsal_4 <- PMELsal_4[which(PMELsal_4$`TA QC` != 3),] 
PMELsal_4$Source <- "ChaseAugToSept16"

colnames(PMELsal_5)[2] <- "ID"
colnames(PMELsal_5)[4] <- "Salinity"
colnames(PMELsal_5)[5] <- "DIC"
colnames(PMELsal_5)[8] <- "TA"
PMELsal_5$SalDIC <- "NA"
colnames(PMELsal_5)[12] <- "Salinity from DIC"
PMELsal_5$Source <- "ChaseDec16ToFeb17"

PMELnames <- c("ID","Date", "Salinity", "DIC","TA","Salinity from DIC", "Source")

#combine all PMEL files
PMEL <- rbind(PMELsal_1[,which(names(PMELsal_1) %in% PMELnames)],PMELsal_2[,which(names(PMELsal_2) %in% PMELnames)], PMELsal_3[,which(names(PMELsal_3) %in% PMELnames)], PMELsal_4[,which(names(PMELsal_4) %in% PMELnames)], PMELsal_5[,which(names(PMELsal_5) %in% PMELnames)])
#remove blank rows
PMEL <- data.frame(subset(PMEL, !is.na(PMEL$ID)))
#add data type column
PMEL$Data_Type <- "PMEL"
#convert date to POSIX
PMEL$Date <- as.POSIXct(strptime(PMEL$Date, "%Y-%m-%d"))
PMEL$Experiment <- ifelse(grepl("M", PMEL$ID),"MOATS","Chase")
colnames(PMEL)[1] <- "MOATS"
PMEL$MOATS <- ifelse(substr(PMEL$MOATS,1,1)=="M", substr(PMEL$MOATS,2,length(PMEL$MOATS)),paste("Chase",PMEL$MOATS))
colnames(PMEL)[4] <- "Salinity_from_DIC"
PMEL <- data.frame(PMEL)

#Create a new column for salinity to use for generating S versus TA relationship
#used as.Date function to not get a warning about unknown timeszones
PMEL$Salinity_for_SxTA <- ifelse(as.Date(PMEL$Date) < as.Date("2016-11-30", "%Y-%m-%d"), PMEL$Salinity_from_DIC, PMEL$Salinity)
PMEL$Salinity_for_SxTA <- as.numeric(PMEL$Salinity_for_SxTA)
PMEL$Salinity_for_SxTA_Source <- ifelse(as.Date(PMEL$Date) < as.Date("2016-11-30", "%Y-%m-%d"), "PMEL", "NWFSC")

##plots
#generate curve for 2016 data
ggplot(PMEL[grep("2016", PMEL$Date),], aes(x= Salinity, y= TA)) + 
  geom_point(aes(color= Source)) + 
  geom_abline(slope= 47.7, intercept= 647)+
  stat_smooth(method = "lm", col = "red", linetype = "dashed", se=FALSE)+
  stat_smooth(data= PMEL[grep("2016", PMEL$Date),], method = "lm", col = "gray", linetype = "dashed", se=FALSE) +
  theme_bw() + ggtitle("2016 NWFSC salinity only")
summary(lm(TA ~ Salinity, data = PMEL[grep("2016", PMEL$Date),]))
#Estimate Std. Error t value Pr(>|t|)    
#(Intercept)   411.34     132.26    3.11  0.00371 ** 
#  Salinity       54.64       4.46   12.25 3.24e-14 ***

####generate curve for 2017 data with NWFSC sal only
ggplot(PMEL[grep("2017", PMEL$Date),], aes(x= Salinity, y= TA)) + 
  geom_point(aes(color= Source)) + 
  geom_abline(slope= 47.7, intercept= 647)+
  stat_smooth(method = "lm", col = "red", linetype = "dashed", se=FALSE)+
  stat_smooth(data= PMEL[grep("2017", PMEL$Date),], method = "lm", col = "gray", linetype = "dashed", se=FALSE) +
  theme_bw() + ggtitle("2017 NWFSC salinity only")

summary(lm(TA ~ Salinity, data = PMEL[grep("2017", PMEL$Date),]))
#(Intercept)  707.536     66.835   10.59 5.57e-15 ***
#  Salinity      44.361      2.295   19.33  < 2e-16 ***

####generate curve with any data that has PMEL salinity
#first need to create a new file because it's having a problem with NAs
PMEL_DICsalonly <- PMEL[which(PMEL$Salinity_from_DIC !="NA"),]
PMEL_DICsalonly$Salinity_from_DIC <- as.numeric(PMEL_DICsalonly$Salinity_from_DIC)                              

ggplot(PMEL_DICsalonly, aes(x= Salinity_from_DIC, y= TA)) + 
  geom_point(aes(color= Source)) + 
  geom_abline(slope= 47.7, intercept= 647)+
  stat_smooth(method = "lm", col = "red", linetype = "dashed", se=FALSE)+
  stat_smooth(data= PMEL_DICsalonly, method = "lm", col = "gray", linetype = "dashed", se=FALSE) +
  theme_bw() + ggtitle("PMEL salinity only")

summary(lm(TA ~ Salinity_from_DIC, data = PMEL_DICsalonly))
#(Intercept)        552.022     51.527   10.71 1.94e-12 ***
 # Salinity_from_DIC   50.345      1.745   28.85  < 2e-16 ***
####generate curve for PMELsal_1-4 with NWFSC salinity only

####generate curve for PMELsal_1-5 with NWFSC salinity only 
ggplot(PMEL, aes(x= Salinity, y= TA)) + 
  geom_point(aes(color= Source)) + 
  geom_abline(slope= 47.7, intercept= 647)+
  stat_smooth(method = "lm", col = "red", linetype = "dashed", se=FALSE)+
  stat_smooth(data= PMEL, method = "lm", col = "gray", linetype = "dashed", se=FALSE) +
  theme_bw() + ggtitle("NWFSC salinity only")

summary(lm(TA ~ Salinity, data = PMEL))
#Estimate Std. Error t value Pr(>|t|)    
#(Intercept)  503.234     67.063   7.504 2.86e-11 ***
#  Salinity      51.492      2.285  22.539  < 2e-16 ***
  
####generate curve for PMELsal_1-4 with PMEL and NWFSC salinity 
ggplot(PMEL[which(PMEL$Source != "ChaseDec16ToFeb17"),], aes(x= Salinity_for_SxTA, y= TA)) + 
  geom_point(aes(color= Source)) + 
  geom_abline(slope= 47.7, intercept= 647)+
  stat_smooth(method = "lm", col = "red", linetype = "dashed", se=FALSE)+
  stat_smooth(data= PMEL[which(PMEL$Source != "ChaseDec16ToFeb17"),], method = "lm", col = "gray", linetype = "dashed", se=FALSE) +
  theme_bw() + ggtitle("PMEL and NWFSC salinity; 4 data files")

summary(lm(TA ~ Salinity_for_SxTA, data = PMEL[which(PMEL$Source != "ChaseDec16ToFeb17"),]))



####generate curve for PMELsal_1-5 with PMEL and NWFSC salinity 
ggplot(PMEL, aes(x= Salinity_for_SxTA, y= TA)) + 
  geom_point(aes(color= Source)) + 
  geom_abline(slope= 47.7, intercept= 647)+
  stat_smooth(method = "lm", col = "red", linetype = "dashed", se=FALSE)+
  stat_smooth(data= PMEL, method = "lm", col = "gray", linetype = "dashed", se=FALSE) +
  theme_bw() + ggtitle("PMEL and NWFSC salinity")
#make diff. lines for different data sources
ggplot(PMEL, aes(x= Salinity_for_SxTA, y= TA)) + 
  geom_point(aes(color= Source)) + 
  geom_abline(slope= 47.7, intercept= 647)+
  stat_smooth(data= PMEL, method = "lm", col = "gray", linetype = "dashed", se=FALSE) +
  stat_smooth(data= PMEL[which(PMEL$Salinity_for_SxTA_Source == "NWFSC"),], method = "lm", col = "red", linetype = "dashed", se=FALSE) +
  stat_smooth(data= PMEL[which(PMEL$Salinity_for_SxTA_Source == "PMEL"),], method = "lm", col = "blue", linetype = "dashed", se=FALSE) +
  theme_bw() + ggtitle("PMEL and NWFSC salinity")

summary(lm(TA ~ Salinity_for_SxTA, data = PMEL))
#Estimate Std. Error t value Pr(>|t|)    
#(Intercept)        522.506     59.521   8.779 5.39e-14 ***
#  Salinity_for_SxTA   50.946      2.032  25.071  < 2e-16 ***

summary(lm(TA ~ Salinity_for_SxTA, data = PMEL[which(PMEL$Salinity_for_SxTA_Source == "NWFSC"),]))
#Estimate Std. Error t value Pr(>|t|)    
#(Intercept)        698.449     62.639   11.15   <2e-16 ***
#  Salinity_for_SxTA   44.683      2.148   20.80   <2e-16 ***

summary(lm(TA ~ Salinity_for_SxTA, data = PMEL[which(PMEL$Salinity_for_SxTA_Source == "PMEL"),]))
#Estimate Std. Error t value Pr(>|t|)    
#(Intercept)        552.022     51.527   10.71 1.94e-12 ***
#  Salinity_for_SxTA   50.345      1.745   28.85  < 2e-16 ***

###
#PMEL Data to be merged in Master_Date
##############

PMEL_for_Master <- PMEL[grep("MOATS", PMEL$Experiment),]
PMEL_for_Master$MOATS <- as.integer(PMEL_for_Master$MOATS)
View(PMEL_for_Master)




#####################
###TREATMENTS########
#####################
#read treatment files 
setwd("/Users/Shelly/Desktop/2016-17_CrabExp_WaterChem/data/")

treat_1 <- read_xlsx("Zoea Treatments March 2016.xlsx")
treat_2 <- read_xlsx("Zoea Treatments April 2016.xlsx")
treat_3 <- read_xlsx("Mega_Treatments_2016.xlsx")

treat_1$MOATS <- as.integer(substr(treat_1$MOATS,2,3))
treat_2$MOATS <- as.integer(substr(treat_2$MOATS,2,3))
treat_3$MOATS <- as.integer(substr(treat_3$MOATS,2,3))



#2017 treatments are in multiple sheets so need to read in all sheets in excel workbook
read_excel_allsheets <- function(fileName, tibble = FALSE) {
  sheets <- readxl::excel_sheets(fileName)
  x <- lapply(sheets, function(X) readxl::read_excel(fileName, sheet = X))
  if(!tibble) x <- lapply(x, as.data.frame)
  names(x) <- sheets
  x
}

treat_4 <- read_excel_allsheets("Zoea Treatments 2017.xlsx")
list2env(treat_4 ,.GlobalEnv)
dt <- data.frame()

for(i in 1:length(treat_4)){
  dtemp <- data.frame(treat_4[i])
  dtemp$Experiment <- names(treat_4)[i]
  colnames(dtemp) <- c("MOATS","Start_Date", "End_Date", "pH", "DO", "T", "Experiment")
  dt <- rbind(dt, dtemp)
}
#View(dt)
treat_4 <- subset(dt, Experiment == "z17_2")
colnames(treat_4)[4]  <- "Target_pH"
colnames(treat_4)[5] <- "Target_DO"
treat_4$pH_Treatment <- c("Constant Low","Constant Low", "High Amplitude", "Low Amplitude", "Constant High", "High Amplitude","High Amplitude", "Constant High", "Low Amplitude", "Constant Mean", "Constant Mean", "Low Amplitude", "Constant Mean")
treat_4$DO_Treatment <- "Saturated"
#correct  M1 end date 5/12/17 and M12 end date 5/18/17 to match end date 8/30/17 of other MOATS from same experiment 
treat_4[which(treat_4$MOATS== 1),3] <- as.Date("08/30/2017","%m/%d/%Y")
treat_4[which(treat_4$MOATS== 12),3] <- as.Date("08/30/2017","%m/%d/%Y")
#remove temp column 
treat_4 <- treat_4[,-grep("^T$", colnames(treat_4))]

treat_5 <- treat_4
treat_5$Experiment <- "Megalopae-Juvenile"
treat_5$Start_Date <- as.Date("06/13/2017","%m/%d/%Y")
treat_5$End_Date <- as.Date("09/07/2017","%m/%d/%Y")
colnames(treat_5)[4]  <- "Target_pH"
colnames(treat_5)[5] <- "Target_DO"
treat_5$pH_Treatment <- c("Constant Low","Constant Low", "High Amplitude", "Low Amplitude", "Constant High", "High Amplitude","High Amplitude", "Constant High", "Low Amplitude", "Constant Mean", "Constant Mean", "Low Amplitude", "Constant Mean")
treat_5$DO_Treatment <- "Saturated"


#make a list of dataframes; one for each experiment; MAYBE EXPERIMENT NAME SHOULD BE UNIQUE?
treats <- list(treat_1, treat_2, treat_3, treat_4, treat_5)

#make an empty data frame that will be filled by for loop
dtreat <- data.frame()
#This is a for loop to go through each experiment's treatment data frame and fill in lines for each date that the experiment ran through
for (i in 1:length(treats)){
  #create an empty data frame 
  ttemp_oneDatecolumn <- data.frame()
  #temporarily save experiment data frame as 'ttemp'
  ttemp <- data.frame(treats[[i]])
  #make a data frame with single date column splitting each line of 'ttemp' into two lines; the first line containing the startdate and second line containing the end date
  for (i in 1:nrow(ttemp)){
    ttemp1 <- data.frame(ttemp[,-3])
    ttemp2 <- data.frame(ttemp[,-2])
    colnames(ttemp1)[2] <- "Date"
    colnames(ttemp2)[2] <- "Date"
    #fill in empty data frame with new data lines
    ttemp_oneDatecolumn <- rbind(ttemp_oneDatecolumn,ttemp1,ttemp2)
    }
  #fills in all missing data from dates between experiment start and end dates
  ttemp3 <- data.frame(ttemp_oneDatecolumn %>%  mutate(Date = as.Date(Date)) %>%
    complete(Date = seq.Date(min(Date), max(Date), by="day"), MOATS) %>%
    group_by(MOATS) %>% fill(names(ttemp_oneDatecolumn[,-2])))
  #appends data to final data frame; unique is used because for each start and end date 12 duplicate lines are created which may have to do with the group_by function?
  dtreat <- rbind(dtreat,unique(ttemp3))
  }
#convert Date column from class 'Date' to class 'POSIX' so it matches other data
dtreat$Date <- as.POSIXct(strptime(dtreat$Date, "%Y-%m-%d"))
#add target temperature column
#for 2016, target temp = 12 C
dtreat$Target_temp <- ifelse(grepl("2016",dtreat$Date), 12, "tbd")
#for 2017 target temp = 9 C for april-may, 10 C for jun, 11 C for July, 12 C for aug-sept. ***These are my estimates of what we did***
#Replace tbd in Target_temp column with above values
#using a conditional loop
#the loop goes through each line of dTreat, looks for dates from 2017 and fills in the corresponding temp. for each matching month
for(i in 1:length(dtreat$Target_temp)){
  if(format(dtreat$Date[i], format="%Y") =="2017" && format(dtreat$Date[i], format="%m") == "04"){
  dtreat$Target_temp[i] <- 9  
  }
  if(format(dtreat$Date[i], format="%Y") =="2017" && format(dtreat$Date[i], format="%m") == "05"){
    dtreat$Target_temp[i] <- 9  
  }
  if(format(dtreat$Date[i], format="%Y") =="2017" && format(dtreat$Date[i], format="%m") == "06"){
    dtreat$Target_temp[i] <- 10  
  }
  if(format(dtreat$Date[i], format="%Y") =="2017" && format(dtreat$Date[i], format="%m") == "07"){
    dtreat$Target_temp[i] <- 11  
  }
  if(format(dtreat$Date[i], format="%Y") =="2017" && format(dtreat$Date[i], format="%m") == "08"){
    dtreat$Target_temp[i] <- 12  
  }
  if(format(dtreat$Date[i], format="%Y") =="2017" && format(dtreat$Date[i], format="%m") == "09"){
    dtreat$Target_temp[i] <- 12  
  }
}



write.csv(dtreat, "2016-2017_MOATS_Treatments.csv", quote = FALSE, row.names = FALSE)


########CHECK FOR MISSING DATA##############

test <- d
test$Date <- as.POSIXct(strptime(test$datetime, format = "%Y-%m-%d"))
test1 <- merge(test[,6:8], dtreat[,1:2], all = TRUE)
#see 2016 files that are missing and not at the end of the experiment 
unique(test1[which(is.na(test1$datetime) & substr(test1$Date,7,7) != 9 & substr(test1$Date,1,4) == 2016),])
#MOATS       Date datetime
#8 2016-08-07     <NA>
#9 2016-07-10     <NA>
#12 2016-06-18     <NA>
#12 2016-06-19     <NA>
#12 2016-06-20     <NA>

#data from MOATS 8 and 9 for these dates was missing from the MOATS computer so we do not have it
#data from MOATS 12 was not collected because it was performing poorly experiments from June 2016 did not use MOATS 12

unique(test1[which(is.na(test1$datetime) & substr(test1$Date,7,7) != 9 & substr(test1$Date,1,4) == 2017),])
#MOATS       Date datetime
#11 2017-08-12     <NA>
#11 2017-08-13     <NA>
#11 2017-08-14     <NA>
#12 2017-06-10     <NA> 
#12 2017-06-11     <NA>
###no experiment was running on 6/10 or 6/11 so missing data here does not matter
###For MOATS 11, does this need to be manually corrected? .lvm files have an incorrect date "1903/12/31" entered in the first date field

################################
#Merging data files into one data frame#
################################

###compare column names of every file###
#first make list of all data frames to be merged
data_list <- list(Spec_pH=Spec_pH,MOATS=d_good_pH,presens=presens, salinity=salinity, PMEL = PMEL_for_Master, treatment = dtreat)
#next make a table that shows the column names in each data frame
data_names <-  data.frame(lapply(data_list,function(x)
              `length<-`(sort(colnames(x)),max(lengths(data_list)))))

###confirm columns with the same names are of the same class###
str(Spec_pH[,])
#data.frame':	2409 obs. of  7 variables:
# MOATS    : num  1 1 1 1 1 1 1 1 1 1 ...
# Jar      : chr  "B7" "D3" "B5" "B3" ...
# Date     : POSIXct, format: "2016-04-01" "2016-04-01" "2017-04-01" "2016-08-01" ...
# Salinity : num  28.5 28.5 27.4 29.9 29.9 ...
# pH_at_25C: num  7.28 7.25 7.3 7.26 7.25 ...
# Treatment: chr  "Low" "Low" "Low" "Low" ...
# Data_Type: chr  "Spec" "Spec" "Spec" "Spec" ...

str(d_good_pH)
# 'data.frame':	322894 obs. of  9 variables:
#   $ Secs     : num  345604 346204 346804 347404 348004 ...
# $ pH       : num  7.49 7.49 7.5 7.5 7.5 ...
# $ OL       : num  12.1 12.2 12.1 12.1 12.2 ...
# $ O        : num  11.4 11.2 10.9 11.1 11 ...
# $ DO       : num  2.93 2.99 2.99 2.97 2.98 ...
# $ MOATS    : num  1 1 1 1 1 1 1 1 1 1 ...
# $ Date     : POSIXct, format: "2016-06-08" "2016-06-08" "2016-06-08" ...
# $ Time     : chr  "11:03:00" "11:13:00" "11:23:00" "11:33:00" ...
# $ Data_Type: chr  "MOATS" "MOATS" "MOATS" "MOATS" ...

str(presens)
#'data.frame':	211 obs. of  4 variables:
#  $ Date     : POSIXct, format: "2016-03-04" "2016-03-04" "2016-03-04" "2016-03-04" ...
#$ MOATS    : int  1 2 3 4 5 6 7 8 9 10 ...
#$ Data_Type: chr  "PreSens" "PreSens" "PreSens" "PreSens" ...
#$ DO       : num  2.88 8.7 2.55 8.85 8.4 3.02 3.27 8.9 8.68 3.3 ...

str(salinity)
#'data.frame':	126 obs. of  3 variables:
#  $ Date     : POSIXct, format: "2016-03-01" "2016-03-02" "2016-03-04" "2016-03-09" ...
#$ Salinity : num  28.7 28.3 28.6 28.4 28.1 ...
#$ Data_Type: chr  "Muk_salinity" "Muk_salinity" "Muk_salinity" "Muk_salinity" ...

str(PMEL_for_Master)
# 'data.frame':	54 obs. of  11 variables:
#   $ MOATS                   : int  6 10 3 3 6 6 10 10 12 13 ...
# $ Date                    : POSIXct, format: "2016-11-29" "2016-11-29" "2016-07-13" "2016-09-08" ...
#X $ Salinity                : num  29.6 29.6 29.8 29.8 29.8 ...
#X $ Salinity_from_DIC       : chr  "29.504" "29.461" "29.278" "29.939" ...
# $ DIC                     : num  1951 2035 2078 2075 2071 ...
# $ TA                      : num  2035 2036 2022 2055 2024 ...
# $ Source                  : chr  "OAlab2016-2017" "OAlab2016-2017" "OAlab2016-2017" "OAlab2016-2017" ...
#X $ Data_Type               : chr  "PMEL" "PMEL" "PMEL" "PMEL" ...
#X $ Experiment              : chr  "MOATS" "MOATS" "MOATS" "MOATS" ...
# $ Salinity_for_SxTA       : num  29.5 29.5 29.3 29.9 29.3 ...
# $ Salinity_for_SxTA_Source: chr  "PMEL" "PMEL" "PMEL" "PMEL" ...

str(dtreat)
# 'data.frame':	4735 obs. of  8 variables:
#   $ Date        : POSIXct, format: "2016-02-29" "2016-03-01" "2016-03-02" ...
# $ MOATS       : num  1 1 1 1 1 1 1 1 1 1 ...
# $ Experiment  : chr  "Crab zoea" "Crab zoea" "Crab zoea" "Crab zoea" ...
# $ Target_pH   : chr  "7.4" "7.4" "7.4" "7.4" ...
# $ Target_DO   : chr  "3" "3" "3" "3" ...
# $ pH_Treatment: chr  "Low" "Low" "Low" "Low" ...
# $ DO_Treatment: chr  "Low" "Low" "Low" "Low" ...
# $ Target_temp : chr  "12" "12" "12" "12" ...

#merge data frames
#first merge combines MOATS data with treatment parameters and all.y = TRUE excludes MOATS data that was not during a treatment period
#d_good_pH[,-9] excludes 'data_type' column from MOATS data
Master_data <- merge(d_good_pH[,-9],dtreat, by = c("MOATS", "Date"), all.y = TRUE)
#next line duplicates each MOATS data line for dates that have spec data because there are two jars per MOATS
#Spec_pH[,-c(6,7)] excludes 'treatment' and 'data_type' column from Spec data
#using all.x = TRUE to exclude any spec data that doesn't relate to MOATS
Master_data <- merge(Master_data,Spec_pH[,-c(6,7)],by = c("MOATS", "Date"),all.x = TRUE)
#presens[,-3] excludes 'data_type' column
#using all.x = TRUE to exclude any presens data that doesn't relate to MOATS and experiment dates
Master_data <- merge(Master_data, presens[,-3], by = c("MOATS", "Date"), all.x = TRUE)
#am no longer running the following line because Spec_pH data salinity column now matches the salinity file
#Master_data <- merge(Master_data, salinity, by = "Date", all = TRUE)
#PMEL_for_Master[,-c(3,4,8,9)] excludes muk salinity values and PMEL salinity values (since these are in the 'Salinity_for_SxTA' column) and 'data_type' and 'experiments' columns
#using all.x = TRUE here because want to exclude PMEL data for dates outside of experiments
Master_data <- merge(Master_data, PMEL_for_Master[,-c(3,4,8,9)], by = c("MOATS", "Date"), all.x = TRUE)

#make a table with the mean daily temp that can be merged with master data
#standard dev. is included to see if any weird data is present
MeanDailyTemp <- Master_data[which(Master_data$OL < 25 & Master_data$O < 25),] %>% group_by(Date,MOATS) %>% summarize(meanT = mean(OL), stdevT = sd(OL))
#merge MeanDailyTemp with Master_data and exclude stdev column
Master_data <- merge(Master_data, MeanDailyTemp[,-4], by = c("MOATS", "Date"))


##########################
###SEACARB CALCULTAIONS###
##This is for calculating DIC from salinity values and 
##Converting pH @ 25C to pH @ in situ temp
##this step takes about 1.5 hr to get through 480K lines
############################
###choose which data set you want to process and save it as 'data'
data <- mega_2016_data

#convert pH at 25C to pH at 12C
for (i in 1:length(data$Date)){
  print(i)
  if(!is.na(data$pH_at_25C[i]) && !is.na(data$meanT[i])){
    #date <- data[i,which(colnames(data)=="Date")]
    Sal <- data$Salinity[i]
    #calculate TA ('alk') from salinity values using the slope and intercept generated by 'Alkalinity v Salinity_KD_2018.R' script. This script compared Fassbender et al 2016 data to data from Muk (Chase's and our data from the past two years). This way, we were able to figure out what our offset is here at Muk.
    TAcalc <- ((50.946 * Sal)+522.506)/1000000
    pHspec <- data$pH_at_25C[i]
    Temp <- data$meanT[i]
    data$carbSpecDIC[i] <- carb(flag = 8, pHspec, TAcalc, Sal,T=25)$DIC
    data$carbSpecpH[i] <- carb(flag = 15, TAcalc,as.numeric(data$carbSpecDIC[i]), Sal,Temp)$pH
  }
  else{
    data$carbSpecDIC[i] <- "NA"
    data$carbSpecpH[i] <- "NA"
  }
}

class(data$carbSpecpH) <- "numeric"
class(data$carbSpecDIC) <- "numeric"
data$carbSpecDIC <- data$carbSpecDIC *1000000
data$Date <- as.Date(data$Date)
data$MOATS <- as.numeric(data$MOATS)
#write.csv(data, "~/Desktop/2016-17_CrabExp_WaterChem/data.csv", quote = FALSE, row.names = FALSE)


########
#Compare to MOATS pH
########
#create empty DF for pH and DO stats
pH_stats <- data.frame()
DO_stats <- data.frame()

for (i in 1:length(treats)){
  #create an empty data frame
  #exp <- data[which(data$Date >= as.Date(treats[[i]]$Start_Date[i]) & data$Date <= as.Date(treats[[i]]$End_Date[i])),]
  exp <- data
  exp$datetime <- as.POSIXct(paste(exp$Date,exp$Time), format="%Y-%m-%d %H:%M:%S")
  colnames(exp)[4] <- "MOATSpH"
  colnames(exp)[7] <- "MOATSDO"
  #prepare tables for plotting pH and DO data
  exp_pH <- tidyr::gather(exp, "source", "pH",c(4,ncol(exp)-1))
  exp_DO <- tidyr::gather(exp, "source", "DO", c(7,18))
  
  #loop through each experimental pH condition and generate plots for data from each MOATS 
  for (j in 1:length(unique(exp_pH$Target_pH))){
    exppH <- unique(exp_pH$Target_pH)[j]
    #time series plot of MOATS pH and Spec pH data
    #p1 <- ggplot(exp_pH[which(exp_pH$Target_pH == exppH),], aes(x= datetime, y= pH)) + geom_point(aes(colour = source)) + scale_x_datetime("datetime", date_breaks = "day", date_labels = "%b %d", expand = c(0,0), minor_breaks = NULL) + xlab("Date") + facet_wrap(~MOATS) + ggtitle(paste(treats[[i]]$Experiment[i],treats[[i]]$Start_Date[i],treats[[i]]$End_Date[i],"pH",exppH, sep = "_")) + theme(axis.text.x=element_text(angle = 90, hjust = 0))
    p1 <- ggplot(exp_pH[which(exp_pH$Target_pH == exppH),], aes(x= datetime, y= pH)) + geom_point(aes(colour = source)) + scale_x_datetime("datetime", date_breaks = "day", date_labels = "%b %d", expand = c(0,0), minor_breaks = NULL) + xlab("Date") + facet_wrap(~MOATS) + ggtitle(paste(data$Experiment[1],data$Start_Date[1],data$Start_Date[nrow(data)],"pH",exppH, sep = "_")) + theme(axis.text.x=element_text(angle = 90, hjust = 0))
    
    #create a unique id for plot filename that contains the experiment subject, start and end date, and specific chemistry parameter
    #id <- paste(treats[[i]]$Experiment[i],treats[[i]]$Start_Date[i],treats[[i]]$End_Date[i],"pH",exppH,sep = "_")
    id <- paste(data$Experiment[1],data$Start_Date[i],treats[[i]]$End_Date[i],"pH",exppH,sep = "_")
    
    #preparing data for generating boxplots of MOATS pH data distributions over course of experiment
    #use unique command to remove duplicate lines that are included for overlapping experiments and exclude experiment column
    exppH_short <- unique(exp_pH[which(exp_pH$Target_pH == exppH & exp_pH$source == "MOATSpH"),-8])
    
    #boxplots of MOATS pH distributions
    p2 <- ggplot(exppH_short, aes(as.character(MOATS), pH)) + geom_boxplot(aes(fill= pH)) + xlab("MOATS") + ggtitle(paste(treats[[i]]$Experiment[i],treats[[i]]$Start_Date[i],treats[[i]]$End_Date[i],"pH",exppH, sep = "_"))
    p3 <- ggplot(exppH_short, aes(as.character(MOATS), pH)) + geom_violin(aes(fill = pH)) + geom_boxplot(aes(fill= pH), width = 0.1) + facet_wrap(~MOATS, scale = "free") + xlab("MOATS") + ggtitle(paste(treats[[i]]$Experiment[i],treats[[i]]$Start_Date[i],treats[[i]]$End_Date[i],"pH",exppH, sep = "_"))
    
    #create a stats table with mean, standard deviation, and standard error
    means <- aggregate(pH ~  MOATS, exppH_short, mean)
    sds <- aggregate(pH ~  MOATS, exppH_short, sd)
    ses <- aggregate(pH ~  MOATS, exppH_short, plotrix::std.error)
    sumstats <- merge(means, sds, by = "MOATS")
    sumstats <- merge(sumstats, ses, by = "MOATS")
    colnames(sumstats) <- c("MOATS", "mean", "stdev", "stderr")
    #add experiment column to stats table to distiguish stats
    sumstats$experiment <- id
    sumstats$MOATS_total_mean <- mean(exppH_short$pH, na.rm = TRUE)
    sumstats$MOATS_total_sd <- sd(exppH_short$pH, na.rm = TRUE)
    sumstats$MOATS_total_se <- std.error(exppH_short$pH)
    sumstats$Spec_total_mean <- mean(exp_pH[which(exp_pH$Target_pH == exppH & exp_pH$source == "carbSpecpH"),"pH"], na.rm = TRUE)
    sumstats$Spec_total_sd <- sd(exp_pH[which(exp_pH$Target_pH == exppH & exp_pH$source == "carbSpecpH"),"pH"], na.rm = TRUE)
    sumstats$Spec_total_se <- std.error(exp_pH[which(exp_pH$Target_pH == exppH & exp_pH$source == "carbSpecpH"),"pH"])
    pH_stats <- rbind(pH_stats,sumstats)
   
    #save plots of pH data comparisons
    ggsave(p1, file = paste("~/Desktop/2016-17_CrabExp_WaterChem/data/plots/pH/",id,".pdf", sep =""), width = 30, height = 10)
    ggsave(p2, file = paste("~/Desktop/2016-17_CrabExp_WaterChem/data/plots/pH/",id,"_allbox.pdf", sep =""), width = 5, height = 7)
    ggsave(p3, file = paste("~/Desktop/2016-17_CrabExp_WaterChem/data/plots/pH/",id,"_indvbox.pdf", sep =""), width = 5, height = 7)
    }
  #loop through each experimental DO condition and generate plots for data from each MOATS 
  for (k in 1:length(unique(exp_DO$Target_DO))){
    expDO <- unique(exp_DO$Target_DO)[k]
    p1 <- ggplot(exp_DO[grep(expDO,exp_DO$Target_DO),], aes(x= datetime, y= DO)) + geom_point(aes(colour = source)) + scale_x_datetime("datetime", date_breaks = "day", date_labels = "%b %d", expand = c(0,0), minor_breaks = NULL) + xlab("Date") + facet_wrap(~MOATS) + ggtitle(paste(treats[[i]]$Experiment[i],treats[[i]]$Start_Date[i],treats[[i]]$End_Date[i],"DO",expDO, sep = "_")) + theme(axis.text.x=element_text(angle = 90, hjust = 0))
    
    #create a unique id for plot filename that contains the experiment subject, start and end date, and specific chemistry parameter
    id <- paste(treats[[i]]$Experiment[i],treats[[i]]$Start_Date[i],treats[[i]]$End_Date[i],"DO",expDO,sep = "_")
    
    #preparing data for generating boxplots of MOATS DO data distributions over course of experiment
    #use unique command to remove duplicate lines that are included for overlapping experiments and exclude experiment column
    expDO_short <- unique(exp_DO[which(exp_DO$Target_DO == expDO & exp_DO$source == "MOATSDO"),-8])

    #boxplots of MOATS DO distributions
    p2 <- ggplot(expDO_short, aes(as.character(MOATS), DO)) + geom_boxplot(aes(fill= DO)) + xlab("MOATS") + ggtitle(paste(treats[[i]]$Experiment[i],treats[[i]]$Start_Date[i],treats[[i]]$End_Date[i],"DO",expDO, sep = "_"))
    p3 <- ggplot(expDO_short, aes(as.character(MOATS), DO)) + geom_violin(aes(fill = DO)) + geom_boxplot(aes(fill= DO), width = 0.1) + facet_wrap(~MOATS, scale = "free") + xlab("MOATS") + ggtitle(paste(treats[[i]]$Experiment[i],treats[[i]]$Start_Date[i],treats[[i]]$End_Date[i],"DO",expDO, sep = "_"))
    
    #create a stats table with mean, standard deviation, and standard error
    means <- aggregate(DO ~  MOATS, expDO_short, mean)
    sds <- aggregate(DO ~  MOATS, expDO_short, sd)
    ses <- aggregate(DO ~  MOATS, expDO_short, plotrix::std.error)
    sumstats <- merge(means, sds, by = "MOATS")
    sumstats <- merge(sumstats, ses, by = "MOATS")
    colnames(sumstats) <- c("MOATS", "mean", "stdev", "stderr")
    
    #add experiment column to stats table to distiguish stats
    sumstats$experiment <- id
    sumstats$MOATS_total_mean <- mean(expDO_short$DO, na.rm = TRUE)
    sumstats$MOATS_total_sd <- sd(expDO_short$DO, na.rm = TRUE)
    sumstats$MOATS_total_se <- std.error(expDO_short$DO)
    sumstats$Presens_total_mean <- mean(exp_DO[which(exp_DO$Target_DO == expDO & exp_DO$source == "presensDO"),"DO"], na.rm = TRUE)
    sumstats$Presens_total_sd <- sd(exp_DO[which(exp_DO$Target_DO == expDO & exp_DO$source == "presensDO"),"DO"], na.rm = TRUE)
    sumstats$Presens_total_se <- std.error(exp_DO[which(exp_DO$Target_DO == expDO & exp_DO$source == "presensDO"),"DO"])
    DO_stats <- rbind(DO_stats,sumstats)
    
    #save plots of DO data comparisons
    ggsave(p1, file = paste("~/Desktop/2016-17_CrabExp_WaterChem/data/plots/DO/",id,".pdf", sep =""), width = 30, height = 10)
    ggsave(p2, file = paste("~/Desktop/2016-17_CrabExp_WaterChem/data/plots/DO/",id,"_allbox.pdf", sep =""), width = 5, height = 7)
    ggsave(p3, file = paste("~/Desktop/2016-17_CrabExp_WaterChem/data/plots/DO/",id,"_indvbox.pdf", sep =""), width = 5, height = 7)
    
    }
  }

  #gives warning message: removed 14638 rows containing missing values (geom_point). This is because of the days that no Spec pH was taken.
  #Here is the code that shows that:
  #View(exp[which(exp$Target_pH ==7.4 & is.na(exp$pH)),])
overall_pH_stats <- unique(pH_stats[,5:ncol(pH_stats)])
overall_DO_stats <- unique(DO_stats[,5:ncol(DO_stats)])
indv_MOATS_pH_stats <- pH_stats[,1:5]
indv_MOATS_DO_stats <- DO_stats[,1:5]

write.csv(overall_pH_stats,"~/Desktop/2016-17_CrabExp_WaterChem/data/overall_pH_stats.csv", row.names = FALSE, quote = FALSE)
write.csv(overall_DO_stats,"~/Desktop/2016-17_CrabExp_WaterChem/data/overall_DO_stats.csv", row.names = FALSE, quote = FALSE)
write.csv(indv_MOATS_DO_stats,"~/Desktop/2016-17_CrabExp_WaterChem/data/indv_MOATS_DO_stats.csv", row.names = FALSE, quote = FALSE)
write.csv(indv_MOATS_pH_stats,"~/Desktop/2016-17_CrabExp_WaterChem/data/indv_MOATS_pH_stats.csv", row.names = FALSE, quote = FALSE)



#################
###Check for overlapping dates in different data types
#####################
#dates_Msal <- data.frame(unique(Master_data[which(Master_data$Data_Type == "Muk_salinity"),1:2]))
#length = 125
#colnames(dates_Msal)[1] <- "Date"

#dates_PMEL <- data.frame(unique(Master_data[which(Master_data$Data_Type == "PMEL"),1:2]))
#length = 38
#colnames(dates_PMEL)[1] <- "Date"

#dates_Spec <- data.frame(unique(Master_data[which(Master_data$Data_Type == "Spec"),1:2]))
#length = 101
#colnames(dates_Spec)[1] <- "Date"

#sal_date_overlap <- rbind(dates_Spec,dates_PMEL,dates_Msal)


###using muk_salinity measurements as salinity values for each MOATS on or near the corresponding date

#first, need to fill in data for dates that are in between Muk_salinity readings



#system('awk -F, \'NR==FNR{a[($1","$2")]=$0;next}{if(($1","$2") in a)print $0","a[($1","$2")];else print $0}\' /Users/Shelly/Desktop/2016-17_CrabExp_WaterChem/data/2016-2017_MOATS_Treatments.csv /Users/Shelly/Desktop/2016-17_CrabExp_WaterChem/Master_data.csv >  /Users/Shelly/Desktop/2016-17_CrabExp_WaterChem/Master_Data_w_treat.csv)

####Practice with SeaCarb####

#alk <- seq(1000,3000,100)
#alk <- alk / 1000000
#pHspec <- 7.35
#carbSpec <- carb(flag = 8, pHspec, alk,S = 29.5, T = 25)
#carbMOATS <- carb(flag = 15, alk, carbSpec$DIC,S = 29.5, T= 9, )
#View(carbMOATS)


###End of Practice######



