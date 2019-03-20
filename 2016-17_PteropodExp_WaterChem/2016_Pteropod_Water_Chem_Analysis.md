2016 Pteropod Water Chem
================
Shelly Trigg
3/19/2019

load libraries

    ## 
    ## Attaching package: 'dplyr'

    ## The following objects are masked from 'package:stats':
    ## 
    ##     filter, lag

    ## The following objects are masked from 'package:base':
    ## 
    ##     intersect, setdiff, setequal, union

    ## Loading required package: oce

    ## Loading required package: testthat

    ## 
    ## Attaching package: 'testthat'

    ## The following object is masked from 'package:dplyr':
    ## 
    ##     matches

    ## Loading required package: gsw

read in and format spec data

``` r
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
```

    ## 'data.frame':    122 obs. of  5 variables:
    ##  $ MOATS   : num  1 1 1 1 1 1 1 1 2 2 ...
    ##  $ Jar     : chr  "C7" "" "" "" ...
    ##  $ Date    : chr  "22-Nov-16" "24-Nov-16" "24-Nov-16" "26-Nov-16" ...
    ##  $ Salinity: num  29.6 29.6 29.6 29.6 29.6 ...
    ##  $ pH      : num  7.4 7.46 7.46 7.37 7.39 ...

``` r
Spec_pH$Date <- as.POSIXct(strptime(Spec_pH$Date, "%e-%b-%y"))

#Add treatment column
lowMOATS <- c(1,4,5,8,10,11)
Spec_pH$Treatment <- "High"
Spec_pH$Treatment[Spec_pH$MOATS %in%lowMOATS] <- "Low"
colnames(Spec_pH)[5] <- "pH_at_25C"
Spec_pH$Data_Type <- "Spec"
```

read in and format MOATS data

``` r
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
```

    ## [1] "/Users/Shelly/Documents/GitHub/Seawater-Chemistry-Analysis/2016-17_PteropodExp_WaterChem/PteropodWaterChem/MOATS_data/m1/m_16-11-21_1512_04.lvm"
    ## [1] "/Users/Shelly/Documents/GitHub/Seawater-Chemistry-Analysis/2016-17_PteropodExp_WaterChem/PteropodWaterChem/MOATS_data/m1/m_16-11-22_1512_05.lvm"
    ## [1] "/Users/Shelly/Documents/GitHub/Seawater-Chemistry-Analysis/2016-17_PteropodExp_WaterChem/PteropodWaterChem/MOATS_data/m1/m_16-11-23_1512_06.lvm"
    ## [1] "/Users/Shelly/Documents/GitHub/Seawater-Chemistry-Analysis/2016-17_PteropodExp_WaterChem/PteropodWaterChem/MOATS_data/m1/m_16-11-24_1512_07.lvm"
    ## [1] "/Users/Shelly/Documents/GitHub/Seawater-Chemistry-Analysis/2016-17_PteropodExp_WaterChem/PteropodWaterChem/MOATS_data/m1/m_16-11-25_1512_08.lvm"
    ## [1] "/Users/Shelly/Documents/GitHub/Seawater-Chemistry-Analysis/2016-17_PteropodExp_WaterChem/PteropodWaterChem/MOATS_data/m1/m_16-11-26_1512_09.lvm"
    ## [1] "/Users/Shelly/Documents/GitHub/Seawater-Chemistry-Analysis/2016-17_PteropodExp_WaterChem/PteropodWaterChem/MOATS_data/m1/m_16-11-27_1512_10.lvm"
    ## [1] "/Users/Shelly/Documents/GitHub/Seawater-Chemistry-Analysis/2016-17_PteropodExp_WaterChem/PteropodWaterChem/MOATS_data/m1/m_16-11-28_1512_11.lvm"
    ## [1] "/Users/Shelly/Documents/GitHub/Seawater-Chemistry-Analysis/2016-17_PteropodExp_WaterChem/PteropodWaterChem/MOATS_data/m1/m_16-11-29_1512_12.lvm"
    ## [1] "/Users/Shelly/Documents/GitHub/Seawater-Chemistry-Analysis/2016-17_PteropodExp_WaterChem/PteropodWaterChem/MOATS_data/m10/m_16-11-21_1530_04.lvm"
    ## [1] "/Users/Shelly/Documents/GitHub/Seawater-Chemistry-Analysis/2016-17_PteropodExp_WaterChem/PteropodWaterChem/MOATS_data/m10/m_16-11-22_1530_05.lvm"
    ## [1] "/Users/Shelly/Documents/GitHub/Seawater-Chemistry-Analysis/2016-17_PteropodExp_WaterChem/PteropodWaterChem/MOATS_data/m10/m_16-11-23_1530_06.lvm"
    ## [1] "/Users/Shelly/Documents/GitHub/Seawater-Chemistry-Analysis/2016-17_PteropodExp_WaterChem/PteropodWaterChem/MOATS_data/m10/m_16-11-24_1530_07.lvm"
    ## [1] "/Users/Shelly/Documents/GitHub/Seawater-Chemistry-Analysis/2016-17_PteropodExp_WaterChem/PteropodWaterChem/MOATS_data/m10/m_16-11-25_1530_08.lvm"
    ## [1] "/Users/Shelly/Documents/GitHub/Seawater-Chemistry-Analysis/2016-17_PteropodExp_WaterChem/PteropodWaterChem/MOATS_data/m10/m_16-11-26_1530_09.lvm"
    ## [1] "/Users/Shelly/Documents/GitHub/Seawater-Chemistry-Analysis/2016-17_PteropodExp_WaterChem/PteropodWaterChem/MOATS_data/m10/m_16-11-27_1530_10.lvm"
    ## [1] "/Users/Shelly/Documents/GitHub/Seawater-Chemistry-Analysis/2016-17_PteropodExp_WaterChem/PteropodWaterChem/MOATS_data/m10/m_16-11-28_1530_11.lvm"
    ## [1] "/Users/Shelly/Documents/GitHub/Seawater-Chemistry-Analysis/2016-17_PteropodExp_WaterChem/PteropodWaterChem/MOATS_data/m10/m_16-11-29_1530_12.lvm"
    ## [1] "/Users/Shelly/Documents/GitHub/Seawater-Chemistry-Analysis/2016-17_PteropodExp_WaterChem/PteropodWaterChem/MOATS_data/m10/m_16-11-30_1530_13.lvm"
    ## [1] "/Users/Shelly/Documents/GitHub/Seawater-Chemistry-Analysis/2016-17_PteropodExp_WaterChem/PteropodWaterChem/MOATS_data/m11/m_16-11-21_1525_04.lvm"
    ## [1] "/Users/Shelly/Documents/GitHub/Seawater-Chemistry-Analysis/2016-17_PteropodExp_WaterChem/PteropodWaterChem/MOATS_data/m11/m_16-11-22_1525_05.lvm"
    ## [1] "/Users/Shelly/Documents/GitHub/Seawater-Chemistry-Analysis/2016-17_PteropodExp_WaterChem/PteropodWaterChem/MOATS_data/m11/m_16-11-23_1525_06.lvm"
    ## [1] "/Users/Shelly/Documents/GitHub/Seawater-Chemistry-Analysis/2016-17_PteropodExp_WaterChem/PteropodWaterChem/MOATS_data/m11/m_16-11-24_1525_07.lvm"
    ## [1] "/Users/Shelly/Documents/GitHub/Seawater-Chemistry-Analysis/2016-17_PteropodExp_WaterChem/PteropodWaterChem/MOATS_data/m11/m_16-11-25_1525_08.lvm"
    ## [1] "/Users/Shelly/Documents/GitHub/Seawater-Chemistry-Analysis/2016-17_PteropodExp_WaterChem/PteropodWaterChem/MOATS_data/m11/m_16-11-26_1525_09.lvm"
    ## [1] "/Users/Shelly/Documents/GitHub/Seawater-Chemistry-Analysis/2016-17_PteropodExp_WaterChem/PteropodWaterChem/MOATS_data/m11/m_16-11-27_1525_10.lvm"
    ## [1] "/Users/Shelly/Documents/GitHub/Seawater-Chemistry-Analysis/2016-17_PteropodExp_WaterChem/PteropodWaterChem/MOATS_data/m11/m_16-11-28_1525_11.lvm"
    ## [1] "/Users/Shelly/Documents/GitHub/Seawater-Chemistry-Analysis/2016-17_PteropodExp_WaterChem/PteropodWaterChem/MOATS_data/m11/m_16-11-29_1525_12.lvm"
    ## [1] "/Users/Shelly/Documents/GitHub/Seawater-Chemistry-Analysis/2016-17_PteropodExp_WaterChem/PteropodWaterChem/MOATS_data/m11/m_16-11-30_1525_13.lvm"
    ## [1] "/Users/Shelly/Documents/GitHub/Seawater-Chemistry-Analysis/2016-17_PteropodExp_WaterChem/PteropodWaterChem/MOATS_data/m12/m_16-11-21_1527_04.lvm"
    ## [1] "/Users/Shelly/Documents/GitHub/Seawater-Chemistry-Analysis/2016-17_PteropodExp_WaterChem/PteropodWaterChem/MOATS_data/m12/m_16-11-22_1527_05.lvm"
    ## [1] "/Users/Shelly/Documents/GitHub/Seawater-Chemistry-Analysis/2016-17_PteropodExp_WaterChem/PteropodWaterChem/MOATS_data/m12/m_16-11-23_1527_06.lvm"
    ## [1] "/Users/Shelly/Documents/GitHub/Seawater-Chemistry-Analysis/2016-17_PteropodExp_WaterChem/PteropodWaterChem/MOATS_data/m12/m_16-11-24_1527_07.lvm"
    ## [1] "/Users/Shelly/Documents/GitHub/Seawater-Chemistry-Analysis/2016-17_PteropodExp_WaterChem/PteropodWaterChem/MOATS_data/m12/m_16-11-25_1527_08.lvm"
    ## [1] "/Users/Shelly/Documents/GitHub/Seawater-Chemistry-Analysis/2016-17_PteropodExp_WaterChem/PteropodWaterChem/MOATS_data/m12/m_16-11-26_1527_09.lvm"
    ## [1] "/Users/Shelly/Documents/GitHub/Seawater-Chemistry-Analysis/2016-17_PteropodExp_WaterChem/PteropodWaterChem/MOATS_data/m12/m_16-11-27_1527_10.lvm"
    ## [1] "/Users/Shelly/Documents/GitHub/Seawater-Chemistry-Analysis/2016-17_PteropodExp_WaterChem/PteropodWaterChem/MOATS_data/m12/m_16-11-28_1527_11.lvm"
    ## [1] "/Users/Shelly/Documents/GitHub/Seawater-Chemistry-Analysis/2016-17_PteropodExp_WaterChem/PteropodWaterChem/MOATS_data/m12/m_16-11-29_1527_12.lvm"
    ## [1] "/Users/Shelly/Documents/GitHub/Seawater-Chemistry-Analysis/2016-17_PteropodExp_WaterChem/PteropodWaterChem/MOATS_data/m12/m_16-11-30_1527_13.lvm"
    ## [1] "/Users/Shelly/Documents/GitHub/Seawater-Chemistry-Analysis/2016-17_PteropodExp_WaterChem/PteropodWaterChem/MOATS_data/m13/m_16-11-21_1053_02.lvm"
    ## [1] "/Users/Shelly/Documents/GitHub/Seawater-Chemistry-Analysis/2016-17_PteropodExp_WaterChem/PteropodWaterChem/MOATS_data/m13/m_16-11-22_1053_03.lvm"
    ## [1] "/Users/Shelly/Documents/GitHub/Seawater-Chemistry-Analysis/2016-17_PteropodExp_WaterChem/PteropodWaterChem/MOATS_data/m13/m_16-11-23_1053_04.lvm"
    ## [1] "/Users/Shelly/Documents/GitHub/Seawater-Chemistry-Analysis/2016-17_PteropodExp_WaterChem/PteropodWaterChem/MOATS_data/m13/m_16-11-24_1053_05.lvm"
    ## [1] "/Users/Shelly/Documents/GitHub/Seawater-Chemistry-Analysis/2016-17_PteropodExp_WaterChem/PteropodWaterChem/MOATS_data/m13/m_16-11-25_1053_06.lvm"
    ## [1] "/Users/Shelly/Documents/GitHub/Seawater-Chemistry-Analysis/2016-17_PteropodExp_WaterChem/PteropodWaterChem/MOATS_data/m13/m_16-11-26_1053_07.lvm"
    ## [1] "/Users/Shelly/Documents/GitHub/Seawater-Chemistry-Analysis/2016-17_PteropodExp_WaterChem/PteropodWaterChem/MOATS_data/m13/m_16-11-27_1053_08.lvm"
    ## [1] "/Users/Shelly/Documents/GitHub/Seawater-Chemistry-Analysis/2016-17_PteropodExp_WaterChem/PteropodWaterChem/MOATS_data/m13/m_16-11-28_1053_09.lvm"
    ## [1] "/Users/Shelly/Documents/GitHub/Seawater-Chemistry-Analysis/2016-17_PteropodExp_WaterChem/PteropodWaterChem/MOATS_data/m13/m_16-11-29_1053_10.lvm"
    ## [1] "/Users/Shelly/Documents/GitHub/Seawater-Chemistry-Analysis/2016-17_PteropodExp_WaterChem/PteropodWaterChem/MOATS_data/m13/m_16-11-30_1053_11.lvm"
    ## [1] "/Users/Shelly/Documents/GitHub/Seawater-Chemistry-Analysis/2016-17_PteropodExp_WaterChem/PteropodWaterChem/MOATS_data/m3/m_16-11-20_1525_03.lvm"
    ## [1] "/Users/Shelly/Documents/GitHub/Seawater-Chemistry-Analysis/2016-17_PteropodExp_WaterChem/PteropodWaterChem/MOATS_data/m3/m_16-11-21_1525_04.lvm"
    ## [1] "/Users/Shelly/Documents/GitHub/Seawater-Chemistry-Analysis/2016-17_PteropodExp_WaterChem/PteropodWaterChem/MOATS_data/m3/m_16-11-22_1525_05.lvm"
    ## [1] "/Users/Shelly/Documents/GitHub/Seawater-Chemistry-Analysis/2016-17_PteropodExp_WaterChem/PteropodWaterChem/MOATS_data/m3/m_16-11-23_1525_06.lvm"
    ## [1] "/Users/Shelly/Documents/GitHub/Seawater-Chemistry-Analysis/2016-17_PteropodExp_WaterChem/PteropodWaterChem/MOATS_data/m3/m_16-11-24_1525_07.lvm"
    ## [1] "/Users/Shelly/Documents/GitHub/Seawater-Chemistry-Analysis/2016-17_PteropodExp_WaterChem/PteropodWaterChem/MOATS_data/m3/m_16-11-25_1525_08.lvm"
    ## [1] "/Users/Shelly/Documents/GitHub/Seawater-Chemistry-Analysis/2016-17_PteropodExp_WaterChem/PteropodWaterChem/MOATS_data/m3/m_16-11-26_1525_09.lvm"
    ## [1] "/Users/Shelly/Documents/GitHub/Seawater-Chemistry-Analysis/2016-17_PteropodExp_WaterChem/PteropodWaterChem/MOATS_data/m3/m_16-11-27_1525_10.lvm"
    ## [1] "/Users/Shelly/Documents/GitHub/Seawater-Chemistry-Analysis/2016-17_PteropodExp_WaterChem/PteropodWaterChem/MOATS_data/m3/m_16-11-28_1525_11.lvm"
    ## [1] "/Users/Shelly/Documents/GitHub/Seawater-Chemistry-Analysis/2016-17_PteropodExp_WaterChem/PteropodWaterChem/MOATS_data/m3/m_16-11-29_1525_12.lvm"
    ## [1] "/Users/Shelly/Documents/GitHub/Seawater-Chemistry-Analysis/2016-17_PteropodExp_WaterChem/PteropodWaterChem/MOATS_data/m4/m_16-11-21_1517_04.lvm"
    ## [1] "/Users/Shelly/Documents/GitHub/Seawater-Chemistry-Analysis/2016-17_PteropodExp_WaterChem/PteropodWaterChem/MOATS_data/m4/m_16-11-22_1517_05.lvm"
    ## [1] "/Users/Shelly/Documents/GitHub/Seawater-Chemistry-Analysis/2016-17_PteropodExp_WaterChem/PteropodWaterChem/MOATS_data/m4/m_16-11-23_1517_06.lvm"
    ## [1] "/Users/Shelly/Documents/GitHub/Seawater-Chemistry-Analysis/2016-17_PteropodExp_WaterChem/PteropodWaterChem/MOATS_data/m4/m_16-11-24_1517_07.lvm"
    ## [1] "/Users/Shelly/Documents/GitHub/Seawater-Chemistry-Analysis/2016-17_PteropodExp_WaterChem/PteropodWaterChem/MOATS_data/m4/m_16-11-25_1517_08.lvm"
    ## [1] "/Users/Shelly/Documents/GitHub/Seawater-Chemistry-Analysis/2016-17_PteropodExp_WaterChem/PteropodWaterChem/MOATS_data/m4/m_16-11-26_1517_09.lvm"
    ## [1] "/Users/Shelly/Documents/GitHub/Seawater-Chemistry-Analysis/2016-17_PteropodExp_WaterChem/PteropodWaterChem/MOATS_data/m4/m_16-11-27_1517_10.lvm"
    ## [1] "/Users/Shelly/Documents/GitHub/Seawater-Chemistry-Analysis/2016-17_PteropodExp_WaterChem/PteropodWaterChem/MOATS_data/m4/m_16-11-28_1517_11.lvm"
    ## [1] "/Users/Shelly/Documents/GitHub/Seawater-Chemistry-Analysis/2016-17_PteropodExp_WaterChem/PteropodWaterChem/MOATS_data/m4/m_16-11-29_1517_12.lvm"
    ## [1] "/Users/Shelly/Documents/GitHub/Seawater-Chemistry-Analysis/2016-17_PteropodExp_WaterChem/PteropodWaterChem/MOATS_data/m5/m_16-11-21_1522_04.lvm"
    ## [1] "/Users/Shelly/Documents/GitHub/Seawater-Chemistry-Analysis/2016-17_PteropodExp_WaterChem/PteropodWaterChem/MOATS_data/m5/m_16-11-22_1522_05.lvm"
    ## [1] "/Users/Shelly/Documents/GitHub/Seawater-Chemistry-Analysis/2016-17_PteropodExp_WaterChem/PteropodWaterChem/MOATS_data/m5/m_16-11-23_1522_06.lvm"
    ## [1] "/Users/Shelly/Documents/GitHub/Seawater-Chemistry-Analysis/2016-17_PteropodExp_WaterChem/PteropodWaterChem/MOATS_data/m5/m_16-11-24_1522_07.lvm"
    ## [1] "/Users/Shelly/Documents/GitHub/Seawater-Chemistry-Analysis/2016-17_PteropodExp_WaterChem/PteropodWaterChem/MOATS_data/m5/m_16-11-25_1522_08.lvm"
    ## [1] "/Users/Shelly/Documents/GitHub/Seawater-Chemistry-Analysis/2016-17_PteropodExp_WaterChem/PteropodWaterChem/MOATS_data/m5/m_16-11-26_1522_09.lvm"
    ## [1] "/Users/Shelly/Documents/GitHub/Seawater-Chemistry-Analysis/2016-17_PteropodExp_WaterChem/PteropodWaterChem/MOATS_data/m5/m_16-11-27_1522_10.lvm"
    ## [1] "/Users/Shelly/Documents/GitHub/Seawater-Chemistry-Analysis/2016-17_PteropodExp_WaterChem/PteropodWaterChem/MOATS_data/m5/m_16-11-28_1522_11.lvm"
    ## [1] "/Users/Shelly/Documents/GitHub/Seawater-Chemistry-Analysis/2016-17_PteropodExp_WaterChem/PteropodWaterChem/MOATS_data/m5/m_16-11-29_1522_12.lvm"
    ## [1] "/Users/Shelly/Documents/GitHub/Seawater-Chemistry-Analysis/2016-17_PteropodExp_WaterChem/PteropodWaterChem/MOATS_data/m6/m_16-11-21_1517_04.lvm"
    ## [1] "/Users/Shelly/Documents/GitHub/Seawater-Chemistry-Analysis/2016-17_PteropodExp_WaterChem/PteropodWaterChem/MOATS_data/m6/m_16-11-22_1517_05.lvm"
    ## [1] "/Users/Shelly/Documents/GitHub/Seawater-Chemistry-Analysis/2016-17_PteropodExp_WaterChem/PteropodWaterChem/MOATS_data/m6/m_16-11-23_1517_06.lvm"
    ## [1] "/Users/Shelly/Documents/GitHub/Seawater-Chemistry-Analysis/2016-17_PteropodExp_WaterChem/PteropodWaterChem/MOATS_data/m6/m_16-11-24_1517_07.lvm"
    ## [1] "/Users/Shelly/Documents/GitHub/Seawater-Chemistry-Analysis/2016-17_PteropodExp_WaterChem/PteropodWaterChem/MOATS_data/m6/m_16-11-25_1517_08.lvm"
    ## [1] "/Users/Shelly/Documents/GitHub/Seawater-Chemistry-Analysis/2016-17_PteropodExp_WaterChem/PteropodWaterChem/MOATS_data/m6/m_16-11-26_1517_09.lvm"
    ## [1] "/Users/Shelly/Documents/GitHub/Seawater-Chemistry-Analysis/2016-17_PteropodExp_WaterChem/PteropodWaterChem/MOATS_data/m6/m_16-11-27_1517_10.lvm"
    ## [1] "/Users/Shelly/Documents/GitHub/Seawater-Chemistry-Analysis/2016-17_PteropodExp_WaterChem/PteropodWaterChem/MOATS_data/m6/m_16-11-28_1517_11.lvm"
    ## [1] "/Users/Shelly/Documents/GitHub/Seawater-Chemistry-Analysis/2016-17_PteropodExp_WaterChem/PteropodWaterChem/MOATS_data/m6/m_16-11-29_1517_12.lvm"
    ## [1] "/Users/Shelly/Documents/GitHub/Seawater-Chemistry-Analysis/2016-17_PteropodExp_WaterChem/PteropodWaterChem/MOATS_data/m6/m_16-11-30_1517_13.lvm"
    ## [1] "/Users/Shelly/Documents/GitHub/Seawater-Chemistry-Analysis/2016-17_PteropodExp_WaterChem/PteropodWaterChem/MOATS_data/m7/m_16-11-21_1521_04.lvm"
    ## [1] "/Users/Shelly/Documents/GitHub/Seawater-Chemistry-Analysis/2016-17_PteropodExp_WaterChem/PteropodWaterChem/MOATS_data/m7/m_16-11-22_1521_05.lvm"
    ## [1] "/Users/Shelly/Documents/GitHub/Seawater-Chemistry-Analysis/2016-17_PteropodExp_WaterChem/PteropodWaterChem/MOATS_data/m7/m_16-11-23_1521_06.lvm"
    ## [1] "/Users/Shelly/Documents/GitHub/Seawater-Chemistry-Analysis/2016-17_PteropodExp_WaterChem/PteropodWaterChem/MOATS_data/m7/m_16-11-24_1521_07.lvm"
    ## [1] "/Users/Shelly/Documents/GitHub/Seawater-Chemistry-Analysis/2016-17_PteropodExp_WaterChem/PteropodWaterChem/MOATS_data/m7/m_16-11-25_1521_08.lvm"
    ## [1] "/Users/Shelly/Documents/GitHub/Seawater-Chemistry-Analysis/2016-17_PteropodExp_WaterChem/PteropodWaterChem/MOATS_data/m7/m_16-11-26_1521_09.lvm"
    ## [1] "/Users/Shelly/Documents/GitHub/Seawater-Chemistry-Analysis/2016-17_PteropodExp_WaterChem/PteropodWaterChem/MOATS_data/m7/m_16-11-27_1521_10.lvm"
    ## [1] "/Users/Shelly/Documents/GitHub/Seawater-Chemistry-Analysis/2016-17_PteropodExp_WaterChem/PteropodWaterChem/MOATS_data/m7/m_16-11-28_1521_11.lvm"
    ## [1] "/Users/Shelly/Documents/GitHub/Seawater-Chemistry-Analysis/2016-17_PteropodExp_WaterChem/PteropodWaterChem/MOATS_data/m7/m_16-11-29_1521_12.lvm"
    ## [1] "/Users/Shelly/Documents/GitHub/Seawater-Chemistry-Analysis/2016-17_PteropodExp_WaterChem/PteropodWaterChem/MOATS_data/m7/m_16-11-30_1521_13.lvm"
    ## [1] "/Users/Shelly/Documents/GitHub/Seawater-Chemistry-Analysis/2016-17_PteropodExp_WaterChem/PteropodWaterChem/MOATS_data/m8/m_16-11-21_1524_04.lvm"
    ## [1] "/Users/Shelly/Documents/GitHub/Seawater-Chemistry-Analysis/2016-17_PteropodExp_WaterChem/PteropodWaterChem/MOATS_data/m8/m_16-11-21_2248_01.lvm"
    ## [1] "/Users/Shelly/Documents/GitHub/Seawater-Chemistry-Analysis/2016-17_PteropodExp_WaterChem/PteropodWaterChem/MOATS_data/m8/m_16-11-22_2249_02.lvm"
    ## [1] "/Users/Shelly/Documents/GitHub/Seawater-Chemistry-Analysis/2016-17_PteropodExp_WaterChem/PteropodWaterChem/MOATS_data/m8/m_16-11-23_2249_03.lvm"
    ## [1] "/Users/Shelly/Documents/GitHub/Seawater-Chemistry-Analysis/2016-17_PteropodExp_WaterChem/PteropodWaterChem/MOATS_data/m8/m_16-11-24_2249_04.lvm"
    ## [1] "/Users/Shelly/Documents/GitHub/Seawater-Chemistry-Analysis/2016-17_PteropodExp_WaterChem/PteropodWaterChem/MOATS_data/m8/m_16-11-25_2249_05.lvm"
    ## [1] "/Users/Shelly/Documents/GitHub/Seawater-Chemistry-Analysis/2016-17_PteropodExp_WaterChem/PteropodWaterChem/MOATS_data/m8/m_16-11-26_2249_06.lvm"
    ## [1] "/Users/Shelly/Documents/GitHub/Seawater-Chemistry-Analysis/2016-17_PteropodExp_WaterChem/PteropodWaterChem/MOATS_data/m8/m_16-11-27_2249_07.lvm"
    ## [1] "/Users/Shelly/Documents/GitHub/Seawater-Chemistry-Analysis/2016-17_PteropodExp_WaterChem/PteropodWaterChem/MOATS_data/m8/m_16-11-28_2249_08.lvm"
    ## [1] "/Users/Shelly/Documents/GitHub/Seawater-Chemistry-Analysis/2016-17_PteropodExp_WaterChem/PteropodWaterChem/MOATS_data/m8/m_16-11-29_2249_09.lvm"

restrict MOATS data to dates of the experiment

``` r
start_date <- as.POSIXct("2016/11/21_17:00:00", "%Y/%m/%d_%H:%M:%S", tz = "GMT")
end_date <- as.POSIXct("2016/11/30", "%Y/%m/%d", tz = "GMT")

d <- d[which(d$datetime >= start_date & d$datetime <= end_date),]
```

plot MOATS pH data

``` r
#plot MOATS pH data 
ggplot(d,aes(datetime, pH)) + geom_point() + facet_wrap(~MOATS)
```

![](2016_Pteropod_Water_Chem_Analysis_files/figure-markdown_github/MOATS%20pH%20data-1.png) blip on MOATS 1 between Nov 24-26; but could be real

plot MOATS temperature data

``` r
#plot temp data for all MOATS
ggplot(d,aes(datetime, OL)) + geom_point() + facet_wrap(~MOATS)
```

![](2016_Pteropod_Water_Chem_Analysis_files/figure-markdown_github/MOATS%20temperature%20data-1.png) MOATS 12 temp probe wonky, don't include that data

plot MOATS temperature data without MOATS 12 which seemed to have a wonky probe

``` r
#plot temp data without 12
ggplot(d[which(d$MOATS != 12),],aes(datetime, OL)) + geom_point() + facet_wrap(~MOATS)
```

![](2016_Pteropod_Water_Chem_Analysis_files/figure-markdown_github/MOATS%20temperature%20data,%20no%20MOATS%2012-1.png) all MOATS are a little spikey, suggesting these spikes are real

plot MOATS DO data

``` r
ggplot(d,aes(datetime, DO)) + geom_point() + facet_wrap(~MOATS)
```

![](2016_Pteropod_Water_Chem_Analysis_files/figure-markdown_github/MOATS%20DO%20data-1.png) MOATS 12 and MOATS 1 DO probes have weird blips between nov 24-26 and nov 25-27, respectively

calculate mean temp

``` r
mean(d[which(d$MOATS != 12),"OL"])
```

    ## [1] 10.29312

``` r
#[1] 10.29312
sd(d[which(d$MOATS != 12),"OL"])
```

    ## [1] 0.165076

``` r
#[1] 0.165076
meanT <- mean(d[which(d$MOATS != 12),"OL"])
```

Convert spec pH at 25C to pH at 12

``` r
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
```

    ## [1] 1
    ## [1] 2
    ## [1] 3
    ## [1] 4
    ## [1] 5
    ## [1] 6
    ## [1] 7
    ## [1] 8
    ## [1] 9
    ## [1] 10
    ## [1] 11
    ## [1] 12
    ## [1] 13
    ## [1] 14
    ## [1] 15
    ## [1] 16
    ## [1] 17
    ## [1] 18
    ## [1] 19
    ## [1] 20
    ## [1] 21
    ## [1] 22
    ## [1] 23
    ## [1] 24
    ## [1] 25
    ## [1] 26
    ## [1] 27
    ## [1] 28
    ## [1] 29
    ## [1] 30
    ## [1] 31
    ## [1] 32
    ## [1] 33
    ## [1] 34
    ## [1] 35
    ## [1] 36
    ## [1] 37
    ## [1] 38
    ## [1] 39
    ## [1] 40
    ## [1] 41
    ## [1] 42
    ## [1] 43
    ## [1] 44
    ## [1] 45
    ## [1] 46
    ## [1] 47
    ## [1] 48
    ## [1] 49
    ## [1] 50
    ## [1] 51
    ## [1] 52
    ## [1] 53
    ## [1] 54
    ## [1] 55
    ## [1] 56
    ## [1] 57
    ## [1] 58
    ## [1] 59
    ## [1] 60
    ## [1] 61
    ## [1] 62
    ## [1] 63
    ## [1] 64
    ## [1] 65
    ## [1] 66
    ## [1] 67
    ## [1] 68
    ## [1] 69
    ## [1] 70
    ## [1] 71
    ## [1] 72
    ## [1] 73
    ## [1] 74
    ## [1] 75
    ## [1] 76
    ## [1] 77
    ## [1] 78
    ## [1] 79
    ## [1] 80
    ## [1] 81
    ## [1] 82
    ## [1] 83
    ## [1] 84
    ## [1] 85
    ## [1] 86
    ## [1] 87
    ## [1] 88
    ## [1] 89
    ## [1] 90
    ## [1] 91
    ## [1] 92
    ## [1] 93
    ## [1] 94
    ## [1] 95
    ## [1] 96
    ## [1] 97
    ## [1] 98
    ## [1] 99
    ## [1] 100
    ## [1] 101
    ## [1] 102
    ## [1] 103
    ## [1] 104
    ## [1] 105
    ## [1] 106
    ## [1] 107
    ## [1] 108
    ## [1] 109
    ## [1] 110
    ## [1] 111
    ## [1] 112
    ## [1] 113
    ## [1] 114
    ## [1] 115
    ## [1] 116
    ## [1] 117
    ## [1] 118
    ## [1] 119
    ## [1] 120
    ## [1] 121
    ## [1] 122

``` r
class(Spec_pH$carbSpecpH) <- "numeric"
class(Spec_pH$carbSpecDIC) <- "numeric"
Spec_pH$carbSpecDIC <- Spec_pH$carbSpecDIC *1000000
```

plot spec pH over time

``` r
ggplot(Spec_pH, aes(Date,carbSpecpH)) + geom_point(aes(color = Treatment)) + facet_wrap(~MOATS)
```

![](2016_Pteropod_Water_Chem_Analysis_files/figure-markdown_github/spec%20pH%20over%20time-1.png) the Nov 22 spec pH for MOATS 13 seems very off; probably can exclude that

boxplots of spec pH facetted by treatment; exclude Nov 22 spec pH for MOATS 13

``` r
ggplot(Spec_pH[which(Spec_pH$MOATS !=13 & Spec_pH$Date != as.POSIXct("2016-11-22")),]) + geom_boxplot(aes(Treatment,carbSpecpH, color = Treatment))
```

![](2016_Pteropod_Water_Chem_Analysis_files/figure-markdown_github/spec%20pH%20treatment%20boxplots-1.png)

Calculate mean and sd for Spec pH

``` r
mean(Spec_pH[which(Spec_pH$MOATS !=13 & Spec_pH$Date != as.POSIXct("2016-11-22") & Spec_pH$Treatment == "Low"),"carbSpecpH"])
```

    ## [1] 7.539746

``` r
sd(Spec_pH[which(Spec_pH$MOATS !=13 & Spec_pH$Date != as.POSIXct("2016-11-22") & Spec_pH$Treatment == "Low"),"carbSpecpH"])
```

    ## [1] 0.04190024

``` r
mean(Spec_pH[which(Spec_pH$MOATS !=13 & Spec_pH$Date != as.POSIXct("2016-11-22") & Spec_pH$Treatment == "High"),"carbSpecpH"])
```

    ## [1] 7.869512

``` r
sd(Spec_pH[which(Spec_pH$MOATS !=13 & Spec_pH$Date != as.POSIXct("2016-11-22") & Spec_pH$Treatment == "High"),"carbSpecpH"])
```

    ## [1] 0.0340665

read in target treatment info and merge with MOATS data so data can be averaged by treatment

``` r
treat <- read.csv("~/Documents/GitHub/Seawater-Chemistry-Analysis/2016-17_PteropodExp_WaterChem/PteropodWaterChem/Treatments.csv", stringsAsFactors = FALSE)
#merge treatment info with MOATS info
dtreat <- merge(d, treat, by = "MOATS", all.x = TRUE)
```

Calculate DO for each treatment I'm leaving in the blips in MOATS 1 and 12 because I don't know if they are real or not

``` r
mean(dtreat[grep("HH", dtreat$Treatment_abbv),"DO"])
```

    ## [1] 9.310603

``` r
sd(dtreat[grep("HH", dtreat$Treatment_abbv),"DO"])
```

    ## [1] 0.1541213

``` r
mean(dtreat[grep("HL", dtreat$Treatment_abbv),"DO"])
```

    ## [1] 3.727451

``` r
sd(dtreat[grep("HL", dtreat$Treatment_abbv),"DO"])
```

    ## [1] 0.4286237

``` r
mean(dtreat[grep("LH", dtreat$Treatment_abbv),"DO"])
```

    ## [1] 9.225278

``` r
sd(dtreat[grep("LH", dtreat$Treatment_abbv),"DO"])
```

    ## [1] 0.1271855

``` r
mean(dtreat[grep("LL", dtreat$Treatment_abbv),"DO"])
```

    ## [1] 3.72926

``` r
sd(dtreat[grep("LL", dtreat$Treatment_abbv),"DO"])
```

    ## [1] 0.4336376

plot DO per treatment as boxplots

``` r
ggplot(dtreat) + geom_boxplot(aes(Target_Treatment,DO, color = Target_Treatment)) 
```

![](2016_Pteropod_Water_Chem_Analysis_files/figure-markdown_github/boxplots%20of%20DO%20per%20treatment-1.png)

Calculate DO for each treatment seems like there is probe drift on day 22 in MOATS 8 and day 29 in MOATS 10, and possibly day 22 MOATS 4. but it only appears to be a few data points, so I'm not removing them for now.

``` r
mean(dtreat[grep("HH", dtreat$Treatment_abbv),"pH"])
```

    ## [1] 7.914669

``` r
sd(dtreat[grep("HH", dtreat$Treatment_abbv),"pH"])
```

    ## [1] 0.02329347

``` r
mean(dtreat[grep("HL", dtreat$Treatment_abbv),"pH"])
```

    ## [1] 7.942784

``` r
sd(dtreat[grep("HL", dtreat$Treatment_abbv),"pH"])
```

    ## [1] 0.01654722

``` r
mean(dtreat[grep("LH", dtreat$Treatment_abbv),"pH"])
```

    ## [1] 7.551894

``` r
sd(dtreat[grep("LH", dtreat$Treatment_abbv),"pH"])
```

    ## [1] 0.0274208

``` r
mean(dtreat[grep("LL", dtreat$Treatment_abbv),"pH"])
```

    ## [1] 7.562572

``` r
sd(dtreat[grep("LL", dtreat$Treatment_abbv),"pH"])
```

    ## [1] 0.03252387

plot pH per treatment as boxplots

``` r
ggplot(dtreat) + geom_boxplot(aes(Target_Treatment,pH, color= Target_Treatment)) 
```

![](2016_Pteropod_Water_Chem_Analysis_files/figure-markdown_github/boxplots%20of%20pH%20per%20treatment-1.png)

make supplementary table

``` r
#add column for jar so that Spec pH will stay in a separate row from MOATS data
dtreat$Jar <- "NA"
#make a column for time only
dtreat$time <- as.character(dtreat$datetime)
dtreat$time <- substr(dtreat$time,12,19)
#make a column for date only
dtreat$Date <- as.Date(dtreat$datetime)

#merge MOATS data with Spec data
all_chem <- merge(dtreat,Spec_pH[,-grep("Treatment|Salinity|Data_Type|pH_at_25C", colnames(Spec_pH))], by = c("Date","MOATS", "Jar"), all = TRUE)
#remove uninformative columns
all_chem<- all_chem[,-c(4,7,9,11)]

#reorder columns
all_chem <- all_chem[,c(7,2,3,1,8,4,5,6,9,10)]
#rename columns
colnames(all_chem)[5:10] <- c("Time", "pH_logger", "Temp_logger", "DO_logger","calculated_DIC", "calculated_Spec_pH_at_12C")

#write out chemistry to file
write.csv(all_chem, "~/Documents/GitHub/Seawater-Chemistry-Analysis/2016-17_PteropodExp_WaterChem/PteropodWaterChem/Supplementary_Table_1.csv")
```
