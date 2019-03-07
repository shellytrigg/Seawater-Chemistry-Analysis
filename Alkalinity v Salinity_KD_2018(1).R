
library(ggplot2)
library(gridExtra)
library(lubridate)

####PMEL data files were renamed as follows:
#Chase_Williams_logsheet_DIC_TA.xlsx =Chase_Williams_DIC_TA.csv 
#chase_williams_july_aug_2017.xlsx = July_Aug_2017_DIC_TA.csv 
#NWFSC_Mar-Sept_2017_DIC_TA.xlsx = Mar_Sept_2017_DIC_TA.csv 
#Paul_McElhany_1.13.17_DIC_TA.xlsx = OA.csv 

setwd("/Users/Shelly/Desktop/2016-17_CrabExp_WaterChem/data/PMEL/")

##OA data
OA_Data <- read.csv("OA.csv", header= TRUE, stringsAsFactors= FALSE)
#remove rows/columns:
OA_Data <- OA_Data[-c(3,12,15, 19,25),]
View(OA_Data)
#chase's data
C_Data <- read.csv("Chase_Williams_DIC_TA.csv", stringsAsFactors=FALSE)
#remove 3's from data (flagged 3 = poor sample)
C_Data<-subset(C_Data, !(TA.QC==3))
C_Data<-C_Data[,-c(4)]
View(C_Data)

#New 2017 data
New_Mar_Sept <- read.csv("Mar_Sept_2017_DIC_TA.csv", stringsAsFactors=FALSE)
New_July_Aug <- read.csv("July_Aug_2017_DIC_TA.csv")

##Combine datasets
OA_Data$Source<- "Old OA"
C_Data$Source <- "Old Chase"
New_Mar_Sept$Source <- "New NWFSC"
New_July_Aug$Source <- "New Chase"
dComb <- rbind(C_Data, OA_Data, New_Mar_Sept, New_July_Aug)
View(dComb)

#change dates

dComb$Date <- as.Date(dComb$Date, "%m/%d/%Y")
dComb$Month <- month(dComb$Date)
dComb$Year <- year(dComb$Date)



##plots of salinity and TA against date 
ggplot(dComb, aes(x= Date, y = Salinity.from.DIC)) + geom_point(aes(color=Source)) + theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggplot(dComb, aes(x= Month, y = Salinity.from.DIC)) + geom_point(aes(color=factor(Year))) + theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + scale_x_continuous(breaks = 1:12, limits = c(1,12))


ggplot(dComb, aes(x= Date, y = TA..umol.kg.)) + geom_point(aes(color=Source)) + theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


#slope and intercepts 
#slope=54.0537, intercept=431.0543
coef(lm(TA..umol.kg. ~ Salinity.from.DIC, data = OA_Data))
#slope=57.11065, intercept=345.40552
coef(lm(TA..umol.kg. ~ Salinity.from.DIC, data = C_Data))
#slope=41.12293, intercept=800.1913
coef(lm(TA..umol.kg. ~ Salinity.from.DIC, data = New_Mar_Sept))
#slope=63.15261, intercept=146.39016
coef(lm(TA..umol.kg. ~ Salinity.from.DIC, data = New_July_Aug))


##plots
g <- ggplot(dComb, aes(x= Salinity.from.DIC, y= TA..umol.kg.)) + 
  geom_point(aes(color=Source)) + 
  geom_abline(slope= 47.7, intercept= 647)+
  stat_smooth(method = "lm", col = "red", linetype = "dashed", se=FALSE)+
  stat_smooth(data= dComb, method = "lm", col = "gray", linetype = "dashed", se=FALSE) +
  theme_bw() 

summary(lm(TA..umol.kg. ~ Salinity.from.DIC, dComb))



#residuals
fit1 <- lm(TA..umol.kg. ~ Salinity.from.DIC , data =subset(dComb, Source== "OA"))
summary(fit1)

fit2<-lm(TA..umol.kg. ~ Salinity.from.DIC , data =dComb)
summary(fit2)
(intercept <- fit2$coefficients[1])
(slope <- fit2$coefficients[2])

y <- function(x, a, b){
  y <- x * a + b
  return(y)
}

y(28.76,slope, intercept)






















 
