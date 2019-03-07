library("seacarb", lib.loc="~/Library/R/3.3/library")

alk <- seq(1000,3000,100)
alk <- alk / 1000000
pHspec <- 7.35
carbSpec <- carb(flag = 8, pHspec, alk,S = 29.5, T = 25)
carbMOATS <- carb(flag = 15, alk, carbSpec$DIC,S = 29.5, T= 9, )
View(carbMOATS)
