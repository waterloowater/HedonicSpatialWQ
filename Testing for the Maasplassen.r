

library(rgdal)
if(Sys.info()["user"] == "linde069"){
  dataPath <- "D:/Analyses/HedonicSpatialWQ/SurveyData"
  setwd("D:/Analyses/HedonicSpatialWQ")
}

if(Sys.info()["user"] == "linde069"){
  Maas2 <- readOGR(dsn = "D:/Analyses/HedonicSpatialWQ/SurveyData", layer = "Maas")
}

#D:/Analyses/HedonicSpatialWQ/SurveyData


#install.packages("rgdal")

summarize(Maas$q95_0)
library(Deducer)

descriptive.table(vars = d(q95_02, q95_03,q95_04, q96_01, q96_02, q96_03, q96_04,
                           q97_01, q97_02, q97_03,q97_04, q98_01, q98_02, q98_03, q98_04
                           ),data= Maas, 
                  func.names = c("Valid N"))

Maas$Y1995 <-  as.numeric(Maas$q95_02) + as.numeric(Maas$q95_03) + as.numeric(Maas$q95_04)
frequencies(Maas$Y1995)
Maas$Y1996 <-  as.numeric(Maas$q96_01) + as.numeric(Maas$q96_02) + as.numeric(Maas$q96_03) + as.numeric(Maas$q96_04)
frequencies(Maas$Y1996)
Maas$Y1997 <-  as.numeric(Maas$q97_01) + as.numeric(Maas$q97_02) + as.numeric(Maas$q97_03) + as.numeric(Maas$q97_04)
frequencies(Maas$Y1997)
Maas$Y1998 <-  as.numeric(Maas$q98_01) + as.numeric(Maas$q98_02) + as.numeric(Maas$q98_03) + as.numeric(Maas$q98_04)
frequencies(Maas$Y1998)
Maas$Y1999 <-  as.numeric(Maas$q99_01) + as.numeric(Maas$q99_02) + as.numeric(Maas$q99_03) + as.numeric(Maas$q99_04)
frequencies(Maas$Y1999)
Maas$Y2000 <-  as.numeric(Maas$q00_01) + as.numeric(Maas$q00_02) + as.numeric(Maas$q00_03) + as.numeric(Maas$q00_04)
frequencies(Maas$Y2000)

Maas$Y2001 <-  as.numeric(Maas$q01_01) + as.numeric(Maas$q01_02) + as.numeric(Maas$q01_03) + as.numeric(Maas$q01_04)
frequencies(Maas$Y2001)
Maas$Y2002 <-  as.numeric(Maas$q02_01) + as.numeric(Maas$q02_02) + as.numeric(Maas$q02_03) + as.numeric(Maas$q02_04)
frequencies(Maas$Y2002)
Maas$Y2003 <-  as.numeric(Maas$q03_01) + as.numeric(Maas$q03_02) + as.numeric(Maas$q03_03) + as.numeric(Maas$q03_04)
frequencies(Maas$Y2003)
Maas$Y2004 <-  as.numeric(Maas$q04_01) + as.numeric(Maas$q04_02) + as.numeric(Maas$q04_03) + as.numeric(Maas$q04_04)
frequencies(Maas$Y2004)
Maas$Y2005 <-  as.numeric(Maas$q05_01) + as.numeric(Maas$q05_02) + as.numeric(Maas$q05_03) 
frequencies(Maas$Y2005)

############################## START OF MODULE ###########################################################
## Constructing more simple variables for Year, Quarter and some variables defined as factor but 
## definitely should be mnumerical.
##########################################################################################################

Maas$Year <- 0
Maas$Year <- ifelse(Maas$q95_02=="1",1995,Maas$Year)
Maas$Year <- ifelse(Maas$q95_03=="1",1995,Maas$Year)
Maas$Year <- ifelse(Maas$q95_04=="1",1995,Maas$Year)

Maas$Year <- ifelse(Maas$q96_01=="1",1996,Maas$Year)
Maas$Year <- ifelse(Maas$q96_02=="1",1996,Maas$Year)
Maas$Year <- ifelse(Maas$q96_03=="1",1996,Maas$Year)
Maas$Year <- ifelse(Maas$q96_04=="1",1996,Maas$Year)

Maas$Year <- ifelse(Maas$q97_01=="1",1997,Maas$Year)
Maas$Year <- ifelse(Maas$q97_02=="1",1997,Maas$Year)
Maas$Year <- ifelse(Maas$q97_03=="1",1997,Maas$Year)
Maas$Year <- ifelse(Maas$q97_04=="1",1997,Maas$Year)

Maas$Year <- ifelse(Maas$q98_01=="1",1998,Maas$Year)
Maas$Year <- ifelse(Maas$q98_02=="1",1998,Maas$Year)
Maas$Year <- ifelse(Maas$q98_03=="1",1998,Maas$Year)
Maas$Year <- ifelse(Maas$q98_04=="1",1998,Maas$Year)

Maas$Year <- ifelse(Maas$q99_01=="1",1999,Maas$Year)
Maas$Year <- ifelse(Maas$q99_02=="1",1999,Maas$Year)
Maas$Year <- ifelse(Maas$q99_03=="1",1999,Maas$Year)
Maas$Year <- ifelse(Maas$q99_04=="1",1999,Maas$Year)

Maas$Year <- ifelse(Maas$q00_01=="1",2000,Maas$Year)
Maas$Year <- ifelse(Maas$q00_02=="1",2000,Maas$Year)
Maas$Year <- ifelse(Maas$q00_03=="1",2000,Maas$Year)
Maas$Year <- ifelse(Maas$q00_04=="1",2000,Maas$Year)

Maas$Year <- ifelse(Maas$q01_01=="1",2001,Maas$Year)
Maas$Year <- ifelse(Maas$q01_02=="1",2001,Maas$Year)
Maas$Year <- ifelse(Maas$q01_03=="1",2001,Maas$Year)
Maas$Year <- ifelse(Maas$q01_04=="1",2001,Maas$Year)

Maas$Year <- ifelse(Maas$q02_01=="1",2002,Maas$Year)
Maas$Year <- ifelse(Maas$q02_02=="1",2002,Maas$Year)
Maas$Year <- ifelse(Maas$q02_03=="1",2002,Maas$Year)
Maas$Year <- ifelse(Maas$q02_04=="1",2002,Maas$Year)

Maas$Year <- ifelse(Maas$q03_01=="1",2003,Maas$Year)
Maas$Year <- ifelse(Maas$q03_02=="1",2003,Maas$Year)
Maas$Year <- ifelse(Maas$q03_03=="1",2003,Maas$Year)
Maas$Year <- ifelse(Maas$q03_04=="1",2003,Maas$Year)

Maas$Year <- ifelse(Maas$q04_01=="1",2004,Maas$Year)
Maas$Year <- ifelse(Maas$q04_02=="1",2004,Maas$Year)
Maas$Year <- ifelse(Maas$q04_03=="1",2004,Maas$Year)
Maas$Year <- ifelse(Maas$q04_04=="1",2004,Maas$Year)

Maas$Year <- ifelse(Maas$q05_01=="1",2005,Maas$Year)
Maas$Year <- ifelse(Maas$q05_02=="1",2005,Maas$Year)
Maas$Year <- ifelse(Maas$q05_03=="1",2005,Maas$Year)
frequencies(Maas$Year)
hist(Maas$Year)


Maas$Quarter <- 200504
Maas$Quarter <- ifelse(Maas$q95_02=="1",199501,Maas$Quarter)
Maas$Quarter <- ifelse(Maas$q95_03=="1",199502,Maas$Quarter)
Maas$Quarter <- ifelse(Maas$q95_04=="1",199503,Maas$Quarter)

Maas$Quarter <- ifelse(Maas$q96_01=="1",199601,Maas$Quarter)
Maas$Quarter <- ifelse(Maas$q96_02=="1",199602,Maas$Quarter)
Maas$Quarter <- ifelse(Maas$q96_03=="1",199603,Maas$Quarter)
Maas$Quarter <- ifelse(Maas$q96_04=="1",199604,Maas$Quarter)

Maas$Quarter <- ifelse(Maas$q97_01=="1",199701,Maas$Quarter)
Maas$Quarter <- ifelse(Maas$q97_02=="1",199702,Maas$Quarter)
Maas$Quarter <- ifelse(Maas$q97_03=="1",199703,Maas$Quarter)
Maas$Quarter <- ifelse(Maas$q97_04=="1",199704,Maas$Quarter)

Maas$Quarter <- ifelse(Maas$q98_01=="1",199801,Maas$Quarter)
Maas$Quarter <- ifelse(Maas$q98_02=="1",199802,Maas$Quarter)
Maas$Quarter <- ifelse(Maas$q98_03=="1",199803,Maas$Quarter)
Maas$Quarter <- ifelse(Maas$q98_04=="1",199804,Maas$Quarter)

Maas$Quarter <- ifelse(Maas$q99_01=="1",199901,Maas$Quarter)
Maas$Quarter <- ifelse(Maas$q99_02=="1",199902,Maas$Quarter)
Maas$Quarter <- ifelse(Maas$q99_03=="1",199903,Maas$Quarter)
Maas$Quarter <- ifelse(Maas$q99_04=="1",199904,Maas$Quarter)

Maas$Quarter <- ifelse(Maas$q00_01=="1",200001,Maas$Quarter)
Maas$Quarter <- ifelse(Maas$q00_02=="1",200002,Maas$Quarter)
Maas$Quarter <- ifelse(Maas$q00_03=="1",200003,Maas$Quarter)
Maas$Quarter <- ifelse(Maas$q00_04=="1",200004,Maas$Quarter)

Maas$Quarter <- ifelse(Maas$q01_01=="1",200101,Maas$Quarter)
Maas$Quarter <- ifelse(Maas$q01_02=="1",200102,Maas$Quarter)
Maas$Quarter <- ifelse(Maas$q01_03=="1",200103,Maas$Quarter)
Maas$Quarter <- ifelse(Maas$q01_04=="1",200104,Maas$Quarter)

Maas$Quarter <- ifelse(Maas$q02_01=="1",200201,Maas$Quarter)
Maas$Quarter <- ifelse(Maas$q02_02=="1",200202,Maas$Quarter)
Maas$Quarter <- ifelse(Maas$q02_03=="1",200203,Maas$Quarter)
Maas$Quarter <- ifelse(Maas$q02_04=="1",200204,Maas$Quarter)

Maas$Quarter <- ifelse(Maas$q03_01=="1",200301,Maas$Quarter)
Maas$Quarter <- ifelse(Maas$q03_02=="1",200302,Maas$Quarter)
Maas$Quarter <- ifelse(Maas$q03_03=="1",200303,Maas$Quarter)
Maas$Quarter <- ifelse(Maas$q03_04=="1",200304,Maas$Quarter)

Maas$Quarter <- ifelse(Maas$q04_01=="1",200401,Maas$Quarter)
Maas$Quarter <- ifelse(Maas$q04_02=="1",200402,Maas$Quarter)
Maas$Quarter <- ifelse(Maas$q04_03=="1",200403,Maas$Quarter)
Maas$Quarter <- ifelse(Maas$q04_04=="1",200404,Maas$Quarter)

Maas$Quarter <- ifelse(Maas$q05_01=="1",200501,Maas$Quarter)
Maas$Quarter <- ifelse(Maas$q05_02=="1",200502,Maas$Quarter)
Maas$Quarter <- ifelse(Maas$q05_03=="1",200503,Maas$Quarter)
frequencies(Maas$Quarter)
hist(Maas$Quarter)

Maas$Quarter <- as.factor(Maas$Quarter)


frequencies(Maas$q95_02)
frequencies(Maas$q95_03)
frequencies(Maas$q95_04)

frequencies(Maas$q96_01)
frequencies(Maas$q96_02)
frequencies(Maas$q96_03)
frequencies(Maas$q96_04)

frequencies(Maas$q97_01)
frequencies(Maas$q97_02)
frequencies(Maas$q97_03)
frequencies(Maas$q97_04)

frequencies(Maas$q98_01)
frequencies(Maas$q98_02)
frequencies(Maas$q98_03)
frequencies(Maas$q98_04)

frequencies(Maas$q99_01)
frequencies(Maas$q99_02)
frequencies(Maas$q99_03)
frequencies(Maas$q99_04)

frequencies(Maas$q00_01)
frequencies(Maas$q00_02)
frequencies(Maas$q00_03)
frequencies(Maas$q00_04)

frequencies(Maas$q01_01)
frequencies(Maas$q01_02)
frequencies(Maas$q01_03)
frequencies(Maas$q01_04)

frequencies(Maas$q02_01)
frequencies(Maas$q02_02)
frequencies(Maas$q02_03)
frequencies(Maas$q02_04)

frequencies(Maas$q03_01)
frequencies(Maas$q03_02)
frequencies(Maas$q03_03)
frequencies(Maas$q03_04)

frequencies(Maas$q04_01)
frequencies(Maas$q04_02)
frequencies(Maas$q04_03)
frequencies(Maas$q04_04)

frequencies(Maas$q05_01)
frequencies(Maas$q05_02)
frequencies(Maas$q05_03)

summary(Maas$q95_02)

#OLS regression

Maas$bevdicht   <- as.numeric(Maas$bevdicht)
Maas$pnietact   <- as.numeric(Maas$pnietact)
Maas$geminkinko <- as.numeric(Maas$geminkinko)

Maas$raildist   <- as.numeric(Maas$raildist)
Maas$highdist   <- as.numeric(Maas$highdist)
Maas$dist_local <- as.numeric(Maas$dist_local)
Maas$dist_3a    <- as.numeric(Maas$dist_3a)
Maas$dist_4a    <- as.numeric(Maas$dist_4a)
Maas$dist_4d    <- as.numeric(Maas$dist_4d)
Maas$dist_4e    <- as.numeric(Maas$dist_4e)
Maas$dist_zwem  <- as.numeric(Maas$dist_zwem)

Maas$nbadk      <- as.numeric(Maas$nbadk)
Maas$isol       <- as.numeric(Maas$isol)
#***************************** END OF MODULE *************************************************************


############################## START OF MODULE ###########################################################
## Descriptive statistics for Maasplassen
## ro the limited degrees of freedom. 
##########################################################################################################

library(Deducer)
descriptive.table(vars = d(Maas$lnprice, Maas$lnwoonop, Maas$nbadk, Maas$isol,
                           Maas$bwpr1905, Maas$bwpr6070, Maas$bwpr7180, Maas$bwpr8190, 
                           Maas$bwpr9100, Maas$bevdicht, 
                           Maas$pnietact, Maas$geminkinko, Maas$raildist, Maas$highdist, Maas$dist_local,  
                           Maas$dist_4e, Maas$dist_3a , Maas$dist_4d, Maas$dist_zwem, Maas$chl, Maas$doo, Maas$ZM),
                  data= Maas, 
                  func.names = c("Mean","St. Deviation", "Min", "Max"))

frequencies(Maas$Quarter)
frequencies(Maas$inpandig)
frequencies(Maas$tuin_zow)
frequencies(Maas$buff25)

#factor(Quarter) + lnwoonop + nbadk + factor(inpandig) + isol + factor(tuin_zow) 
#+ bwpr1905 + bwpr6070 + bwpr7180 + bwpr8190 + bwpr9100 + bevdicht + 
#  pnietact + geminkinko + raildist + highdist + dist_local + factor(buff25) + 
#  dist_4e + dist_3a + dist_4d + dist_zwem + chl + doo + ZM
#***************************** END OF MODULE *************************************************************


############################## START OF MODULE ###########################################################
## Regression for Maasplassen 1996, 2000, 2004 and overall. In 1996, not all variables can be included dur
## ro the limited degrees of freedom. 
##########################################################################################################


MaasSubset <- subset(Maas, Year==1996)
frequencies(MaasSubset$inpandig)
frequencies(MaasSubset$tuin_zow)
frequencies(MaasSubset$won_4)
frequencies(MaasSubset$won_1)
frequencies(MaasSubset$buff25)

OLSMaas1996 <- lm(lnprice ~ factor(Quarter) + lnwoonop + nbadk + factor(inpandig) + isol + factor(tuin_zow) 
                   + bwpr1905 + bwpr6070 + bwpr7180 + bwpr8190 + bwpr9100 + bevdicht + 
                    pnietact + geminkinko + raildist + highdist + dist_local + factor(buff25) + 
                    dist_4e + dist_3a + dist_4d + dist_zwem + chl + doo + ZM,
                  data=MaasSubset)
#OLSMaas2004

MaasSubset <- subset(Maas, Year==2000)
OLSMaas2000 <- lm(lnprice ~ factor(Quarter) + lnwoonop + nbadk + factor(inpandig) + isol + factor(tuin_zow) 
                  + factor(won_4) + 
                    factor(won_1) + bwpr1905 + bwpr6070 + bwpr7180 + bwpr8190 + bwpr9100 + bevdicht + 
                    pnietact + geminkinko + raildist + highdist + dist_local + factor(buff25) + 
                    dist_4e + dist_3a + dist_4d + dist_zwem + chl + doo + ZM,
                  data=MaasSubset)
#OLSMaas2004

MaasSubset <- subset(Maas, Year==2004)
OLSMaas2004 <- lm(lnprice ~ factor(Quarter) + lnwoonop + nbadk + factor(inpandig) + isol + factor(tuin_zow) 
                  + factor(won_4) + 
                    factor(won_1) + bwpr1905 + bwpr6070 + bwpr7180 + bwpr8190 + bwpr9100 + bevdicht + 
                    pnietact + geminkinko + raildist + highdist + dist_local + factor(buff25) + 
                    dist_4e + dist_3a + dist_4d + dist_zwem + chl + doo + ZM,
                  data=MaasSubset)
OLSMaas2004

OLSMaas <- lm(lnprice ~ factor(Quarter) + lnwoonop + nbadk + factor(inpandig) + isol + factor(tuin_zow) 
              + factor(won_4) + 
                factor(won_1) + bwpr1905 + bwpr6070 + bwpr7180 + bwpr8190 + bwpr9100 + bevdicht + 
                pnietact + geminkinko + raildist + highdist + dist_local + factor(buff25) + 
                dist_4e + dist_3a + dist_4d + dist_zwem + chl + doo + ZM, data=Maas)

library(stargazer)
hist(OLSMaas$residuals, breaks = 100)
shapiro.test(OLSMaas$residuals) #not working, too many observations!

hist(OLSMaas2004$residuals)
shapiro.test(OLSMaas2004$residuals)
shapiro.test(MaasSubset$lnprice)
shapiro.test(MaasSubset$price3)



stargazer(OLSMaas, OLSMaas2004, OLSMaas2000, OLSMaas1996, out="Results/Maas_OLS_period.htm") 
#***************************** END OF MODULE *************************************************************


############################## START OF MODULE ###########################################################
## Regression for Maasplassen stepwise
## ro the limited degrees of freedom. 
##########################################################################################################


OLSMaas_A <- lm(lnprice ~ factor(Quarter) + lnwoonop + nbadk + factor(inpandig) + isol + factor(tuin_zow) 
                + factor(won_4) + 
                  factor(won_1) + bwpr1905 + bwpr6070 + bwpr7180 + bwpr8190 + bwpr9100,
                data=Maas)

OLSMaas_B <- lm(lnprice ~ factor(Quarter) + lnwoonop + nbadk + factor(inpandig) + isol + factor(tuin_zow) 
                + factor(won_4) + 
                  factor(won_1) + bwpr1905 + bwpr6070 + bwpr7180 + bwpr8190 + bwpr9100 + bevdicht + 
                  pnietact + geminkinko + raildist + highdist,
                data=Maas)

OLSMaas_C <- lm(lnprice ~ factor(Quarter) + lnwoonop + nbadk + factor(inpandig) + isol + factor(tuin_zow) 
                + factor(won_4) + 
                  factor(won_1) + bwpr1905 + bwpr6070 + bwpr7180 + bwpr8190 + bwpr9100 + bevdicht + 
                  pnietact + geminkinko + raildist + highdist + dist_local,
                data=Maas)

OLSMaas_D <- lm(lnprice ~ factor(Quarter) + lnwoonop + nbadk + factor(inpandig) + isol + factor(tuin_zow) 
                + factor(won_4) + 
                  factor(won_1) + bwpr1905 + bwpr6070 + bwpr7180 + bwpr8190 + bwpr9100 + bevdicht + 
                  pnietact + geminkinko + raildist + highdist + dist_local + factor(buff25) + 
                  dist_4e + dist_3a + dist_4d + dist_zwem,
                data=Maas)


OLSMaas_E <- lm(lnprice ~ factor(Quarter) + lnwoonop + nbadk + factor(inpandig) + isol + factor(tuin_zow) 
              + factor(won_4) + 
                factor(won_1) + bwpr1905 + bwpr6070 + bwpr7180 + bwpr8190 + bwpr9100 + bevdicht + 
                pnietact + geminkinko + raildist + highdist + dist_local + factor(buff25) + 
                dist_4e + dist_3a + dist_4d + dist_zwem + chl + doo + ZM,
              data=Maas)

library(stargazer)
stargazer(OLSMaas_A, OLSMaas_B, OLSMaas_C, OLSMaas_D, OLSMaas_E, out="Results/Maas_OLS.htm") 
#***************************** END OF MODULE *************************************************************


############################## START OF MODULE ###########################################################
## Check on the variables "chl", "doo" and "ZM". A large number of these variables is coded missing "-999"
## Does it really mean missing, or can we set them to 0?
##########################################################################################################

Maas$DropVar <- 0
Maas$DropVar <- ifelse(Maas$chl=="-999", 1, Maas$DropVar)

descriptive.table(vars = d(Maas$chl),data= Maas, 
                  strata = Maas$DropVar,
                  func.names = c("Mean","St. Deviation", "Min", "Max", "Valid N"))
descriptive.table(vars = d(Maas$chl),data= Maas, 
                  strata = Maas$Year,
                  func.names = c("Mean","St. Deviation", "Min", "Max", "Valid N"))

Maas$DropVar <- 0
Maas$DropVar <- ifelse(Maas$doo=="-999", 1, Maas$DropVar)
descriptive.table(vars = d(Maas$doo),data= Maas, 
                  strata = Maas$DropVar,
                  func.names = c("Mean","St. Deviation", "Min", "Max", "Valid N"))
descriptive.table(vars = d(Maas$doo),data= Maas, 
                  strata = Maas$Year,
                  func.names = c("Mean","St. Deviation", "Min", "Max", "Valid N"))

Maas$DropVar <- 0
Maas$DropVar <- ifelse(Maas$ZM=="-999", 1, Maas$DropVar)
descriptive.table(vars = d(Maas$ZM),data= Maas, 
                  strata = Maas$DropVar,
                  func.names = c("Mean","St. Deviation", "Min", "Max", "Valid N"))
descriptive.table(vars = d(Maas$ZM),data= Maas, 
                  strata = Maas$Year,
                  func.names = c("Mean","St. Deviation", "Min", "Max", "Valid N"))

# All three together, more than 9,000 records missing!
Maas$DropVar <- 0
Maas$DropVar <- ifelse(Maas$chl=="-999", 1, Maas$DropVar)
Maas$DropVar <- ifelse(Maas$doo=="-999", 1, Maas$DropVar)
Maas$DropVar <- ifelse(Maas$ZM=="-999", 1, Maas$DropVar)
descriptive.table(vars = d(Maas$chl, Maas$doo, Maas$ZM),data= Maas, 
                  strata = Maas$DropVar,
                  func.names = c("Mean","St. Deviation", "Min", "Max", "Valid N"))

frequencies(Maas$DropVar)
#***************************** END OF MODULE *************************************************************

############################## START OF MODULE ###########################################################



############################## START OF MODULE ###########################################################
## Histograms on the price of houses
##########################################################################################################



hist(Maas$lnprice)

descriptive.table(vars = d(Maas$lnprice),data= Maas, 
                  strata = Maas$Year,
                  func.names = c("Mean","St. Deviation", "Min", "Max", "Valid N"))
Maas$price3 <- exp(Maas$lnprice)/1000

descriptive.table(vars = d(Maas$price3),data= Maas, 
                  strata = Maas$Year,
                  func.names = c("Mean","St. Deviation", "Min", "Max"))

hist(Maas$price3)
MaasSubset <- subset(Maas, Year==2004)
hist(MaasSubset$price3)
MaasSubset <- subset(Maas, Year==2000)
hist(MaasSubset$price3)
MaasSubset <- subset(Maas, Year==1996)
hist(MaasSubset$price3)

hist(Maas$price)
MaasSubsetln <- subset(Maas, Year==2004)
hist(MaasSubset$lnprice)
MaasSubset <- subset(Maas, Year==2000)
hist(MaasSubset$lnprice)
MaasSubset <- subset(Maas, Year==1996)
hist(MaasSubset$lnprice)



#Maas$lnpriceClass <- cut(Maas$lnprice, c(8, 8.5, 9, 9.5, 10, 10.5, 11, 12, 13, Inf), labels = FALSE)
#hist(Maas$lnpriceClass)
#frequencies(Maas$lnpriceClass)

ggplot(Maas$lnprice, aes()) +
  geom_histogram()

centroids <- coordinates(Maas)
plot(centroids,xlab="longitude",ylab="latitude")
plot(Maas,add=TRUE)
title("Maas with centroids")
plot(Maas,border="black")
#invisible(text(centroids, labels=as.character(Maas$obj_id), cex=0.4))
invisible(text(centroids, labels=1, cex=0.4))
title("Maas with number label")

install.packages("spdep")
library(spdep)
queen1 <- poly2nb(Maas,queen=FALSE)
summary(queen1)
wq1 <- nb2listw(queen1) # Not working because there are more than 5,000 observations without any neighbour!
summary(wq1)
plot.listw(wq1,centroids)
title("Links in the queen matrix")

knearneigh1 <- knearneigh(centroids,k=10)
knn1 <- spdep::knn2nb(knearneigh1)
knn1_w <- nb2listw(knn1)

plot(Maas, border="black")
plot(knn1, centroids, add=TRUE)
title(main="K nearest neighbours, k = 10")



#linmod <- lm(form_base_3,nuts.dat)
#summary(linmod)
#install.packages(c("car","systemfit"),repo="http://cran.stat.ucla.edu",dep=TRUE)

install.packages("car")
library("lmtest")
bp <- bptest(OLSMaas_E)
bp

knn1 <- spdep::knn2nb(knearneigh1)
lm <- lm.LMtests(OLSMaas_E, knn1_w, zero.policy=TRUE, test="all")
lm

SPEMaas_E <- errorsarlm(lnprice ~ factor(Quarter) + lnwoonop + nbadk + factor(inpandig) + isol + factor(tuin_zow) 
                + factor(won_4) + 
                  factor(won_1) + bwpr1905 + bwpr6070 + bwpr7180 + bwpr8190 + bwpr9100 + bevdicht + 
                  pnietact + geminkinko + raildist + highdist + dist_local + factor(buff25) + 
                  dist_4e + dist_3a + dist_4d + dist_zwem + chl + doo + ZM,
                data=Maas,
                listw = knn1_w)
summary(SPEMaas_E)

SPLMaas_E <- lagsarlm(lnprice ~ factor(Quarter) + lnwoonop + nbadk + factor(inpandig) + isol + factor(tuin_zow) 
                        + factor(won_4) + 
                          factor(won_1) + bwpr1905 + bwpr6070 + bwpr7180 + bwpr8190 + bwpr9100 + bevdicht + 
                          pnietact + geminkinko + raildist + highdist + dist_local + factor(buff25) + 
                          dist_4e + dist_3a + dist_4d + dist_zwem + chl + doo + ZM,
                        data=Maas,
                        listw = knn1_w)
summary(SPLMaas_E)


errmod <- errorsarlm(log(TOUR22_10)~log(gCAP12_10)+log(CAP12_01)+log(XPOPDENS)+log(XLAND1)+XUNEMP+XCLC_URB
                     +XCLC_FOR+XCLC_WET+XCLC_BEA+PORT+AIRPORT+log(XM399C+0.01)+log(TOUR22_01)+log(WCAP12_10)+
                       log(WXM399C+0.01)
                     ,data=nuts1.dat,
                     listw=wq1)
summary(errmod)

lagmod <- lagsarlm(log(TOUR22_10)~log(gCAP12_10)+log(CAP12_01)+log(XPOPDENS)+log(XLAND1)+XUNEMP+XCLC_URB
                   +XCLC_FOR+XCLC_WET+XCLC_BEA+PORT+AIRPORT+log(XM399C+0.01)+log(TOUR22_01)+log(WCAP12_10)+log(WXM399C+0.01),data=nuts1.dat,listw=wq1)
summary(lagmod)

testlin22<-{
  rbind(
    cbind('==linear model==','==22===========')
    ,cbind(coef(linmod),summary(linmod)$coefficients[,4])
    ,cbind(summary(linmod)$fstatistic[1],summary(linmod)$df[1]-1)
    ,cbind(summary(linmod)$adj.r.squared,'')
    ,cbind(bp$statistic,bp$p.value)
    ,cbind(lmspat$LMerr$statistic, lmspat$LMerr$p.value)
    ,cbind(lmspat$LMlag$statistic, lmspat$LMlag$p.value)
    ,cbind(lmspat$RLMerr$statistic,lmspat$RLMerr$p.value)
    ,cbind(lmspat$RLMlag$statistic,lmspat$RLMlag$p.value)
    ,cbind(lmspat$SARMA$statistic, lmspat$SARMA$p.value)
  )}
colnames(testlin22)<-c('coeff','p-value')
rownames(testlin22)[summary(linmod)$df[1]+2]<-c('F-statistic')
rownames(testlin22)[summary(linmod)$df[1]+3]<-c('Adjusted R-squared')
testlin22

testerr22 <- {
  rbind(cbind('==error model==','==22===========')
        ,summary(errmod)$Coef[,c(1,4)]
        ,cbind(errmod$lambda,errmod$lambda.se)
        ,cbind(summary(errmod)$LR1$statistic[1],summary(errmod)$LR1$p.value)
        ,cbind(summary(errmod)$Wald1$statistic[1],summary(errmod)$Wald1$p.value)
        ,cbind(errmod$LL,errmod$logLik_lm.model)
        ,cbind(errmod$AIC,errmod$AIC_lm.model)
        ,cbind(summary(errmod,Nagelkerke=TRUE)$NK,'')
  )}
rownames(testerr22)[summary(errmod)$parameters[1]+3]<-c('Likelihoods')
rownames(testerr22)[summary(errmod)$parameters[1]+4]<-c('AIC')
rownames(testerr22)[summary(errmod)$parameters[1]+5]<-c('Nagelkerke R-squared')
testerr22

testlag22 <- {
  rbind(
    cbind('==lag model==','==22===========')
    ,summary(lagmod)$Coef[,c(1,4)]
    ,cbind(lagmod$rho,lagmod$rho.se)
    ,cbind(summary(lagmod)$LR1$statistic[1],summary(lagmod)$LR1$p.value)
    ,cbind(summary(lagmod)$Wald1$statistic[1],summary(lagmod)$Wald1$p.value)
    ,cbind(lagmod$LL,lagmod$logLik_lm.model)
    ,cbind(lagmod$LMtest,'')
  )}
rownames(testlag22)[summary(lagmod)$parameters[1]+3]<-c('Likelihoods')
rownames(testlag22)[summary(lagmod)$parameters[1]+4]<-c('LM-test statistic')
testlag22

test22 <- rbind(testlin22, testerr22, testlag22)
test22

# Make a subsample
MaasSubSet <- subset(Maas, Year>=2004)
MaasSubset <- subset(MaasSubSet, DropVar == 0 )
centroidsSubSet <- coordinates(MaasSubSet)
plot(centroidsSubSet,xlab="longitude",ylab="latitude")
plot(MaasSubSet,add=TRUE)
title("Maas with centroids")
plot(MaasSubSet,border="black")
#invisible(text(centroids, labels=as.character(Maas$obj_id), cex=0.4))
invisible(text(centroidsSubSet, labels=1, cex=0.4))
title("Maas with number label")

#install.packages("spdep")
#library(spdep)
#queen1 <- poly2nb(Maas,queen=FALSE)
#summary(queen1)
#wq1 <- nb2listw(queen1) # Not working because there are more than 5,000 observations without any neighbour!
#summary(wq1)
#lot.listw(wq1,centroids)
#title("Links in the queen matrix")

knnSubSet <- knearneigh(centroidsSubSet,k=10)
knnSubSet1 <- spdep::knn2nb(knnSubSet)
knnSubSet1_w <- nb2listw(knnSubSet1)


OLSMaas_E <- lm(lnprice ~ factor(Quarter) + lnwoonop + nbadk + factor(inpandig) + isol + factor(tuin_zow) 
                + factor(won_4) + 
                  factor(won_1) + bwpr1905 + bwpr6070 + bwpr7180 + bwpr8190 + bwpr9100 + bevdicht + 
                  pnietact + geminkinko + raildist + highdist + dist_local + factor(buff25) + 
                  dist_4e + dist_3a + dist_4d + dist_zwem + chl + doo + ZM,
                data=MaasSubSet)

bp <- bptest(OLSMaas_E)
bp

lm <- lm.LMtests(OLSMaas_E, knnSubSet1_w, zero.policy=TRUE, test="all")
lm

SPEMaas_E <- errorsarlm(lnprice ~ factor(Quarter) + lnwoonop + nbadk + factor(inpandig) + isol + factor(tuin_zow) 
                        + factor(won_4) + 
                          factor(won_1) + bwpr1905 + bwpr6070 + bwpr7180 + bwpr8190 + bwpr9100 + bevdicht + 
                          pnietact + geminkinko + raildist + highdist + dist_local + factor(buff25) + 
                          dist_4e + dist_3a + dist_4d + dist_zwem + chl + doo + ZM,
                        data=MaasSubSet,
                        listw = knnSubSet1_w)
summary(SPEMaas_E)

SPLMaas_E <- lagsarlm(lnprice ~ factor(Quarter) + lnwoonop + nbadk + factor(inpandig) + isol + factor(tuin_zow) 
                      + factor(won_4) + 
                        factor(won_1) + bwpr1905 + bwpr6070 + bwpr7180 + bwpr8190 + bwpr9100 + bevdicht + 
                        pnietact + geminkinko + raildist + highdist + dist_local + factor(buff25) + 
                        dist_4e + dist_3a + dist_4d + dist_zwem + chl + doo + ZM,
                      data=MaasSubSet,
                      listw = knnSubSet1_w)
summary(SPLMaas_E)

library(stargazer)
stargazer(OLSMaas_E, SPEMaas_E, SPLMaas_E, out="Results/Maas_Spatial0405.htm") 


# Make a subsample
MaasSubSet <- subset(Maas, Year==2005)
MaasSubSet <- subset(MaasSubSet, DropVar == 0 )
centroidsSubSet <- coordinates(MaasSubSet)
plot(centroidsSubSet,xlab="longitude",ylab="latitude")
plot(MaasSubSet,add=TRUE)
title("Maas with centroids")
plot(MaasSubSet,border="black")
#invisible(text(centroids, labels=as.character(Maas$obj_id), cex=0.4))
invisible(text(centroidsSubSet, labels=1, cex=0.4))
title("Maas with number label")

knnSubSet <- knearneigh(centroidsSubSet,k=10)
knnSubSet1 <- spdep::knn2nb(knnSubSet)
knnSubSet1_w <- nb2listw(knnSubSet1)


OLSMaas_E <- lm(lnprice ~ factor(Quarter) + lnwoonop + nbadk + factor(inpandig) + isol + factor(tuin_zow) 
                + factor(won_4) + 
                  factor(won_1) + bwpr1905 + bwpr6070 + bwpr7180 + bwpr8190 + bwpr9100 + bevdicht + 
                  pnietact + geminkinko + raildist + highdist + dist_local + factor(buff25) + 
                  dist_4e + dist_3a + dist_4d + dist_zwem + chl + doo + ZM,
                data=MaasSubSet)
OLSMaas_E
bp <- bptest(OLSMaas_E)
bp

lm <- lm.LMtests(OLSMaas_E, knnSubSet1_w, zero.policy=TRUE, test="all")
lm

SPEMaas_E <- errorsarlm(lnprice ~ factor(Quarter) + lnwoonop + nbadk + factor(inpandig) + isol + factor(tuin_zow) 
                        + factor(won_4) + 
                          factor(won_1) + bwpr1905 + bwpr6070 + bwpr7180 + bwpr8190 + bwpr9100 + bevdicht + 
                          pnietact + geminkinko + raildist + highdist + dist_local + factor(buff25) + 
                          dist_4e + dist_3a + dist_4d + dist_zwem + chl + doo + ZM,
                        data=MaasSubSet,
                        listw = knnSubSet1_w)
summary(SPEMaas_E)

SPLMaas_E <- lagsarlm(lnprice ~ factor(Quarter) + lnwoonop + nbadk + factor(inpandig) + isol + factor(tuin_zow) 
                      + factor(won_4) + 
                        factor(won_1) + bwpr1905 + bwpr6070 + bwpr7180 + bwpr8190 + bwpr9100 + bevdicht + 
                        pnietact + geminkinko + raildist + highdist + dist_local + factor(buff25) + 
                        dist_4e + dist_3a + dist_4d + dist_zwem + chl + doo + ZM,
                      data=MaasSubSet,
                      listw = knnSubSet1_w)
summary(SPLMaas_E)

library(stargazer)
stargazer(OLSMaas_E, SPEMaas_E, SPLMaas_E, out="Results/Maas_Spatial2005.htm") 

# Make a subsample
MaasSubSet <- subset(Maas, Year==2004)
MaasSubSet <- subset(MaasSubSet, DropVar == 0 )
centroidsSubSet <- coordinates(MaasSubSet)
plot(centroidsSubSet,xlab="longitude",ylab="latitude")
plot(MaasSubSet,add=TRUE)
title("Maas with centroids")
plot(MaasSubSet,border="black")
#invisible(text(centroids, labels=as.character(Maas$obj_id), cex=0.4))
invisible(text(centroidsSubSet, labels=1, cex=0.4))
title("Maas with number label")

knnSubSet <- knearneigh(centroidsSubSet,k=10)
knnSubSet1 <- spdep::knn2nb(knnSubSet)
knnSubSet1_w <- nb2listw(knnSubSet1)


OLSMaas_E <- lm(lnprice ~ factor(Quarter) + lnwoonop + nbadk + factor(inpandig) + isol + factor(tuin_zow) 
                + factor(won_4) + 
                  factor(won_1) + bwpr1905 + bwpr6070 + bwpr7180 + bwpr8190 + bwpr9100 + bevdicht + 
                  pnietact + geminkinko + raildist + highdist + dist_local + factor(buff25) + 
                  dist_4e + dist_3a + dist_4d + dist_zwem + chl + doo + ZM,
                data=MaasSubSet)
OLSMaas_E
bp <- bptest(OLSMaas_E)
bp

lm <- lm.LMtests(OLSMaas_E, knnSubSet1_w, zero.policy=TRUE, test="all")
lm

SPEMaas_E <- errorsarlm(lnprice ~ factor(Quarter) + lnwoonop + nbadk + factor(inpandig) + isol + factor(tuin_zow) 
                        + factor(won_4) + 
                          factor(won_1) + bwpr1905 + bwpr6070 + bwpr7180 + bwpr8190 + bwpr9100 + bevdicht + 
                          pnietact + geminkinko + raildist + highdist + dist_local + factor(buff25) + 
                          dist_4e + dist_3a + dist_4d + dist_zwem + chl + doo + ZM,
                        data=MaasSubSet,
                        listw = knnSubSet1_w)
summary(SPEMaas_E)

SPLMaas_E <- lagsarlm(lnprice ~ factor(Quarter) + lnwoonop + nbadk + factor(inpandig) + isol + factor(tuin_zow) 
                      + factor(won_4) + 
                        factor(won_1) + bwpr1905 + bwpr6070 + bwpr7180 + bwpr8190 + bwpr9100 + bevdicht + 
                        pnietact + geminkinko + raildist + highdist + dist_local + factor(buff25) + 
                        dist_4e + dist_3a + dist_4d + dist_zwem + chl + doo + ZM,
                      data=MaasSubSet,
                      listw = knnSubSet1_w)
summary(SPLMaas_E)

library(stargazer)
stargazer(OLSMaas_E, SPEMaas_E, SPLMaas_E, out="Results/Maas_Spatial2004.htm") 
DataHedonic <- read.csv2("SurveyData/NewdataVL.csv")
