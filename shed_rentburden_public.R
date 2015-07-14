############################################################################################################
##
## Copyright (c) 2015 Zillow.com. All rights reserved. 
##
## shed_rentburden.R: This program uses Federal Reserve Board, 2014 Survey of Household Economics and  
## Decisionmaking (SHED) data to look at the relationship between rent burdens and other expenditures
##
## Created: 06/04/15 by Aaron Terrazas
## 
############################################################################################################


## Set working directory
wd <- "C:/Users/YOURNAME/Desktop/shed_rentburden"  # working directory

## Load packages
packages <- c("plyr", "foreign", "isotone", "questionr", "data.table", "Hmisc")
lapply(packages, require, character.only=TRUE)
rm(packages)

options(scipen=999)
options(stringsAsFactors=FALSE)

setwd(wd)

## Download and unzip Fed microdata from 
## http://www.federalreserve.gov/communitydev/files/SHED_public_use_data_2014_(CSV).zip
## Read unzipped csv data from local file
shed <- read.csv("SHED_public_use_data_2014.csv", header=TRUE)

## Download U.S. Census Bureau, American Community Survey (ACS) 2013 data from usa.ipums.org
## SHED data on household income are binned, so we merge household incomes from the ACS 
## conditional on several variables listed below.
## Read ACS data from local file
acs <- read.dta("usa_00035.dta")

## Clean and format ACS data
acs <- acs[acs$gq != "Group quarters--Institutions" & acs$gq != "Other group quarters", ] ## drop group quarters
acs[,"ppage"] <- ifelse(substr(acs$age, 1, 2) == 'Le', 0, as.integer(substr(acs$age, 1, 2)))  ## re-format age

# Re-format age into the same age bins as provided in the SHED
acs[,"ppagecat"] <- NA
acs[,"ppagecat"][acs$ppage <= 17] <- 0
acs[,"ppagecat"][acs$ppage >= 18 & acs$ppage <= 24] <- 1
acs[,"ppagecat"][acs$ppage >= 25 & acs$ppage <= 34] <- 2
acs[,"ppagecat"][acs$ppage >= 35 & acs$ppage <= 44] <- 3
acs[,"ppagecat"][acs$ppage >= 45 & acs$ppage <= 56] <- 4
acs[,"ppagecat"][acs$ppage >= 55 & acs$ppage <= 64] <- 5
acs[,"ppagecat"][acs$ppage >= 65 & acs$ppage <= 74] <- 6
acs[,"ppagecat"][acs$ppage >= 75] <- 7

# Re-format household tenure
acs$ownershpR <- NA
acs$ownershpR[acs$ownershpd == 'Owned free and clear'] <- 1
acs$ownershpR[acs$ownershpd == 'Owned with mortgage or loan'] <- 2
acs$ownershpR[acs$ownershpd == 'With cash rent'] <- 3
acs$ownershpR[acs$ownershpd == 'No cash rent'] <- 4

# Bin household income into the groups provided in the SHED
acs$ppincimp <- NA
acs$ppincimp[acs$hhincome < 5000] <- 1
acs$ppincimp[acs$hhincome >= 5000 & acs$hhincome <= 7499] <- 2
acs$ppincimp[acs$hhincome >= 7500 & acs$hhincome <= 9999] <- 3
acs$ppincimp[acs$hhincome >= 10000 & acs$hhincome <= 12499] <- 4
acs$ppincimp[acs$hhincome >= 12500 & acs$hhincome <= 14999] <- 5
acs$ppincimp[acs$hhincome >= 15000 & acs$hhincome <= 19999] <- 6
acs$ppincimp[acs$hhincome >= 20000 & acs$hhincome <= 24999] <- 7
acs$ppincimp[acs$hhincome >= 25000 & acs$hhincome <= 29999] <- 8
acs$ppincimp[acs$hhincome >= 30000 & acs$hhincome <= 34999] <- 9
acs$ppincimp[acs$hhincome >= 35000 & acs$hhincome <= 39999] <- 10
acs$ppincimp[acs$hhincome >= 40000 & acs$hhincome <= 49999] <- 11
acs$ppincimp[acs$hhincome >= 50000 & acs$hhincome <= 59999] <- 12
acs$ppincimp[acs$hhincome >= 60000 & acs$hhincome <= 74999] <- 13
acs$ppincimp[acs$hhincome >= 75000 & acs$hhincome <= 84999] <- 14
acs$ppincimp[acs$hhincome >= 85000 & acs$hhincome <= 99999] <- 15
acs$ppincimp[acs$hhincome >= 100000 & acs$hhincome <= 124999] <- 16
acs$ppincimp[acs$hhincome >= 125000 & acs$hhincome <= 149999] <- 17
acs$ppincimp[acs$hhincome >= 150000 & acs$hhincome <= 174999] <- 18
acs$ppincimp[acs$hhincome >= 175000] <- 19
acs$ppincimp[acs$hhincome == 9999999] <- NA

# Re-format state 
acs$ppstaten <- revalue(acs$statefip, c(
  'Maine'=11, 'New Hampshire'=12, 'Vermont'=13, 'Massachusetts'=14, 'Rhode Island'=15, 'Connecticut'=16, 'New York'=21,
  'New Jersey'=22, 'Pennsylvania'=23, 'Ohio'=31, 'Indiana'=32, 'Illinois'=33, 'Michigan'=34, 'Wisconsin'=35, 'Minnesota'=41,
  'Iowa'=42, 'Missouri'=43, 'North Dakota'=44, 'South Dakota'=45, 'Nebraska'=46, 'Kansas'=47, 'Delaware'=51, 'Maryland'=52,
  'District of Columbia'=53, 'Virginia'=54, 'West Virginia'=55, 'North Carolina'=56, 'South Carolina'=57, 'Georgia'=58,
  'Florida'=59, 'Kentucky'=61, 'Tennessee'=62, 'Alabama'=63, 'Mississippi'=64, 'Arkansas'=71, 'Louisiana'=72,
  'Oklahoma'=73, 'Texas'=74, 'Montana'=81, 'Idaho'=82, 'Wyoming'=83, 'Colorado'=84, 'New Mexico'=85, 'Arizona'=86,
  'Utah'=87, 'Nevada'=88, 'Washington'=91, 'Oregon'=92, 'California'=93, 'Alaska'=94, 'Hawaii'=95))

# Bin states into the 9 region groups provided in the SHED
acs$ppreg9 <- revalue(acs$statefip, c(
  'Maine'=1, 'New Hampshire'=1, 'Vermont'=1, 'Massachusetts'=1, 'Rhode Island'=1, 'Connecticut'=1,
  'New York'=2, 'New Jersey'=2, 'Pennsylvania'=2,
  'Ohio'=3, 'Michigan'=3, 'Indiana'=3, 'Illinois'=3, 'Wisconsin'=3,
  'Minnesota'=4, 'Iowa'=4, 'Missouri'=4, 'Kansas'=4, 'Nebraska'=4, 'South Dakota'=4, 'North Dakota'=4,
  'Delaware'=5, 'Maryland'=5, 'District of Columbia'=5, 'West Virginia'=5, 'Virginia'=5, 'North Carolina'=5, 'South Carolina'=5, 'Georgia'=5, 'Florida'=5,
  'Kentucky'=6, 'Tennessee'=6, 'Mississippi'=6, 'Alabama'=6,
  'Arkansas'=7, 'Oklahoma'=7, 'Louisiana'=7, 'Texas'=7,
  'Montana'=8, 'Wyoming'=8, 'Idaho'=8, 'Colorado'=8, 'Utah'=8, 'Nevada'=8, 'Arizona'=8, 'New Mexico'=8,
  'Washington'=9, 'Alaska'=9, 'Oregon'=9, 'California'=9, 'Hawaii'=9))

## Compute the median household income in ACS, controling for:
## age category, tenure, household income bin, region of residence
income <- ddply(acs, 
                .(ppagecat, ownershpR, ppincimp, ppreg9), 
                summarise,
                income_med = weighted.median(hhincome, hhwt),
                obs = length(hhincome))

income$ppagecat <- as.integer(income$ppagecat)
income$ownershpR <- as.integer(income$ownershpR)
income$ppincimp <- as.integer(income$ppincimp)
income$ppreg9 <- as.integer(as.character(income$ppreg9))

## Recode tenure in the SHED data since tenure is based on several variables
shed$ownershpR <- NA
shed$ownershpR[shed$S2 == 1 & shed$M0 == 0] <- 1
shed$ownershpR[shed$S2 == 1 & shed$M0 == 1] <- 2
shed$ownershpR[shed$S2 == 2] <- 3
shed$ownershpR[shed$S2 == 3] <- 4
shed$ownershpR <- as.integer(shed$ownershpR)

## Merge ACS incomes onto SHED data
shed <- merge(shed, income, by=c('ppagecat', 'ownershpR', 'ppincimp', 'ppreg9'), all.x=TRUE)

## Calculate housing cost burden
shed$hcostburd <- NA
shed$hcostburd[shed$S2 == 1 & shed$M0 == 0] <- 0
shed$hcostburd[shed$S2 == 1 & shed$M0 == 1] <- ifelse(shed$M4[shed$S2 == 1 & shed$M0 == 1] %in% c(-1, 888888), NA, shed$M4[shed$S2 == 1 & shed$M0 == 1]/(shed$income_med[shed$S2 == 1 & shed$M0 == 1]/12))
shed$hcostburd[shed$S2 == 2] <- ifelse(shed$R3[shed$S2 == 2] %in% c(-1, 888888), NA, shed$R3[shed$S2 == 2]/(shed$income_med[shed$S2 == 2]/12))
shed$hcostburd[shed$S2 == 3] <- 0

## Calculate housing cost burden tier cutoffs for owners and renters, and create tier variable
wtd.quantile(shed$hcostburd[shed$S2 == 1 & shed$M0 == 1],
             weights=shed$weight3[shed$S2 == 1 & shed$M0 == 1],
             probs=seq(0, 1, 1/3),
             na.rm=TRUE)
wtd.quantile(shed$hcostburd[shed$hcostburd > 0 & shed$S2 == 2],
             weights=shed$weight3[shed$hcostburd > 0 & shed$S2 == 2],
             probs=seq(0, 1, 1/3),
             na.rm=TRUE)

shed$burdentier <- NA
shed$burdentier[shed$S2 == 2 & shed$hcostburd > 0 & shed$hcostburd <= .163] <- 1
shed$burdentier[shed$S2 == 2 & shed$hcostburd > .163 & shed$hcostburd <= .304] <- 2
shed$burdentier[shed$S2 == 2 & shed$hcostburd > .304] <- 3
shed$burdentier[shed$S2 == 1 & shed$M0 == 1 & shed$hcostburd > 0 & shed$hcostburd <= .125] <- 1
shed$burdentier[shed$S2 == 1 & shed$M0 == 1 & shed$hcostburd > .125 & shed$hcostburd <= .199] <- 2
shed$burdentier[shed$S2 == 1 & shed$M0 == 1 & shed$hcostburd > .199] <- 3

## Question E1. During the past 12 months, was there a time when you needed any of the following
## but didn't get it because you couldn't afford it?
ddply(shed, .(S2, burdentier), summarise, 
      prescription = weighted.mean(E1_a, weight3),
      seedoc = weighted.mean(E1_b, weight3),
      mentalhealth = weighted.mean(E1_c, weight3),
      dental = weighted.mean(E1_d, weight3),
      specialist = weighted.mean(E1_e, weight3),
      followup = weighted.mean(E1_f, weight3))

## Question E1_total. Combined responses to E1A and E1B -- could you cover 3 months expenses?
## -1: Refused. 0: Cannot cover 3 months expense. 1: Cover 3 months expense w/ borrowing.
## 2: Cover 3 months expense w/ savings.

wtd.table(shed$E1_total[shed$S2 == 1],
          weights=shed$weight3[shed$S2 == 1]) ## owners

wtd.table(shed$burdentier[shed$S2 == 2],
          shed$E1_total[shed$S2 == 2],
          weights=shed$weight3[shed$S2 == 2]) ## renters, but rent burden tier

## Question K0. How much thought have you given to the financial planning for your retirement?
## -1: Refused. 1: None at all. 2: A little. 3: Some. 4: A fair amount. 5: A lot.
## Young adults age 23-34 only, 

wtd.table(shed$burdentier[shed$S2 == 2 & shed$ppage >= 23 & shed$ppage <= 34 & shed$D2 < 8],
          shed$K0[shed$S2 == 2 & shed$ppage >= 23 & shed$ppage <= 34 & shed$D2 < 8],
          weights=shed$weight3[shed$S2 == 2 & shed$ppage >= 23 & shed$ppage <= 34 & shed$D2 < 8],
          normwt=FALSE, na.rm=TRUE)

## owners only, excluding retirees (D2)

wtd.table(shed$K0[shed$S2 == 1 & shed$ppage >= 23 & shed$ppage <= 34 & shed$D2 < 8],
          weights=shed$weight3[shed$S2 == 1 & shed$ppage >= 23 & shed$ppage <= 34 & shed$D2 < 8],
          normwt=FALSE, na.rm=TRUE)
## renters by rent burden tier, excluding retirees

## Question K14. You stated that you do not participate in a 401(k), 403(b), Thrift, or other defined
## contribution plan from work. Please state all the reasons below for why you do not currently invest in
## this type of retirement plan.
## Young adults age 23-34, excluding retirees (D2), with an employer offering a thrift of defined
## contribution pention plan (K2_b)

ddply(shed[shed$ppage >= 23 & shed$ppage <= 34 & shed$D2 < 8 & shed$K2_b %in% c(0, -1), ], .(S2), summarise,
      noncontrib = weighted.mean(K14_c, weight3, na.rm=TRUE))
## By tenure

ddply(shed[shed$S2 == 2 & shed$ppage >= 23 & shed$ppage <= 34 & shed$D2 < 8 & shed$K2_b %in% c(0, -1), ], .(burdentier), summarise,
      noncontrib = weighted.mean(K14_c, weight3, na.rm=TRUE))
## Renters, by rent burdne tier
