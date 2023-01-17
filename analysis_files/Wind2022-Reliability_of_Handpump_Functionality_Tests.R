## Uganda Handpump Project - Reliability of Functionality Tests
# Samantha Wind 
# swind@stanford.edu 

##########################################################################################
# packages

library("tidyverse")
library("readxl")
library("dplyr")
library("writexl")
library("irr")
library("lubridate")
library("psych")
library("reshape")
library("lme4")
library("lmerTest")
library("ggthemes")
library("cowplot")
library("DescTools")
library("irr")
library("lpSolve")
library("matrixStats")
library("gridExtra")
library("patchwork")

#######################################################################
## load data

ds01<-read.csv("https://raw.githubusercontent.com/samanthawind/ugandahandpumps2022/main/analysis_files/ds01.csv")
ds02<-read.csv("https://raw.githubusercontent.com/samanthawind/ugandahandpumps2022/main/analysis_files/ds02.csv")
ds03<-read.csv("https://raw.githubusercontent.com/samanthawind/ugandahandpumps2022/main/analysis_files/ds03.csv")

# fix bh_id column name
colnames(ds01)[colnames(ds01) == "ï..bh_id"] <- "bh_id"
colnames(ds02)[colnames(ds02) == "ï..bh_id"] <- "bh_id"
colnames(ds03)[colnames(ds03) == "ï..bh_id"] <- "bh_id"

# add "pumping depth" variable (num_pipes*3 meters)
ds03$pump_depth <- ds03$num_pipes*3

# borehole characteristics
AllData_bh_traits <- ds03

# functionality test data
Round1_testdata <- ds01
Round2_testdata <- ds02

###############################################################################
### prepare functionality test data for summary tables


## ROUND 2 sensitivity - exclude 231 & 226
runsensitivity <- 0
if (runsensitivity == 1){
  Round2_testdata <- Round2_testdata[Round2_testdata$bh_id != 231 &Round2_testdata$bh_id != 226, ]   
}


# Round 1 - mean of three trials for each borehole/enumerator combination
Round1_testdata$strokes_water_mean3 = rowMeans(select(Round1_testdata,strokes_water_1,strokes_water_2,strokes_water_3)) 
Round1_testdata$pump_strokes_mean3 = rowMeans(select(Round1_testdata,pump_strokes_1,pump_strokes_2,pump_strokes_3)) 
Round1_testdata$pump_time_mean3 = rowMeans(select(Round1_testdata,pump_time_1,pump_time_2,pump_time_3)) 
Round1_testdata$pump_cap_mean3 = rowMeans(select(Round1_testdata,pump_cap_1,pump_cap_2,pump_cap_3))
Round1_testdata$leak_rate_10_mean3 = rowMeans(select(Round1_testdata,leak_rate_10_1,leak_rate_10_2,leak_rate_10_3))
Round1_testdata$flow_rate_mean3 = rowMeans(select(Round1_testdata,flow_rate_1,flow_rate_2,flow_rate_3))

# create separate dataframes for summaries within each borehole (average rater results)
Round1_summarydata <- Round1_testdata
Round2_summarydata <- Round2_testdata

# mean results for each borehole
Round1_summarydata <- Round1_summarydata %>%
  group_by(bh_id) %>%
  summarise(strokes_water = mean(strokes_water_mean3),
            pump_strokes = mean(pump_strokes_mean3),
            pump_time =  mean(pump_time_mean3),
            pump_cap =  mean(pump_cap_mean3),
            leak_rate_10 =  mean(leak_rate_10_mean3),
            flow_rate =  mean(flow_rate_mean3),
            conductivity = mean(conductivity,na.rm = TRUE),
            turbidity = mean(turbidity,na.rm = TRUE))
            
Round2_summarydata <- Round2_summarydata %>%
  group_by(bh_id) %>%
  summarise(strokes_water = mean(strokes_water,na.rm = TRUE),
            pump_strokes = mean(pump_strokes,na.rm = TRUE),
            pump_time =  mean(pump_time,na.rm = TRUE),
            pump_cap =  mean(pump_cap,na.rm = TRUE),
            leak_rate_10 =  mean(leak_rate_10,na.rm = TRUE),
            flow_rate =  mean(flow_rate,na.rm = TRUE),
            conductivity = mean(conductivity,na.rm = TRUE),
            turbidity = mean(turbidity,na.rm = TRUE))

# make combined test result dataframe for summary stats
All_summarydata <- rbind(Round1_summarydata,Round2_summarydata)


#################
### summary tables

## Table 1 - borehole characteristics #######

# pump type 
AllData_bh_traits %>%
  group_by(pump_type) %>%
  summarise(n = n()) %>%
  mutate(freq = n/sum(n)*100)

# pipe_mat (Pipe material)
AllData_bh_traits %>%
  group_by(pipe_mat) %>%
  summarise(n = n()) %>%
  mutate(freq = n/sum(n)*100)

# rehabbed (How many handpumps have been rehabilitated)
AllData_bh_traits %>%
  group_by(rehabbed) %>%
  summarise(n = n()) %>%
  mutate(freq = n/sum(n)*100)

# number of riser pipes
AllData_bh_traits[is.na(AllData_bh_traits$num_pipes) == FALSE,] %>%
  summarise(n = n(),
            Mean = mean(num_pipes), 
            SD = sd(num_pipes),
            Median = median(num_pipes),
            IQR = IQR(num_pipes),
            Q_05 = quantile(num_pipes,0.05),
            Q_25 = quantile(num_pipes,0.25),
            Q_75 = quantile(num_pipes,0.75),
            Q_95 = quantile(num_pipes,0.95))

# pumping depth (meters)
AllData_bh_traits[is.na(AllData_bh_traits$pump_depth) == FALSE,] %>%
  summarise(n = n(),
            Mean = mean(pump_depth), 
            SD = sd(pump_depth),
            Median = median(pump_depth),
            IQR = IQR(pump_depth),
            Q_05 = quantile(pump_depth,0.05),
            Q_25 = quantile(pump_depth,0.25),
            Q_75 = quantile(pump_depth,0.75),
            Q_95 = quantile(pump_depth,0.95))

# borehole age
AllData_bh_traits[is.na(AllData_bh_traits$yrs_install) == FALSE,] %>%
  summarise(n = n(),
            Mean = mean(yrs_install), 
            SD = sd(yrs_install),
            Median = median(yrs_install),
            IQR = IQR(yrs_install))

# years since rehab
AllData_bh_traits[is.na(AllData_bh_traits$yrs_rehab) == FALSE,] %>%
  summarise(n = n(),
            Mean = mean(yrs_rehab), 
            SD = sd(yrs_rehab),
            Median = median(yrs_rehab),
            IQR = IQR(yrs_rehab))

# years since rehab or install
AllData_bh_traits[is.na(AllData_bh_traits$yrs_min) == FALSE,] %>%
  summarise(n = n(),
            Mean = mean(yrs_min), 
            SD = sd(yrs_min),
            Median = median(yrs_min),
            IQR = IQR(yrs_min))


#################################
## Table 2 - test results #######

## both rounds

# strokes to fill 20L
a12 <- All_summarydata[is.na(All_summarydata$pump_strokes) == FALSE,] %>%
  summarise(n = n(),
            Mean = mean(pump_strokes), 
            SD = sd(pump_strokes))

# pump capacity (cm3/stroke)
b12 <- All_summarydata[is.na(All_summarydata$pump_cap) == FALSE,] %>%
  summarise(Mean = mean(pump_cap), 
            SD = sd(pump_cap))

# strokes to first water, L10 
c12 <- All_summarydata[is.na(All_summarydata$strokes_water) == FALSE,] %>%
  summarise(Mean = mean(strokes_water), 
            SD = sd(strokes_water))

# 10-minute leakage rate, LR10
d12 <- All_summarydata[is.na(All_summarydata$leak_rate_10) == FALSE,] %>%
  summarise(Mean = mean(leak_rate_10), 
            SD = sd(leak_rate_10))

# seconds to fill 20L
e12 <- All_summarydata[is.na(All_summarydata$pump_time) == FALSE,] %>%
  summarise(Mean = mean(pump_time), 
            SD = sd(pump_time))

# flowrate, Q (m3/hour)
f12 <- All_summarydata[is.na(All_summarydata$flow_rate) == FALSE,] %>%
  summarise(Mean = mean(flow_rate), 
            SD = sd(flow_rate))

Summary_bothrounds <- c(a12,b12,c12,d12,e12,f12)
names(Summary_bothrounds) <- c("n","strokes to fill - mean",
                                  "strokes to fill - sd",
                                  "pump cap - mean",
                                  "pump cap - sd",
                                  "strokes to first water - mean",
                                  "strokes to first water - sd",
                                  "leak rate - mean",
                                  "leak rate - sd",
                                  "seconds to fill - mean",
                                  "seconds to fill - sd",
                                  "flowrate - mean",
                                  "flowrate - sd")
Summary_bothrounds

## Round 1

# strokes to fill 20L
a1 <- Round1_summarydata[is.na(Round1_summarydata$pump_strokes) == FALSE,] %>%
  summarise(n = n(),
            Mean = mean(pump_strokes), 
            SD = sd(pump_strokes))

# pump capacity (cm3/stroke)
b1 <- Round1_summarydata[is.na(Round1_summarydata$pump_cap) == FALSE,] %>%
  summarise(Mean = mean(pump_cap), 
            SD = sd(pump_cap))

# strokes to first water, L10 
c1 <- Round1_summarydata[is.na(Round1_summarydata$strokes_water) == FALSE,] %>%
  summarise(Mean = mean(strokes_water), 
            SD = sd(strokes_water))

# 10-minute leakage rate, LR10
d1 <- Round1_summarydata[is.na(Round1_summarydata$leak_rate_10) == FALSE,] %>%
  summarise(Mean = mean(leak_rate_10), 
            SD = sd(leak_rate_10))

# seconds to fill 20L
e1 <- Round1_summarydata[is.na(Round1_summarydata$pump_time) == FALSE,] %>%
  summarise(Mean = mean(pump_time), 
            SD = sd(pump_time))

# flowrate, Q (m3/hour)
f1 <- Round1_summarydata[is.na(Round1_summarydata$flow_rate) == FALSE,] %>%
  summarise(Mean = mean(flow_rate), 
            SD = sd(flow_rate))

Summary_Round1 <- c(a1,b1,c1,d1,e1,f1)
names(Summary_Round1) <- c("n","strokes to fill - mean",
                               "strokes to fill - sd",
                               "pump cap - mean",
                               "pump cap - sd",
                               "strokes to first water - mean",
                               "strokes to first water - sd",
                               "leak rate - mean",
                               "leak rate - sd",
                               "seconds to fill - mean",
                               "seconds to fill - sd",
                               "flowrate - mean",
                               "flowrate - sd")
Summary_Round1


## Round 2

# strokes to fill 20L
a2 <- Round2_summarydata[is.na(Round2_summarydata$pump_strokes) == FALSE,] %>%
  summarise(n = n(),
            Mean = mean(pump_strokes), 
            SD = sd(pump_strokes))

# pump capacity (cm3/stroke)
b2 <- Round2_summarydata[is.na(Round2_summarydata$pump_cap) == FALSE,] %>%
  summarise(Mean = mean(pump_cap), 
            SD = sd(pump_cap))

# strokes to first water, L10 
c2 <- Round2_summarydata[is.na(Round2_summarydata$strokes_water) == FALSE,] %>%
  summarise(Mean = mean(strokes_water), 
            SD = sd(strokes_water))

# 10-minute leakage rate, LR10
d2 <- Round2_summarydata[is.na(Round2_summarydata$leak_rate_10) == FALSE,] %>%
  summarise(Mean = mean(leak_rate_10), 
            SD = sd(leak_rate_10))

# seconds to fill 20L
e2 <- Round2_summarydata[is.na(Round2_summarydata$pump_time) == FALSE,] %>%
  summarise(Mean = mean(pump_time), 
            SD = sd(pump_time))

# flowrate, Q (m3/hour)
f2 <- Round2_summarydata[is.na(Round2_summarydata$flow_rate) == FALSE,] %>%
  summarise(Mean = mean(flow_rate), 
            SD = sd(flow_rate))

Summary_Round2 <- c(a2,b2,c2,d2,e2,f2)
names(Summary_Round2) <- c("n","strokes to fill - mean",
                           "strokes to fill - sd",
                           "pump cap - mean",
                           "pump cap - sd",
                           "strokes to first water - mean",
                           "strokes to first water - sd",
                           "leak rate - mean",
                           "leak rate - sd",
                           "seconds to fill - mean",
                           "seconds to fill - sd",
                           "flowrate - mean",
                           "flowrate - sd")
Summary_Round2

## Mann-Whitney U Test

# add "round" label to boreholes
All_summarydata$round <-All_summarydata$pump_strokes*0 # initialize 
All_summarydata$round[All_summarydata$bh_id < 200] <- 1  
All_summarydata$round[All_summarydata$bh_id > 200] <- 2  

# run test
wilcox_pump_strokes <- wilcox.test(pump_strokes~ round,data = All_summarydata,exact = FALSE)
wilcox_pump_cap <- wilcox.test(pump_cap~ round,data = All_summarydata,exact = FALSE)
wilcox_strokes_water <- wilcox.test(strokes_water~ round,data = All_summarydata,exact = FALSE)
wilcox_leak_rate_10 <- wilcox.test(leak_rate_10~ round,data = All_summarydata,exact = FALSE)
wilcox_pump_time <- wilcox.test(pump_time~ round,data = All_summarydata,exact = FALSE)
wilcox_flow_rate <- wilcox.test(flow_rate~ round,data = All_summarydata,exact = FALSE)

# print
wilcox_pump_strokes 
wilcox_pump_cap 
wilcox_strokes_water
wilcox_leak_rate_10 
wilcox_pump_time 
wilcox_flow_rate 


###################################################################
### ICC analysis

## Round 1 Inter-rater: format dataset for analysis
# model: ICC(2,1), use first trial only
# format: rows = boreholes, columns = trial 1 of each rater

data_Round1_inter_pumpcap_condensed <- select(Round1_testdata,bh_id,enum_id,pump_cap_1)
data_Round1_inter_leakrate_condensed <- select(Round1_testdata,bh_id,enum_id,leak_rate_10_1)
data_Round1_inter_flowrate_condensed <- select(Round1_testdata,bh_id,enum_id,flow_rate_1)

data_Round1_inter_pumpcap_condensed 
data_Round1_inter_leakrate_condensed 
data_Round1_inter_flowrate_condensed 

# reformat for analysis
data_Round1_inter_pumpcap_wide <- data_Round1_inter_pumpcap_condensed %>%
  pivot_wider(names_from = enum_id,values_from = pump_cap_1)

data_Round1_inter_leakrate_wide <- data_Round1_inter_leakrate_condensed %>%
  pivot_wider(names_from = enum_id,values_from = leak_rate_10_1)

data_Round1_inter_flowrate_wide <- data_Round1_inter_flowrate_condensed %>%
  pivot_wider(names_from = enum_id,values_from = flow_rate_1)

data_Round1_inter_pumpcap_wide
data_Round1_inter_leakrate_wide
data_Round1_inter_flowrate_wide

# four-rater scenario (remove rows with any NAs)
data_Round1_inter_pumpcap_4 <-data_Round1_inter_pumpcap_wide[complete.cases(data_Round1_inter_pumpcap_wide),]
data_Round1_inter_leakrate_4 <-data_Round1_inter_leakrate_wide[complete.cases(data_Round1_inter_leakrate_wide),]
data_Round1_inter_flowrate_4 <-data_Round1_inter_flowrate_wide[complete.cases(data_Round1_inter_flowrate_wide),]

# three-rater scenario (e1 missed some days of data collection)
data_Round1_inter_pumpcap_3 <-data_Round1_inter_pumpcap_wide[complete.cases(data_Round1_inter_pumpcap_wide[,-2]),-2]
data_Round1_inter_leakrate_3 <-data_Round1_inter_leakrate_wide[complete.cases(data_Round1_inter_leakrate_wide[,-2]),-2]
data_Round1_inter_flowrate_3 <-data_Round1_inter_flowrate_wide[complete.cases(data_Round1_inter_flowrate_wide[,-2]),-2]


# remove bh_ids for analysis

data_Round1_inter_pumpcap_4 <- data_Round1_inter_pumpcap_4[,-1] 
data_Round1_inter_leakrate_4 <- data_Round1_inter_leakrate_4[,-1] 
data_Round1_inter_flowrate_4 <- data_Round1_inter_flowrate_4[,-1] 

data_Round1_inter_pumpcap_3 <- data_Round1_inter_pumpcap_3[,-1] 
data_Round1_inter_leakrate_3 <- data_Round1_inter_leakrate_3[,-1] 
data_Round1_inter_flowrate_3 <- data_Round1_inter_flowrate_3[,-1] 

data_Round1_inter_pumpcap_4
data_Round1_inter_leakrate_4 
data_Round1_inter_flowrate_4 

data_Round1_inter_pumpcap_3 
data_Round1_inter_leakrate_3 
data_Round1_inter_flowrate_3 

######
# Round 1 Inter-rater reliability calculations
#ICC (2,1)
icc_inter_Round1_pumpcap_3 <- psych::ICC(data_Round1_inter_pumpcap_3,missing=TRUE,alpha=.05,lmer=TRUE,check.keys=FALSE)
icc_inter_Round1_leakrate_3 <- psych::ICC(data_Round1_inter_leakrate_3,missing=TRUE,alpha=.05,lmer=TRUE,check.keys=FALSE)
icc_inter_Round1_flowrate_3 <- psych::ICC(data_Round1_inter_flowrate_3,missing=TRUE,alpha=.05,lmer=TRUE,check.keys=FALSE)

icc_inter_Round1_pumpcap_4 <- psych::ICC(data_Round1_inter_pumpcap_4,missing=TRUE,alpha=.05,lmer=TRUE,check.keys=FALSE)
icc_inter_Round1_leakrate_4 <- psych::ICC(data_Round1_inter_leakrate_4,missing=TRUE,alpha=.05,lmer=TRUE,check.keys=FALSE)
icc_inter_Round1_flowrate_4 <- psych::ICC(data_Round1_inter_flowrate_4,missing=TRUE,alpha=.05,lmer=TRUE,check.keys=FALSE)

icc_inter_Round1_pumpcap_3  #ICC2: 0.68 [0.34,0.85] x
icc_inter_Round1_leakrate_3 #ICC2: 0.98 [0.95,0.99] x
icc_inter_Round1_flowrate_3 #ICC2: 0.62 [0.30,0.81] x

icc_inter_Round1_pumpcap_4  #ICC2: 0.72 [0.47,0.88] x
icc_inter_Round1_leakrate_4 #ICC2: 0.98 [0.96,0.99] x
icc_inter_Round1_flowrate_4 #ICC2: 0.62 [0.37,0.81] x



###################################################
## Round 2 Inter-rater: format dataset for analysis
# model: ICC(1,1) [only one trial recorded, different group of raters for each borehole]
# format: rows = boreholes, columns = measurement # (aka rater)


data_Round2_inter_pumpcap_condensed <- select(Round2_testdata,bh_id,measure_num,pump_cap)
data_Round2_inter_leakrate_condensed <- select(Round2_testdata,bh_id,measure_num,leak_rate_10)
data_Round2_inter_flowrate_condensed <- select(Round2_testdata,bh_id,measure_num,flow_rate)

data_Round2_inter_pumpcap_condensed
data_Round2_inter_leakrate_condensed
data_Round2_inter_flowrate_condensed


# reformat for analysis
data_Round2_inter_pumpcap_wide <- data_Round2_inter_pumpcap_condensed %>%
  pivot_wider(names_from = measure_num,values_from = pump_cap)

data_Round2_inter_leakrate_wide <- data_Round2_inter_leakrate_condensed %>%
  pivot_wider(names_from = measure_num,values_from = leak_rate_10)

data_Round2_inter_flowrate_wide <- data_Round2_inter_flowrate_condensed %>%
  pivot_wider(names_from = measure_num,values_from = flow_rate)

data_Round2_inter_pumpcap_wide
data_Round2_inter_leakrate_wide
data_Round2_inter_flowrate_wide

# four-measurement scenario (most boreholes have around 4 measurements, use first four)
data_Round2_inter_pumpcap_4 <-data_Round2_inter_pumpcap_wide[complete.cases(data_Round2_inter_pumpcap_wide[,1:5]),1:5]
data_Round2_inter_leakrate_4 <-data_Round2_inter_leakrate_wide[complete.cases(data_Round2_inter_leakrate_wide[,1:5]),1:5]
data_Round2_inter_flowrate_4 <-data_Round2_inter_flowrate_wide[complete.cases(data_Round2_inter_flowrate_wide[,1:5]),1:5]

# five-measurement scenario (similar amount have 5 measurements, use first 5)
data_Round2_inter_pumpcap_5 <-data_Round2_inter_pumpcap_wide[complete.cases(data_Round2_inter_pumpcap_wide[,1:6]),1:6]
data_Round2_inter_leakrate_5 <-data_Round2_inter_leakrate_wide[complete.cases(data_Round2_inter_leakrate_wide[,1:6]),1:6]
data_Round2_inter_flowrate_5 <-data_Round2_inter_flowrate_wide[complete.cases(data_Round2_inter_flowrate_wide[,1:6]),1:6]

# remove bh_ids for analysis

data_Round2_inter_pumpcap_4 <- data_Round2_inter_pumpcap_4[,-1] 
data_Round2_inter_leakrate_4 <- data_Round2_inter_leakrate_4[,-1] 
data_Round2_inter_flowrate_4 <- data_Round2_inter_flowrate_4[,-1] 

data_Round2_inter_pumpcap_5 <- data_Round2_inter_pumpcap_5[,-1] 
data_Round2_inter_leakrate_5 <- data_Round2_inter_leakrate_5[,-1] 
data_Round2_inter_flowrate_5 <- data_Round2_inter_flowrate_5[,-1] 

data_Round2_inter_pumpcap_4
data_Round2_inter_leakrate_4 
data_Round2_inter_flowrate_4 

data_Round2_inter_pumpcap_5 
data_Round2_inter_leakrate_5 
data_Round2_inter_flowrate_5 

######
# Round 2 Inter-rater reliability calculations
#ICC (1,1)
icc_inter_Round2_pumpcap_4 <- psych::ICC(data_Round2_inter_pumpcap_4,missing=TRUE,alpha=.05,lmer=TRUE,check.keys=FALSE)
icc_inter_Round2_leakrate_4 <- psych::ICC(data_Round2_inter_leakrate_4,missing=TRUE,alpha=.05,lmer=TRUE,check.keys=FALSE)
icc_inter_Round2_flowrate_4 <- psych::ICC(data_Round2_inter_flowrate_4,missing=TRUE,alpha=.05,lmer=TRUE,check.keys=FALSE)

icc_inter_Round2_pumpcap_5 <- psych::ICC(data_Round2_inter_pumpcap_5,missing=TRUE,alpha=.05,lmer=TRUE,check.keys=FALSE)
icc_inter_Round2_leakrate_5 <- psych::ICC(data_Round2_inter_leakrate_5,missing=TRUE,alpha=.05,lmer=TRUE,check.keys=FALSE)
icc_inter_Round2_flowrate_5 <- psych::ICC(data_Round2_inter_flowrate_5,missing=TRUE,alpha=.05,lmer=TRUE,check.keys=FALSE)


icc_inter_Round2_pumpcap_4  #ICC1: 0.85 [0.75,0.91] x
icc_inter_Round2_leakrate_4 #ICC1: 0.79 [0.68,0.88] x
icc_inter_Round2_flowrate_4 #ICC1: 0.59 [0.42,0.74] x

icc_inter_Round2_pumpcap_5  #ICC1: 0.87 [0.78,0.93] x
icc_inter_Round2_leakrate_5 #ICC1: 0.86 [0.76,0.93] x
icc_inter_Round2_flowrate_5 #ICC1: 0.54 [0.36,0.72] x


###################################################
## Round 1 Intra-rater: format dataset for analysis
# model: ICC(3,1) [raters are fixed]
# format: rows = boreholes, columns = trial # 

data_Round1_intra_pumpcap_allenum <- select(Round1_testdata,bh_id,enum_id,pump_cap_1,pump_cap_2,pump_cap_3)
data_Round1_intra_leakrate_allenum <- select(Round1_testdata,bh_id,enum_id,leak_rate_10_1,leak_rate_10_2,leak_rate_10_3)
data_Round1_intra_flowrate_allenum <- select(Round1_testdata,bh_id,enum_id,flow_rate_1,flow_rate_2,flow_rate_3)

data_Round1_intra_pumpcap_allenum 
data_Round1_intra_leakrate_allenum 
data_Round1_intra_flowrate_allenum 

# make separate dataset for each rater 
#& remove bh_id and enum_id columns for analysis

data_Round1_intra_pumpcap_e1 <- data_Round1_intra_pumpcap_allenum[data_Round1_intra_pumpcap_allenum$enum_id == "e1",3:5]
data_Round1_intra_pumpcap_e2 <- data_Round1_intra_pumpcap_allenum[data_Round1_intra_pumpcap_allenum$enum_id == "e2",3:5]
data_Round1_intra_pumpcap_e3 <- data_Round1_intra_pumpcap_allenum[data_Round1_intra_pumpcap_allenum$enum_id == "e3",3:5]
data_Round1_intra_pumpcap_e4 <- data_Round1_intra_pumpcap_allenum[data_Round1_intra_pumpcap_allenum$enum_id == "e4",3:5]

data_Round1_intra_pumpcap_e1
data_Round1_intra_pumpcap_e2
data_Round1_intra_pumpcap_e3
data_Round1_intra_pumpcap_e4

data_Round1_intra_leakrate_e1 <- data_Round1_intra_leakrate_allenum[data_Round1_intra_leakrate_allenum$enum_id == "e1",3:5]
data_Round1_intra_leakrate_e2 <- data_Round1_intra_leakrate_allenum[data_Round1_intra_leakrate_allenum$enum_id == "e2",3:5]
data_Round1_intra_leakrate_e3 <- data_Round1_intra_leakrate_allenum[data_Round1_intra_leakrate_allenum$enum_id == "e3",3:5]
data_Round1_intra_leakrate_e4 <- data_Round1_intra_leakrate_allenum[data_Round1_intra_leakrate_allenum$enum_id == "e4",3:5]

data_Round1_intra_leakrate_e1
data_Round1_intra_leakrate_e2
data_Round1_intra_leakrate_e3
data_Round1_intra_leakrate_e4

data_Round1_intra_flowrate_e1 <- data_Round1_intra_flowrate_allenum[data_Round1_intra_flowrate_allenum$enum_id == "e1",3:5]
data_Round1_intra_flowrate_e2 <- data_Round1_intra_flowrate_allenum[data_Round1_intra_flowrate_allenum$enum_id == "e2",3:5]
data_Round1_intra_flowrate_e3 <- data_Round1_intra_flowrate_allenum[data_Round1_intra_flowrate_allenum$enum_id == "e3",3:5]
data_Round1_intra_flowrate_e4 <- data_Round1_intra_flowrate_allenum[data_Round1_intra_flowrate_allenum$enum_id == "e4",3:5]

data_Round1_intra_flowrate_e1
data_Round1_intra_flowrate_e2
data_Round1_intra_flowrate_e3
data_Round1_intra_flowrate_e4

######
# Round 1 Intra-rater reliability calculations
#ICC (3,1)
icc_intra_Round1_pumpcap_e1 <- psych::ICC(data_Round1_intra_pumpcap_e1,missing=TRUE,alpha=.05,lmer=TRUE,check.keys=FALSE)
icc_intra_Round1_pumpcap_e2 <- psych::ICC(data_Round1_intra_pumpcap_e2,missing=TRUE,alpha=.05,lmer=TRUE,check.keys=FALSE)
icc_intra_Round1_pumpcap_e3 <- psych::ICC(data_Round1_intra_pumpcap_e3,missing=TRUE,alpha=.05,lmer=TRUE,check.keys=FALSE)
icc_intra_Round1_pumpcap_e4 <- psych::ICC(data_Round1_intra_pumpcap_e4,missing=TRUE,alpha=.05,lmer=TRUE,check.keys=FALSE)

icc_intra_Round1_leakrate_e1 <- psych::ICC(data_Round1_intra_leakrate_e1,missing=TRUE,alpha=.05,lmer=TRUE,check.keys=FALSE)
icc_intra_Round1_leakrate_e2 <- psych::ICC(data_Round1_intra_leakrate_e2,missing=TRUE,alpha=.05,lmer=TRUE,check.keys=FALSE)
icc_intra_Round1_leakrate_e3 <- psych::ICC(data_Round1_intra_leakrate_e3,missing=TRUE,alpha=.05,lmer=TRUE,check.keys=FALSE)
icc_intra_Round1_leakrate_e4 <- psych::ICC(data_Round1_intra_leakrate_e4,missing=TRUE,alpha=.05,lmer=TRUE,check.keys=FALSE)

icc_intra_Round1_flowrate_e1 <- psych::ICC(data_Round1_intra_flowrate_e1,missing=TRUE,alpha=.05,lmer=TRUE,check.keys=FALSE)
icc_intra_Round1_flowrate_e2 <- psych::ICC(data_Round1_intra_flowrate_e2,missing=TRUE,alpha=.05,lmer=TRUE,check.keys=FALSE)
icc_intra_Round1_flowrate_e3 <- psych::ICC(data_Round1_intra_flowrate_e3,missing=TRUE,alpha=.05,lmer=TRUE,check.keys=FALSE)
icc_intra_Round1_flowrate_e4 <- psych::ICC(data_Round1_intra_flowrate_e4,missing=TRUE,alpha=.05,lmer=TRUE,check.keys=FALSE)


icc_intra_Round1_pumpcap_e1 #ICC3: 0.95 [0.91,0.98] x
icc_intra_Round1_pumpcap_e2 #ICC3: 0.94 [0.89,0.97] x
icc_intra_Round1_pumpcap_e3 #ICC3: 0.97 [0.95,0.99] x
icc_intra_Round1_pumpcap_e4 #ICC3: 0.93 [0.87,0.96] x

icc_intra_Round1_leakrate_e1 #ICC3: 0.99 [0.99,1] x
icc_intra_Round1_leakrate_e2 #ICC3: 0.96 [0.92,0.98] x
icc_intra_Round1_leakrate_e3 #ICC3: 0.99 [0.99,1] x
icc_intra_Round1_leakrate_e4 #ICC3: 0.99 [0.98,1] x

icc_intra_Round1_flowrate_e1 #ICC3: 0.95 [0.90,0.98] x
icc_intra_Round1_flowrate_e2 #ICC3: 0.94 [0.88,0.97] x
icc_intra_Round1_flowrate_e3 #ICC3: 0.92 [0.86,0.96] x
icc_intra_Round1_flowrate_e4 #ICC3: 0.90 [0.82,0.95] x


#########################################################
# for Supplemental Information - ICC for inter-rater reliability of Round 1, mean of three trials

#### format for analysis
data_Round1_inter_pumpcap_3trial <- select(Round1_testdata,bh_id,enum_id,pump_cap_mean3)
data_Round1_inter_leakrate_3trial <- select(Round1_testdata,bh_id,enum_id,leak_rate_10_mean3)
data_Round1_inter_flowrate_3trial <- select(Round1_testdata,bh_id,enum_id,flow_rate_mean3)

data_Round1_inter_pumpcap_3trial
data_Round1_inter_leakrate_3trial 
data_Round1_inter_flowrate_3trial 

## reformat to be wider
data_Round1_inter_pumpcap_3trial_wide <- data_Round1_inter_pumpcap_3trial %>%
  pivot_wider(names_from = enum_id,values_from = pump_cap_mean3)

data_Round1_inter_leakrate_3trial_wide <- data_Round1_inter_leakrate_3trial %>%
  pivot_wider(names_from = enum_id,values_from = leak_rate_10_mean3)

data_Round1_inter_flowrate_3trial_wide <- data_Round1_inter_flowrate_3trial %>%
  pivot_wider(names_from = enum_id,values_from = flow_rate_mean3)

data_Round1_inter_pumpcap_3trial_wide
data_Round1_inter_leakrate_3trial_wide
data_Round1_inter_flowrate_3trial_wide


# four-rater scenario (remove rows with any NAs)
data_Round1_inter_pumpcap_3trial_4 <-data_Round1_inter_pumpcap_3trial_wide[complete.cases(data_Round1_inter_pumpcap_3trial_wide),]
data_Round1_inter_leakrate_3trial_4 <-data_Round1_inter_leakrate_3trial_wide[complete.cases(data_Round1_inter_leakrate_3trial_wide),]
data_Round1_inter_flowrate_3trial_4 <-data_Round1_inter_flowrate_3trial_wide[complete.cases(data_Round1_inter_flowrate_3trial_wide),]

# three-rater scenario (e1 missed some days of data collection)
data_Round1_inter_pumpcap_3trial_3 <-data_Round1_inter_pumpcap_3trial_wide[complete.cases(data_Round1_inter_pumpcap_3trial_wide[,-2]),-2]
data_Round1_inter_leakrate_3trial_3 <-data_Round1_inter_leakrate_3trial_wide[complete.cases(data_Round1_inter_leakrate_3trial_wide[,-2]),-2]
data_Round1_inter_flowrate_3trial_3 <-data_Round1_inter_flowrate_3trial_wide[complete.cases(data_Round1_inter_flowrate_3trial_wide[,-2]),-2]

# remove bh_ids for analysis
data_Round1_inter_pumpcap_3trial_4 <- data_Round1_inter_pumpcap_3trial_4[,-1] 
data_Round1_inter_leakrate_3trial_4 <- data_Round1_inter_leakrate_3trial_4[,-1] 
data_Round1_inter_flowrate_3trial_4 <- data_Round1_inter_flowrate_3trial_4[,-1] 

data_Round1_inter_pumpcap_3trial_3 <- data_Round1_inter_pumpcap_3trial_3[,-1] 
data_Round1_inter_leakrate_3trial_3 <- data_Round1_inter_leakrate_3trial_3[,-1] 
data_Round1_inter_flowrate_3trial_3 <- data_Round1_inter_flowrate_3trial_3[,-1] 

data_Round1_inter_pumpcap_3trial_4
data_Round1_inter_leakrate_3trial_4 
data_Round1_inter_flowrate_3trial_4 

data_Round1_inter_pumpcap_3trial_3 
data_Round1_inter_leakrate_3trial_3 
data_Round1_inter_flowrate_3trial_3 

###
# Round 1 Inter-rater reliability calculations - mean of 3 trials
#ICC (2,1)
icc_inter_Round1_pumpcap_3trial_3 <- psych::ICC(data_Round1_inter_pumpcap_3trial_3,missing=TRUE,alpha=.05,lmer=TRUE,check.keys=FALSE)
icc_inter_Round1_leakrate_3trial_3 <- psych::ICC(data_Round1_inter_leakrate_3trial_3,missing=TRUE,alpha=.05,lmer=TRUE,check.keys=FALSE)
icc_inter_Round1_flowrate_3trial_3 <- psych::ICC(data_Round1_inter_flowrate_3trial_3,missing=TRUE,alpha=.05,lmer=TRUE,check.keys=FALSE)

icc_inter_Round1_pumpcap_3trial_4 <- psych::ICC(data_Round1_inter_pumpcap_3trial_4,missing=TRUE,alpha=.05,lmer=TRUE,check.keys=FALSE)
icc_inter_Round1_leakrate_3trial_4 <- psych::ICC(data_Round1_inter_leakrate_3trial_4,missing=TRUE,alpha=.05,lmer=TRUE,check.keys=FALSE)
icc_inter_Round1_flowrate_3trial_4 <- psych::ICC(data_Round1_inter_flowrate_3trial_4,missing=TRUE,alpha=.05,lmer=TRUE,check.keys=FALSE)


icc_inter_Round1_pumpcap_3trial_3  #ICC2: 0.71 [0.35, 0.87] x
icc_inter_Round1_leakrate_3trial_3 #ICC2: 0.99 [0.97,0.99] x
icc_inter_Round1_flowrate_3trial_3 #ICC2: 0.64 [0.29,0.84] x 

icc_inter_Round1_pumpcap_3trial_4  #ICC2: 0.74 [0.48,0.89] x
icc_inter_Round1_leakrate_3trial_4 #ICC2: 0.99 [0.98,1] x
icc_inter_Round1_flowrate_3trial_4 #ICC2: 0.68 [0.41,0.85] x


#########################################################
## Data Visualization

# summarize pumpcap, leakrate, flowrate with mean,min,max

Round1_pumpcap_data <- gather(select(Round1_testdata,bh_id,pump_cap_1,pump_cap_2,pump_cap_3),trial,pump_cap, pump_cap_1:pump_cap_3)
Round1_leakrate_data <- gather(select(Round1_testdata,bh_id,leak_rate_10_1,leak_rate_10_2,leak_rate_10_3),trial,leak_rate_10, leak_rate_10_1:leak_rate_10_3)
Round1_flowrate_data <- gather(select(Round1_testdata,bh_id,flow_rate_1,flow_rate_2,flow_rate_3),trial,flow_rate, flow_rate_1:flow_rate_3)

Round1_pumpcap_data 
Round1_leakrate_data 
Round1_flowrate_data 

# pumpcap
Round1_pumpcap_summary <- Round1_pumpcap_data %>%
  group_by(bh_id) %>%
  summarise(Num = n(),
            Mean = mean(pump_cap),
            Min = min(pump_cap),
            Max = max(pump_cap),
            SD = sd(pump_cap)) %>%
  mutate(SE = SD/sqrt(Num)) %>%
  mutate(errorbar_max = Mean + SD) %>%
  mutate(errorbar_min = Mean - SD) 

Round1_pumpcap_summary

# leakrate

Round1_leakrate_summary <- Round1_leakrate_data %>%
  group_by(bh_id) %>%
  summarise(Num = n(),
            Mean = mean(leak_rate_10),
            Min = min(leak_rate_10),
            Max = max(leak_rate_10),
            SD = sd(leak_rate_10)) %>%
  mutate(SE = SD/sqrt(Num)) %>%
  mutate(errorbar_max = Mean + SD) %>%
  mutate(errorbar_min = Mean - SD) 

Round1_leakrate_summary

# flow rate
Round1_flowrate_summary <- Round1_flowrate_data %>%
  group_by(bh_id) %>%
  summarise(Num = n(),
            Mean = mean(flow_rate),
            Min = min(flow_rate),
            Max = max(flow_rate),
            SD = sd(flow_rate)) %>%
  mutate(SE = SD/sqrt(Num)) %>%
  mutate(errorbar_max = Mean + SD) %>%
  mutate(errorbar_min = Mean - SD) 

Round1_flowrate_summary


# Round 2 ####################################################
Round2_pumpcap_data <- select(Round2_testdata,bh_id,pump_cap)
Round2_leakrate_data <- select(Round2_testdata,bh_id,leak_rate_10)
Round2_flowrate_data <- select(Round2_testdata,bh_id,flow_rate)

# pumpcap

Round2_pumpcap_summary <- Round2_pumpcap_data %>%
  group_by(bh_id) %>%
  summarise(Num = n(),
            Mean = mean(pump_cap),
            Min = min(pump_cap),
            Max = max(pump_cap),
            SD = sd(pump_cap)) %>%
  mutate(SE = SD/sqrt(Num)) %>%
  mutate(errorbar_max = Mean + SD) %>%
  mutate(errorbar_min = Mean - SD) 

Round2_pumpcap_summary

# leakrate

Round2_leakrate_summary <- Round2_leakrate_data %>%
  group_by(bh_id) %>%
  summarise(Num = n(),
            Mean = mean(leak_rate_10),
            Min = min(leak_rate_10),
            Max = max(leak_rate_10),
            SD = sd(leak_rate_10)) %>%
  mutate(SE = SD/sqrt(Num)) %>%
  mutate(errorbar_max = Mean + SD) %>%
  mutate(errorbar_min = Mean - SD) 

Round2_leakrate_summary

# flowrate

Round2_flowrate_summary <- Round2_flowrate_data %>%
  group_by(bh_id) %>%
  summarise(Num = n(),
            Mean = mean(flow_rate),
            Min = min(flow_rate),
            Max = max(flow_rate),
            SD = sd(flow_rate)) %>%
  mutate(SE = SD/sqrt(Num)) %>%
  mutate(errorbar_max = Mean + SD) %>%
  mutate(errorbar_min = Mean - SD) 

Round2_flowrate_summary


# combine round 1 & 2 summaries

All_pumpcap_summary <- rbind(Round1_pumpcap_summary,Round2_pumpcap_summary)
All_leakrate_summary <- rbind(Round1_leakrate_summary,Round2_leakrate_summary)
All_flowrate_summary <- rbind(Round1_flowrate_summary,Round2_flowrate_summary)


# sort by rank
All_pumpcap_summary <- All_pumpcap_summary %>%
  arrange(Mean) %>%
  mutate(rank = -row_number(Mean)+61)
# high pump cap is better

All_leakrate_summary <- All_leakrate_summary %>%
  arrange(Mean) %>%
  mutate(rank = row_number(Mean))

All_flowrate_summary <- All_flowrate_summary %>%
  arrange(Mean) %>%
  mutate(rank = -row_number(Mean)+61)
# high flow rate is better

All_pumpcap_summary$Round <- All_pumpcap_summary$bh_id*0 
All_pumpcap_summary$Round[All_pumpcap_summary$bh_id < 200] <- 1
All_pumpcap_summary$Round[All_pumpcap_summary$bh_id > 200] <- 2

All_leakrate_summary$Round <- All_leakrate_summary$bh_id*0 
All_leakrate_summary$Round[All_leakrate_summary$bh_id < 200] <- 1
All_leakrate_summary$Round[All_leakrate_summary$bh_id > 200] <- 2

All_flowrate_summary$Round <- All_flowrate_summary$bh_id*0 
All_flowrate_summary$Round[All_flowrate_summary$bh_id < 200] <- 1
All_flowrate_summary$Round[All_flowrate_summary$bh_id > 200] <- 2


All_pumpcap_summary$Round <- as.factor(All_pumpcap_summary$Round)
All_leakrate_summary$Round <- as.factor(All_leakrate_summary$Round)
All_flowrate_summary$Round <- as.factor(All_flowrate_summary$Round)

All_pumpcap_summary
All_leakrate_summary
All_flowrate_summary


## plot test results

All_pumpcap_plot <- ggplot(All_pumpcap_summary, aes(x=rank,y=Mean,color=Round))     
All_pumpcap_plot <- All_pumpcap_plot + 
  geom_point(size = 1.75,show.legend = FALSE) +
  geom_errorbar(aes(ymin = errorbar_min,ymax = errorbar_max,color=Round),show.legend = FALSE,size=0.5) +
  scale_color_manual(values = c("dodgerblue","royalblue4")) +
  geom_hline(yintercept=200, linetype="dashed", color = "black") +
  ylab(expression(Pump~Capacity~(cm^3/stroke))) + xlab(" ") +
  ggtitle("(a)") + 
  theme(plot.title = element_text(hjust = 0.5,size = 16),
        axis.title = element_text(size = 13),
        axis.text = element_text(size = 11))


All_leakrate_plot <- ggplot(All_leakrate_summary, aes(x=rank,y=Mean,color=Round))     
All_leakrate_plot <- All_leakrate_plot + 
  geom_point(size = 1.75,show.legend = FALSE) +
  geom_errorbar(aes(ymin = errorbar_min,ymax = errorbar_max,color=Round),show.legend = FALSE,size = 0.5) +
  scale_color_manual(values = c("dodgerblue","royalblue4")) +
  geom_hline(yintercept=200, linetype="dashed", color = "black") +
  geom_hline(yintercept=60, linetype="dashed", color = "black") +
  xlab("Rank") + ylab(expression(Leakage~Rate[10]~(cm^3/min))) +
  ggtitle("(b)") + 
  theme(plot.title = element_text(hjust = 0.5,size = 16),
        axis.title = element_text(size = 13),
        axis.text = element_text(size = 11))

All_flowrate_plot <- ggplot(All_flowrate_summary, aes(x=rank,y=Mean,color=Round))     
All_flowrate_plot <- All_flowrate_plot + 
  geom_point(size = 1.75) +
  geom_errorbar(aes(ymin = errorbar_min,ymax = errorbar_max,color=Round), size = 0.5) +
  scale_color_manual(values = c("dodgerblue","royalblue4")) +
  ylab(expression(Flow~Rate~(m^3/hour)))  + xlab(" ") +
  ggtitle("(c)") +
  theme(plot.title = element_text(hjust = 0.5,size = 16),
        axis.title = element_text(size = 13),
        axis.text = element_text(size = 11),
        legend.text = element_text(size = 11),
        legend.title = element_text(size = 13))

Figure2 <- All_pumpcap_plot + All_leakrate_plot + All_flowrate_plot + plot_layout(guides = "collect")
Figure2


######################################################
# Supplemental Analysis - reliability of proposed classification system

## initialize data frames 
# Round 1 inter-rater (blank numeric)
data_Round1_inter_4_newclass <- data_Round1_inter_flowrate_4*0 
data_Round1_inter_3_newclass <- data_Round1_inter_flowrate_3*0 

# Round 2 inter-rater (blank numeric)
data_Round2_inter_4_newclass <- data_Round2_inter_flowrate_4*0 
data_Round2_inter_5_newclass <- data_Round2_inter_flowrate_5*0 

## # of observations in dataframes
ncol(data_Round1_inter_4_newclass)*nrow(data_Round1_inter_4_newclass) # 72
ncol(data_Round1_inter_3_newclass)*nrow(data_Round1_inter_3_newclass) # 78
ncol(data_Round2_inter_4_newclass)*nrow(data_Round2_inter_4_newclass) # 124
ncol(data_Round2_inter_5_newclass)*nrow(data_Round2_inter_5_newclass) # 120

## classify 
# full function (1): leak rate < 60 & pump cap >= 200
data_Round1_inter_4_newclass[data_Round1_inter_leakrate_4 < 60 & data_Round1_inter_pumpcap_4 >= 200] <- 1  
data_Round1_inter_3_newclass[data_Round1_inter_leakrate_3 < 60 & data_Round1_inter_pumpcap_3 >= 200] <- 1 
data_Round2_inter_4_newclass[data_Round2_inter_leakrate_4 < 60 & data_Round2_inter_pumpcap_4 >= 200] <- 1  
data_Round2_inter_5_newclass[data_Round2_inter_leakrate_5 < 60 & data_Round2_inter_pumpcap_5 >= 200] <- 1  

# moderate (2): 60 =< leak rate < 200 & pump cap >= 200
data_Round1_inter_4_newclass[data_Round1_inter_leakrate_4 >= 60 & data_Round1_inter_leakrate_4 < 200 & data_Round1_inter_pumpcap_4 >= 200] <- 2  
data_Round1_inter_3_newclass[data_Round1_inter_leakrate_3 >= 60 & data_Round1_inter_leakrate_3 < 200 & data_Round1_inter_pumpcap_3 >= 200] <- 2  
data_Round2_inter_4_newclass[data_Round2_inter_leakrate_4 >= 60 & data_Round2_inter_leakrate_4 < 200 & data_Round2_inter_pumpcap_4 >= 200] <- 2  
data_Round2_inter_5_newclass[data_Round2_inter_leakrate_5 >= 60 & data_Round2_inter_leakrate_5 < 200 & data_Round2_inter_pumpcap_5 >= 200] <- 2  

# limited (3): leak rate >= 200 or pump cap < 200
data_Round1_inter_4_newclass[data_Round1_inter_leakrate_4 >= 200 | data_Round1_inter_pumpcap_4 < 200] <- 3 
data_Round1_inter_3_newclass[data_Round1_inter_leakrate_3 >= 200 | data_Round1_inter_pumpcap_3 < 200] <- 3 
data_Round2_inter_4_newclass[data_Round2_inter_leakrate_4 >= 200 | data_Round2_inter_pumpcap_4 < 200] <- 3 
data_Round2_inter_5_newclass[data_Round2_inter_leakrate_5 >= 200 | data_Round2_inter_pumpcap_5 < 200] <- 3 

# check amount in each class

# full
sum(data_Round1_inter_4_newclass[data_Round1_inter_4_newclass == 1]) # 46/72
sum(data_Round1_inter_3_newclass[data_Round1_inter_3_newclass == 1]) # 47/78
sum(data_Round2_inter_4_newclass[data_Round2_inter_4_newclass == 1]) # 70/124
sum(data_Round2_inter_5_newclass[data_Round2_inter_5_newclass == 1]) # 78/120

# moderate
sum(data_Round1_inter_4_newclass[data_Round1_inter_4_newclass == 2])/2 # 16/72
sum(data_Round1_inter_3_newclass[data_Round1_inter_3_newclass == 2])/2 # 15/78
sum(data_Round2_inter_4_newclass[data_Round2_inter_4_newclass == 2])/2 # 19/124
sum(data_Round2_inter_5_newclass[data_Round2_inter_5_newclass == 2])/2 # 17/120

# limited
sum(data_Round1_inter_4_newclass[data_Round1_inter_4_newclass == 3])/3 # 10/72
sum(data_Round1_inter_3_newclass[data_Round1_inter_3_newclass == 3])/3 # 16/78
sum(data_Round2_inter_4_newclass[data_Round2_inter_4_newclass == 3])/3 # 35/124
sum(data_Round2_inter_5_newclass[data_Round2_inter_5_newclass == 3])/3 # 25/120

# double-check that all classified (should equal 1)
(sum(data_Round1_inter_4_newclass[data_Round1_inter_4_newclass == 1]) + 
  sum(data_Round1_inter_4_newclass[data_Round1_inter_4_newclass == 2])/2 +
    sum(data_Round1_inter_4_newclass[data_Round1_inter_4_newclass == 3])/3) / (ncol(data_Round1_inter_4_newclass)*nrow(data_Round1_inter_4_newclass))

(sum(data_Round1_inter_3_newclass[data_Round1_inter_3_newclass == 1]) + 
    sum(data_Round1_inter_3_newclass[data_Round1_inter_3_newclass == 2])/2 +
    sum(data_Round1_inter_3_newclass[data_Round1_inter_3_newclass == 3])/3) / (ncol(data_Round1_inter_3_newclass)*nrow(data_Round1_inter_3_newclass))

(sum(data_Round2_inter_4_newclass[data_Round2_inter_4_newclass == 1]) + 
    sum(data_Round2_inter_4_newclass[data_Round2_inter_4_newclass == 2])/2 +
    sum(data_Round2_inter_4_newclass[data_Round2_inter_4_newclass == 3])/3) / (ncol(data_Round2_inter_4_newclass)*nrow(data_Round2_inter_4_newclass))

(sum(data_Round2_inter_5_newclass[data_Round2_inter_5_newclass == 1]) + 
    sum(data_Round2_inter_5_newclass[data_Round2_inter_5_newclass == 2])/2 +
    sum(data_Round2_inter_5_newclass[data_Round2_inter_5_newclass == 3])/3) / (ncol(data_Round2_inter_5_newclass)*nrow(data_Round2_inter_5_newclass))


## Fleiss/light's kappa calculations #####
# Round 1 inter-rater (Light's kappa - unique raters)
kappa_Round1_inter_4_newclass <-  KappaM(data_Round1_inter_4_newclass,method = "Light",conf.level = 0.95)
kappa_Round1_inter_4_newclass # k = 0.93 (-0.30,1.00) - upper 95% is higher but maximum kappa is 1.0
# for p value
kappam.light(data_Round1_inter_4_newclass) # p = 0.14


kappa_Round1_inter_3_newclass <-  KappaM(data_Round1_inter_3_newclass,method = "Light",conf.level = 0.95)
kappa_Round1_inter_3_newclass # k = 0.86 (0.49,1.00) - upper 95% is higher but maximum kappa is 1.0
# for p value
kappam.light(data_Round1_inter_3_newclass)  # p < 0.001


# Round 2 inter-rater
kappa_Round2_inter_4_newclass <-  KappaM(data_Round2_inter_4_newclass,method = "Fleiss",conf.level = 0.95)
kappa_Round2_inter_4_newclass # k = 0.89 (0.78,1.0)
# for p value
kappam.fleiss(data_Round2_inter_4_newclass) # p = 0

kappa_Round2_inter_5_newclass <-  KappaM(data_Round2_inter_5_newclass,method = "Fleiss",conf.level = 0.95)
kappa_Round2_inter_5_newclass # k = 0.90 (0.81,1.0)
# for p value
kappam.fleiss(data_Round2_inter_5_newclass) # p = 0








