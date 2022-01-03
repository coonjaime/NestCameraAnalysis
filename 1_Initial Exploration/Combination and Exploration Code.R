####Code Contributors
  #Jaime Coon, coonja@earlham.edu
  #
  #

#Exploration of Nest Camera Data collected in
  #2015, 2016, and 2021
#Species:
  #Dickcissel (DICK)
  #Red-winged blackbird (RWBL)
  #Bobolink (BOBO)
  #Grasshopper sparrow (GRSP)
  #etc
#_____________________________________________________####
####1. SETUP & IMPORT ####

#Jaime's computer setup
setwd("~/NestCameraAnalysis/1_Initial Exploration")
#feel free to add your own setup if using this code

#__1a. PACKAGES                                       ####
#install.packages('easypackages')#do once to manage packages
library('easypackages')#load package managing package

packages('tidyverse','ggplot2','glmmTMB','readxl','janitor','lubridate')

#__1b. IMPORTING SEPARATE DATASETS                     ####

JJC_NB <- read_excel("Initial Data/JJC_Nestling Behavior_1.2.22.xlsx")
JJC_PB <- read_excel("Initial Data/JJC_Parent Behavior_1.2.22.xlsx")
JJC_VM <- read_excel("Initial Data/JJC_Video_Master_1.2.22.xlsx")

JNA_NB <- read_excel("Initial Data/JNA_Nestling Behavior_1.2.22.xlsx")
JNA_PB <- read_excel("Initial Data/JNA_Parent Behavior_1.2.22.xlsx")
JNA_VM <- read_excel("Initial Data/JNA_Video_Master_1.2.22.xlsx")

MFM_NB <- read_excel("Initial Data/MFM_Nestling Behavior_1.2.22.xlsx")
MFM_PB <- read_excel("Initial Data/MFM_Parent Behavior_1.2.22.xlsx")
MFM_VM <- read_excel("Initial Data/MFM_Video_Master_1.2.22.xlsx")

#__1c. COMBINING DATASETS                             ####

#combine and add key codes for each person for 'Behavior ID'
#Wendy:001
#Jaime:002
#Josh:003
#Thea:004
#Hannah:005
#Ethan:006
#Molly:007
#Claudette:008
#filter out 2015 and 2016 from everyone but Jaime's

#JAIME
JJC_VM_cleaned=JJC_VM%>%
  clean_names(case = "upper_camel", abbreviations = c("ID"))%>%
  add_column("CoderID"="002")%>%
  rename(FilmStartTime=StartTime,
         FilmEndTime=EndTime,
         SessionY=Session)%>%
  mutate(output=format(as.Date(Date),format="%Y-%m-%d"))%>%
  mutate("Year"=(year(Date)))

JJC_NB_cleaned=JJC_NB%>%
  clean_names(case = "upper_camel", abbreviations = c("ID"))%>%
  add_column("CoderID"="003")%>%
  unite("BehaviorID",c("BehaviorID","CoderID"),remove=TRUE,sep="_")%>%
  left_join(JJC_VM_cleaned,select(
    Year, NestVisablity,FilmStartTime,FilmEndTime),
    by="NestIDSession")

JJC_PB_cleaned=JJC_PB%>%
  clean_names(case = "upper_camel", abbreviations = c("ID"))%>%
  add_column("CoderID"="003")%>%
  unite("BehaviorID",c("BehaviorID","CoderID"),remove=TRUE,sep="_")%>%
  left_join(JJC_VM_cleaned,select(
    Year, NestVisablity,FilmStartTime,FilmEndTime),
    by="NestIDSession")

#JOSH
JNA_VM_cleaned=JNA_VM%>%
  clean_names(case = "upper_camel", abbreviations = c("ID"))%>%
  add_column("CoderID"="003")%>%
  rename(FilmStartTime=StartTime,
         FilmEndTime=EndTime,
         SessionY=Session)%>%
  mutate(output=format(as.Date(Date),format="%Y-%m-%d"))%>%
  mutate("Year"=(year(Date)))%>%
  filter(!grepl('2015|2016', Year))

JNA_NB_cleaned=JNA_NB%>%
  clean_names(case = "upper_camel", abbreviations = c("ID"))%>%
  add_column("CoderID"="002")%>%
  unite("BehaviorID",c("BehaviorID","CoderID"),remove=TRUE,sep="_")%>%
  left_join(JNA_VM_cleaned,select(
    Year, NestVisablity,FilmStartTime,FilmEndTime),
    by="NestIDSession")%>%
  filter(!grepl('2015|2016', Year))

JNA_PB_cleaned=JNA_PB%>%
  clean_names(case = "upper_camel", abbreviations = c("ID"))%>%
  add_column("CoderID"="002")%>%
  unite("BehaviorID",c("BehaviorID","CoderID"),remove=TRUE,sep="_")%>%
  left_join(JNA_VM_cleaned,select(
    Year, NestVisablity,FilmStartTime,FilmEndTime),
    by="NestIDSession")%>%
  filter(!grepl('2015|2016', Year))

#MOLLY
MFM_VM_cleaned=MFM_VM%>%
  clean_names(case = "upper_camel", abbreviations = c("ID"))%>%
  add_column("CoderID"="003")%>%
  rename(FilmStartTime=StartTime,
         FilmEndTime=EndTime,
         SessionY=Session)%>%
  mutate(output=format(as.Date(Date),format="%Y-%m-%d"))%>%
  mutate("Year"=(year(Date)))%>%
  filter(!grepl('2015|2016', Year))

MFM_NB_cleaned=MFM_NB%>%
  clean_names(case = "upper_camel", abbreviations = c("ID"))%>%
  add_column("CoderID"="002")%>%
  unite("BehaviorID",c("BehaviorID","CoderID"),remove=TRUE,sep="_")%>%
  left_join(MFM_VM_cleaned,select(
    Year, NestVisablity,FilmStartTime,FilmEndTime),
    by="NestIDSession")%>%
  filter(!grepl('2015|2016', Year))

MFM_PB_cleaned=MFM_PB%>%
  clean_names(case = "upper_camel", abbreviations = c("ID"))%>%
  add_column("CoderID"="002")%>%
  unite("BehaviorID",c("BehaviorID","CoderID"),remove=TRUE,sep="_")%>%
  left_join(MFM_VM_cleaned,select(
    Year, NestVisablity,FilmStartTime,FilmEndTime),
    by="NestIDSession")%>%
  filter(!grepl('2015|2016', Year))

compare_df_cols(JJC_NB_cleaned,JNA_NB_cleaned) #to compare before merging

NB_combined=rbind(JJC_NB_cleaned,JNA_NB_cleaned,MFM_NB_cleaned)
PB_combined=rbind(JJC_PB_cleaned,JNA_PB_cleaned,MFM_PB_cleaned)
VM_combined=rbind(JJC_VM_cleaned,JNA_VM_cleaned,MFM_VM_cleaned)

#__1d. DATA CLEANING                                  ####
VM=VM_combined%>%
  filter(!grepl('PTER', NestIDSession))%>%
  filter(!grepl('EJT', NestID))%>%
  filter(!grepl('RANC', NestID))%>%
  filter(!grepl('BAD DATA', NestIDSession))%>%
  filter(!grepl('KLT_RWBL_4_21(1)', NestIDSession))
  
NB=NB_combined%>%
  filter(!grepl('PTER', NestIDSession))%>%
  filter(!grepl('EJT', NestID))%>%
  filter(!grepl('RANC', NestID))%>%
  filter(!grepl('BAD DATA', NestIDSession))%>%
  filter(!grepl('KLT_RWBL_4_21(1)', NestIDSession))

PB=PB_combined%>%
  filter(!grepl('PTER', NestIDSession))%>%
  filter(!grepl('EJT', NestID))%>%
  filter(!grepl('RANC', NestID))%>%
  filter(!grepl('BAD DATA', NestIDSession))%>%
  filter(!grepl('KLT_RWBL_4_21(1)', NestIDSession))

#_____________________________________________________####
####2. Calculate Durations and Rates ####



#_____________________________________________________####
####3. Summary Stats ####

#_____________________________________________________####
####4. Prelim analysis ####
