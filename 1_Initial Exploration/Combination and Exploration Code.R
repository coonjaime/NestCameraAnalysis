####Code Contributors
  #Jaime Coon, coonja@earlham.edu
  #
  #

#Code updated at: https://github.com/coonjaime/NestCameraAnalysis

#Exploration of Nest Camera Data collected in
  #2015, 2016, and 2021
#Species:
  #Dickcissel (DICK)
  #Red-winged blackbird (RWBL)
  #Bobolink (BOBO)
  #Grasshopper sparrow (GRSP)
  #etc
#Other Acronyms
  #PB = Parent Behavior (from Access file)
  #NB = Nestling Behavior (from Access file)
  #VM = Video Metadata (from Access file)
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

#Nestling numbers for updated nests as of 1.2.22
NestlingNums <- read_excel("Initial Data/NestlingNums21_1.2.22.xlsx")


#__1c. COMBINING DATASETS                             ####

#combine and add key codes for each person for 'Behavior ID'
#Wendy:001 #Jaime:002 #Josh:003 #Thea:004
#Hannah:005 #Ethan:006 #Molly:007 #Claudette:008

#need to filter out 2015 and 2016 from everyone but Jaime's

#WENDY

#JAIME
JJC_VM_cleaned=JJC_VM%>%
  clean_names(case = "upper_camel", abbreviations = c("ID","BHCO"))%>%
  add_column("CoderID"="002")%>%
  rename(FilmStartTime=StartTime,
         FilmEndTime=EndTime,
         SessionY=Session)%>%
  mutate(Date=format(as.Date(Date),format="%Y-%m-%d"))%>%
  mutate("Year"=(year(Date)))%>%
  filter(!grepl('KLT RWBL 4 21 (1)', NestIDSession))%>% #duplicate 
  rename(NumHosts = Dickcissel,
         NumBHCO  = Cowbird)%>%
  left_join(NestlingNums,select(NumHosts,NumBHCO,TotalNestling),by="NestIDSession")%>%
  mutate(NumHosts.x = replace(NumHosts.x, NumHosts.x==0, NA))%>%
  mutate(NumHosts.y = replace(NumHosts.y, NumHosts.y==0, NA))%>%
  mutate(NumBHCO.x  = replace(NumBHCO.x,  NumBHCO.x ==0, NA))%>%
  mutate(NumBHCO.y  = replace(NumBHCO.y,  NumBHCO.y==0, NA))%>%
  mutate(TotalNestling.x = replace(TotalNestling.x, TotalNestling.x==0, NA))%>%
  mutate(TotalNestling.y = replace(TotalNestling.y, TotalNestling.y==0, NA))%>%
  mutate(NumHosts = coalesce(NumHosts.x, NumHosts.y))%>%
  mutate(NumBHCO  = coalesce(NumBHCO.x,  NumBHCO.y))%>%
  mutate(TotalNestling = coalesce(TotalNestling.x,TotalNestling.y))%>%
  select(-NumHosts.x,-NumHosts.y,-NumBHCO.x,-NumBHCO.y,-TotalNestling.x,-TotalNestling.y)%>%
  mutate_at(vars(NumHosts:TotalNestling), ~replace(., is.na(.), 0))
  
JJC_NB_cleaned=JJC_NB%>%
  clean_names(case = "upper_camel", abbreviations = c("ID","BHCO"))%>%
  add_column("CoderID"="003")%>%
  unite("BehaviorID",c("BehaviorID","CoderID"),remove=TRUE,sep="_")%>%
  left_join(JJC_VM_cleaned,select(
    Year, NestVisablity,FilmStartTime,FilmEndTime,NumHosts,NumBHCO,TotalNestling,FilmingDurationInSec),
    by="NestIDSession")%>%
  filter(!grepl('KLT RWBL 4 21 (1)', NestIDSession)) #duplicate

JJC_PB_cleaned=JJC_PB%>%
  clean_names(case = "upper_camel", abbreviations = c("ID","BHCO"))%>%
  add_column("CoderID"="003")%>%
  unite("BehaviorID",c("BehaviorID","CoderID"),remove=TRUE,sep="_")%>%
  left_join(JJC_VM_cleaned,select(
    Year, NestVisablity,FilmStartTime,FilmEndTime,NumHosts,NumBHCO,TotalNestling,FilmingDurationInSec),
    by="NestIDSession")%>%
  filter(!grepl('KLT RWBL 4 21 (1)', NestIDSession)) #duplicate

#JOSH
JNA_VM_cleaned=JNA_VM%>%
  clean_names(case = "upper_camel", abbreviations = c("ID","BHCO"))%>%
  add_column("CoderID"="003")%>%
  rename(FilmStartTime=StartTime,
         FilmEndTime=EndTime,
         SessionY=Session)%>%
  mutate(Date=format(as.Date(Date),format="%Y-%m-%d"))%>%
  mutate("Year"=(year(Date)))%>%
  filter(!grepl('2015|2016', Year))%>%
  rename(NumHosts = Dickcissel,
         NumBHCO  = Cowbird)%>%
  left_join(NestlingNums,select(NumHosts,NumBHCO,TotalNestling),by="NestIDSession")%>%
  mutate(NumHosts.x = replace(NumHosts.x, NumHosts.x==0, NA))%>%
  mutate(NumHosts.y = replace(NumHosts.y, NumHosts.y==0, NA))%>%
  mutate(NumBHCO.x  = replace(NumBHCO.x,  NumBHCO.x ==0, NA))%>%
  mutate(NumBHCO.y  = replace(NumBHCO.y,  NumBHCO.y==0, NA))%>%
  mutate(TotalNestling.x = replace(TotalNestling.x, TotalNestling.x==0, NA))%>%
  mutate(TotalNestling.y = replace(TotalNestling.y, TotalNestling.y==0, NA))%>%
  mutate(NumHosts = coalesce(NumHosts.x, NumHosts.y))%>%
  mutate(NumBHCO  = coalesce(NumBHCO.x,  NumBHCO.y))%>%
  mutate(TotalNestling = coalesce(TotalNestling.x,TotalNestling.y))%>%  
  select(-NumHosts.x,-NumHosts.y,-NumBHCO.x,-NumBHCO.y,-TotalNestling.x,-TotalNestling.y)%>%
  mutate_at(vars(NumHosts:TotalNestling), ~replace(., is.na(.), 0))

JNA_NB_cleaned=JNA_NB%>%
  clean_names(case = "upper_camel", abbreviations = c("ID","BHCO"))%>%
  add_column("CoderID"="002")%>%
  unite("BehaviorID",c("BehaviorID","CoderID"),remove=TRUE,sep="_")%>%
  left_join(JNA_VM_cleaned,select(
    Year, NestVisablity,FilmStartTime,FilmEndTime,NumHosts,NumBHCO,TotalNestling,FilmingDurationInSec),
    by="NestIDSession")%>%
  filter(!grepl('2015|2016', Year))

JNA_PB_cleaned=JNA_PB%>%
  clean_names(case = "upper_camel", abbreviations = c("ID","BHCO"))%>%
  add_column("CoderID"="002")%>%
  unite("BehaviorID",c("BehaviorID","CoderID"),remove=TRUE,sep="_")%>%
  left_join(JNA_VM_cleaned,select(
    Year, NestVisablity,FilmStartTime,FilmEndTime,NumHosts,NumBHCO,TotalNestling,FilmingDurationInSec),
    by="NestIDSession")%>%
  filter(!grepl('2015|2016', Year))

#MOLLY
MFM_VM_cleaned=MFM_VM%>%
  clean_names(case = "upper_camel", abbreviations = c("ID","BHCO"))%>%
  add_column("CoderID"="003")%>%
  rename(FilmStartTime=StartTime,
         FilmEndTime=EndTime,
         SessionY=Session)%>%
  mutate(Date=format(as.Date(Date),format="%Y-%m-%d"))%>%
  mutate("Year"=(year(Date)))%>%
  filter(!grepl('2015|2016', Year))%>%
  rename(NumHosts = Dickcissel,
         NumBHCO  = Cowbird)%>%
  left_join(NestlingNums,select(NumHosts,NumBHCO,TotalNestling),by="NestIDSession")%>%
  mutate(NumHosts.x = replace(NumHosts.x, NumHosts.x==0, NA))%>%
  mutate(NumHosts.y = replace(NumHosts.y, NumHosts.y==0, NA))%>%
  mutate(NumBHCO.x  = replace(NumBHCO.x,  NumBHCO.x ==0, NA))%>%
  mutate(NumBHCO.y  = replace(NumBHCO.y,  NumBHCO.y==0, NA))%>%
  mutate(TotalNestling.x = replace(TotalNestling.x, TotalNestling.x==0, NA))%>%
  mutate(TotalNestling.y = replace(TotalNestling.y, TotalNestling.y==0, NA))%>%
  mutate(NumHosts = coalesce(NumHosts.x, NumHosts.y))%>%
  mutate(NumBHCO  = coalesce(NumBHCO.x,  NumBHCO.y))%>%
  mutate(TotalNestling = coalesce(TotalNestling.x,TotalNestling.y))%>%  
  select(-NumHosts.x,-NumHosts.y,-NumBHCO.x,-NumBHCO.y,-TotalNestling.x,-TotalNestling.y)%>%
  mutate_at(vars(NumHosts:TotalNestling), ~replace(., is.na(.), 0))

MFM_NB_cleaned=MFM_NB%>%
  clean_names(case = "upper_camel", abbreviations = c("ID","BHCO"))%>%
  add_column("CoderID"="002")%>%
  unite("BehaviorID",c("BehaviorID","CoderID"),remove=TRUE,sep="_")%>%
  left_join(MFM_VM_cleaned,select(
    Year, NestVisablity,FilmStartTime,FilmEndTime,NumHosts,NumBHCO,TotalNestling,FilmingDurationInSec),
    by="NestIDSession")%>%
  filter(!grepl('2015|2016', Year))

MFM_PB_cleaned=MFM_PB%>%
  clean_names(case = "upper_camel", abbreviations = c("ID","BHCO"))%>%
  add_column("CoderID"="002")%>%
  unite("BehaviorID",c("BehaviorID","CoderID"),remove=TRUE,sep="_")%>%
  left_join(MFM_VM_cleaned,select(
    Year, NestVisablity,FilmStartTime,FilmEndTime,NumHosts,NumBHCO,TotalNestling,FilmingDurationInSec),
    by="NestIDSession")%>%
  filter(!grepl('2015|2016', Year))

compare_df_cols(JJC_PB_cleaned,JNA_PB_cleaned) #to compare before merging

NB_combined=rbind(JJC_NB_cleaned,JNA_NB_cleaned,MFM_NB_cleaned)
PB_combined=rbind(JJC_PB_cleaned,JNA_PB_cleaned,MFM_PB_cleaned)
VM_combined=rbind(JJC_VM_cleaned,JNA_VM_cleaned,MFM_VM_cleaned)

#__1d. DATA CLEANING                                  ####
VM=VM_combined%>%
  filter(!grepl('PTER', NestIDSession))%>%
  filter(!grepl('EJT', NestID))%>%
  filter(!grepl('RANC', NestID))%>%
  filter(!grepl('BAD DATA', NestIDSession))%>%
  mutate(NestIDSession=recode(NestIDSession,'KLN_DICK_1_21_1'="KLN_DICK_1_15_1"))%>% #fixing JJC mistype
  mutate(NestIDn=recode(NestID,'KLN_DICK_1_21'="KLN_DICK_1_15"))%>% #fixing JJC mistype
  
  #filtering out incomplete nests as of 1.2.22:       
  filter(!grepl(' 235_DICK_21_21_1', NestIDSession))%>%
  filter(!grepl(' KLT_EAME_1_21_1', NestIDSession))%>%
  filter(!grepl(' KLT_EAME_2_21_1', NestIDSession))%>%
  filter(!grepl(' 235_GRSP_2_21_1', NestIDSession))%>%
  filter(!grepl(' KLT_RWBL_6_21_1', NestIDSession))%>%
  filter(!grepl(' RIE_RWBL_11_21_2', NestIDSession))%>%
  filter(!grepl(' KLT_RWBL_23_21_1', NestIDSession))%>%
  filter(!grepl(' RIE_RWBL_26_21_1', NestIDSession))%>%
  
  mutate(NestIDSession=str_replace(NestIDSession,"[(]",""))%>%
  mutate(NestIDSession=str_replace(NestIDSession,"[)]",""))%>%
  mutate(NestIDSession=str_replace_all(NestIDSession,"[ ]","_"))%>%
  mutate(NestID=str_replace_all(NestID,"[ ]","_"))%>%
  mutate(NestVisability=recode(NestVisability, 
                               '100% - 85%'="85-100",
                               '49% - 0%'="0-49",
                               '84% - 50%'="50-84"))%>%
  mutate(FilmStartTime=format(ymd_hms(FilmStartTime),'%H:%M'))%>%
  mutate(FilmEndTime=format(ymd_hms(FilmEndTime),'%H:%M'))%>%
  mutate(OrdDate=yday(Date))%>%
  mutate_at(vars(Species), ~replace_na(., 'DICK'))


NB=NB_combined%>%
  filter(!grepl('0', VideoClip))%>%
  filter(!grepl('Unknown', BehaviorCode))%>%
  filter(!grepl('PTER', NestIDSession))%>%
  filter(!grepl('EJT', NestID))%>%
  filter(!grepl('RANC', NestID))%>%
  filter(!grepl('BAD DATA', NestIDSession))%>%
  mutate(NestIDSession=recode(NestIDSession,'KLN_DICK_1_21_1'="KLN_DICK_1_15_1"))%>% #fixing JJC mistype
  mutate(NestIDn=recode(NestID,'KLN_DICK_1_21'="KLN_DICK_1_15"))%>% #fixing JJC mistype
  
  #filtering out incomplete nests as of 1.2.22:       
  filter(!grepl(' 235_DICK_21_21_1', NestIDSession))%>%
  filter(!grepl(' KLT_EAME_1_21_1', NestIDSession))%>%
  filter(!grepl(' KLT_EAME_2_21_1', NestIDSession))%>%
  filter(!grepl(' 235_GRSP_2_21_1', NestIDSession))%>%
  filter(!grepl(' KLT_RWBL_6_21_1', NestIDSession))%>%
  filter(!grepl(' RIE_RWBL_11_21_2', NestIDSession))%>%
  filter(!grepl(' KLT_RWBL_23_21_1', NestIDSession))%>%
  filter(!grepl(' RIE_RWBL_26_21_1', NestIDSession))%>%
  
  mutate(NestIDSession=str_replace(NestIDSession,"[(]",""))%>%
  mutate(NestIDSession=str_replace(NestIDSession,"[)]",""))%>%
  mutate(NestIDSession=str_replace_all(NestIDSession,"[ ]","_"))%>%
  mutate(NestID=str_replace_all(NestID,"[ ]","_"))%>%
  mutate(NestVisability=recode(NestVisability, 
                               '100% - 85%'="85-100",
                               '49% - 0%'="0-49",
                               '84% - 50%'="50-84"))%>%
  mutate(FilmStartTime=format(ymd_hms(FilmStartTime),'%H:%M'))%>%
  mutate(FilmEndTime=format(ymd_hms(FilmEndTime),'%H:%M'))%>%
  mutate(OrdDate=yday(Date))%>%
  mutate_at(vars(Species), ~replace_na(., 'DICK'))

PB=PB_combined%>%
  filter(!grepl('0', VideoClip))%>%
  filter(!grepl('Unknown', BehaviorCode))%>%
  filter(!grepl('PTER', NestIDSession))%>%
  filter(!grepl('EJT', NestID))%>%
  filter(!grepl('RANC', NestID))%>%
  filter(!grepl('BAD DATA', NestIDSession))%>%
  mutate(NestIDSession=recode(NestIDSession,'KLN_DICK_1_21_1'="KLN_DICK_1_15_1"))%>% #fixing JJC mistype
  mutate(NestIDn=recode(NestID,'KLN_DICK_1_21'="KLN_DICK_1_15"))%>% #fixing JJC mistype
  
  #filtering out incomplete nests as of 1.2.22:       
  filter(!grepl(' 235_DICK_21_21_1', NestIDSession))%>%
  filter(!grepl(' KLT_EAME_1_21_1', NestIDSession))%>%
  filter(!grepl(' KLT_EAME_2_21_1', NestIDSession))%>%
  filter(!grepl(' 235_GRSP_2_21_1', NestIDSession))%>%
  filter(!grepl(' KLT_RWBL_6_21_1', NestIDSession))%>%
  filter(!grepl(' RIE_RWBL_11_21_2', NestIDSession))%>%
  filter(!grepl(' KLT_RWBL_23_21_1', NestIDSession))%>%
  filter(!grepl(' RIE_RWBL_26_21_1', NestIDSession))%>%
  
  mutate(NestIDSession=str_replace(NestIDSession,"[(]",""))%>%
  mutate(NestIDSession=str_replace(NestIDSession,"[)]",""))%>%
  mutate(NestIDSession=str_replace_all(NestIDSession,"[ ]","_"))%>%
  mutate(NestID=str_replace_all(NestID,"[ ]","_"))%>%
  mutate(NestVisability=recode(NestVisability, 
                               '100% - 85%'="85-100",
                               '49% - 0%'="0-49",
                               '84% - 50%'="50-84"))%>%
  mutate(FilmStartTime=format(ymd_hms(FilmStartTime),'%H:%M'))%>%
  mutate(FilmEndTime=format(ymd_hms(FilmEndTime),'%H:%M'))%>%
  mutate(OrdDate=yday(Date))%>%
  mutate_at(vars(Species), ~replace_na(., 'DICK'))

#_____________________________________________________####
####2. Calculate Durations and Rates ####

#Parent behavior rates

#calculating behavior rates per hour by the session
PB_sum_bySession=PB%>%
  #filter(!grepl('TRUE', AbleSeeDipping))%>% 
  #filter(!grepl('0-49', NestVisability))%>% 
  group_by(Species,NestIDSession,NestID,SessionY,BehaviorCode,FilmingDurationInSec)%>%
  summarize(BehaviorCount = n())%>%
  mutate(Beh_per_h=BehaviorCount/(FilmingDurationInSec/(60*60)))

##calculating behavior rates per hour by the clip
PB_sum_byClip=PB%>%
  #filter(!grepl('TRUE', AbleSeeDipping))%>% 
  #filter(!grepl('0-49', NestVisability))%>% 
  group_by(Species,VideoClip,NestIDSession,NestID,SessionY,BehaviorCode,FilmingDurationInSec)%>%
  summarize(BehaviorCount = n())%>%
  mutate(Beh_per_h=BehaviorCount/(FilmingDurationInSec/(60*60)))

#calculating the percent of visible feeding attempts had dips by session
PB_dips_bySession=PB%>%
  filter(!grepl(FALSE, AbleSeeDipping),na.rm=TRUE)%>%
  filter(grepl("Provisioning", BehaviorCode),na.rm=TRUE)%>% 
  #filter(!grepl('0-49', NestVisability))%>% 
  group_by(Species,NestIDSession,NestID,SessionY,FilmingDurationInSec)%>%
  summarize(
    nDips=n(),
    sumDips = sum(DippingPresent))%>%
  mutate(PercentDips=sumDips/nDips)

#calculating the percent of visible feeding attempts had dips by species
PB_dips_bySpecies=PB%>%
  filter(!grepl(FALSE, AbleSeeDipping),na.rm=TRUE)%>%
  filter(grepl("Provisioning", BehaviorCode),na.rm=TRUE)%>% 
  #filter(!grepl('0-49', NestVisability))%>% 
  group_by(Species,NestIDSession,NestID,SessionY,FilmingDurationInSec)%>%
  summarize(
    nDips=n(),
    sumDips = sum(DippingPresent))%>%
  mutate(PercentDips=sumDips/nDips)%>%
  group_by(Species)%>%
  summarize(
    nSessions=n(),
    mean=mean(PercentDips))

#_____________________________________________________####
####3. Prelim analysis ####

