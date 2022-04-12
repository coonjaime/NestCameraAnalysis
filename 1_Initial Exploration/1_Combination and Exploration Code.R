####Code Contributors
  #Jaime Coon, coonja@earlham.edu
  #Joshua Angell
  #Ethan King

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
####1. SETUP  ####

#Ethan wd
setwd("~/Desktop/GRGCoding")

#Josh
setwd("~/RStudio/NestCameraAnalysis/1_Initial Exploration")

#RStudio Cloud setup
setwd("/cloud/project/1_Initial Exploration")

#Jaime comp wd
setwd("~/Dropbox/_Manuscripts/Dipping/NestCameraAnalysis/1_Initial Exploration")

#__1a. PACKAGES                                       ####
#install.packages('easypackages')#do once to manage packages
library('easypackages')#load package managing package

packages('TMB','tidyverse','ggplot2','glmmTMB','readxl','janitor','lubridate')

#_____________________________________________________####
#2. IMPORTING SEPARATE DATASETS                     ####

veg<-read_csv("Initial Data/NestVeg_2.Mar.2022.csv")%>%  #Check about NAs in dataset
  clean_names(case = "upper_camel", abbreviations = c("ID"))%>%
  mutate(NestID=str_replace_all(NestID,"[ ]","_"))%>%
  filter(Year>2014)%>%
  filter(!(NestID=="RIS_10_10"))%>%
  group_by(NestID)%>%
  mutate(LitDepth=(Litdepth+Litdepth2+Litdepth3)/3)%>%
  mutate(Robel=(Robel1+Robel2+Robel3+Robel4)/4)%>%
  summarize_at(vars(Fear, Csg, Wsg, Forb,Legume,Covlit,Robel,LitDepth),mean)

WPT_NB <- read_excel("Initial Data/WPT_Nestling Behavior_4.9.22.xlsx")
WPT_PB <- read_excel("Initial Data/WPT_Parent Behavior_4.9.22.xlsx") 
WPT_VM <- read_excel("Initial Data/WPT_Video_Master_4.9.22.xlsx")

JJC_NB <- read_excel("Initial Data/JJC_Nestling Behavior_4.9.22.xlsx")
JJC_PB <- read_excel("Initial Data/JJC_Parent Behavior_4.9.22.xlsx")
JJC_VM <- read_excel("Initial Data/JJC_Video_Master_4.9.22.xlsx")

JNA_NB <- read_excel("Initial Data/JNA_Nestling Behavior_4.2.22.xlsx")
JNA_PB <- read_excel("Initial Data/JNA_Parent Behavior_4.2.22.xlsx")
JNA_VM <- read_excel("Initial Data/JNA_Video_Master_4.2.22.xlsx")

TEC_NB <- read_excel("Initial Data/TEC_Nestling Behavior_4.2.22.xlsx")
TEC_PB <- read_excel("Initial Data/TEC_Parent Behavior_4.2.22.xlsx")
TEC_VM <- read_excel("Initial Data/TEC_Video_Master_4.2.22.xlsx")

HKG_NB <- read_excel("Initial Data/HKG_Nestling Behavior_4.2.22.xlsx")
HKG_PB <- read_excel("Initial Data/HKG_Parent Behavior_4.2.22.xlsx")
HKG_VM <- read_excel("Initial Data/HKG_Video_Master_4.2.22.xlsx")

ESK_NB <- read_excel("Initial Data/ESK_Nestling Behavior_4.9.22.xlsx")
ESK_PB <- read_excel("Initial Data/ESK_Parent Behavior_4.9.22.xlsx")
ESK_VM <- read_excel("Initial Data/ESK_Video_Master_4.9.22.xlsx")

MFM_NB <- read_excel("Initial Data/MFM_Nestling Behavior_1.2.22.xlsx")
MFM_PB <- read_excel("Initial Data/MFM_Parent Behavior_1.2.22.xlsx")
MFM_VM <- read_excel("Initial Data/MFM_Video_Master_1.2.22.xlsx")

CER_NB <- read_excel("Initial Data/CER_Nestling Behavior_4.2.22.xlsx")
CER_PB <- read_excel("Initial Data/CER_Parent Behavior_4.2.22.xlsx")
CER_VM <- read_excel("Initial Data/CER_Video_Master_4.2.22.xlsx")

#Nestling numbers for updated nests as of 1.2.22 #These need to be updated
NestlingNums <- read_excel("Initial Data/NestlingNums21_1.2.22.xlsx")

#_____________________________________________________####
#3. CLEANING & COMBINING DATASETS                      ####

#combine and add key codes for each person for 'Behavior ID'
#Wendy:001 #Jaime:002 #Josh:003 #Thea:004
#Hannah:005 #Ethan:006 #Molly:007 #Claudette:008

#need to filter out 2015 and 2016 from everyone but Jaime's

#WENDY
WPT_VM_cleaned=WPT_VM%>%
  clean_names(case = "upper_camel", abbreviations = c("ID","BHCO"))%>%
  add_column("CoderID"="001")%>%
  rename(FilmStartTime=StartTime,
         FilmEndTime=EndTime,
         SessionY=Session)%>%
  mutate(Date=format(as.Date(Date),format="%Y-%m-%d"))%>%
  mutate("Year"=(year(Date)))%>%
  filter(!(Year<2021))%>% 
  mutate(Species=recode(Species,"RIE"="DICK"))%>% #fixing WPT mistype
  mutate(WhoIsScoring=recode(WhoIsScoring,"Wendy"="WPT"))%>% #fixing WPT mistype
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

WPT_NB_cleaned=WPT_NB%>%
  clean_names(case = "upper_camel", abbreviations = c("ID","BHCO"))%>%
  add_column("CoderID"="001")%>%
  unite("BehaviorID",c("BehaviorID","CoderID"),remove=TRUE,sep="_")%>%
  left_join(WPT_VM_cleaned,select(
    Year, NestVisablity,FilmStartTime,FilmEndTime,NumHosts,NumBHCO,TotalNestling),
    by="NestIDSession")%>%
  filter(!(Year<2021)) 

WPT_PB_cleaned=WPT_PB%>%
  clean_names(case = "upper_camel", abbreviations = c("ID","BHCO"))%>%
  add_column("CoderID"="001")%>%
  unite("BehaviorID",c("BehaviorID","CoderID"),remove=TRUE,sep="_")%>%
  left_join(WPT_VM_cleaned,select(
    Year, NestVisablity,FilmStartTime,FilmEndTime,NumHosts,NumBHCO,TotalNestling),
    by="NestIDSession")%>%
  filter(!(Year<2021))

#JAIME
JJC_VM_cleaned=JJC_VM%>%
  clean_names(case = "upper_camel", abbreviations = c("ID","BHCO"))%>%
  add_column("CoderID"="002")%>%
  rename(FilmStartTime=StartTime,
         FilmEndTime=EndTime,
         SessionY=Session)%>%
  mutate(Date=format(as.Date(Date),format="%Y-%m-%d"))%>%
  mutate("Year"=(year(Date)))%>%
  filter(!(Year==2016))%>%
  filter(!(Year==2017))%>% #One 2016 nest coded as 2017
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
  add_column("CoderID"="002")%>%
  unite("BehaviorID",c("BehaviorID","CoderID"),remove=TRUE,sep="_")%>%
  left_join(JJC_VM_cleaned,select(
    Year, NestVisablity,FilmStartTime,FilmEndTime,NumHosts,NumBHCO,TotalNestling,VideoClip),
    by="NestIDSession")%>%
  filter(!(Year==2016))%>%
  filter(!grepl('KLT RWBL 4 21 (1)', NestIDSession)) #duplicate

JJC_PB_cleaned=JJC_PB%>%
  clean_names(case = "upper_camel", abbreviations = c("ID","BHCO"))%>%
  add_column("CoderID"="002")%>%
  unite("BehaviorID",c("BehaviorID","CoderID"),remove=TRUE,sep="_")%>%
  left_join(JJC_VM_cleaned,select(
    Year, NestVisablity,FilmStartTime,FilmEndTime,NumHosts,NumBHCO,TotalNestling),
    by="NestIDSession")%>%
  filter(!(Year==2016))%>%
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
  filter(!(Year<2021))%>% 
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
  add_column("CoderID"="003")%>%
  unite("BehaviorID",c("BehaviorID","CoderID"),remove=TRUE,sep="_")%>%
  left_join(JNA_VM_cleaned,select(
    Year, NestVisablity,FilmStartTime,FilmEndTime,NumHosts,NumBHCO,TotalNestling),
    by="NestIDSession")%>%
  filter(!(Year<2021)) 
  
JNA_PB_cleaned=JNA_PB%>%
  clean_names(case = "upper_camel", abbreviations = c("ID","BHCO"))%>%
  add_column("CoderID"="003")%>%
  unite("BehaviorID",c("BehaviorID","CoderID"),remove=TRUE,sep="_")%>%
  left_join(JNA_VM_cleaned,select(
    Year, NestVisablity,FilmStartTime,FilmEndTime,NumHosts,NumBHCO,TotalNestling),
    by="NestIDSession")%>%
  filter(!(Year<2021))

#THEA
TEC_VM_cleaned=TEC_VM%>%
  clean_names(case = "upper_camel", abbreviations = c("ID","BHCO"))%>%
  add_column("CoderID"="004")%>%
  rename(FilmStartTime=StartTime,
         FilmEndTime=EndTime,
         SessionY=Session)%>%
  mutate(Date=format(as.Date(Date),format="%Y-%m-%d"))%>%
  mutate("Year"=(year(Date)))%>%
  filter(!(Year<2021))%>% 
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

TEC_NB_cleaned=TEC_NB%>%
  clean_names(case = "upper_camel", abbreviations = c("ID","BHCO"))%>%
  add_column("CoderID"="004")%>%
  unite("BehaviorID",c("BehaviorID","CoderID"),remove=TRUE,sep="_")%>%
  left_join(TEC_VM_cleaned,select(
    Year, NestVisablity,FilmStartTime,FilmEndTime,NumHosts,NumBHCO,TotalNestling),
    by="NestIDSession")%>%
  filter(!(Year<2021)) 

TEC_PB_cleaned=TEC_PB%>%
  clean_names(case = "upper_camel", abbreviations = c("ID","BHCO"))%>%
  add_column("CoderID"="004")%>%
  unite("BehaviorID",c("BehaviorID","CoderID"),remove=TRUE,sep="_")%>%
  left_join(TEC_VM_cleaned,select(
    Year, NestVisablity,FilmStartTime,FilmEndTime,NumHosts,NumBHCO,TotalNestling),
    by="NestIDSession")%>%
  filter(!(Year<2021))

#HANNAH
HKG_VM_cleaned=HKG_VM%>%
  clean_names(case = "upper_camel", abbreviations = c("ID","BHCO"))%>%
  add_column("CoderID"="005")%>%
  rename(FilmStartTime=StartTime,
         FilmEndTime=EndTime,
         SessionY=Session)%>%
  mutate(Date=format(as.Date(Date),format="%Y-%m-%d"))%>%
  mutate("Year"=(year(Date)))%>%
  filter(!(Year<2021))%>% 
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

HKG_NB_cleaned=HKG_NB%>%
  clean_names(case = "upper_camel", abbreviations = c("ID","BHCO"))%>%
  add_column("CoderID"="005")%>%
  unite("BehaviorID",c("BehaviorID","CoderID"),remove=TRUE,sep="_")%>%
  left_join(HKG_VM_cleaned,select(
    Year, NestVisablity,FilmStartTime,FilmEndTime,NumHosts,NumBHCO,TotalNestling),
    by="NestIDSession")%>%
  filter(!(Year<2021)) 

HKG_PB_cleaned=HKG_PB%>%
  clean_names(case = "upper_camel", abbreviations = c("ID","BHCO"))%>%
  add_column("CoderID"="005")%>%
  unite("BehaviorID",c("BehaviorID","CoderID"),remove=TRUE,sep="_")%>%
  left_join(HKG_VM_cleaned,select(
    Year, NestVisablity,FilmStartTime,FilmEndTime,NumHosts,NumBHCO,TotalNestling),
    by="NestIDSession")%>%
  filter(!(Year<2021))

#ETHAN
ESK_VM_cleaned=ESK_VM%>%
  clean_names(case = "upper_camel", abbreviations = c("ID","BHCO"))%>%
  add_column("CoderID"="006")%>%
  rename(FilmStartTime=StartTime,
         FilmEndTime=EndTime,
         SessionY=Session)%>%
  mutate(Date=format(as.Date(Date),format="%Y-%m-%d"))%>%
  mutate("Year"=(year(Date)))%>%
  filter(!(Year<2021))%>% 
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

ESK_NB_cleaned=ESK_NB%>%
  clean_names(case = "upper_camel", abbreviations = c("ID","BHCO"))%>%
  add_column("CoderID"="006")%>%
  unite("BehaviorID",c("BehaviorID","CoderID"),remove=TRUE,sep="_")%>%
  left_join(ESK_VM_cleaned,select(
    Year, NestVisablity,FilmStartTime,FilmEndTime,NumHosts,NumBHCO,TotalNestling),
    by="NestIDSession")%>%
  filter(!(Year<2021)) 

ESK_PB_cleaned=ESK_PB%>%
  clean_names(case = "upper_camel", abbreviations = c("ID","BHCO"))%>%
  add_column("CoderID"="006")%>%
  unite("BehaviorID",c("BehaviorID","CoderID"),remove=TRUE,sep="_")%>%
  left_join(ESK_VM_cleaned,select(
    Year, NestVisablity,FilmStartTime,FilmEndTime,NumHosts,NumBHCO,TotalNestling),
    by="NestIDSession")%>%
  filter(!(Year<2021))

#MOLLY
MFM_VM_cleaned=MFM_VM%>%
  clean_names(case = "upper_camel", abbreviations = c("ID","BHCO"))%>%
  add_column("CoderID"="007")%>%
  rename(FilmStartTime=StartTime,
         FilmEndTime=EndTime,
         SessionY=Session)%>%
  mutate(Date=format(as.Date(Date),format="%Y-%m-%d"))%>%
  mutate("Year"=(year(Date)))%>%
  filter(!(Year<2021))%>% 
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
  add_column("CoderID"="007")%>%
  unite("BehaviorID",c("BehaviorID","CoderID"),remove=TRUE,sep="_")%>%
  left_join(MFM_VM_cleaned,select(
    Year, NestVisablity,FilmStartTime,FilmEndTime,NumHosts,NumBHCO,TotalNestling,VideoClip),
    by="NestIDSession")%>%
  filter(!(Year<2021)) 
  
MFM_PB_cleaned=MFM_PB%>%
  clean_names(case = "upper_camel", abbreviations = c("ID","BHCO"))%>%
  add_column("CoderID"="007")%>%
  unite("BehaviorID",c("BehaviorID","CoderID"),remove=TRUE,sep="_")%>%
  left_join(MFM_VM_cleaned,select(
    Year, NestVisablity,FilmStartTime,FilmEndTime,NumHosts,NumBHCO,TotalNestling),
    by="NestIDSession")%>%
  filter(!(Year<2021)) 

#CLAUDETTE    #Is there data we need from CER's old access file?
CER_VM_cleaned=CER_VM%>%
  clean_names(case = "upper_camel", abbreviations = c("ID","BHCO"))%>%
  add_column("CoderID"="008")%>%
  rename(FilmStartTime=StartTime,
         FilmEndTime=EndTime,
         SessionY=Session)%>%
  mutate(Date=format(as.Date(Date),format="%Y-%m-%d"))%>%
  mutate("Year"=(year(Date)))%>%
  filter(!(Year<2021))%>%
  filter(!grepl("JNA",WhoIsScoring))%>%
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

CER_NB_cleaned=CER_NB%>%
  clean_names(case = "upper_camel", abbreviations = c("ID","BHCO"))%>%
  add_column("CoderID"="008")%>%
  unite("BehaviorID",c("BehaviorID","CoderID"),remove=TRUE,sep="_")%>%
  left_join(CER_VM_cleaned,select(
    Year, NestVisablity,FilmStartTime,FilmEndTime,NumHosts,NumBHCO,TotalNestling,VideoClip),
    by="NestIDSession")%>%
  filter(!(Year<2021)) 

CER_PB_cleaned=CER_PB%>%
  clean_names(case = "upper_camel", abbreviations = c("ID","BHCO"))%>%
  add_column("CoderID"="008")%>%
  unite("BehaviorID",c("BehaviorID","CoderID"),remove=TRUE,sep="_")%>%
  left_join(CER_VM_cleaned,select(
    Year, NestVisablity,FilmStartTime,FilmEndTime,NumHosts,NumBHCO,TotalNestling),
    by="NestIDSession")%>%
  filter(!(Year<2021)) 
  
compare_df_cols(JJC_PB_cleaned,JNA_PB_cleaned) #to compare before merging

NB_combined=rbind(WPT_NB_cleaned,JJC_NB_cleaned,JNA_NB_cleaned,TEC_NB_cleaned,HKG_NB_cleaned,ESK_NB_cleaned,MFM_NB_cleaned,CER_NB_cleaned)
PB_combined=rbind(WPT_PB_cleaned,JJC_PB_cleaned,JNA_PB_cleaned,TEC_PB_cleaned,HKG_PB_cleaned,ESK_PB_cleaned,MFM_PB_cleaned,CER_PB_cleaned)
VM_combined=rbind(WPT_VM_cleaned,JJC_VM_cleaned,JNA_VM_cleaned,TEC_VM_cleaned,HKG_VM_cleaned,ESK_VM_cleaned,MFM_VM_cleaned,CER_VM_cleaned)

#_____________________________________________________####
#4. DATA CLEANING OF WHOLE DATASET                    ####

VM=VM_combined%>%
  filter(!grepl('PTER', NestIDSession))%>%
  filter(!grepl('EJT', NestID))%>%
  filter(!grepl('RANC', NestID))%>%
  filter(!grepl('BAD DATA', NestIDSession))%>%
  mutate(NestIDSession=str_replace(NestIDSession,"[(]",""))%>%
  mutate(NestIDSession=str_replace(NestIDSession,"[)]",""))%>%
  mutate(NestIDSession=str_replace_all(NestIDSession,"[ ]","_"))%>%
  mutate(NestID=str_replace_all(NestID,"[ ]","_"))%>%
  mutate(NestVisability=recode(NestVisability, 
                               '100% - 85%'="85-100",
                               '49% - 0%'="0-49",
                               '84% - 50%'="50-84"))%>%
  mutate(NestIDSession=recode(NestIDSession,'KLN_DICK_1_21_1'="KLN_DICK_1_15_1"))%>% #fixing JJC mistype
  mutate(NestID=recode(NestID,'KLN_DICK_1_21'="KLN_DICK_1_15"))%>% #fixing JJC mistype
  add_column("Parasitized"=as.logical(.$NumBHCO))%>%
  
  #filtering out incomplete nests as of 4.9.22: #recheck which nests are incomplete once all datasets are ready
  filter(!(NestIDSession=='235_DICK_21_21_1'))%>%
  filter(!(NestIDSession=='KLT_EAME_2_21_1'))%>%
  filter(!(NestIDSession=='235_GRSP_2_21_1'))%>%
  filter(!(NestIDSession=='RIE_RWBL_11_21_2'))%>%
  filter(!(NestIDSession=='RIE_RWBL_7_21_2'))%>%
  
  mutate(FilmStartTime=format(ymd_hms(FilmStartTime),'%H:%M:%S'))%>%
  mutate(FilmEndTime=format(ymd_hms(FilmEndTime),'%H:%M:%S')) %>%
  mutate(FilmEnd=(hour(hms(FilmEndTime)))+(minute(hms(FilmEndTime))/60))%>%
  mutate(FilmStart=(hour(hms(FilmStartTime)))+(minute(hms(FilmStartTime))/60))%>%
  mutate(FilmDuration=FilmEnd-FilmStart)%>%
  filter(FilmDuration > .5)%>%
 # mutate(FilmDuration=
 #          format(hms(FilmEndTime)-hms(FilmStartTime)))%>%
  mutate(OrdDate=yday(Date))%>%
  mutate_at(vars(Species), ~replace_na(., 'DICK'))

summary(VM$FilmDuration)


NB=NB_combined%>%
  filter(!grepl('0', VideoClip))%>%
  filter(!grepl('Unknown', BehaviorCode))%>%
  filter(!grepl('PTER', NestIDSession))%>%
  filter(!grepl('EJT', NestID))%>%
  filter(!grepl('RANC', NestID))%>%
  filter(!grepl('BAD DATA', NestIDSession))%>%
  mutate(NestIDSession=recode(NestIDSession,'KLN_DICK_1_21_1'="KLN_DICK_1_15_1"))%>% #fixing JJC mistype
  mutate(NestID=recode(NestID,'KLN_DICK_1_21'="KLN_DICK_1_15"))%>% #fixing JJC mistype
  
  mutate(NestIDSession=str_replace(NestIDSession,"[(]",""))%>%
  mutate(NestIDSession=str_replace(NestIDSession,"[)]",""))%>%
  mutate(NestIDSession=str_replace_all(NestIDSession,"[ ]","_"))%>%
  mutate(NestID=str_replace_all(NestID,"[ ]","_"))%>%
  
  #filtering out incomplete nests as of 4.9.22:  
  
  filter(!(NestIDSession=='235_DICK_21_21_1'))%>%
  filter(!(NestIDSession=='KLT_EAME_2_21_1'))%>%
  filter(!(NestIDSession=='235_GRSP_2_21_1'))%>%
  filter(!(NestIDSession=='RIE_RWBL_11_21_2'))%>%
  filter(!(NestIDSession=='RIE_RWBL_7_21_2'))%>%
  
  mutate(NestVisability=recode(NestVisability, 
                               '100% - 85%'="85-100",
                               '49% - 0%'="0-49",
                               '84% - 50%'="50-84"))%>%
  mutate(OrdDate=yday(Date))%>%
  mutate(BStart=StartMinute*60+ StartSecond)%>%
  mutate(BEnd  =  EndMinute*60+ EndSecond)%>%
  mutate(Duration_Sec=BEnd-BStart)%>%
  mutate_at(vars(Species), ~replace_na(., 'DICK'))

names(PB_combined)

PB=PB_combined%>%
  filter(!grepl('0', VideoClip))%>%
  filter(!grepl('Unknown', BehaviorCode))%>%
  filter(!grepl('PTER', NestIDSession))%>%
  filter(!grepl('EJT', NestID))%>%
  filter(!grepl('RANC', NestID))%>%
  filter(!grepl('BAD DATA', NestIDSession))%>%
  mutate(NestIDSession=recode(NestIDSession,'KLN_DICK_1_21_1'="KLN_DICK_1_15_1"))%>% #fixing JJC mistype
  mutate(NestIDn=recode(NestID,'KLN_DICK_1_21'="KLN_DICK_1_15"))%>% #fixing JJC mistype
  mutate(NestIDSession=str_replace(NestIDSession,"[(]",""))%>%
  mutate(NestIDSession=str_replace(NestIDSession,"[)]",""))%>%
  mutate(NestIDSession=str_replace_all(NestIDSession,"[ ]","_"))%>%
  mutate(NestID=str_replace_all(NestID,"[ ]","_"))%>%
  
  #filtering out incomplete nests as of 4.9.22:   
  filter(!(NestIDSession=='235_DICK_21_21_1'))%>%
  filter(!(NestIDSession=='KLT_EAME_2_21_1'))%>%
  filter(!(NestIDSession=='235_GRSP_2_21_1'))%>%
  filter(!(NestIDSession=='RIE_RWBL_11_21_2'))%>%
  filter(!(NestIDSession=='RIE_RWBL_7_21_2'))%>%
  
  mutate(NestVisability=recode(NestVisability, 
                               '100% - 85%'="85-100",
                               '49% - 0%'="0-49",
                               '84% - 50%'="50-84"))%>%
  mutate(FilmStartTime=format(ymd_hms(FilmStartTime),'%H:%M:%S'))%>%
  mutate(FilmEndTime=format(ymd_hms(FilmEndTime),'%H:%M:%S'))%>%
  mutate(FilmEnd=(hour(hms(FilmEndTime)))+(minute(hms(FilmEndTime))/60))%>%
  mutate(FilmStart=(hour(hms(FilmStartTime)))+(minute(hms(FilmStartTime))/60))%>%
  mutate(FilmDuration=FilmEnd-FilmStart)%>%
  mutate(ClipStart=FilmStart+(VideoClip-1)*(20/60))%>%
  mutate(BehaviorStart=ClipStart+(StartMinute/60))%>%
  filter(FilmDuration > .5)%>%
  mutate(OrdDate=yday(Date))%>%
  mutate_at(vars(Species), ~replace_na(., 'DICK'))%>%
  
  #Based on avg female DICK beak length of 16.1mm (Birds of the World)
  mutate(Arthmm = case_when(Species=="DICK" & ArthSize=="Small" ~ 9.05,
                            Species=="DICK" & ArthSize=="Medium" ~ 24.15,
                            Species=="DICK" & ArthSize=="Large" ~ 41.1,
                            
  #Based on avg female RWBL beak length of 19.3mm (Birds of the World)
  #Check RWBL subspecies and measurements. Pyle has 19.3 at top of range
                            Species=="RWBL" & ArthSize=="Small" ~ 10.65,
                            Species=="RWBL" & ArthSize=="Medium" ~ 28.95,
                            Species=="RWBL" & ArthSize=="Large" ~ 44.1,
                            TRUE ~ NA_real_))%>%
  mutate_at(c(23:58), as.numeric)
  
save(VM,file="VM.RData")
save(PB,file="PB.RData")
save(NB,file="NB.RData")

