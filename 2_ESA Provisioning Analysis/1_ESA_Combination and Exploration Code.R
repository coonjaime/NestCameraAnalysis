###JJC To Do:
  #1- Update Nestling Nums - done
  #2- Upload Daub, Nestling Condition, Nest Veg, and arth size datasets - done
  #3- Clean up nest survival/nest visit dataset 
  #4- Check what patch all nests are in on QGIS - done
  #5- outline analyses
  #6- nestling survival analysis
  #7- clean Daubenmire data - done (still need to add KLL!)
  #8- merge all veg data - done

#Other:
  #Organize veg data at multiple scales

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
setwd("~/RStudio/NestCameraAnalysis/2_ESA Provisioning Analysis")

#RStudio Cloud setup
setwd("/cloud/project/2_ESA Provisioning Analysis")

#Jaime comp wd
setwd("~/Dropbox/_Manuscripts/Dipping/NestCameraAnalysis/2_ESA Provisioning Analysis")

#__1a. PACKAGES                                       ####
#install.packages('easypackages')#do once to manage packages
library('easypackages')#load package managing package

packages('TMB','tidyverse','ggplot2','glmmTMB','readxl','janitor','lubridate')

#_____________________________________________________####
#2. IMPORTING SEPARATE DATASETS                     ####

veg_25<-read_csv("Data_July2022/NestVeg.csv")[-c(4904),]%>%
  clean_names(case = "upper_camel", abbreviations = c("ID","CSG","WSG","FEAR"))%>%
  mutate(NestID=str_replace_all(NestID,"[ ]","_"))%>%
 # filter(Year>2014)%>%
 # filter(grepl('DICK', NestID))%>%
  filter(!(NestID=="RIS_10_10"))%>%
  group_by(NestID)%>%
  mutate(LitDepth=(Litdepth+Litdepth2+Litdepth3)/3)%>%
  mutate(Robel=(Robel1+Robel2+Robel3+Robel4)/4)%>%
  summarize_at(vars(FEAR, CSG, WSG, Forb,Legume,Covlit,Robel,LitDepth),mean)%>%
  rename_with(~paste0(., "_25"), FEAR:LitDepth)

save(veg_25,file="veg_25.RData")

veg_5<-read_csv("Data_July2022/NestVeg.csv")[-c(4904),]%>% #Note from JJC - what does 7342 do?
  clean_names(case = "upper_camel", abbreviations = c("ID","CSG","WSG","FEAR"))%>%
  mutate(NestID=str_replace_all(NestID,"[ ]","_"))%>%
  #filter(grepl('DICK', NestID))%>%
  filter(!(NestID=="RIS_10_10"))%>%
  filter(Distance<6)%>%
  group_by(NestID)%>%
  mutate(LitDepth=(Litdepth+Litdepth2+Litdepth3)/3)%>%
  mutate(Robel=(Robel1+Robel2+Robel3+Robel4)/4)%>%
  summarize_at(vars(FEAR, CSG, WSG, Forb,Legume,Covlit,Robel,LitDepth),mean)%>%
  rename_with(~paste0(., "_5"), FEAR:LitDepth)
save(veg_5,file="veg_5.RData")


veg_pasture<-read_csv("Data_July2022/DaubData.csv")%>%
  clean_names(case = "upper_camel", abbreviations = c("ID","CSG","WSG","FEAR"))%>%
  filter(Year>2014)%>%
  filter(!(Year=="2017"))%>%
  filter(!(Year=="2018"))%>%
  rename(FEAR=Fescue)%>%
  rename(Covlit=Litter)%>%
  rename(LitDepth=LitterDepth)%>%
  rename(Forb=Forbs)%>%
  rename(Legume=Legumes)%>%
  select(-c(X28,X29))%>%
  group_by(PasturePatchYear)%>%
  mutate(Robel=(RobelN+RobelE+RobelS+RobelW)/4)%>%
  summarize_at(vars(FEAR, CSG, WSG, Forb,Legume,Covlit,Robel,LitDepth),mean)%>%
  rename_with(~paste0(., "_Pasture"), FEAR:LitDepth)

save(veg_pasture,file="veg_pasture.RData")

EarlyYearNestInfo=read_csv("Data_July2022/NestMonitoring_2015_2016.csv")%>%
  clean_names(case = "upper_camel", abbreviations = c("ID"))%>%
  mutate(NestID=str_replace_all(NestID,"[ ]","_"))%>%
  group_by(NestID) %>% 
  filter(row_number()==1)%>%
  filter(Year>2014)%>%
  mutate(PasturePatch = str_replace(PasturePatch, "[.]", "_"))%>%
  unite("PasturePatchYear",c(PasturePatch,Year), sep="_", remove = FALSE)%>%
  select(c(NestID,Pasture,PasturePatchYear,PasturePatch))

LateYearNestInfo=read_csv("Data_July2022/NestPatches_2021.csv")%>%
  select(name,Patch,Pasture,Year)%>%
  rename(NestID=name)%>%
  rename(PasturePatch=Patch)%>%
  mutate(NestID=str_replace_all(NestID,"[ ]","_"))%>%
  unite("PasturePatchYear",c(PasturePatch,Year), sep="_", remove = FALSE)%>%
  select(-Year)

NestInfo_all=rbind(EarlyYearNestInfo,LateYearNestInfo)
names(NestInfo_all)

save(NestInfo_all,file="NestInfo.RData")

Veg_All=NestInfo_all%>%
  left_join(veg_25,by="NestID")%>%
  left_join(veg_5,by="NestID")%>%
  left_join(veg_pasture,by="PasturePatchYear")
    #lots of NAs- are these the ones we didn't do veg at? maybe need to check datasheets. 
    #possible some data entry wasn't done

save(Veg_All,file="Merged_Veg_Data.RData")    
  

WPT_NB <- read_excel("Data_July2022/WPT_Nestling Behavior_7.2.22.xlsx")
WPT_PB <- read_excel("Data_July2022/WPT_Parent Behavior_7.2.22.xlsx", col_types = c(rep("guess",22),rep("numeric",36),"logical")) 
WPT_VM <- read_excel("Data_July2022/WPT_Video_Master_7.2.22.xlsx")

JJC_NB <- read_excel("Data_July2022/JJC_Nestling Behavior_4.9.22.xlsx")
JJC_PB <- read_excel("Data_July2022/JJC_Parent Behavior_4.9.22.xlsx", col_types = c(rep("guess",22),rep("numeric",36),"logical"))
JJC_VM <- read_excel("Data_July2022/JJC_Video_Master_4.9.22.xlsx")

JNA_NB <- read_excel("Data_July2022/JNA_Nestling Behavior_6.15.22.xlsx")
JNA_PB <- read_excel("Data_July2022/JNA_Parent Behavior_6.15.22.xlsx", col_types = c(rep("guess",22),rep("numeric",36),"logical"))
JNA_VM <- read_excel("Data_July2022/JNA_Video_Master_6.15.22.xlsx")

TEC_NB <- read_excel("Data_July2022/TEC_Nestling Behavior_7.4.22.xlsx")
TEC_PB <- read_excel("Data_July2022/TEC_Parent Behavior_7.4.22.xlsx", col_types = c(rep("guess",22),rep("numeric",36),"logical"))
TEC_VM <- read_excel("Data_July2022/TEC_Video_Master_7.4.22.xlsx")

HKG_NB <- read_excel("Data_July2022/HKG_Nestling Behavior_7.16.22.xlsx")
HKG_PB <- read_excel("Data_July2022/HKG_Parent Behavior_7.16.22.xlsx", col_types = c(rep("guess",22),rep("numeric",36),"logical"))
HKG_VM <- read_excel("Data_July2022/HKG_Video_Master_7.16.22.xlsx")

ESK_NB <- read_excel("Data_July2022/ESK_Nestling Behavior_6.27.22.xlsx")
ESK_PB <- read_excel("Data_July2022/ESK_Parent Behavior_6.27.22.xlsx", col_types = c(rep("guess",22),rep("numeric",36),"logical"))
ESK_VM <- read_excel("Data_July2022/ESK_Video_Master_6.27.22.xlsx")

MFM_NB <- read_excel("Data_July2022/MFM_Nestling Behavior_1.2.22.xlsx")
MFM_PB <- read_excel("Data_July2022/MFM_Parent Behavior_1.2.22.xlsx", col_types = c(rep("guess",22),rep("numeric",36),"logical"))
MFM_VM <- read_excel("Data_July2022/MFM_Video_Master_1.2.22.xlsx")

MFM_NB2 <- read_excel("Data_July2022/MFM_Nestling Behavior_9.15.21.xlsx")
MFM_PB2 <- read_excel("Data_July2022/MFM_Parent Behavior_9.15.21.xlsx", col_types = c(rep("guess",22),rep("numeric",36),"logical"))
MFM_VM2 <- read_excel("Data_July2022/MFM_Video_Master_9.15.21.xlsx")

CER_NB <- read_excel("Data_July2022/CER_Nestling Behavior_4.2.22.xlsx")
CER_PB <- read_excel("Data_July2022/CER_Parent Behavior_4.2.22.xlsx", col_types = c(rep("guess",22),rep("numeric",36),"logical"))
CER_VM <- read_excel("Data_July2022/CER_Video_Master_4.2.22.xlsx")

#Nestling numbers for updated nests as of 7.12.22
NestlingNums <- read.csv("Data_July2022/NestlingNumsAge.csv")[1:265,1:17]%>%
  clean_names(case = "upper_camel", abbreviations = c("ID","BHCO"))%>%
  mutate(NestIDSession=str_replace(NestIDSession,"[(]",""))%>%
  mutate(NestIDSession=str_replace(NestIDSession,"[)]",""))%>%
  mutate(NestIDSession=str_replace_all(NestIDSession,"[ ]","_"))%>%
  mutate(NestID=str_replace_all(NestID,"[ ]","_"))%>%
  select(c(-Year,-NestID,-Species,-CanSeeDippingBegging,-ParentsReturn,-ReviewerInitials,-NestlingAgeNotes,-Priority,-Comments,-Patch))

save(NestlingNums,file="NestlingNums.RData")  

#Arthropod calculations
Arth <- read_excel("Data_July2022/ArthropodMeasuring_DICKData.xlsx", sheet = 3)
#Calculating means within each size class. Based on avg female DICK beak length of 16.1mm (Birds of the World)
mean(filter(Arth, Length_mm <= 16.1)$Length_mm) #7.074
mean(filter(Arth, Length_mm > 16.1 & Length_mm <= 32.2)$Length_mm) #19.596
mean(filter(Arth, Length_mm > 32.2)$Length_mm) #61.571

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
  mutate(WhoIsScoring=recode(WhoIsScoring,"Wendy"="WPT")) #fixing WPT mistype


WPT_NB_cleaned=WPT_NB%>%
  clean_names(case = "upper_camel", abbreviations = c("ID","BHCO"))%>%
  add_column("CoderID"="001")%>%
  unite("BehaviorID",c("BehaviorID","CoderID"),remove=TRUE,sep="_")%>%
  left_join(WPT_VM_cleaned,select(
    NestID,Year, NestVisablity,FilmStartTime,FilmEndTime,NumHosts,NumBHCO,TotalNestling),
    by="NestIDSession")%>%
  filter(!(Year<2021)) 

WPT_PB_cleaned=WPT_PB%>%
  clean_names(case = "upper_camel", abbreviations = c("ID","BHCO"))%>%
  add_column("CoderID"="001")%>%
  unite("BehaviorID",c("BehaviorID","CoderID"),remove=TRUE,sep="_")%>%
  left_join(WPT_VM_cleaned,select(
    NestID,Year, NestVisablity,FilmStartTime,FilmEndTime,NumHosts,NumBHCO,TotalNestling),
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
  filter(!(Year==2017))%>% #One 2016 nest coded as 2017
  filter(!grepl('KLT RWBL 4 21 (1)', NestIDSession))#duplicate 

 

JJC_NB_cleaned=JJC_NB%>%
  clean_names(case = "upper_camel", abbreviations = c("ID","BHCO"))%>%
  add_column("CoderID"="002")%>%
  unite("BehaviorID",c("BehaviorID","CoderID"),remove=TRUE,sep="_")%>%
  left_join(JJC_VM_cleaned,select(
    NestID,Year, NestVisablity,FilmStartTime,FilmEndTime,NumHosts,NumBHCO,TotalNestling,VideoClip),
    by="NestIDSession")%>%
  filter(!grepl('KLT RWBL 4 21 (1)', NestIDSession)) #duplicate

JJC_PB_cleaned=JJC_PB%>%
  clean_names(case = "upper_camel", abbreviations = c("ID","BHCO"))%>%
  add_column("CoderID"="002")%>%
  unite("BehaviorID",c("BehaviorID","CoderID"),remove=TRUE,sep="_")%>%
  left_join(JJC_VM_cleaned,select(
    NestID,Year, NestVisablity,FilmStartTime,FilmEndTime,NumHosts,NumBHCO,TotalNestling),
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
  filter(!(Year<2021))

JNA_NB_cleaned=JNA_NB%>%
  clean_names(case = "upper_camel", abbreviations = c("ID","BHCO"))%>%
  add_column("CoderID"="003")%>%
  unite("BehaviorID",c("BehaviorID","CoderID"),remove=TRUE,sep="_")%>%
  left_join(JNA_VM_cleaned,select(
    NestID,Year, NestVisablity,FilmStartTime,FilmEndTime,NumHosts,NumBHCO,TotalNestling),
    by="NestIDSession")%>%
  filter(!(Year<2021)) 
  
JNA_PB_cleaned=JNA_PB%>%
  clean_names(case = "upper_camel", abbreviations = c("ID","BHCO"))%>%
  add_column("CoderID"="003")%>%
  unite("BehaviorID",c("BehaviorID","CoderID"),remove=TRUE,sep="_")%>%
  left_join(JNA_VM_cleaned,select(
    NestID,Year, NestVisablity,FilmStartTime,FilmEndTime,NumHosts,NumBHCO,TotalNestling),
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
  filter(!(Year<2021))

TEC_NB_cleaned=TEC_NB%>%
  clean_names(case = "upper_camel", abbreviations = c("ID","BHCO"))%>%
  add_column("CoderID"="004")%>%
  unite("BehaviorID",c("BehaviorID","CoderID"),remove=TRUE,sep="_")%>%
  left_join(TEC_VM_cleaned,select(
    NestID,Year, NestVisablity,FilmStartTime,FilmEndTime,NumHosts,NumBHCO,TotalNestling),
    by="NestIDSession")%>%
  filter(!(Year<2021)) 

TEC_PB_cleaned=TEC_PB%>%
  clean_names(case = "upper_camel", abbreviations = c("ID","BHCO"))%>%
  add_column("CoderID"="004")%>%
  unite("BehaviorID",c("BehaviorID","CoderID"),remove=TRUE,sep="_")%>%
  left_join(TEC_VM_cleaned,select(
    NestID,Year, NestVisablity,FilmStartTime,FilmEndTime,NumHosts,NumBHCO,TotalNestling),
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
  filter(!(Year<2021))

HKG_NB_cleaned=HKG_NB%>%
  clean_names(case = "upper_camel", abbreviations = c("ID","BHCO"))%>%
  add_column("CoderID"="005")%>%
  unite("BehaviorID",c("BehaviorID","CoderID"),remove=TRUE,sep="_")%>%
  left_join(HKG_VM_cleaned,select(
    NestID,Year, NestVisablity,FilmStartTime,FilmEndTime,NumHosts,NumBHCO,TotalNestling),
    by="NestIDSession")%>%
  filter(!(Year<2021)) 

HKG_PB_cleaned=HKG_PB%>%
  clean_names(case = "upper_camel", abbreviations = c("ID","BHCO"))%>%
  add_column("CoderID"="005")%>%
  unite("BehaviorID",c("BehaviorID","CoderID"),remove=TRUE,sep="_")%>%
  left_join(HKG_VM_cleaned,select(
    NestID,Year, NestVisablity,FilmStartTime,FilmEndTime,NumHosts,NumBHCO,TotalNestling),
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
  filter(!(Year<2021))

ESK_NB_cleaned=ESK_NB%>%
  clean_names(case = "upper_camel", abbreviations = c("ID","BHCO"))%>%
  add_column("CoderID"="006")%>%
  unite("BehaviorID",c("BehaviorID","CoderID"),remove=TRUE,sep="_")%>%
  left_join(ESK_VM_cleaned,select(
    NestID,Year, NestVisablity,FilmStartTime,FilmEndTime,NumHosts,NumBHCO,TotalNestling),
    by="NestIDSession")%>%
  filter(!(Year<2021)) 

ESK_PB_cleaned=ESK_PB%>%
  clean_names(case = "upper_camel", abbreviations = c("ID","BHCO"))%>%
  add_column("CoderID"="006")%>%
  unite("BehaviorID",c("BehaviorID","CoderID"),remove=TRUE,sep="_")%>%
  left_join(ESK_VM_cleaned,select(
    NestID,Year, NestVisablity,FilmStartTime,FilmEndTime,NumHosts,NumBHCO,TotalNestling),
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
  filter(!(Year<2021))

MFM_NB_cleaned=MFM_NB%>%
  clean_names(case = "upper_camel", abbreviations = c("ID","BHCO"))%>%
  add_column("CoderID"="007")%>%
  unite("BehaviorID",c("BehaviorID","CoderID"),remove=TRUE,sep="_")%>%
  left_join(MFM_VM_cleaned,select(
    NestID,Year, NestVisablity,FilmStartTime,FilmEndTime,NumHosts,NumBHCO,TotalNestling,VideoClip),
    by="NestIDSession")%>%
  filter(!(Year<2021)) 
  
MFM_PB_cleaned=MFM_PB%>%
  clean_names(case = "upper_camel", abbreviations = c("ID","BHCO"))%>%
  add_column("CoderID"="007")%>%
  unite("BehaviorID",c("BehaviorID","CoderID"),remove=TRUE,sep="_")%>%
  left_join(MFM_VM_cleaned,select(
    NestID,Year, NestVisablity,FilmStartTime,FilmEndTime,NumHosts,NumBHCO,TotalNestling),
    by="NestIDSession")%>%
  filter(!(Year<2021)) 

#MOLLY2
MFM_VM_cleaned2=MFM_VM2%>% #These datasheets are for BSH FISP 1 21 data
  clean_names(case = "upper_camel", abbreviations = c("ID","BHCO"))%>%
  add_column("CoderID"="007")%>%
  rename(FilmStartTime=StartTime,
         FilmEndTime=EndTime,
         SessionY=Session)%>%
  mutate(Date=format(as.Date(Date),format="%Y-%m-%d"))%>%
  mutate("Year"=(year(Date)))%>%
  filter(!(Year<2021))%>%
  filter(NestID=="BSH FISP 1 21")%>%
  mutate(NestIDSession=recode(NestIDSession,"BSH FISP 1 21(1)"="BSH FISP 1 21 (1)")) #fixing MFM mistype

MFM_NB_cleaned2=MFM_NB2%>%
  clean_names(case = "upper_camel", abbreviations = c("ID","BHCO"))%>%
  add_column("CoderID"="007")%>%
  unite("BehaviorID",c("BehaviorID","CoderID"),remove=TRUE,sep="_")%>%
  mutate(NestIDSession=recode(NestIDSession,"BSH FISP 1 21(1)"="BSH FISP 1 21 (1)")) %>%
  left_join(MFM_VM_cleaned2,select(
    NestID,Year, NestVisablity,FilmStartTime,FilmEndTime,NumHosts,NumBHCO,TotalNestling,VideoClip),
    by="NestIDSession")%>%
  filter(!(Year<2021)) 

MFM_PB_cleaned2=MFM_PB2%>%
  clean_names(case = "upper_camel", abbreviations = c("ID","BHCO"))%>%
  add_column("CoderID"="007")%>%
  unite("BehaviorID",c("BehaviorID","CoderID"),remove=TRUE,sep="_")%>%
  mutate(NestIDSession=recode(NestIDSession,"BSH FISP 1 21(1)"="BSH FISP 1 21 (1)")) %>%
  left_join(MFM_VM_cleaned2,select(
    NestID,Year, NestVisablity,FilmStartTime,FilmEndTime,NumHosts,NumBHCO,TotalNestling),
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
  filter(!grepl("JNA",WhoIsScoring))

CER_NB_cleaned=CER_NB%>%
  clean_names(case = "upper_camel", abbreviations = c("ID","BHCO"))%>%
  add_column("CoderID"="008")%>%
  unite("BehaviorID",c("BehaviorID","CoderID"),remove=TRUE,sep="_")%>%
  left_join(CER_VM_cleaned,select(
    NestID,Year, NestVisablity,FilmStartTime,FilmEndTime,NumHosts,NumBHCO,TotalNestling,VideoClip),
    by="NestIDSession")%>%
  filter(!(Year<2021)) 

CER_PB_cleaned=CER_PB%>%
  clean_names(case = "upper_camel", abbreviations = c("ID","BHCO"))%>%
  add_column("CoderID"="008")%>%
  unite("BehaviorID",c("BehaviorID","CoderID"),remove=TRUE,sep="_")%>%
  left_join(CER_VM_cleaned,select(
    NestID,Year, NestVisablity,FilmStartTime,FilmEndTime,NumHosts,NumBHCO,TotalNestling),
    by="NestIDSession")%>%
  filter(!(Year<2021)) 
  
compare_df_cols(JJC_PB_cleaned,JNA_PB_cleaned) #to compare before merging

NB_combined=rbind(WPT_NB_cleaned,JJC_NB_cleaned,JNA_NB_cleaned,TEC_NB_cleaned,HKG_NB_cleaned,ESK_NB_cleaned,MFM_NB_cleaned,MFM_NB_cleaned2,CER_NB_cleaned)
PB_combined=rbind(WPT_PB_cleaned,JJC_PB_cleaned,JNA_PB_cleaned,TEC_PB_cleaned,HKG_PB_cleaned,ESK_PB_cleaned,MFM_PB_cleaned,MFM_PB_cleaned2,CER_PB_cleaned)
VM_combined=rbind(WPT_VM_cleaned,JJC_VM_cleaned,JNA_VM_cleaned,TEC_VM_cleaned,HKG_VM_cleaned,ESK_VM_cleaned,MFM_VM_cleaned,MFM_VM_cleaned2,CER_VM_cleaned)

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
  select(-NestlingAgeDays)%>%
 
   mutate(NestVisability=recode(NestVisability, 
                               '100% - 85%'="85-100",
                               '49% - 0%'="0-49",
                               '84% - 50%'="50-84"))%>%
  
  mutate(NestIDSession=recode(NestIDSession,'KLN_DICK_1_21_1'="KLN_DICK_1_15_1"))%>% #fixing JJC mistype
  mutate(NestID=recode(NestID,'KLN_DICK_1_21'="KLN_DICK_1_15"))%>% #fixing JJC mistype
  mutate(NestIDSession=recode(NestIDSession,'235_EAKI_1_21'="235_EAKI_1_21_1"))%>%

  # left_join(NestlingNums,select(XHosts,XBHCO),by="NestIDSession")%>%
  # mutate(TotalNestling = as.numeric(XHosts)+as.numeric(XBHCO))%>%
  select(c(-Cowbird,-Dickcissel,-TotalNestling))%>%
  
  # add_column("Parasitized"=as.numeric(as.logical(.$XBHCO)))%>%
  # d_column("propBHCO"=.$XBHCO/.$TotalNestling)%>%
  
  #filtering out incomplete nests as of 7.19.22: 
  filter(!(NestIDSession=='KLT_EAME_2_21_1'))%>%
  filter(!(NestIDSession=='235_GRSP_2_21_1'))%>%
  
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


NB=NB_combined%>%
  filter(!grepl('0', VideoClip))%>%
  filter(!grepl('Unknown', BehaviorCode))%>%
  filter(!grepl('PTER', NestIDSession))%>%
  filter(!grepl('EJT', NestID))%>%
  filter(!grepl('RANC', NestID))%>%
  filter(!grepl('BAD DATA', NestIDSession))%>%
    
  mutate(NestIDSession=str_replace(NestIDSession,"[(]",""))%>%
  mutate(NestIDSession=str_replace(NestIDSession,"[)]",""))%>%
  mutate(NestIDSession=str_replace_all(NestIDSession,"[ ]","_"))%>%
  mutate(NestID=str_replace_all(NestID,"[ ]","_"))%>%
  
  mutate(NestIDSession=recode(NestIDSession,'KLN_DICK_1_21_1'="KLN_DICK_1_15_1"))%>% #fixing JJC mistype
  mutate(NestID=recode(NestID,'KLN_DICK_1_21'="KLN_DICK_1_15"))%>% #fixing JJC mistype
  mutate(NestIDSession=recode(NestIDSession,'235_EAKI_1_21'="235_EAKI_1_21_1"))%>%
  
  left_join(NestlingNums,select(XHosts,XBHCO,AvgAgeDays),by="NestIDSession")%>%
  mutate(TotalNestling = as.numeric(XHosts)+as.numeric(XBHCO))%>%
  select(c(-Cowbird,-Dickcissel))%>%
  add_column("Parasitized"=as.numeric(as.logical(.$XBHCO)))%>%
  add_column("propBHCO"=.$XBHCO/.$TotalNestling)%>%
  
  #Behavior Durations
  separate(StartTime,sep=":",into=c("StartMinute","StartSecond"))%>%
  separate(EndTime,sep=":",into=c("EndMinute","EndSecond"))%>%
  mutate(StartMinute=as.numeric(StartMinute))%>%
  mutate(StartSecond=as.numeric(StartSecond))%>%
  mutate(EndMinute=as.numeric(EndMinute))%>%
  mutate(EndSecond=as.numeric(EndSecond))%>%
  
  mutate(Duration_Sec=(EndMinute*60+EndSecond)-(StartMinute*60+StartSecond))%>%
  
  #Getting times in the right formats
  mutate(FilmStartTime=format(ymd_hms(FilmStartTime),'%H:%M:%S'))%>%
  mutate(FilmEndTime=format(ymd_hms(FilmEndTime),'%H:%M:%S'))%>%
  mutate(FilmEnd=(hour(hms(FilmEndTime)))+(minute(hms(FilmEndTime))/60))%>%
  mutate(FilmStart=(hour(hms(FilmStartTime)))+(minute(hms(FilmStartTime))/60))%>%
  mutate(FilmDuration=FilmEnd-FilmStart)%>%
  mutate(ClipStart=FilmStart+(VideoClip-1)*(20/60))%>%
  mutate(BehaviorStart=ClipStart+(StartMinute/60))%>%
  filter(FilmDuration > .5)%>% #in hours
  
  #filtering out incomplete nests as of 7.19.22: 
  filter(!(NestIDSession=='KLT_EAME_2_21_1'))%>%
  filter(!(NestIDSession=='235_GRSP_2_21_1'))%>%
  
  mutate(NestVisability=recode(NestVisability, 
                               '100% - 85%'="85-100",
                               '49% - 0%'="0-49",
                               '84% - 50%'="50-84"))%>%
  mutate(OrdDate=yday(Date))
  #mutate_at(vars(Species), ~replace_na(., 'DICK'))
names(PB_combined)

PB=PB_combined%>%
  select(-Comments.x,-ArthLength,-BeakLength,-Screenshot,-Comments.y,-Cowbird,-Dickcissel,
         -TotalNestling,-NestlingAgeDays,-StartMinute,-StartSecond,-EndMinute,-EndSecond)%>%
  filter(!grepl('0', VideoClip))%>%
  filter(!grepl('Unknown', BehaviorCode))%>%
  filter(!grepl('PTER', NestIDSession))%>%
  filter(!grepl('EJT', NestID))%>%
  filter(!grepl('RANC', NestID))%>%
  filter(!grepl('BAD DATA', NestIDSession))%>%
  mutate(NestIDSession=str_replace(NestIDSession,"[(]",""))%>%
  mutate(NestIDSession=str_replace(NestIDSession,"[)]",""))%>%
  mutate(NestIDSession=str_replace_all(NestIDSession,"[ ]","_"))%>%
  mutate(NestID=str_replace_all(NestID,"[ ]","_"))%>%
  
  mutate(NestIDSession=recode(NestIDSession,'KLN_DICK_1_21_1'="KLN_DICK_1_15_1"))%>% #fixing JJC mistype
  mutate(NestID=recode(NestID,'KLN_DICK_1_21'="KLN_DICK_1_15"))%>% #fixing JJC mistype
  mutate(NestIDSession=recode(NestIDSession,'235_EAKI_1_21'="235_EAKI_1_21_1"))%>%
  
  #filtering out incomplete nests as of 7.19.22: 
  filter(!(NestIDSession=='KLT_EAME_2_21_1'))%>%
  filter(!(NestIDSession=='235_GRSP_2_21_1'))%>%
  
  mutate(NestVisability=recode(NestVisability, 
                               '100% - 85%'="85-100",
                               '49% - 0%'="0-49",
                               '84% - 50%'="50-84"))%>%
  
  #Behavior Durations
  separate(StartTime,sep=":",into=c("StartMinute","StartSecond"))%>%
  separate(EndTime,sep=":",into=c("EndMinute","EndSecond"))%>%
  mutate(StartMinute=as.numeric(StartMinute))%>%
  mutate(StartSecond=as.numeric(StartSecond))%>%
  mutate(EndMinute=as.numeric(EndMinute))%>%
  mutate(EndSecond=as.numeric(EndSecond))%>%
  
  mutate(Duration_Sec=(EndMinute*60+EndSecond)-(StartMinute*60+StartSecond))%>%
  
  #Getting times in the right formats
  mutate(FilmStartTime=format(ymd_hms(FilmStartTime),'%H:%M:%S'))%>%
  mutate(FilmEndTime=format(ymd_hms(FilmEndTime),'%H:%M:%S'))%>%
  mutate(FilmEnd=(hour(hms(FilmEndTime)))+(minute(hms(FilmEndTime))/60))%>%
  mutate(FilmStart=(hour(hms(FilmStartTime)))+(minute(hms(FilmStartTime))/60))%>%
  mutate(FilmDuration=FilmEnd-FilmStart)%>%
  mutate(ClipStart=FilmStart+(VideoClip-1)*(20/60))%>%
  mutate(BehaviorStart=ClipStart+(StartMinute/60))%>%
  filter(FilmDuration > .5)%>%
  
  #
  mutate(OrdDate=yday(Date))%>%
  mutate_at(vars(Species), ~replace_na(., 'DICK'))%>%
  
  #Means within each size class. Based on avg female DICK beak length of 16.1mm (Birds of the World)
  mutate(Arthmm = case_when(Species=="DICK" & ArthSize=="Small" ~ 7.074,
                            Species=="DICK" & ArthSize=="Medium" ~ 19.596,
                            Species=="DICK" & ArthSize=="Large" ~ 61.571,
                            
  #Midpoints within each size class. Based on avg female RWBL beak length of 19.3mm (Birds of the World)
  #Check RWBL subspecies and measurements. Pyle has 19.3 at top of range
                            Species=="RWBL" & ArthSize=="Small" ~ 10.65,
                            Species=="RWBL" & ArthSize=="Medium" ~ 28.95,
                            Species=="RWBL" & ArthSize=="Large" ~ 44.1,
                            TRUE ~ NA_real_))%>%
  left_join(Veg_All,by="NestID")%>%
  left_join(NestlingNums,by="NestIDSession")%>%
  mutate(TotalNestling = as.numeric(.$XHosts)+as.numeric(.$XBHCO))%>%
  add_column("Parasitized"=as.numeric(as.logical(.$XBHCO)))%>%
  add_column("propBHCO"=.$XBHCO/.$TotalNestling)
  
  
save(VM,file="VM.RData")
save(PB,file="PB.RData")
save(NB,file="NB.RData")
