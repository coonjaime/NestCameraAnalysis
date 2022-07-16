###JJC To do:
#1-Code for nest survival analysis

###Code Contributors
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

#Jaime's computer setup
#setwd("~/NestCameraAnalysis/2_ESA Provisioning Analysis")
#feel free to add your own setup if using this code

#Ethan wd
setwd("~/Desktop/GRGCoding/NestCameraAnalysis/2_ESA Provisioning Analysis")
remove.packages('glmmTMB')
install.packages('ggeffects')
install.packages('TMB', type = 'source')
install.packages('glmmTMB', type = 'source')

#Josh
setwd("~/RStudio/NestCameraAnalysis/2_ESA Provisioning Analysis")

#RStudio Cloud setup
setwd("/cloud/project/2_ESA Provisioning Analysis")

#Jaime comp wd
setwd("~/Dropbox/_Manuscripts/Dipping/NestCameraAnalysis/2_ESA Provisioning Analysis")

#ggplot themes----
theme_bar_noleg <- function () { 
  theme(text=element_text(size=14),
        axis.title=element_text(face="bold", size=16),
        axis.text=element_text(size=14,color="black"),
        axis.line=element_line(color="black",size=1),
        panel.background=element_rect("snow2"),
        panel.grid=element_blank(),
        legend.position="none")}

theme_bar_leg <- function () { 
  theme(text=element_text(size=14),
        axis.title=element_text(face="bold", size=16),
        axis.text=element_text(size=14,color="black"),
        axis.line=element_line(color="black",size=1),
        panel.background=element_rect("snow2"),
        panel.grid=element_blank(),
        legend.position="right",
        legend.text=element_text(size=10, color="black"))}

#install.packages('easypackages')#do once to manage packages
library('easypackages')#load package managing package

packages('TMB','ggeffects','tidyverse','ggplot2','ggpubr','glmmTMB','readxl','janitor','lubridate','stringr','reshape2', 'AICcmodavg')

#_____________________________________________________####
####2. DATA  ####

load("VM.RData")
load("PB.RData")
load("NB.RData")
load("Merged_Veg_Data.RData")

#calculating behavior rates per hour by the session
ProvDataSession=PB%>%
  #filter(!grepl('TRUE', AbleSeeDipping))%>% 
  #filter(!grepl('0-49', NestVisability))%>% 
  group_by(Species,NestIDSession,NestID,Session,BehaviorCode,NestVisability,FilmDuration,Year, FilmStart,
           Pasture,PasturePatch,PasturePatchYear, OrdDate,
           XBHCO,XHosts,propBHCO,TotalNestling,AvgAgeDays,Parasitized,
           FEAR_5,FEAR_25,FEAR_Pasture,
           WSG_5,WSG_25,WSG_Pasture,
           CSG_5,CSG_25,CSG_Pasture,
           Covlit_5,Covlit_25,Covlit_Pasture,
           LitDepth_5,LitDepth_25,LitDepth_Pasture,
           Forb_5,Forb_25,Forb_Pasture,
           Robel_5,Robel_25,Robel_Pasture)%>%
  summarize(BehaviorCount = n())%>%
  mutate(Beh_per_h=BehaviorCount/(FilmDuration))%>%
  filter(BehaviorCode=="Provisioning")%>%
  filter(Species=="DICK")


##calculating behavior rates per hour by the clip
ProvDataClip=PB%>%
  filter(!(StartMinute>30))%>% #filtering out any weird data
  #filter(!grepl('TRUE', AbleSeeDipping))%>% 
  #filter(!grepl('0-49', NestVisability))%>% 
  group_by(Species,VideoClip, NestIDSession,NestID,SessionY,BehaviorCode,NestVisability,FilmDuration,Year, ClipStart,
           FilmStart,Pasture,PasturePatch,PasturePatchYear,OrdDate,
           XBHCO,XHosts,propBHCO,TotalNestling,AvgAgeDays,Parasitized,
           FEAR_5,FEAR_25,FEAR_Pasture,
           WSG_5,WSG_25,WSG_Pasture,
           CSG_5,CSG_25,CSG_Pasture,
           Covlit_5,Covlit_25,Covlit_Pasture,
           LitDepth_5,LitDepth_25,LitDepth_Pasture,
           Forb_5,Forb_25,Forb_Pasture,
           Robel_5,Robel_25,Robel_Pasture)%>%  summarize(BehaviorCount = n())%>%
  mutate(Beh_per_h=BehaviorCount/(FilmDuration))%>%
  filter(BehaviorCode=="Provisioning")%>%
  filter(Species=="DICK")%>%
  filter(TotalNestling>0)


BegData=NB%>%
  filter(BehaviorCode=="Begging")%>%
  filter(!(NestVisability=="0-49"))%>%
  mutate_at(vars(Species), ~replace_na(., 'DICK'))%>%
  filter(Species=="DICK")%>%
  filter(!(StartMinute>30))%>%
  group_by(Year,OrdDate,FilmDuration,NestID,BehaviorCode,NestIDSession,TotalNestling,XBHCO,XHosts,Parasitized,propBHCO,AvgAgeDays,FilmStart)%>%
  summarize(BegDuration = sum(Duration_Sec))%>%
  mutate(PercentTime=BegDuration/(FilmDuration*60*60))
  filter(TotalNestling>0)

  NestChecks=read_csv("Data_July2022/NestChecks_7.16.2022.csv")%>%
    clean_names(case = "upper_camel", abbreviations = c("ID"))%>%
    mutate(NestID=str_replace_all(NestID,"[ ]","_"))%>%
    mutate(Date=format(mdy(Date),'%m/%d/%y'))%>%
    mutate(Year=year(mdy(Date)))%>%
    filter(Year>2014)%>%
    filter(Year<2022)%>%
    mutate(Stage = replace(Stage, Stage=="Fledge", "Fledged"))%>%
    mutate(Stage = replace(Stage, Stage=="Cowbird fledge", "Cowbird fledged"))%>%
    filter(grepl('Nestling|Fledged|Abandoned|Dead|Cowbird fledged', Stage))%>% 
    group_by(NestID)%>%
    mutate(MaxBHCO=max(ParasiteChicks),
           MaxHost=max(HostChicks),
           MaxChicks=MaxHost+MaxBHCO)%>%
    arrange(NestID, VisitID) %>% 
    group_by(NestID) %>% 
    summarise_all(last)%>%
    left_join(Veg_All,by="NestID")%>%
    mutate_at(vars(Species), ~replace_na(., 'DICK'))%>%
    filter(Species=="DICK")
  
# _____________________________________________________####
####3. Analysis for ESA  - DICK ####
# _____________________________________________________####

####4a. Descriptive data - sample size, average provisioning rates, most commonly provisioned order and size####

#Sample Size
length(unique(ProvDataSession$NestIDSession)) #Sample Size - How many nest filming sessions (Will change when new data is inputted)
length(unique(ProvDataSession$NestID)) #Sample Size - How many nests (Will change when new data is inputted)

#Most common food items and sizes
NestlingDiet=PB%>%
  filter(grepl("Provisioning", BehaviorCode),na.rm=TRUE)%>% 
  filter(grepl("DICK",Species),na.rm=TRUE)%>%
  filter(!grepl('0-49', NestVisability))%>% 
  group_by(Species,ArthSize,ArthID)%>%
  summarize(count=n())%>%
  mutate(percent=count/sum(count))

print(NestlingDiet)
#need average provisioning rates 


####4b. Impact of vegetation and BHCO on provisioning rates####

names(ProvDataSession) #Names of columns

Nuisance_Mods_DICK= function(df) {
  Null                   = glmmTMB(Beh_per_h ~ 1                 + (1|PasturePatchYear) + (1|NestIDSession),  data=df, family="gaussian")
  TimeofDay              = glmmTMB(Beh_per_h ~ FilmStart         + (1|PasturePatchYear) + (1|NestIDSession),  data=df, family="gaussian")
  Date                   = glmmTMB(Beh_per_h ~ OrdDate           + (1|PasturePatchYear) + (1|NestIDSession),  data=df, family="gaussian")

  mods=list(Null,   TimeofDay,   Date)  
  names=c( "Null", "TimeofDay", "Date")
  
  print(aictab(cand.set = mods, modnames = names,second.ord = FALSE), digits = 4) }

Nuisance_Mods_DICK(ProvDataSession)


NestContents_Mods_DICK= function(df) {
  Null                   = glmmTMB(Beh_per_h ~ 1               + (1|PasturePatchYear),  data=df, family="gaussian")
  TotalNestling          = glmmTMB(Beh_per_h ~ TotalNestling   + (1|PasturePatchYear),  data=df, family="gaussian")
  PropBHCO               = glmmTMB(Beh_per_h ~ propBHCO        + (1|PasturePatchYear),  data=df, family="gaussian")
  NumBHCO                = glmmTMB(Beh_per_h ~ XBHCO           + (1|PasturePatchYear),  data=df, family="gaussian")
  PresBHCO               = glmmTMB(Beh_per_h ~ Parasitized     + (1|PasturePatchYear),  data=df, family="gaussian")
  NestlingAge            = glmmTMB(Beh_per_h ~ AvgAgeDays      + (1|PasturePatchYear),  data=df, family="gaussian")
  
  mods=list(Null,   TotalNestling,   PropBHCO,    NumBHCO,   PresBHCO,   NestlingAge)  
  names=c( "Null", "TotalNestling", "PropBHCO",  "NumBHCO", "PresBHCO", "NestlingAge")
  
  print(aictab(cand.set = mods, modnames = names,second.ord = FALSE), digits = 4) }

NestContents_Mods_DICK(ProvDataSession)

Veg_Mods_DICK = function(df) {
  Null                      = glmmTMB(Beh_per_h ~ 1             + TotalNestling + Parasitized + (1|PasturePatchYear),  data=df, family="gaussian")
  FEAR5                     = glmmTMB(Beh_per_h ~ FEAR_5        + TotalNestling + Parasitized + (1|PasturePatchYear),  data=df, family="gaussian")
  FEAR25                    = glmmTMB(Beh_per_h ~ FEAR_25       + TotalNestling + Parasitized + (1|PasturePatchYear),  data=df, family="gaussian")
  FEARPasture               = glmmTMB(Beh_per_h ~ FEAR_Pasture  + TotalNestling + Parasitized + (1|PasturePatchYear),  data=df, family="gaussian")
  CSG5                      = glmmTMB(Beh_per_h ~ CSG_5         + TotalNestling + Parasitized + (1|PasturePatchYear),  data=df, family="gaussian")
  CSG25                     = glmmTMB(Beh_per_h ~ CSG_25        + TotalNestling + Parasitized + (1|PasturePatchYear),  data=df, family="gaussian")
  CSGPasture                = glmmTMB(Beh_per_h ~ CSG_Pasture   + TotalNestling + Parasitized + (1|PasturePatchYear),  data=df, family="gaussian")
  WSG5                      = glmmTMB(Beh_per_h ~ WSG_5         + TotalNestling + Parasitized + (1|PasturePatchYear),  data=df, family="gaussian")
  WSG25                     = glmmTMB(Beh_per_h ~ WSG_25        + TotalNestling + Parasitized + (1|PasturePatchYear),  data=df, family="gaussian")
  WSGPasture                = glmmTMB(Beh_per_h ~ WSG_Pasture   + TotalNestling + Parasitized + (1|PasturePatchYear),  data=df, family="gaussian")
  Forb5                     = glmmTMB(Beh_per_h ~ Forb_5        + TotalNestling + Parasitized + (1|PasturePatchYear),  data=df, family="gaussian")
  Forb25                    = glmmTMB(Beh_per_h ~ Forb_25       + TotalNestling + Parasitized + (1|PasturePatchYear),  data=df, family="gaussian")
  ForbPasture               = glmmTMB(Beh_per_h ~ Forb_Pasture  + TotalNestling + Parasitized + (1|PasturePatchYear),  data=df, family="gaussian")
  Robel5                    = glmmTMB(Beh_per_h ~ Robel_5       + TotalNestling + Parasitized + (1|PasturePatchYear),  data=df, family="gaussian")
  Robel25                   = glmmTMB(Beh_per_h ~ Robel_25      + TotalNestling + Parasitized + (1|PasturePatchYear),  data=df, family="gaussian")
  RobelPasture              = glmmTMB(Beh_per_h ~ Robel_Pasture + TotalNestling + Parasitized + (1|PasturePatchYear),  data=df, family="gaussian")
  
  mods=list(Null,   FEAR5,   FEAR25,    FEARPasture,   CSG5,     CSG25,      CSGPasture,   WSG5,   WSG25,   WSGPasture,Forb5,   Forb25,   ForbPasture,   Robel5,   Robel25,  RobelPasture)  
  names=c( "Null", "FEAR5", "FEAR25",  "FEARPasture", "CSG5",   "CSG25",    "CSGPasture", "WSG5", "WSG25", "WSGPasture","Forb5", "Forb25", "ForbPasture", "Robel5", "Robel25", "RobelPasture")
  
  print(aictab(cand.set = mods, modnames = names,second.ord = FALSE), digits = 4) }

Veg_Mods_DICK(ProvDataSession)


####4c. Impact of BHCO on provisioning rates####

Nuisance_Mods_BHCO= function(df) {
  Null                   = glmmTMB(PercentTime ~ 1         ,  data=df, family="beta_family")
  TimeofDay              = glmmTMB(PercentTime ~ FilmStart ,  data=df, family="beta_family")
  Date                   = glmmTMB(PercentTime ~ OrdDate   ,  data=df, family="beta_family")
  Age                    = glmmTMB(PercentTime ~ AvgAgeDays,  data=df, family="beta_family")
  
  mods=list(Null,   TimeofDay,   Date,   Age)  
  names=c( "Null", "TimeofDay", "Date", "Age")
  
  print(aictab(cand.set = mods, modnames = names,second.ord = FALSE), digits = 4) }

Nuisance_Mods_BHCO(BegData)


NestContents_Mods_BHCO= function(df) {
  Null                   = glmmTMB(PercentTime ~1                + FilmStart,  data=df, family="beta_family")
  TotNestling            = glmmTMB(PercentTime ~ TotalNestling   + FilmStart,  data=df, family="beta_family")
  PropBHCO               = glmmTMB(PercentTime ~ propBHCO        + FilmStart,  data=df, family="beta_family")
  NumBHCO                = glmmTMB(PercentTime ~ XBHCO           + FilmStart,  data=df, family="beta_family")
  PresBHCO               = glmmTMB(PercentTime ~ Parasitized     + FilmStart,  data=df, family="beta_family")
  NestlingAge            = glmmTMB(PercentTime ~ AvgAgeDays      + FilmStart,  data=df, family="beta_family")
  
  mods=list(Null,   TotNestling,   PropBHCO,    NumBHCO,   PresBHCO,   NestlingAge)  
  names=c( "Null", "TotNestling", "PropBHCO",  "NumBHCO", "PresBHCO", "NestlingAge")
  
  print(aictab(cand.set = mods, modnames = names,second.ord = FALSE), digits = 4) }

NestContents_Mods_BHCO(BegData)



####4d. Nest survival analysis#### Jaime will need to do 

####4e. Impact of vegetation on parasitism

summary(as.factor(NestChecks$Year))
summary(as.factor(NestChecks$Stage))
summary(as.factor(NestChecks$Species))


Model1=glm(MaxBHCO~FEAR_Pasture, data=NestChecks,family="poisson")
summary(Model1)

names(NestChecks)

Veg_Mods_Checks = function(df) {
  Null                      = glmmTMB(MaxBHCO ~ 1            + (1|PasturePatchYear),  data=df, family="gaussian")
  FEAR5                     = glmmTMB(MaxBHCO ~ FEAR_5       + (1|PasturePatchYear),  data=df, family="gaussian")
  FEAR25                    = glmmTMB(MaxBHCO ~ FEAR_25      + (1|PasturePatchYear),  data=df, family="gaussian")
  FEARPasture               = glmmTMB(MaxBHCO ~ FEAR_Pasture + (1|PasturePatchYear),  data=df, family="gaussian")
  CSG5                      = glmmTMB(MaxBHCO ~ CSG_5        + (1|PasturePatchYear),  data=df, family="gaussian")
  CSG25                     = glmmTMB(MaxBHCO ~ CSG_25       + (1|PasturePatchYear),  data=df, family="gaussian")
  CSGPasture                = glmmTMB(MaxBHCO ~ CSG_Pasture  + (1|PasturePatchYear),  data=df, family="gaussian")
  WSG5                      = glmmTMB(MaxBHCO ~ WSG_5        + (1|PasturePatchYear),  data=df, family="gaussian")
  WSG25                     = glmmTMB(MaxBHCO ~ WSG_25       + (1|PasturePatchYear),  data=df, family="gaussian")
  WSGPasture                = glmmTMB(MaxBHCO ~ WSG_Pasture  + (1|PasturePatchYear),  data=df, family="gaussian")
  Forb5                     = glmmTMB(MaxBHCO ~ Forb_5       + (1|PasturePatchYear),  data=df, family="gaussian")
  Forb25                    = glmmTMB(MaxBHCO ~ Forb_25      + (1|PasturePatchYear),  data=df, family="gaussian")
  ForbPasture               = glmmTMB(MaxBHCO ~ Forb_Pasture + (1|PasturePatchYear),  data=df, family="gaussian")
  Robel5                    = glmmTMB(MaxBHCO ~ Robel_5      + (1|PasturePatchYear),  data=df, family="gaussian")
  Robel25                   = glmmTMB(MaxBHCO ~ Robel_25     + (1|PasturePatchYear),  data=df, family="gaussian")
  RobelPasture              = glmmTMB(MaxBHCO ~ Robel_Pasture + (1|PasturePatchYear),  data=df, family="gaussian")
  
  mods=list(Null,   FEAR5,   FEAR25,    FEARPasture,   CSG5,     CSG25,      CSGPasture,   WSG5,   WSG25,   WSGPasture,Forb5,   Forb25,   ForbPasture,   Robel5,   Robel25,  RobelPasture)  
  names=c( "Null", "FEAR5", "FEAR25",  "FEARPasture", "CSG5",   "CSG25",    "CSGPasture", "WSG5", "WSG25", "WSGPasture","Forb5", "Forb25", "ForbPasture", "Robel5", "Robel25", "RobelPasture")
  
  print(aictab(cand.set = mods, modnames = names,second.ord = FALSE), digits = 4) }

Veg_Mods_Checks(NestChecks)

# trying Robel in all models
Veg_Mods_Checks2 = function(df) {
  Null                      = glmmTMB(MaxBHCO ~ Robel_5       + Robel_5 + (1|PasturePatchYear),  data=df, family="gaussian")
  FEAR5                     = glmmTMB(MaxBHCO ~ FEAR_5        + Robel_5 + (1|PasturePatchYear),  data=df, family="gaussian")
  FEAR25                    = glmmTMB(MaxBHCO ~ FEAR_25       + Robel_5 + (1|PasturePatchYear),  data=df, family="gaussian")
  FEARPasture               = glmmTMB(MaxBHCO ~ FEAR_Pasture  + Robel_5 + (1|PasturePatchYear),  data=df, family="gaussian")
  CSG5                      = glmmTMB(MaxBHCO ~ CSG_5         + Robel_5 + (1|PasturePatchYear),  data=df, family="gaussian")
  CSG25                     = glmmTMB(MaxBHCO ~ CSG_25        + Robel_5 + (1|PasturePatchYear),  data=df, family="gaussian")
  CSGPasture                = glmmTMB(MaxBHCO ~ CSG_Pasture   + Robel_5 + (1|PasturePatchYear),  data=df, family="gaussian")
  WSG5                      = glmmTMB(MaxBHCO ~ WSG_5         + Robel_5 + (1|PasturePatchYear),  data=df, family="gaussian")
  WSG25                     = glmmTMB(MaxBHCO ~ WSG_25        + Robel_5 + (1|PasturePatchYear),  data=df, family="gaussian")
  WSGPasture                = glmmTMB(MaxBHCO ~ WSG_Pasture   + Robel_5 + (1|PasturePatchYear),  data=df, family="gaussian")
  Forb5                     = glmmTMB(MaxBHCO ~ Forb_5        + Robel_5 + (1|PasturePatchYear),  data=df, family="gaussian")
  Forb25                    = glmmTMB(MaxBHCO ~ Forb_25       + Robel_5 + (1|PasturePatchYear),  data=df, family="gaussian")
  ForbPasture               = glmmTMB(MaxBHCO ~ Forb_Pasture  + Robel_5 + (1|PasturePatchYear),  data=df, family="gaussian")

  mods=list(Null,   FEAR5,   FEAR25,    FEARPasture,   CSG5,     CSG25,      CSGPasture,   WSG5,   WSG25,   WSGPasture,Forb5,   Forb25,   ForbPasture)  
  names=c( "Null", "FEAR5", "FEAR25",  "FEARPasture", "CSG5",   "CSG25",    "CSGPasture", "WSG5", "WSG25", "WSGPasture","Forb5", "Forb25", "ForbPasture")
  
  print(aictab(cand.set = mods, modnames = names,second.ord = FALSE), digits = 4) }

Veg_Mods_Checks2(NestChecks)


