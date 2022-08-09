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
setwd("~/Documents/GitHub/NestCameraAnalysis/2_ESA Provisioning Analysis")

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

theme_line<-function() {
  theme(text=element_text(size=10),
        axis.title.y=element_text(margin=margin (b = -1),face="bold", size=14, family="Arial"),
        axis.text=element_text(size=10,color="black"),
        axis.title.x=element_text(margin=margin (b = -1),face="bold", size=14, family="Arial"),
        axis.line=element_line(color="black",size=0.75),
        panel.background=element_rect("snow2"),
        panel.grid=element_blank(),
        legend.title=element_blank(),
        legend.text=element_text(size=12, color="black"))
}

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
           Robel_5,Robel_25,Robel_Pasture,Arthperh)%>%
  summarize(BehaviorCount = n())%>%
  filter(TotalNestling>0)%>%
  mutate(AvgAgeDays=as.numeric(AvgAgeDays))%>%
  mutate(AvgAgeDays=as.numeric(AvgAgeDays))%>%
  mutate(AvgAgeDays = ifelse(is.na(AvgAgeDays), 
                            mean(AvgAgeDays, na.rm = TRUE), 
                            AvgAgeDays))%>%
  mutate(Beh_per_h       = BehaviorCount/(FilmDuration))%>%
  mutate(Beh_per_h_chick = Beh_per_h/TotalNestling)%>%
  mutate(Arthperh_chick = Arthperh/TotalNestling)%>%
  unite("PastureYear",c(Pasture,Year), sep="_", remove = FALSE)%>%
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
  mutate(AvgAgeDays=as.numeric(AvgAgeDays))%>%
  mutate(AvgAgeDays = ifelse(is.na(AvgAgeDays), 
                             mean(AvgAgeDays, na.rm = TRUE), 
                             AvgAgeDays))%>%  filter(BehaviorCode=="Provisioning")%>%
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
  mutate(PercentTime=BegDuration/(FilmDuration*60*60))%>%
  mutate(AvgAgeDays=as.numeric(AvgAgeDays))%>%
  mutate(AvgAgeDays = ifelse(is.na(AvgAgeDays), 
                             mean(AvgAgeDays, na.rm = TRUE), 
                             AvgAgeDays))%>%
  filter(TotalNestling>0)%>%
  filter(PercentTime>0)

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
length(unique(ProvDataSession$NestIDSession))

  #Sample Size - How many nest filming sessions (Will change when new data is inputted)
length(unique(ProvDataSession$NestID)) #Sample Size - How many nests (Will change when new data is inputted)


length(unique(PB$NestIDSession)) #Sample Size - How many nest filming sessions (Will change when new data is inputted)
length(unique(PB$NestID)) #Sample Size - How many nests (Will change when new data is inputted)


#Within the restricted provisioning data (which omits weird nests where you can't see anything)

#a) Number of filming sessions
samplesize=ProvDataSession%>%
  group_by(Year,NestIDSession)%>%
  summarize_at(vars(OrdDate),first)%>%
  group_by(Year)%>%
  tally()%>%
  spread(Year, n)
samplesize

#b) Number of nests filmed
samplesize2=ProvDataSession%>%
  group_by(Year,NestID)%>%
  summarize_at(vars(OrdDate),first)%>%
  group_by(Year)%>%
  tally()%>%
  spread(Year, n)
samplesize2

#Within the full dataset that is currently done
#a) Number of filming sessions
samplesize3=PB%>%
  filter(Species=="DICK")%>%
  group_by(Year,NestIDSession)%>%
  summarize_at(vars(OrdDate),first)%>%
  group_by(Year)%>%
  tally()%>%
  spread(Year, n)
samplesize3

#b) Number of nests filmed
samplesize4=PB%>%
  filter(Species=="DICK")%>%
  group_by(Year,NestID)%>%
  summarize_at(vars(OrdDate),first)%>%
  group_by(Year)%>%
  tally()%>%
  spread(Year, n)
samplesize4


#Most common food items and sizes
NestlingDietSize=PB%>%
  filter(grepl("Provisioning", BehaviorCode),na.rm=TRUE)%>% 
  filter(grepl("DICK",Species),na.rm=TRUE)%>%
  filter(!grepl('0-49', NestVisability))%>% 
  replace_na(list(ArthSize="Unknown"))%>%
  drop_na("ArthID")%>%
  group_by(Species,ArthSize,ArthID)%>%
  summarize(count=n())%>%
  ungroup()%>%
  mutate(countsum=sum(count))%>%
  mutate(percent=count/countsum)
NestlingDietSize

NestlingDietOrder=PB%>%
  filter(grepl("Provisioning", BehaviorCode),na.rm=TRUE)%>% 
  filter(grepl("DICK",Species),na.rm=TRUE)%>%
  filter(!grepl('0-49', NestVisability))%>% 
  replace_na(list(ArthSize="Unknown"))%>%
  drop_na("ArthID")%>%
  group_by(Species,ArthID)%>%
  summarize(count=n())%>%
  ungroup()%>%
  mutate(countsum=sum(count))%>%
  mutate(percent=count/countsum)
NestlingDietOrder

#Calculating means/sds/etc for major variables

means_sds=ProvDataSession%>%
  group_by(Species)%>%
  summarize_at(vars(Beh_per_h,FilmDuration,Parasitized,XBHCO,XHosts,TotalNestling),c(mean="mean",sd="sd"),na.rm=TRUE)%>%
  pivot_longer(Beh_per_h_mean:TotalNestling_sd)

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
  PresBHCO               = glmmTMB(Beh_per_h ~ as.factor(Parasitized)     + (1|PasturePatchYear),  data=df, family="gaussian")
  NestlingAge            = glmmTMB(Beh_per_h ~ AvgAgeDays      + (1|PasturePatchYear),  data=df, family="gaussian")
  
  mods=list(Null,   TotalNestling,   PropBHCO,    NumBHCO,   PresBHCO,   NestlingAge)  
  names=c( "Null", "TotalNestling", "PropBHCO",  "NumBHCO", "PresBHCO", "NestlingAge")
  
  print(aictab(cand.set = mods, modnames = names,second.ord = FALSE), digits = 4) }

NestContents_Mods_DICK(ProvDataSession)

Veg_Mods_DICK = function(df) {
  Null                      = glmmTMB(Beh_per_h ~ 1             + TotalNestling + as.factor(Parasitized) + AvgAgeDays + (1|PasturePatchYear),  data=df, family="gaussian")
  FEAR5                     = glmmTMB(Beh_per_h ~ FEAR_5        + TotalNestling + as.factor(Parasitized) + AvgAgeDays + (1|PasturePatchYear),  data=df, family="gaussian")
  FEAR25                    = glmmTMB(Beh_per_h ~ FEAR_25       + TotalNestling + as.factor(Parasitized) + AvgAgeDays + (1|PasturePatchYear),  data=df, family="gaussian")
  FEARPasture               = glmmTMB(Beh_per_h ~ FEAR_Pasture  + TotalNestling + as.factor(Parasitized) + AvgAgeDays + (1|PasturePatchYear),  data=df, family="gaussian")
  CSG5                      = glmmTMB(Beh_per_h ~ CSG_5         + TotalNestling + as.factor(Parasitized) + AvgAgeDays + (1|PasturePatchYear),  data=df, family="gaussian")
  CSG25                     = glmmTMB(Beh_per_h ~ CSG_25        + TotalNestling + as.factor(Parasitized) + AvgAgeDays + (1|PasturePatchYear),  data=df, family="gaussian")
  CSGPasture                = glmmTMB(Beh_per_h ~ CSG_Pasture   + TotalNestling + as.factor(Parasitized) + AvgAgeDays + (1|PasturePatchYear),  data=df, family="gaussian")
  WSG5                      = glmmTMB(Beh_per_h ~ WSG_5         + TotalNestling + as.factor(Parasitized) + AvgAgeDays + (1|PasturePatchYear),  data=df, family="gaussian")
  WSG25                     = glmmTMB(Beh_per_h ~ WSG_25        + TotalNestling + as.factor(Parasitized) + AvgAgeDays + (1|PasturePatchYear),  data=df, family="gaussian")
  WSGPasture                = glmmTMB(Beh_per_h ~ WSG_Pasture   + TotalNestling + as.factor(Parasitized) + AvgAgeDays + (1|PasturePatchYear),  data=df, family="gaussian")
  Forb5                     = glmmTMB(Beh_per_h ~ Forb_5        + TotalNestling + as.factor(Parasitized) + AvgAgeDays + (1|PasturePatchYear),  data=df, family="gaussian")
  Forb25                    = glmmTMB(Beh_per_h ~ Forb_25       + TotalNestling + as.factor(Parasitized) + AvgAgeDays + (1|PasturePatchYear),  data=df, family="gaussian")
  ForbPasture               = glmmTMB(Beh_per_h ~ Forb_Pasture  + TotalNestling + as.factor(Parasitized) + AvgAgeDays + (1|PasturePatchYear),  data=df, family="gaussian")
  
  mods=list(Null,   FEAR5,   FEAR25,    FEARPasture,   CSG5,     CSG25,      CSGPasture,   WSG5,   WSG25,   WSGPasture,Forb5,   Forb25,   ForbPasture)  
  names=c( "Null", "FEAR5", "FEAR25",  "FEARPasture", "CSG5",   "CSG25",    "CSGPasture", "WSG5", "WSG25", "WSGPasture","Forb5", "Forb25", "ForbPasture")
  
  print(aictab(cand.set = mods, modnames = names,second.ord = FALSE), digits = 4) }

Veg_Mods_DICK(ProvDataSession)

Prov_Top = glmmTMB(Beh_per_h ~ Robel_25 + CSG_25 + TotalNestling + as.factor(Parasitized) + AvgAgeDays + (1|PasturePatchYear),  data=ProvDataSession, family="gaussian")
summary(Prov_Top)

Prov_Top = glmmTMB(Beh_per_h ~ + CSG_25 + TotalNestling + as.factor(Parasitized) + AvgAgeDays + (1|PasturePatchYear),  data=ProvDataSession, family="gaussian")
summary(Prov_Top)


#Nest contents graphs
#AvgAgeDays
summary(ProvDataSession$AvgAgeDays)

Age_Top = glmmTMB(Beh_per_h ~ AvgAgeDays + (1|PasturePatchYear),  data=ProvDataSession, family="gaussian")

ggpredict(Age_Top,terms=c("AvgAgeDays[3,4,5,6,7,8,9]"),ci.lvl=0.85, back.transform=TRUE, append=TRUE)
Age_Pred = as.data.frame(ggpredict(Age_Top,terms=c("AvgAgeDays[3,4,5,6,7,8,9]"),ci.lvl=0.85, back.transform=TRUE, append=TRUE)
)#turns predictions into a dataframe that we can more easily manipulate
colnames(Age_Pred)=c("AvgAgeDays", "Predicted", "SE", "Lower","Upper","group") #renames columns 
Age_Pred

dodge = position_dodge(width=20) 
Age_Plot=ggplot(data=Age_Pred, y=Predicted, x=AvgAgeDays)+  
  geom_smooth(aes(x=AvgAgeDays, y=Predicted, ymin=Lower, ymax=Upper),color="black", fill="goldenrod4",stat="identity")+
  #scale_fill_manual(name = "Method", labels = c("Sweep", "Vacuum"), values=c("turquoise4","orange3"))+ 
  #scale_color_manual(name = "Method", labels = c("Sweep", "Vacuum"), values=c("turquoise4","orange3")) +
  scale_x_continuous(limits=c(3,9),expand=c(0,0))+
  scale_y_continuous(limits = c(0,15), breaks =c(0,5,10,15), expand = c(0, 0)) +
  #annotate(geom="text", x=1.9, y=51, label= "*",size=12)+
  theme_line()+
  labs(y = "Prov per h", x="Avg Age (Days)")+ 
  ggtitle("")
print(Age_Plot)

#ggsave(Ants_Robel_Plot, filename = "Ants_Robel_Plot.png",  bg = "transparent", scale = 1, width = 6.5, height = 9, units = c("in"),dpi = 300)


#TotalNestling

Nestling_Top = glmmTMB(Beh_per_h ~ TotalNestling + (1|PasturePatchYear),  data=ProvDataSession, family="gaussian")
summary(Nestling_Top)
summary(ProvDataSession$TotalNestling)

ggpredict(Nestling_Top,terms=c("TotalNestling[1,2,3,4,5,6]"),ci.lvl=0.85, back.transform=TRUE, append=TRUE)
NumNestling_Pred = as.data.frame(ggpredict(Nestling_Top,terms=c("TotalNestling[1,2,3,4,5,6]"),ci.lvl=0.85, back.transform=TRUE, append=TRUE))
#turns predictions into a dataframe that we can more easily manipulate
colnames(NumNestling_Pred)=c("TotalNestling", "Predicted", "SE", "Lower","Upper","group") #renames columns 
NumNestling_Pred

NumNestling_Plot=ggplot(data=NumNestling_Pred, y=Predicted, x=TotalNestling)+  
  geom_smooth(aes(x=TotalNestling, y=Predicted, ymin=Lower, ymax=Upper),color="black", fill="maroon",stat="identity")+
  #scale_fill_manual(name = "Method", labels = c("Sweep", "Vacuum"), values=c("turquoise4","orange3"))+ 
  #scale_color_manual(name = "Method", labels = c("Sweep", "Vacuum"), values=c("turquoise4","orange3")) +
  scale_x_continuous(limits=c(1,6),expand=c(0,0))+
  scale_y_continuous(limits = c(0,15), breaks =c(0,5,10,15), expand = c(0, 0)) +
  #annotate(geom="text", x=1.9, y=51, label= "*",size=12)+
  theme_line()+
  labs(y = "Prov per h", x="Total Nestlings")+ 
  ggtitle("")
print(NumNestling_Plot)


#CSG_25

summary(ProvDataSession$CSG_25)

CSG_Top = glmmTMB(Beh_per_h ~ CSG_25 + (1|PasturePatchYear),  data=ProvDataSession, family="gaussian")
summary(CSG_Top)

ggpredict(CSG_Top,terms=c("CSG_25[0,10,20,30,40,50,60,70,80,90]"),ci.lvl=0.85, back.transform=TRUE, append=TRUE)
CSG_Pred = as.data.frame(ggpredict(CSG_Top,terms=c("CSG_25[0,10,20,30,40,50,60,70,80,90]"),ci.lvl=0.85, back.transform=TRUE, append=TRUE)
)
#turns predictions into a dataframe that we can more easily manipulate
colnames(CSG_Pred)=c("CSG_25", "Predicted", "SE", "Lower","Upper") #renames columns 
CSG_Pred

CSG_Plot=ggplot(data=CSG_Pred, y=Predicted, x=CSG_25)+  
  geom_smooth(aes(x=CSG_25, y=Predicted, ymin=Lower, ymax=Upper),color="black", fill="dodgerblue", stat="identity")+
  #scale_fill_manual(name = "Method", labels = c("Sweep", "Vacuum"), values=c("turquoise4","orange3"))+ 
  #scale_color_manual(name = "Method", labels = c("Sweep", "Vacuum"), values=c("turquoise4","orange3")) +
  scale_x_continuous(limits=c(10,90),expand=c(0,0))+
  scale_y_continuous(limits = c(0,15), breaks =c(0,5,10,15), expand = c(0, 0)) +
  #annotate(geom="text", x=1.9, y=51, label= "*",size=12)+
  theme_line()+
  labs(y = "Prov per h", x="CSG percent cover within 25m")+ 
  ggtitle("")
print(CSG_Plot)


#Robel_25

summary(ProvDataSession$Robel_25)


#Parasitized
Paras_Top = glmmTMB(Beh_per_h ~ as.factor(Parasitized) + (1|PasturePatchYear),  data=ProvDataSession, family="gaussian")
summary(Paras_Top)

ggpredict(Paras_Top,terms=c("Parasitized"),ci.lvl=0.85, back.transform=TRUE, append=TRUE)
Paras_Pred = as.data.frame(ggpredict(Paras_Top,terms=c("Parasitized"),ci.lvl=0.85, back.transform=TRUE, append=TRUE))
#turns predictions into a dataframe that we can more easily manipulate

colnames(Paras_Pred)=c("Parasitized", "Predicted", "SE", "Lower","Upper","group") #renames columns 
Paras_Pred

Paras_Plot=ggplot(data=Paras_Pred, y=Predicted, x=as.factor(Parasitized))+  
  geom_bar(aes(x=as.factor(Parasitized), y=Predicted, fill=as.factor(Parasitized),alpha=.1),  stat="identity")+
  geom_errorbar(aes(x = as.factor(Parasitized), ymin = Lower, ymax = Upper, group = as.factor(Parasitized)), position = dodge, width = 0.2)+
  scale_fill_manual(values=c("aquamarine4","salmon2")) +
  
  #scale_fill_manual(name = "Method", labels = c("Sweep", "Vacuum"), values=c("turquoise4","orange3"))+ 
  #scale_color_manual(name = "Method", labels = c("Sweep", "Vacuum"), values=c("turquoise4","orange3")) +
  scale_x_discrete(labels=c("Not Parasitized","Parasitized"))+
  scale_y_continuous(limits = c(0,10), breaks =c(0,5,10), expand = c(0, 0)) +
  #annotate(geom="text", x=1.9, y=51, label= "*",size=12)+
  theme(text=element_text(size=14),
        axis.title.y=element_text(face="bold", size=14),
        #axis.title.x=element_blank(),
        axis.text=element_text(size=10,color="black"),
        axis.line=element_line(color="black",size=0.75),
        panel.background=element_rect("snow2"),
        panel.grid=element_blank(),
        legend.position="none",
        axis.ticks.x = element_blank())+
  ggtitle("")+
  labs(y="Prov per h",x="Parasitized")
print(Paras_Plot)



####4c. Impact of BHCO on begging rates####

Nuisance_Mods_BHCO= function(df) {
  Null                   = glmmTMB(PercentTime ~ 1         ,  data=df, family="beta_family")
  TimeofDay              = glmmTMB(PercentTime ~ FilmStart ,  data=df, family="beta_family")
  Date                   = glmmTMB(PercentTime ~ OrdDate   ,  data=df, family="beta_family")

  mods=list(Null,   TimeofDay,   Date)  
  names=c( "Null", "TimeofDay", "Date")
  
  print(aictab(cand.set = mods, modnames = names,second.ord = FALSE), digits = 4) }

Nuisance_Mods_BHCO(BegData)


NestContents_Mods_BHCO= function(df) {
  Null                   = glmmTMB(PercentTime ~ 1               + FilmStart,  data=df, family="beta_family")
  TotNestling            = glmmTMB(PercentTime ~ TotalNestling   + FilmStart,  data=df, family="beta_family")
  PropBHCO               = glmmTMB(PercentTime ~ propBHCO        + FilmStart,  data=df, family="beta_family")
  NumBHCO                = glmmTMB(PercentTime ~ XBHCO           + FilmStart,  data=df, family="beta_family")
  PresBHCO               = glmmTMB(PercentTime ~ Parasitized     + FilmStart,  data=df, family="beta_family")
  NestlingAge            = glmmTMB(PercentTime ~ AvgAgeDays      + FilmStart,  data=df, family="beta_family")
  
  mods=list(Null,   TotNestling,   PropBHCO,    NumBHCO,   PresBHCO,   NestlingAge)  
  names=c( "Null", "TotNestling", "PropBHCO",  "NumBHCO", "PresBHCO", "NestlingAge")
  
  print(aictab(cand.set = mods, modnames = names,second.ord = FALSE), digits = 4) }

NestContents_Mods_BHCO(BegData)


  #Parasitized
Paras_Top_Beg = glmmTMB(PercentTime ~ XBHCO + FilmStart,  data=BegData, family="beta_family")
summary(Paras_Top_Beg)

ggpredict(Paras_Top_Beg,terms=c("XBHCO[0,1,2,3,4]"),ci.lvl=0.85, back.transform=TRUE, append=TRUE)
Paras_Pred_Beg = as.data.frame(ggpredict(Paras_Top_Beg,terms=c("XBHCO[0,1,2,3,4]"),ci.lvl=0.85, back.transform=TRUE, append=TRUE))
#turns predictions into a dataframe that we can more easily manipulate

colnames(Paras_Pred_Beg)=c("NumBHCO", "Predicted", "SE", "Lower","Upper","group") #renames columns 
Paras_Pred_Beg

Paras_Plot_Beg=ggplot(data=Paras_Pred_Beg, y=Predicted*100, x=NumBHCO)+  
  geom_smooth(aes(x=NumBHCO, y=Predicted*100, ymin=Lower*100, ymax=Upper*100),color="black", fill="salmon4", stat="identity")+
  #scale_fill_manual(name = "Method", labels = c("Sweep", "Vacuum"), values=c("turquoise4","orange3"))+ 
  #scale_color_manual(name = "Method", labels = c("Sweep", "Vacuum"), values=c("turquoise4","orange3")) +
  scale_x_continuous(limits=c(0,4),expand=c(0,0))+
 # scale_y_continuous(limits = c(0,15), breaks =c(0,5,10,15), expand = c(0, 0)) +
  #annotate(geom="text", x=1.9, y=51, label= "*",size=12)+
  theme_line()+
  ggtitle("")+
  labs(y="% Time Spent Begging",x="Number of BHCO")
print(Paras_Plot_Beg)




####4d. Nest survival analysis#### Jaime will need to do ####

####4e. Impact of vegetation on parasitism####

summary(as.factor(NestChecks$Year))
summary(as.factor(NestChecks$Stage))
summary(as.factor(NestChecks$Species))


Model1=glm(MaxBHCO~FEAR_Pasture, data=NestChecks,family="poisson")
summary(Model1)

names(NestChecks)

Veg_Mods_Checks = function(df) {
  Null                      = glmmTMB(MaxBHCO ~ 1            + (1|PasturePatchYear),  data=df, family="poisson")
  FEAR5                     = glmmTMB(MaxBHCO ~ FEAR_5       + (1|PasturePatchYear),  data=df, family="poisson")
  FEAR25                    = glmmTMB(MaxBHCO ~ FEAR_25      + (1|PasturePatchYear),  data=df, family="poisson")
  FEARPasture               = glmmTMB(MaxBHCO ~ FEAR_Pasture + (1|PasturePatchYear),  data=df, family="poisson")
  CSG5                      = glmmTMB(MaxBHCO ~ CSG_5        + (1|PasturePatchYear),  data=df, family="poisson")
  CSG25                     = glmmTMB(MaxBHCO ~ CSG_25       + (1|PasturePatchYear),  data=df, family="poisson")
  CSGPasture                = glmmTMB(MaxBHCO ~ CSG_Pasture  + (1|PasturePatchYear),  data=df, family="poisson")
  WSG5                      = glmmTMB(MaxBHCO ~ WSG_5        + (1|PasturePatchYear),  data=df, family="poisson")
  WSG25                     = glmmTMB(MaxBHCO ~ WSG_25       + (1|PasturePatchYear),  data=df, family="poisson")
  WSGPasture                = glmmTMB(MaxBHCO ~ WSG_Pasture  + (1|PasturePatchYear),  data=df, family="poisson")
  Forb5                     = glmmTMB(MaxBHCO ~ Forb_5       + (1|PasturePatchYear),  data=df, family="poisson")
  Forb25                    = glmmTMB(MaxBHCO ~ Forb_25      + (1|PasturePatchYear),  data=df, family="poisson")
  ForbPasture               = glmmTMB(MaxBHCO ~ Forb_Pasture + (1|PasturePatchYear),  data=df, family="poisson")

  mods=list(Null,   FEAR5,   FEAR25,    FEARPasture,   CSG5,     CSG25,      CSGPasture,   WSG5,   WSG25,   WSGPasture,Forb5,   Forb25,   ForbPasture)  
  names=c( "Null", "FEAR5", "FEAR25",  "FEARPasture", "CSG5",   "CSG25",    "CSGPasture", "WSG5", "WSG25", "WSGPasture","Forb5", "Forb25", "ForbPasture")
  
  print(aictab(cand.set = mods, modnames = names,second.ord = FALSE), digits = 4) }

Veg_Mods_Checks(NestChecks)



Forb_Top_Checks = glmmTMB(MaxBHCO ~ Forb_5 + (1|PasturePatchYear),  data=NestChecks, family="poisson")
summary(Forb_Top_Checks)

ggpredict(Forb_Top_Checks,terms=c("Forb_5[0,10,20,30,40,50,60,70,80,90]"),ci.lvl=0.85, back.transform=TRUE, append=TRUE)
Forb_Pred_Checks = as.data.frame(ggpredict(Forb_Top_Checks,terms=c("Forb_5[0,10,20,30,40,50,60,70,80,90]"),ci.lvl=0.85, back.transform=TRUE, append=TRUE)
)
#turns predictions into a dataframe that we can more easily manipulate
colnames(Forb_Pred_Checks)=c("Forb_5", "Predicted", "SE", "Lower","Upper") #renames columns 
Forb_Pred_Checks

Forb_Plot=ggplot(data=Forb_Pred_Checks, y=Predicted, x=Forb_5)+  
  geom_smooth(aes(x=Forb_5, y=Predicted, ymin=Lower, ymax=Upper),color="black", fill="darkgreen", stat="identity")+
  #scale_fill_manual(name = "Method", labels = c("Sweep", "Vacuum"), values=c("turquoise4","orange3"))+ 
  #scale_color_manual(name = "Method", labels = c("Sweep", "Vacuum"), values=c("turquoise4","orange3")) +
  scale_x_continuous(limits=c(10,90),expand=c(0,0))+
  scale_y_continuous(limits = c(0,1.2), breaks =c(0,.2,4,.6,.8,1), expand = c(0, 0)) +
  #annotate(geom="text", x=1.9, y=51, label= "*",size=12)+
  theme_line()+
  labs(y = "Parasitism Intensity", x="Forb percent cover within 5m")+ 
  ggtitle("")
print(Forb_Plot)


#CSG

CSG_Top_Checks = glmmTMB(MaxBHCO ~ CSG_5 + (1|PasturePatchYear),  data=NestChecks, family="poisson")
summary(CSG_Top_Checks)

ggpredict(CSG_Top_Checks,terms=c("CSG_5[0,10,20,30,40,50,60,70,80,90]"),ci.lvl=0.85, back.transform=TRUE, append=TRUE)
CSG_Pred_Checks = as.data.frame(ggpredict(CSG_Top_Checks,terms=c("CSG_5[0,10,20,30,40,50,60,70,80,90]"),ci.lvl=0.85, back.transform=TRUE, append=TRUE)
)
#turns predictions into a dataframe that we can more easily manipulate
colnames(CSG_Pred_Checks)=c("CSG_5", "Predicted", "SE", "Lower","Upper") #renames columns 
CSG_Pred_Checks

CSG_Plot=ggplot(data=CSG_Pred_Checks, y=Predicted, x=CSG_5)+  
  geom_smooth(aes(x=CSG_5, y=Predicted, ymin=Lower, ymax=Upper),color="black", fill="darkblue", stat="identity")+
  #scale_fill_manual(name = "Method", labels = c("Sweep", "Vacuum"), values=c("turquoise4","orange3"))+ 
  #scale_color_manual(name = "Method", labels = c("Sweep", "Vacuum"), values=c("turquoise4","orange3")) +
  scale_x_continuous(limits=c(10,90),expand=c(0,0))+
  scale_y_continuous(limits = c(0,1.2), breaks =c(0,.2,4,.6,.8,1), expand = c(0, 0)) +
  #annotate(geom="text", x=1.9, y=51, label= "*",size=12)+
  theme_line()+
  labs(y = "Parasitism Intensity", x="CSG percent cover within 5m")+ 
  ggtitle("")
print(CSG_Plot)


####4f. Impact of vegetation and parasitism on amount of arthropods provisioned (size)####
summary(ProvDataSession$Arthperh)
Nuisance_Mods_Arth= function(df) {
  Null                   = glmmTMB(Arthperh ~ 1                 + (1|PasturePatchYear),  data=df, family="gaussian")
  TimeofDay              = glmmTMB(Arthperh ~ FilmStart         + (1|PasturePatchYear),  data=df, family="gaussian")
  Date                   = glmmTMB(Arthperh ~ OrdDate           + (1|PasturePatchYear),  data=df, family="gaussian")
  
  mods=list(Null,   TimeofDay,   Date)  
  names=c( "Null", "TimeofDay", "Date")
  
  print(aictab(cand.set = mods, modnames = names,second.ord = FALSE), digits = 4) }

Nuisance_Mods_Arth(ProvDataSession)


NestContents_Mods_Arth= function(df) {
  Null                   = glmmTMB(Arthperh ~ 1                        + OrdDate + (1|PasturePatchYear),  data=df, family="gaussian")
  TotalNestling          = glmmTMB(Arthperh ~ TotalNestling            + OrdDate + (1|PasturePatchYear),  data=df, family="gaussian")
  PropBHCO               = glmmTMB(Arthperh ~ propBHCO                 + OrdDate + (1|PasturePatchYear),  data=df, family="gaussian")
  NumBHCO                = glmmTMB(Arthperh ~ XBHCO                    + OrdDate + (1|PasturePatchYear),  data=df, family="gaussian")
  PresBHCO               = glmmTMB(Arthperh ~ as.factor(Parasitized)   + OrdDate + (1|PasturePatchYear),  data=df, family="gaussian")
  NestlingAge            = glmmTMB(Arthperh ~ AvgAgeDays               + OrdDate + (1|PasturePatchYear),  data=df, family="gaussian")
  
  mods=list(Null,   TotalNestling,   PropBHCO,    NumBHCO,   PresBHCO,   NestlingAge)  
  names=c( "Null", "TotalNestling", "PropBHCO",  "NumBHCO", "PresBHCO", "NestlingAge")
  
  print(aictab(cand.set = mods, modnames = names,second.ord = FALSE), digits = 4) }

NestContents_Mods_Arth(ProvDataSession)

Veg_Mods_Arth= function(df) {
  Null                      = glmmTMB(Arthperh ~ 1             + TotalNestling + AvgAgeDays + (1|PasturePatchYear),  data=df, family="gaussian")
  FEAR5                     = glmmTMB(Arthperh ~ FEAR_5        + TotalNestling + AvgAgeDays + (1|PasturePatchYear),  data=df, family="gaussian")
  FEAR25                    = glmmTMB(Arthperh ~ FEAR_25       + TotalNestling + AvgAgeDays + (1|PasturePatchYear),  data=df, family="gaussian")
  FEARPasture               = glmmTMB(Arthperh ~ FEAR_Pasture  + TotalNestling + AvgAgeDays + (1|PasturePatchYear),  data=df, family="gaussian")
  CSG5                      = glmmTMB(Arthperh ~ CSG_5         + TotalNestling + AvgAgeDays + (1|PasturePatchYear),  data=df, family="gaussian")
  CSG25                     = glmmTMB(Arthperh ~ CSG_25        + TotalNestling + AvgAgeDays + (1|PasturePatchYear),  data=df, family="gaussian")
  CSGPasture                = glmmTMB(Arthperh ~ CSG_Pasture   + TotalNestling + AvgAgeDays + (1|PasturePatchYear),  data=df, family="gaussian")
  WSG5                      = glmmTMB(Arthperh ~ WSG_5         + TotalNestling + AvgAgeDays + (1|PasturePatchYear),  data=df, family="gaussian")
  WSG25                     = glmmTMB(Arthperh ~ WSG_25        + TotalNestling + AvgAgeDays + (1|PasturePatchYear),  data=df, family="gaussian")
  WSGPasture                = glmmTMB(Arthperh ~ WSG_Pasture   + TotalNestling + AvgAgeDays + (1|PasturePatchYear),  data=df, family="gaussian")
  Forb5                     = glmmTMB(Arthperh ~ Forb_5        + TotalNestling + AvgAgeDays + (1|PasturePatchYear),  data=df, family="gaussian")
  Forb25                    = glmmTMB(Arthperh ~ Forb_25       + TotalNestling + AvgAgeDays + (1|PasturePatchYear),  data=df, family="gaussian")
  ForbPasture               = glmmTMB(Arthperh ~ Forb_Pasture  + TotalNestling + AvgAgeDays + (1|PasturePatchYear),  data=df, family="gaussian")
  
  mods=list(Null,   FEAR5,   FEAR25,    FEARPasture,   CSG5,     CSG25,      CSGPasture,   WSG5,   WSG25,   WSGPasture,Forb5,   Forb25,   ForbPasture)  
  names=c( "Null", "FEAR5", "FEAR25",  "FEARPasture", "CSG5",   "CSG25",    "CSGPasture", "WSG5", "WSG25", "WSGPasture","Forb5", "Forb25", "ForbPasture")
  
  print(aictab(cand.set = mods, modnames = names,second.ord = FALSE), digits = 4) }

Veg_Mods_Arth(ProvDataSession)


#Graphing Arthperh and forb_pasture
Forb_Top_Arth = glmmTMB(Arthperh ~ Forb_Pasture + TotalNestling + AvgAgeDays + (1|PasturePatchYear),  data=ProvDataSession, family="gaussian")
summary(Forb_Top_Arth)

ggpredict(Forb_Top_Arth,terms=c("Forb_Pasture[0,10,20,30,40,50,60,70]"),ci.lvl=0.85, back.transform=TRUE, append=TRUE)
Forb_Pred_Arth = as.data.frame(ggpredict(Forb_Top_Arth,terms=c("Forb_Pasture[0,10,20,30,40,50,60,70]"),ci.lvl=0.85, back.transform=TRUE, append=TRUE)
)
#turns predictions into a dataframe that we can more easily manipulate
colnames(Forb_Pred_Arth)=c("Forb_Pasture", "Predicted", "SE", "Lower","Upper") #renames columns 
Forb_Pred_Arth

Forb_Plot_Arth=ggplot(data=Forb_Pred_Arth, y=Predicted, x=Forb_Pasture)+  
  geom_smooth(aes(x=Forb_Pasture, y=Predicted, ymin=Lower, ymax=Upper),color="black", fill="darkorange", stat="identity")+
  #scale_fill_manual(name = "Method", labels = c("Sweep", "Vacuum"), values=c("turquoise4","orange3"))+ 
  #scale_color_manual(name = "Method", labels = c("Sweep", "Vacuum"), values=c("turquoise4","orange3")) +
  scale_x_continuous(limits=c(0,70),expand=c(0,0))+
  #scale_y_continuous(limits = c(0,1.2), breaks =c(0,.2,4,.6,.8,1), expand = c(0, 0)) +
  #annotate(geom="text", x=1.9, y=51, label= "*",size=12)+
  theme_line()+
  labs(y = "Arthropod Biomass Index per Session", x="Forb percent cover (site)")+ 
  ggtitle("")
print(Forb_Plot_Arth)


#FEAR

summary(ProvDataSession$Forb_Pasture)

FEAR_Top_Arth = glmmTMB(Arthperh ~ FEAR_Pasture + TotalNestling + AvgAgeDays + (1|PasturePatchYear),  data=ProvDataSession, family="gaussian")
summary(FEAR_Top_Arth)

ggpredict(FEAR_Top_Arth,terms=c("FEAR_Pasture[0,10,20,30,40]"),ci.lvl=0.85, back.transform=TRUE, append=TRUE)
FEAR_Pred_Arth = as.data.frame(ggpredict(FEAR_Top_Arth,terms=c("FEAR_Pasture[0,10,20,30,40]"),ci.lvl=0.85, back.transform=TRUE, append=TRUE)
)
#turns predictions into a dataframe that we can more easily manipulate
colnames(FEAR_Pred_Arth)=c("FEAR_Pasture", "Predicted", "SE", "Lower","Upper") #renames columns 
FEAR_Pred_Arth

FEAR_Plot=ggplot(data=FEAR_Pred_Arth, y=Predicted, x=FEAR_Pasture)+  
  geom_smooth(aes(x=FEAR_Pasture, y=Predicted, ymin=Lower, ymax=Upper),color="black", fill="darkgreen", stat="identity")+
  #scale_fill_manual(name = "Method", labels = c("Sweep", "Vacuum"), values=c("turquoise4","orange3"))+ 
  #scale_color_manual(name = "Method", labels = c("Sweep", "Vacuum"), values=c("turquoise4","orange3")) +
  scale_x_continuous(limits=c(0,40),expand=c(0,0))+
  #scale_y_continuous(limits = c(0,1.2), breaks =c(0,.2,4,.6,.8,1), expand = c(0, 0)) +
  #annotate(geom="text", x=1.9, y=51, label= "*",size=12)+
  theme_line()+
  labs(y = "Arthropod Biomass Index per Session", x="FEAR percent cover (site)")+ 
  ggtitle("")
print(FEAR_Plot)



#WSG_pasture


summary(ProvDataSession$WSG_Pasture)

WSG_Top_Arth = glmmTMB(Arthperh ~ WSG_Pasture + TotalNestling + AvgAgeDays + (1|PasturePatchYear),  data=ProvDataSession, family="gaussian")
summary(WSG_Top_Arth)

ggpredict(WSG_Top_Arth,terms=c("WSG_Pasture[0,10,20,30]"),ci.lvl=0.85, back.transform=TRUE, append=TRUE)
WSG_Pred_Arth = as.data.frame(ggpredict(WSG_Top_Arth,terms=c("WSG_Pasture[0,10,20,30]"),ci.lvl=0.85, back.transform=TRUE, append=TRUE)
)
#turns predictions into a dataframe that we can more easily manipulate
colnames(WSG_Pred_Arth)=c("WSG_Pasture", "Predicted", "SE", "Lower","Upper") #renames columns 
WSG_Pred_Arth

WSG_Plot=ggplot(data=WSG_Pred_Arth, y=Predicted, x=WSG_Pasture)+  
  geom_smooth(aes(x=WSG_Pasture, y=Predicted, ymin=Lower, ymax=Upper),color="black", fill="darkblue", stat="identity")+
  #scale_fill_manual(name = "Method", labels = c("Sweep", "Vacuum"), values=c("turquoise4","orange3"))+ 
  #scale_color_manual(name = "Method", labels = c("Sweep", "Vacuum"), values=c("turquoise4","orange3")) +
  scale_x_continuous(limits=c(0,30),expand=c(0,0))+
  #scale_y_continuous(limits = c(0,1.2), breaks =c(0,.2,4,.6,.8,1), expand = c(0, 0)) +
  #annotate(geom="text", x=1.9, y=51, label= "*",size=12)+
  theme_line()+
  labs(y = "Arthropod Biomass Index per Session", x="WSG percent cover (site)")+ 
  ggtitle("")
print(WSG_Plot)

####4f. Impact of vegetation and parasitism on amount of arthropods provisioned (size) PER CHICK ####
summary(ProvDataSession$Arthperh_chick)
Nuisance_Mods_Arth= function(df) {
  Null                   = glmmTMB(log(Arthperh_chick) ~ 1                  + (1|PasturePatchYear), data=df, family="gaussian")
  TimeofDay              = glmmTMB(log(Arthperh_chick) ~ FilmStart          + (1|PasturePatchYear), data=df, family="gaussian")
  Date                   = glmmTMB(log(Arthperh_chick) ~ OrdDate            + (1|PasturePatchYear), data=df, family="gaussian")
 
  mods=list(Null,   TimeofDay,   Date)  
  names=c( "Null", "TimeofDay", "Date")
  
  print(aictab(cand.set = mods, modnames = names,second.ord = FALSE), digits = 4) }

Nuisance_Mods_Arth(subset(ProvDataSession, Arthperh_chick>0))


NestContents_Mods_Arth= function(df) {
  Null                   = glmmTMB(log(Arthperh_chick) ~ 1                      + (1|PasturePatchYear),  data=df, family="gaussian")
  PropBHCO               = glmmTMB(log(Arthperh_chick) ~ propBHCO               + (1|PasturePatchYear),  data=df, family="gaussian")
  NumBHCO                = glmmTMB(log(Arthperh_chick) ~ XBHCO                  + (1|PasturePatchYear),  data=df, family="gaussian")
  PresBHCO               = glmmTMB(log(Arthperh_chick) ~ as.factor(Parasitized) + (1|PasturePatchYear),  data=df, family="gaussian")
  NestlingAge            = glmmTMB(log(Arthperh_chick) ~ AvgAgeDays             + (1|PasturePatchYear),  data=df, family="gaussian")
  
  mods=list(Null,   PropBHCO,    NumBHCO,   PresBHCO,   NestlingAge)  
  names=c( "Null", "PropBHCO",  "NumBHCO", "PresBHCO", "NestlingAge")
  
  print(aictab(cand.set = mods, modnames = names,second.ord = FALSE), digits = 4) }

NestContents_Mods_Arth(subset(ProvDataSession, Arthperh_chick>0))

Veg_Mods_Arth= function(df) {
  Null                      = glmmTMB(log(Arthperh_chick) ~ 1            + AvgAgeDays + (1|PasturePatchYear),  data=df, family="gaussian")
  FEAR5                     = glmmTMB(log(Arthperh_chick) ~ FEAR_5       + AvgAgeDays + (1|PasturePatchYear),  data=df, family="gaussian")
  FEAR25                    = glmmTMB(log(Arthperh_chick) ~ FEAR_25      + AvgAgeDays + (1|PasturePatchYear),  data=df, family="gaussian")
  FEARPasture               = glmmTMB(log(Arthperh_chick) ~ FEAR_Pasture + AvgAgeDays + (1|PasturePatchYear),  data=df, family="gaussian")
  CSG5                      = glmmTMB(log(Arthperh_chick) ~ CSG_5        + AvgAgeDays + (1|PasturePatchYear),  data=df, family="gaussian")
  CSG25                     = glmmTMB(log(Arthperh_chick) ~ CSG_25       + AvgAgeDays + (1|PasturePatchYear),  data=df, family="gaussian")
  CSGPasture                = glmmTMB(log(Arthperh_chick) ~ CSG_Pasture  + AvgAgeDays + (1|PasturePatchYear),  data=df, family="gaussian")
  WSG5                      = glmmTMB(log(Arthperh_chick) ~ WSG_5        + AvgAgeDays + (1|PasturePatchYear),  data=df, family="gaussian")
  WSG25                     = glmmTMB(log(Arthperh_chick) ~ WSG_25       + AvgAgeDays + (1|PasturePatchYear),  data=df, family="gaussian")
  WSGPasture                = glmmTMB(log(Arthperh_chick) ~ WSG_Pasture  + AvgAgeDays + (1|PasturePatchYear),  data=df, family="gaussian")
  Forb5                     = glmmTMB(log(Arthperh_chick) ~ Forb_5       + AvgAgeDays + (1|PasturePatchYear),  data=df, family="gaussian")
  Forb25                    = glmmTMB(log(Arthperh_chick) ~ Forb_25      + AvgAgeDays + (1|PasturePatchYear),  data=df, family="gaussian")
  ForbPasture               = glmmTMB(log(Arthperh_chick) ~ Forb_Pasture + AvgAgeDays + (1|PasturePatchYear),  data=df, family="gaussian")
  
  mods=list(Null,   FEAR5,   FEAR25,    FEARPasture,   CSG5,     CSG25,      CSGPasture,   WSG5,   WSG25,   WSGPasture,Forb5,   Forb25,   ForbPasture)  
  names=c( "Null", "FEAR5", "FEAR25",  "FEARPasture", "CSG5",   "CSG25",    "CSGPasture", "WSG5", "WSG25", "WSGPasture","Forb5", "Forb25", "ForbPasture")
  
  print(aictab(cand.set = mods, modnames = names,second.ord = FALSE), digits = 4) }

Veg_Mods_Arth(subset(ProvDataSession, Arthperh_chick>0))


#Graphing Arthperh and forb_pasture
Forb_Top_Arth_chick = glmmTMB(Arthperh_chick ~ Forb_Pasture  + AvgAgeDays + (1|PasturePatchYear),  data=ProvDataSession, family="gaussian")
summary(Forb_Top_Arth_chick)

ggpredict(Forb_Top_Arth_chick,terms=c("Forb_Pasture[0,10,20,30,40,50,60,70]"),ci.lvl=0.85, back.transform=TRUE, append=TRUE)
Forb_Pred_Arth_chick = as.data.frame(ggpredict(Forb_Top_Arth_chick,terms=c("Forb_Pasture[0,10,20,30,40,50,60,70]"),ci.lvl=0.85, back.transform=TRUE, append=TRUE)
)
#turns predictions into a dataframe that we can more easily manipulate
colnames(Forb_Pred_Arth_chick)=c("Forb_Pasture", "Predicted", "SE", "Lower","Upper") #renames columns 
Forb_Pred_Arth_chick

Forb_Plot_Arth_chick=ggplot(data=Forb_Pred_Arth_chick, y=Predicted, x=Forb_Pasture)+  
  geom_smooth(aes(x=Forb_Pasture, y=Predicted, ymin=Lower, ymax=Upper),color="black", fill="darkorange", stat="identity")+
  #scale_fill_manual(name = "Method", labels = c("Sweep", "Vacuum"), values=c("turquoise4","orange3"))+ 
  #scale_color_manual(name = "Method", labels = c("Sweep", "Vacuum"), values=c("turquoise4","orange3")) +
  scale_x_continuous(limits=c(0,70),expand=c(0,0))+
  #scale_y_continuous(limits = c(0,1.2), breaks =c(0,.2,4,.6,.8,1), expand = c(0, 0)) +
  #annotate(geom="text", x=1.9, y=51, label= "*",size=12)+
  theme_line()+
  labs(y = "Arthropod Biomass Index per Session per Chick", x="Forb percent cover (site)")+ 
  ggtitle("")
print(Forb_Plot_Arth_chick)


#FEAR

summary(ProvDataSession$Forb_Pasture)

FEAR_Top_Arth_chick = glmmTMB(Arthperh_chick ~ FEAR_Pasture + AvgAgeDays + (1|PasturePatchYear),  data=ProvDataSession, family="gaussian")
summary(FEAR_Top_Arth_chick)

ggpredict(FEAR_Top_Arth_chick,terms=c("FEAR_Pasture[0,10,20,30,40]"),ci.lvl=0.85, back.transform=TRUE, append=TRUE)
FEAR_Pred_Arth_chick = as.data.frame(ggpredict(FEAR_Top_Arth_chick,terms=c("FEAR_Pasture[0,10,20,30,40]"),ci.lvl=0.85, back.transform=TRUE, append=TRUE)
)
#turns predictions into a dataframe that we can more easily manipulate
colnames(FEAR_Pred_Arth_chick)=c("FEAR_Pasture", "Predicted", "SE", "Lower","Upper") #renames columns 
FEAR_Pred_Arth_chick

FEAR_Plot_chick=ggplot(data=FEAR_Pred_Arth_chick, y=Predicted, x=FEAR_Pasture)+  
  geom_smooth(aes(x=FEAR_Pasture, y=Predicted, ymin=Lower, ymax=Upper),color="black", fill="darkgreen", stat="identity")+
  #scale_fill_manual(name = "Method", labels = c("Sweep", "Vacuum"), values=c("turquoise4","orange3"))+ 
  #scale_color_manual(name = "Method", labels = c("Sweep", "Vacuum"), values=c("turquoise4","orange3")) +
  scale_x_continuous(limits=c(0,40),expand=c(0,0))+
  #scale_y_continuous(limits = c(0,1.2), breaks =c(0,.2,4,.6,.8,1), expand = c(0, 0)) +
  #annotate(geom="text", x=1.9, y=51, label= "*",size=12)+
  theme_line()+
  labs(y = "Arthropod Biomass Index per Session per Chick", x="FEAR percent cover (site)")+ 
  ggtitle("")
print(FEAR_Plot_chick)



#WSG_pasture


summary(ProvDataSession$WSG_Pasture)

WSG_Top_Arth_chick = glmmTMB(Arthperh_chick ~ WSG_Pasture + AvgAgeDays + (1|PasturePatchYear),  data=ProvDataSession, family="gaussian")
summary(WSG_Top_Arth_chick)

ggpredict(WSG_Top_Arth_chick,terms=c("WSG_Pasture[0,10,20,30]"),ci.lvl=0.85, back.transform=TRUE, append=TRUE)
WSG_Pred_Arth_chick = as.data.frame(ggpredict(WSG_Top_Arth_chick,terms=c("WSG_Pasture[0,10,20,30]"),ci.lvl=0.85, back.transform=TRUE, append=TRUE)
)
#turns predictions into a dataframe that we can more easily manipulate
colnames(WSG_Pred_Arth_chick)=c("WSG_Pasture", "Predicted", "SE", "Lower","Upper") #renames columns 
WSG_Pred_Arth_chick

WSG_Plot_chick=ggplot(data=WSG_Pred_Arth_chick, y=Predicted, x=WSG_Pasture)+  
  geom_smooth(aes(x=WSG_Pasture, y=Predicted, ymin=Lower, ymax=Upper),color="black", fill="darkblue", stat="identity")+
  #scale_fill_manual(name = "Method", labels = c("Sweep", "Vacuum"), values=c("turquoise4","orange3"))+ 
  #scale_color_manual(name = "Method", labels = c("Sweep", "Vacuum"), values=c("turquoise4","orange3")) +
  scale_x_continuous(limits=c(0,30),expand=c(0,0))+
  #scale_y_continuous(limits = c(0,1.2), breaks =c(0,.2,4,.6,.8,1), expand = c(0, 0)) +
  #annotate(geom="text", x=1.9, y=51, label= "*",size=12)+
  theme_line()+
  labs(y = "Arthropod Biomass Index per Session per Chick", x="WSG percent cover (site)")+ 
  ggtitle("")
print(WSG_Plot_chick)



####4h. Impact of vegetation and BHCO on provisioning rates####

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
  Null                   = glmmTMB(Beh_per_h_chick ~ 1                           + (1|PasturePatchYear),  data=df, family="gaussian")
  PropBHCO               = glmmTMB(Beh_per_h_chick ~ propBHCO                    + (1|PasturePatchYear),  data=df, family="gaussian")
  NumBHCO                = glmmTMB(Beh_per_h_chick ~ XBHCO                       + (1|PasturePatchYear),  data=df, family="gaussian")
  PresBHCO               = glmmTMB(Beh_per_h_chick ~ as.factor(Parasitized)      + (1|PasturePatchYear),  data=df, family="gaussian")
  NestlingAge            = glmmTMB(Beh_per_h_chick ~ AvgAgeDays                  + (1|PasturePatchYear),  data=df, family="gaussian")
  
  mods=list(Null,   PropBHCO,    NumBHCO,   PresBHCO,   NestlingAge)  
  names=c( "Null", "PropBHCO",  "NumBHCO", "PresBHCO", "NestlingAge")
  
  print(aictab(cand.set = mods, modnames = names,second.ord = FALSE), digits = 4) }

NestContents_Mods_DICK(ProvDataSession)

Veg_Mods_DICK = function(df) {
  Null                      = glmmTMB(Beh_per_h_chick ~ 1            + AvgAgeDays + (1|PasturePatchYear),  data=df, family="gaussian")
  FEAR5                     = glmmTMB(Beh_per_h_chick ~ FEAR_5       + AvgAgeDays + (1|PasturePatchYear),  data=df, family="gaussian")
  FEAR25                    = glmmTMB(Beh_per_h_chick ~ FEAR_25      + AvgAgeDays + (1|PasturePatchYear),  data=df, family="gaussian")
  FEARPasture               = glmmTMB(Beh_per_h_chick ~ FEAR_Pasture + AvgAgeDays + (1|PasturePatchYear),  data=df, family="gaussian")
  CSG5                      = glmmTMB(Beh_per_h_chick ~ CSG_5        + AvgAgeDays + (1|PasturePatchYear),  data=df, family="gaussian")
  CSG25                     = glmmTMB(Beh_per_h_chick ~ CSG_25       + AvgAgeDays + (1|PasturePatchYear),  data=df, family="gaussian")
  CSGPasture                = glmmTMB(Beh_per_h_chick ~ CSG_Pasture  + AvgAgeDays + (1|PasturePatchYear),  data=df, family="gaussian")
  WSG5                      = glmmTMB(Beh_per_h_chick ~ WSG_5        + AvgAgeDays + (1|PasturePatchYear),  data=df, family="gaussian")
  WSG25                     = glmmTMB(Beh_per_h_chick ~ WSG_25       + AvgAgeDays + (1|PasturePatchYear),  data=df, family="gaussian")
  WSGPasture                = glmmTMB(Beh_per_h_chick ~ WSG_Pasture  + AvgAgeDays + (1|PasturePatchYear),  data=df, family="gaussian")
  Forb5                     = glmmTMB(Beh_per_h_chick ~ Forb_5       + AvgAgeDays + (1|PasturePatchYear),  data=df, family="gaussian")
  Forb25                    = glmmTMB(Beh_per_h_chick ~ Forb_25      + AvgAgeDays + (1|PasturePatchYear),  data=df, family="gaussian")
  ForbPasture               = glmmTMB(Beh_per_h_chick ~ Forb_Pasture + AvgAgeDays + (1|PasturePatchYear),  data=df, family="gaussian")
  
  mods=list(Null,   FEAR5,   FEAR25,    FEARPasture,   CSG5,     CSG25,      CSGPasture,   WSG5,   WSG25,   WSGPasture,Forb5,   Forb25,   ForbPasture)  
  names=c( "Null", "FEAR5", "FEAR25",  "FEARPasture", "CSG5",   "CSG25",    "CSGPasture", "WSG5", "WSG25", "WSGPasture","Forb5", "Forb25", "ForbPasture")
  
  print(aictab(cand.set = mods, modnames = names,second.ord = FALSE), digits = 4) }

Veg_Mods_DICK(ProvDataSession)


FEAR_Top_perchick = glmmTMB(Beh_per_h_chick ~ FEAR_Pasture + AvgAgeDays + (1|PasturePatchYear),  data=ProvDataSession, family="gaussian")
summary(FEAR_Top_perchick)

ggpredict(FEAR_Top_perchick,terms=c("FEAR_Pasture[0,10,20,30,40]"),ci.lvl=0.85, back.transform=TRUE, append=TRUE)
FEAR_Pred_perchick= as.data.frame(ggpredict(FEAR_Top_perchick,terms=c("FEAR_Pasture[0,10,20,30,40]"),ci.lvl=0.85, back.transform=TRUE, append=TRUE)
)
#turns predictions into a dataframe that we can more easily manipulate
colnames(FEAR_Pred_perchick)=c("FEAR_Pasture", "Predicted", "SE", "Lower","Upper") #renames columns 
FEAR_Pred_perchick

FEAR_Plot_perchick=ggplot(data=FEAR_Pred_perchick, y=Predicted, x=FEAR_Pasture)+  
  geom_smooth(aes(x=FEAR_Pasture, y=Predicted, ymin=Lower, ymax=Upper),color="black", fill="darkgreen", stat="identity")+
  #scale_fill_manual(name = "Method", labels = c("Sweep", "Vacuum"), values=c("turquoise4","orange3"))+ 
  #scale_color_manual(name = "Method", labels = c("Sweep", "Vacuum"), values=c("turquoise4","orange3")) +
  scale_x_continuous(limits=c(0,40),expand=c(0,0))+
  #scale_y_continuous(limits = c(0,1.2), breaks =c(0,.2,4,.6,.8,1), expand = c(0, 0)) +
  #annotate(geom="text", x=1.9, y=51, label= "*",size=12)+
  theme_line()+
  labs(y = "Provisioning per hour per chick", x="FEAR percent cover (site)")+ 
  ggtitle("")
print(FEAR_Plot_perchick)


#Forb
summary(ProvDataSession$Forb_Pasture)

Forb_Top_perchick = glmmTMB(Beh_per_h_chick ~ Forb_Pasture + AvgAgeDays + (1|PasturePatchYear),  data=ProvDataSession, family="gaussian")
summary(Forb_Top_perchick)

ggpredict(Forb_Top_perchick,terms=c("Forb_Pasture[0,10,20,30,40, 50, 60]"),ci.lvl=0.85, back.transform=TRUE, append=TRUE)
Forb_Pred_perchick= as.data.frame(ggpredict(Forb_Top_perchick,terms=c("Forb_Pasture[0,10,20,30,40, 50,60]"),ci.lvl=0.85, back.transform=TRUE, append=TRUE)
)
#turns predictions into a dataframe that we can more easily manipulate
colnames(Forb_Pred_perchick)=c("Forb_Pasture", "Predicted", "SE", "Lower","Upper") #renames columns 
Forb_Pred_perchick

Forb_Plot_perchick=ggplot(data=Forb_Pred_perchick, y=Predicted, x=Forb_Pasture)+  
  geom_smooth(aes(x=Forb_Pasture, y=Predicted, ymin=Lower, ymax=Upper),color="black", fill="goldenrod", stat="identity")+
  #scale_fill_manual(name = "Method", labels = c("Sweep", "Vacuum"), values=c("turquoise4","orange3"))+ 
  #scale_color_manual(name = "Method", labels = c("Sweep", "Vacuum"), values=c("turquoise4","orange3")) +
  scale_x_continuous(limits=c(0,60),expand=c(0,0))+
  #scale_y_continuous(limits = c(0,1.2), breaks =c(0,.2,4,.6,.8,1), expand = c(0, 0)) +
  #annotate(geom="text", x=1.9, y=51, label= "*",size=12)+
  theme_line()+
  labs(y = "Provisioning per hour per chick", x="Forb percent cover (site)")+ 
  ggtitle("")
print(Forb_Plot_perchick)


