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
#setwd("~/NestCameraAnalysis/1_Initial Exploration")
#feel free to add your own setup if using this code

#Ethan wd
setwd("~/Desktop/GRGCoding")

#Josh
setwd("~/RStudio/NestCameraAnalysis/1_Initial Exploration")

#RStudio Cloud setup
setwd("/cloud/project/1_Initial Exploration")

#Jaime comp wd
setwd("~/Dropbox/_Manuscripts/Dipping/NestCameraAnalysis/1_Initial Exploration")

####2. PACKAGES                                       ####
#install.packages('easypackages')#do once to manage packages
library('easypackages')#load package managing package

packages('tidyverse','ggplot2','glmmTMB','readxl','janitor','lubridate')


#_____________________________________________________####
####2. DATA  ####

load("VM.RData")
load("PB.RData")
load("NB.RData")

#_____________________________________________________####
####3. Descriptive Data ####


#Parent behavior rates

#calculating behavior rates per hour by the session
PB_sum_bySession=PB%>%
  #filter(!grepl('TRUE', AbleSeeDipping))%>% 
  #filter(!grepl('0-49', NestVisability))%>% 
  group_by(Species,NestIDSession,NestID,SessionY,BehaviorCode,FilmDuration, FilmStart)%>%
  summarize(BehaviorCount = n())%>%
  mutate(Beh_per_h=BehaviorCount/(FilmDuration))

##calculating behavior rates per hour by the clip
PB_sum_byClip=PB%>%
  #filter(!grepl('TRUE', AbleSeeDipping))%>% 
  #filter(!grepl('0-49', NestVisability))%>% 
  group_by(Species,VideoClip,NestIDSession,NestID,SessionY,BehaviorCode,FilmDuration,ClipStart)%>%
  summarize(BehaviorCount = n())%>%
  mutate(Beh_per_h=BehaviorCount/(FilmDuration))

#calculating the percent of visible feeding attempts had dips by session
PB_dips_bySession=PB%>%
  filter(!grepl(FALSE, AbleSeeDipping),na.rm=TRUE)%>%
  filter(grepl("Provisioning", BehaviorCode),na.rm=TRUE)%>% 
  #filter(!grepl('0-49', NestVisability))%>% 
  group_by(Species,NestIDSession,NestID,SessionY,FilmDuration)%>%
  summarize(
    nDips=n(),
    sumDips = sum(DippingPresent))%>%
  mutate(PercentDips=sumDips/nDips)

#calculating the percent of visible feeding attempts had dips by species
PB_dips_bySpecies=PB%>%
  filter(!grepl(FALSE, AbleSeeDipping),na.rm=TRUE)%>%
  filter(grepl("Provisioning", BehaviorCode),na.rm=TRUE)%>% 
  #filter(!grepl('0-49', NestVisability))%>% 
  group_by(Species,NestIDSession,NestID,SessionY,FilmDuration)%>%
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

#Provisioning and tall fescue

#add nest ID column from VM to PB

ProvData=PB_sum_byClip%>%
  filter(grepl('Arrive at Nest',BehaviorCode))%>%
  left_join(veg,by="NestID")%>%
  left_join(VM,select(NumHosts,NumBHCO),by="NestID")%>%
  filter(TotalNestling>0)

length(unique(ProvData$NestID))
names(ProvData)

Model1=glmmTMB(Beh_per_h~NumBHCO+Csg+TotalNestling+(1|NestIDSession.x), data=ProvData,family="gaussian")
summary(Model1)

BegData=NB%>%
  left_join(VM,select(NumHosts,NumBHCO,TotalNestling,OrdDate),by="NestID")%>%
  filter(BehaviorCode=="Begging")%>%
  group_by(NestID,BehaviorCode,NestIDSession.y,TotalNestling.y,NumBHCO.y,NumHosts.y,NestlingAgeDays.y)%>%
  summarize(BegDuration = sum(Duration_Sec))%>%
  mutate(PercentTime=BegDuration/(20*60))%>%
  filter(TotalNestling.y>0)


PercentTime=glmmTMB(PercentTime~NumBHCO.y+NumHosts.y+NestlingAgeDays.y+(1|NestIDSession.y),family="tweedie",data=BegData)
summary(PercentTime)


NestlingDiet=PB%>%
  filter(grepl("Provisioning", BehaviorCode),na.rm=TRUE)%>% 
  filter(grepl("DICK",Species),na.rm=TRUE)%>%
  filter(!grepl('0-49', NestVisability))%>% 
  group_by(Species,ArthSize,ArthID)%>%
  summarize(count=n())%>%
  mutate(percent=count/1065)

#To do: figure out how to extract species and put it in it's own column
NestChecks=read_csv("Initial Data/NestChecks_2.Mar.2022.csv")%>%
  clean_names(case = "upper_camel", abbreviations = c("ID"))%>%
  mutate(NestID=str_replace_all(NestID,"[ ]","_"))%>%
  mutate(Date=format(mdy(Date),'%m/%d/%y'))%>%
  mutate(Year=year(mdy(Date)))%>%
  filter(Year>2014)%>%
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
  left_join(veg,by="NestID")

summary(as.factor(NestChecks$Year))
summary(as.factor(NestChecks$Stage))
summary(as.factor(NestChecks$Species))


Model1=glm(MaxBHCO~Fear, data=NestChecks,family="poisson")
summary(Model1)


names(NestChecks)

#_____________________________________________________####
####4. Descriptive Data ####

#Dipping percentages
DipPercent_bySpecies <- ggplot(filter(PB_dips_bySpecies,Species!="EAKI"),aes(x=Species,y=mean))+
  geom_col(aes(color="black",fill=Species))+
  scale_x_discrete(labels=c("Bobolink","Brown\nThrasher","Common\nGrackle","Dickcissel","Eastern\nMeadowlark","Gray\nCatbird","Grasshopper\nSparrow","Red-winged\nBlackbird"))+
  scale_fill_manual(values=c("burlywood1","sienna2","mediumorchid4","goldenrod1","yellow2","gray50","peru","red3"))+
  scale_color_manual(values = "black")+
  scale_y_continuous(limits = 0:1,labels = c("0%","25%","50%","75%","100%"))+
  labs(y="Percent of Provisioning Events with Dipping",x="")+
  theme_minimal()+
  theme(legend.position = "none")
DipPercent_bySpecies #Include n= for each species

