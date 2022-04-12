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
setwd("~/Desktop/GRGCoding/NestCameraAnalysis/1_Initial Exploration")
remove.packages('glmmTMB')
install.packages('glmmTMB')

#Josh
setwd("~/RStudio/NestCameraAnalysis/1_Initial Exploration")

#RStudio Cloud setup
setwd("/cloud/project/1_Initial Exploration")

#Jaime comp wd
setwd("~/Dropbox/_Manuscripts/Dipping/NestCameraAnalysis/1_Initial Exploration")

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
        legend.position="right",
        legend.text=element_text(size=10, color="black"))}

#_____________________________________________________####

####2. PACKAGES                                       ####
#install.packages('easypackages')#do once to manage packages
library('easypackages')#load package managing package

packages('TMB','tidyverse','ggplot2','glmmTMB','readxl','janitor','lubridate','stringr','reshape2')

#_____________________________________________________####
####3. DATA  ####

load("VM.RData")
load("PB.RData")
load("NB.RData")

#_____________________________________________________####
####4. Descriptive Data ####


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
####4. Descriptive Data ####

#Dipping percentages
DipPercent_bySpecies <- ggplot(filter(PB_dips_bySpecies,Species!="EAKI"),aes(x=Species,y=mean))+
  geom_col(aes(color="black",fill=Species))+
  scale_x_discrete(labels=c("Bobolink","Brown\nThrasher","Common\nGrackle","Dickcissel","Eastern\nMeadowlark","Gray\nCatbird","Grasshopper\nSparrow","Red-winged\nBlackbird"))+
  scale_fill_manual(values=c("burlywood1","sienna2","mediumorchid4","goldenrod1","yellow2","gray50","peru","red3"))+
  scale_color_manual(values = "black")+
  scale_y_continuous(expand=c(0,0),limits = 0:1,labels = c("0%","25%","50%","75%","100%"))+
  labs(y="% Provisioning with Dipping",x="")+
  theme_bar_noleg()+
  theme(legend.position = "none")
DipPercent_bySpecies #Include n= for each species

ggsave(DipPercent_bySpecies,filename="DipPercent_by_Species.png",dpi=600,units="in",height=5,width=8)

#gift for Josh -I think this could be used to create your boxplot!
PB_Dips_Descr=PB%>%
  filter(BehaviorCode=="Provisioning")%>%
  filter(Species=="DICK"|Species=="RWBL")%>%
  group_by(NestIDSession)%>%
  mutate(DipsSum = rowSums(cbind(HostDipsA,HostDipsB,HostDipsC,HostDipsE,HostDipsF,
                                 BHCODipsA,BHCODipsB,BHCODipsC,BHCODipsD,BHCODipsF), na.rm = T))%>%
  mutate(DipsSumPerChick = rowSums(cbind(HostDipsA,HostDipsB,HostDipsC,HostDipsE,HostDipsF,
                                         BHCODipsA,BHCODipsB,BHCODipsC,BHCODipsD,BHCODipsF), na.rm = T)/TotalNestling)%>%
  mutate(SessionsSum = rowSums(cbind(HostSessionsA,HostSessionsB,HostSessionsC,HostSessionsE,HostSessionsF,
                                     BHCOSessionsA,BHCOSessionsB,BHCOSessionsC,BHCOSessionsD,BHCOSessionsF), na.rm = T))%>%
  mutate(SessionsSumPerChick = rowSums(cbind(HostSessionsA,HostSessionsB,HostSessionsC,HostSessionsE,HostSessionsF,
                                             BHCOSessionsA,BHCOSessionsB,BHCOSessionsC,BHCOSessionsD,BHCOSessionsF), na.rm = T)/TotalNestling)


#Ethan _____________________________________________________####
####5. Analysis for Epic Expo - DICK ####

#Calculating Dips Per Chick Per Prov

#Note: I think dips per chick probably makes the most sense? open to other thoughts. 

#Hyp1 - satiation ####

## Make it long
Dips_By_Chick=PB%>%
  filter(BehaviorCode=="Provisioning")%>%
  filter(Species=="DICK")%>%
  filter(AbleSeeDipping=TRUE)%>%
  select(c(NestIDSession,BehaviorStart,BehaviorID,HostDipsA:BHCODipsF))%>%
  melt(id.vars = c("NestIDSession", "BehaviorID","BehaviorStart"))%>%
  mutate(Chick_Letter=str_sub(variable, start= -1))%>%
  mutate(Chick_Sp=str_sub(variable,-9,-6))%>%
  unite("Chick_ID",c("NestIDSession","Chick_Sp","Chick_Letter"), sep= "_",remove = FALSE)%>%
  select(Chick_ID,NestIDSession,BehaviorID,value,Chick_Sp)%>%
  rename("NumDips"="value")

Fed_By_Chick=PB%>%
  filter(BehaviorCode=="Provisioning")%>%
  filter(Species=="DICK")%>%
  filter(AbleSeeDipping=TRUE)%>%
  select(c(NestIDSession,BehaviorID,HostFedA:BHCOFedF,
           ArthID,ArthSize,Arthmm,BehaviorStart,
           NestlingAgeDays.y,Parasitized,TotalNestling,NumBHCO,NumHosts))%>%
  melt(id.vars = c("NestIDSession", "BehaviorID","BehaviorStart",
                   "ArthID","ArthSize","Arthmm",
                   "NestlingAgeDays.y","Parasitized","TotalNestling","NumBHCO",'NumHosts'))%>%
  mutate(Chick_Letter=str_sub(variable, start= -1))%>%
  mutate(Chick_Sp=str_sub(variable,-8,-5))%>%
  unite("Chick_ID",c("NestIDSession","Chick_Sp","Chick_Letter"), sep= "_",remove = FALSE)%>%
  select(Chick_ID,BehaviorStart,value,ArthSize,
         Arthmm,BehaviorStart,NestlingAgeDays.y,Parasitized,TotalNestling,NumBHCO,NumHosts)%>%
  rename("Fed"="value",
         "NestlingAgeDays"="NestlingAgeDays.y")

ChickData=Dips_ByChick%>%
  left_join(Fed_By_Chick,by="Chick_ID",na.rm=TRUE)
  
  
  for (val in ChickData) {
    if(val %% "Fed" == 0)  time_since_fed = count+1
  }
  



#Hyp2 - arthropod size ####
    #DipsSumPerChick =  ArthID  + (1|NestID)
    #DipsSumPerChick = ArthSize + (1|NestID)
    #DipsSumPerChick = Arthmm   + (1|NestID)
    #DippingPresent =  ArthID  + (1|NestID)
    #DippingPresent = ArthSize + (1|NestID)
    #DippingPresent = Arthmm   + (1|NestID)

PB_Dips=PB%>%
  filter(BehaviorCode=="Provisioning")%>%
  filter(Species=="DICK")%>%
  group_by(NestIDSession)%>%
  mutate(DipsSum = rowSums(cbind(HostDipsA,HostDipsB,HostDipsC,HostDipsE,HostDipsF,
                                 BHCODipsA,BHCODipsB,BHCODipsC,BHCODipsD,BHCODipsF), na.rm = T))%>%
  mutate(DipsSumPerChick = rowSums(cbind(HostDipsA,HostDipsB,HostDipsC,HostDipsE,HostDipsF,
                                         BHCODipsA,BHCODipsB,BHCODipsC,BHCODipsD,BHCODipsF), na.rm = T)/TotalNestling)%>%
  mutate(SessionsSum = rowSums(cbind(HostSessionsA,HostSessionsB,HostSessionsC,HostSessionsE,HostSessionsF,
                                     BHCOSessionsA,BHCOSessionsB,BHCOSessionsC,BHCOSessionsD,BHCOSessionsF), na.rm = T))%>%
  mutate(SessionsSumPerChick = rowSums(cbind(HostSessionsA,HostSessionsB,HostSessionsC,HostSessionsE,HostSessionsF,
                                             BHCOSessionsA,BHCOSessionsB,BHCOSessionsC,BHCOSessionsD,BHCOSessionsF), na.rm = T)/TotalNestling)



PB_Dips_ArthSize=PB_Dips%>%
  filter(!(ArthSize=="Unknown"))

PB_Dips_ArthID=PB_Dips%>%
  filter(!(ArthID=="Unknown"))

#template
DipsPerChick_ArthSize=glmmTMB(DipsSumPerChick ~ ArthSize + (1|NestID),family="gaussian",data=PB_Dips_ArthSize)
summary(DipsPerChick_ArthSize)
DipsPerChick_ArthSize

#DipsSumPerChick =  ArthID  + (1|NestID)
DipsPerChick_ArthID=glmmTMB(DipsSumPerChick ~ ArthID + (1|NestID),family="gaussian",data=PB_Dips_ArthID)
summary(DipsPerChick_ArthID)

#DipsSumPerChick = ArthSize + (1|NestID)
DipsPerChick_ArthSize=glmmTMB(DipsSumPerChick ~ ArthSize + (1|NestID),family="gaussian",data=PB_Dips_ArthSize)
summary(DipsPerChick_ArthSize)

#DipsSumPerChick = Arthmm   + (1|NestID)
DipsPerChick_Arthmm=glmmTMB(DipsSumPerChick ~ Arthmm + (1|NestID),family="gaussian",data=PB_Dips_ArthSize)
summary(DipsPerChick_Arthmm)

#DippingPresent =  ArthID  + (1|NestID)
DipsPresent_ArthID=glmmTMB(DippingPresent ~ ArthID + (1|NestID),family="binomial",data=PB_Dips_ArthID)
summary(DipsPresent_ArthID)

#DippingPresent = ArthSize + (1|NestID)
DipsPresent_ArthSize=glmmTMB(DippingPresent ~ ArthSize + (1|NestID),family="binomial",data=PB_Dips_ArthSize)
summary(DipsPresent_ArthSize)

#DippingPresent = Arthmm   + (1|NestID)
DipsPresent_Arthmm=glmmTMB(DippingPresent ~ Arthmm + (1|NestID),family="binomial",data=PB_Dips_ArthSize)
summary(DipsPresent_Arthmm)


#Hyp3 - cowbird presence####

      #DippingPresent = NumBHCO   + (1|NestID) (family would need to be binomial)
      #DippingPresent = Parasitized + (1|NestID)
      #DipsSumPerChick =  NumBHCO  + (1|NestID)
      #DipsSumPerChick = Parasitized + (1|NestID)

#DippingPresent = NumBHCO   + (1|NestID) (family would need to be binomial)
DipsPresent_BHCONum=glmmTMB(DippingPresent ~ NumBHCO + (1|NestID),family="binomial", data=PB_Dips)
summary(DipsPresent_BHCONum)

#DippingPresent = Parasitized + (1|NestID)
DipsPresent_Parasite=glmmTMB(DippingPresent ~ Parasitized + (1|NestID),family="binomial", data=PB_Dips)
summary(DipsPresent_Parasite)

#DipsSumPerChick =  NumBHCO  + (1|NestID)
DipsPerChick_BHCONum=glmmTMB(DippingPresent ~ NumBHCO + (1|NestID),family="gaussian", data=PB_Dips)
summary(DipsPerChick_BHCONum)

#DipsSumPerChick = Parasitized + (1|NestID)
DipsPerChick_Parasite=glmmTMB(DippingPresent ~ Parasitized + (1|NestID),family="gaussian", data=PB_Dips)
summary(DipsPerChick_Parasite)

#producing predicted values
TotNests_Pred = as.data.frame(ggpredict(TotNestsFinal,c("HerbTreat", "GrazingYesNo"),ci.lvl=0.85, back.transform=TRUE, append=TRUE)) 
colnames(TotNests_Pred)=c("HerbTreat", "Predicted","SE","Lower","Upper", "Grazing") #renames columns
print(TotNests_Pred) 

#plot
TotNests_Plot=ggplot(data=TotNests_Pred, y=Predicted, x=Grazing)+  
  geom_bar(aes(x=Grazing, y=Predicted,fill=HerbTreat), position=dodge, stat="identity")+
  scale_fill_manual(values=c("goldenrod3","darkseagreen4","darkslategray"))+
  scale_x_discrete(labels=c("Ungrazed","Grazed"))+
  scale_y_continuous(limits=c(0,40),expand = c(0, 0)) +
  geom_errorbar(aes(x = Grazing, ymin = Lower, ymax = Upper, group=HerbTreat),position = dodge, width = 0.2)+
  labs(y = "Total Nests per Patch", x=" ")+
  theme_bar_leg()+
  theme(legend.title=element_blank())
TotNests_Plot

#_____________________________________________________####
####8. Prelim analysis for ESA ####

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
