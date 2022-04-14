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
install.packages('ggeffects')
install.packages('TMB', type = 'source')
install.packages('glmmTMB', type = 'source')

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
        panel.grid=element_blank(),
        legend.position="right",
        legend.text=element_text(size=10, color="black"))}

#_____________________________________________________####
####2. PACKAGES                                       ####
#install.packages('easypackages')#do once to manage packages
library('easypackages')#load package managing package

packages('TMB','ggeffects','tidyverse','ggplot2','ggpubr','glmmTMB','readxl','janitor','lubridate','stringr','reshape2')

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
DipPercent_bySpecies <- ggplot(filter(PB_dips_bySpecies,Species!="EAKI"),aes(x=reorder(Species, -mean),y=mean))+
  geom_col(aes(color="black",fill=Species))+
  scale_x_discrete(labels=c("Eastern\nMeadowlark","Common\nGrackle","Dickcissel","Bobolink","Brown\nThrasher","Gray\nCatbird","Grasshopper\nSparrow","Red-winged\nBlackbird"))+
  scale_fill_manual(values=c("burlywood1","sienna2","orchid4","gold2","yellow2","gray50","peru","orangered3"))+
  scale_color_manual(values = "black")+
  scale_y_continuous(expand=c(0,0),limits = 0:1,labels = c("0%","25%","50%","75%","100%"))+
  geom_text(label = paste("n =",filter(PB_dips_bySpecies,Species!="EAKI")$nSessions), nudge_y=-.03, fontface="bold")+
  labs(y="% Provisioning with Dipping",x="")+
  theme_bar_noleg()+
  theme(legend.position = "none")
DipPercent_bySpecies #Include FISP when ready

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
#Rows 294:302 have NAs for TotalNestling var, so PerChick variables not working


OnlyDippersAllowed <- PB%>%
  filter(BehaviorCode=="Provisioning")%>%
  filter(Species=="DICK"|Species=="RWBL")%>%
  filter(DippingPresent==T)%>%
  .[!duplicated(.$NestID),60]%>%
  left_join(PB_Dips_Descr)


Dipping_Box1 <- ggplot(OnlyDippersAllowed,aes(x=DipsSum,fill=Species))+
  geom_histogram()+
  scale_fill_manual(values=c("gold2","orangered3"),labels=c("Dickcissel","Red-winged Blackbird"))+
  labs(x="Number of Dips in Provisioning",y="Frequency")+
  theme_bar_leg()
Dipping_Box1 #These likely need to have y axis transformed, but having trouble with it right now

Dipping_Box2 <- ggplot(OnlyDippersAllowed,aes(x=SessionsSum,fill=Species))+
  geom_histogram()+
  scale_fill_manual(values=c("gold2","orangered3"),labels=c("Dickcissel","Red-winged Blackbird"))+
  labs(x="Number of Dipping Sessions in Provisioning",y="Frequency")+
  theme_bar_leg()
Dipping_Box2

DoubleDipping <- ggarrange(Dipping_Box1,Dipping_Box2,nrow = 1,labels = "AUTO",common.legend = T,legend="bottom") 
ggsave(DoubleDipping,filename="DoubleDipping.png",dpi=600,units="in",height=5,width=8)

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
  unite("Chick_Beh_ID",c("Chick_ID","BehaviorID"), sep= "_",remove = FALSE)%>%
  select(Chick_ID,Chick_Beh_ID,NestIDSession,BehaviorID,value,Chick_Sp)%>%
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
  unite("Chick_Beh_ID",c("Chick_ID","BehaviorID"), sep= "_",remove = FALSE)%>%
  select(Chick_Beh_ID,BehaviorStart,value,ArthSize,
         Arthmm,BehaviorStart,NestlingAgeDays.y,Parasitized,TotalNestling,NumBHCO,NumHosts)%>%
  rename("Fed"="value",
         "NestlingAgeDays"="NestlingAgeDays.y")

ChickData=Dips_By_Chick%>%
  left_join(Fed_By_Chick,by="Chick_Beh_ID",na.rm=TRUE)%>% 
  relocate(Fed, .after = NumHosts)

last_event_index=cumsum(ChickData$Fed)+1
last_event_index=c(1,last_event_index[1:length(last_event_index)-1])
last_event_time=c(as.numeric(NA),ChickData[which(ChickData$Fed==1),"BehaviorStart"])[last_event_index]

#Time-Since-Fed
ChickData$TSF=ChickData$BehaviorStart-last_event_time





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
  mutate(DipsSum =  rowSums(cbind(HostDipsA,HostDipsB,HostDipsC,HostDipsE,HostDipsF,BHCODipsA,BHCODipsB,BHCODipsC,BHCODipsD,BHCODipsF), na.rm = T))%>%
  mutate(DipsSumPerChick = rowSums(cbind(HostDipsA,HostDipsB,HostDipsC,HostDipsE,HostDipsF,BHCODipsA,BHCODipsB,BHCODipsC,BHCODipsD,BHCODipsF), na.rm = T)/TotalNestling)%>%
  mutate(SessionsSum =  rowSums(cbind(HostSessionsA,HostSessionsB,HostSessionsC,HostSessionsE,HostSessionsF,BHCOSessionsA,BHCOSessionsB,BHCOSessionsC,BHCOSessionsD,BHCOSessionsF), na.rm = T))%>%
  mutate(SessionsSumPerChick =  rowSums(cbind(HostSessionsA,HostSessionsB,HostSessionsC,HostSessionsE,HostSessionsF,BHCOSessionsA,BHCOSessionsB,BHCOSessionsC,BHCOSessionsD,BHCOSessionsF), na.rm = T)/TotalNestling)



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
DipsPerChick_ArthSize=glmmTMB(as.numeric(DipsSumPerChick) ~ ArthSize + (1|NestID),family="gaussian",data=PB_Dips_ArthSize)
summary(DipsPerChick_ArthSize)

DipsPerChick_ArthSize_Pred = as.data.frame(ggpredict(DipsPerChick_ArthSize,c("ArthSize"),ci.lvl=0.85, back.transform=TRUE, append=TRUE)) 
colnames(DipsPerChick_ArthSize_Pred)=c("ArthSize", "Predicted","SE","Lower","Upper", "group") #renames columns
print(DipsPerChick_ArthSize_Pred) 

dodge=position_dodge(.9)
DipsPerChick_ArthSize_Plot=ggplot(data=DipsPerChick_ArthSize_Pred, aes(y=Predicted, x=ArthSize,fill=ArthSize))+  
  geom_bar(aes(y=Predicted, x=ArthSize),position=dodge, stat="identity")+
  scale_fill_manual(values=c("goldenrod","brown","darkgreen"))+
  #scale_x_discrete(labels=c("",""))+
  scale_y_continuous(expand = c(0, 0)) +
  geom_errorbar(aes(x = ArthSize, ymin = Lower, ymax = Upper),position = dodge, width = 0.2)+
  labs(y = "Dips/Chick", x="Arthropod Size")+
  theme(legend.title=element_blank())+
  theme_bar_leg()
DipsPerChick_ArthSize_Plot

#DipsSumPerChick = Arthmm   + (1|NestID)
DipsPerChick_Arthmm=glmmTMB(DipsSumPerChick ~ Arthmm + (1|NestID),family="gaussian",data=PB_Dips_ArthSize)
summary(DipsPerChick_Arthmm)

#DippingPresent =  ArthID  + (1|NestID)
DipsPresent_ArthID=glmmTMB(DippingPresent ~ ArthID + (1|NestID),family="binomial",data=PB_Dips_ArthID)
summary(DipsPresent_ArthID)

#DippingPresent = ArthSize + (1|NestID)
DipsPresent_ArthSize=glmmTMB(as.numeric(DippingPresent) ~ ArthSize + (1|NestID),family="binomial",data=PB_Dips_ArthSize)
summary(DipsPresent_ArthSize)

DipsPresent_ArthSize_Pred = as.data.frame(ggpredict(DipsPresent_ArthSize,c("ArthSize"),ci.lvl=0.85, back.transform=TRUE, append=TRUE)) 
colnames(DipsPresent_ArthSize_Pred)=c("ArthSize", "Predicted","SE","Lower","Upper", "group") #renames columns
print(DipsPresent_ArthSize_Pred) 

dodge=position_dodge(.9)
DipsPresent_ArthSize_Plot=ggplot(data=DipsPresent_ArthSize_Pred, aes(y=Predicted, x=ArthSize,fill=ArthSize))+  
  geom_bar(aes(y=Predicted, x=ArthSize),position=dodge, stat="identity")+
  scale_fill_manual(values=c("goldenrod","brown", "darkgreen"))+
  scale_x_discrete(labels=c("Small","Medium", "Large"))+
  scale_y_continuous(expand = c(0, 0)) +
  geom_errorbar(aes(x = ArthSize, ymin = Lower, ymax = Upper),position = dodge, width = 0.2)+
  labs(y = "Dipping Present", x="Arthropod Size")+
  theme(legend.title=element_blank())+
  theme_bar_leg()
DipsPresent_ArthSize_Plot

#DippingPresent = Arthmm   + (1|NestID)
DipsPresent_Arthmm=glmmTMB(DippingPresent ~ Arthmm + (1|NestID),family="binomial",data=PB_Dips_ArthSize)
summary(DipsPresent_Arthmm)


#########Hyp3 - cowbird presence##########################################################

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

DipsPresent_Parasite_Pred = as.factor(ggpredict(DipsPresent_Parasite,c("Parasitized"),ci.lvl=0.85, back.transform=TRUE, append=TRUE)) 
colnames(DipsPresent_Parasite_Pred)=c("Parasitized", "Predicted","SE","Lower","Upper","group") #renames columns
print(DipsPresent_Parasite_Pred) 

dodge=position_dodge(.9)
DipsPerChick_Parasite_Plot=ggplot(data=DipsPerChick_Parasite_Pred, aes(y=Predicted, x=as.factor(Parasitized),fill=as.factor(Parasitized)))+  
  geom_bar(aes(y=Predicted, x=as.factor(Parasitized)),position=dodge, stat="identity")+
  scale_fill_manual(values=c("darkorange4","gray20"))+
  scale_x_discrete(labels=c("No","Yes"))+
  scale_y_continuous(expand = c(0, 0)) +
  geom_errorbar(aes(x = as.factor(Parasitized), ymin = Lower, ymax = Upper),position = dodge, width = 0.2)+
  labs(y = "Dipping Present", x="Parasitized")+
  theme(legend.title=element_blank())+
  theme_bar_noleg()
DipsPerChick_Parasite_Plot

#DipsSumPerChick =  NumBHCO  + (1|NestID)
DipsPerChick_BHCONum=glmmTMB(DipsSumPerChick ~ NumBHCO + (1|NestID),family="gaussian", data=PB_Dips)
summary(DipsPerChick_BHCONum)

#DipsSumPerChick = Parasitized + (1|NestID)
DipsPerChick_Parasite=glmmTMB(as.numeric(DipsSumPerChick) ~ as.factor(Parasitized) + (1|NestID),family="gaussian", data=PB_Dips)
summary(DipsPerChick_Parasite)

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
