

WoodyDF <- function(CoarseWood, FineWood) {
  

#in the cases where there are two classes listed, selecting the first class, filtering out the single instance of T only 
Coarse_wood<-CoarseWood%>%
  filter(Decay_class!="T")%>%
  mutate(Decay_class=recode(Decay_class,
                            "R, T"="R",
                            "I, T"="I",
                            "I, R"="I"))

#change dbh from cm to meters
Coarse_wood<-Coarse_wood%>%
  mutate(DBH_m=DBH_cm/100)

#calculate coarse woody debris volume/unit area separately for each debris degradation class 
#this is using the formula from the paper
totaldebris<-Coarse_wood%>%
  group_by(SY, Site, Plot, Transect_num, Decay_class)%>%
  summarise(sumd2=sum(DBH_m^2, na.rm=TRUE), .groups="drop")%>% #adding the squared DBH for each site plot combo
  pivot_wider(names_from = Decay_class , values_from = sumd2)

#merging coarse woody debris information with other, sele
Woodmerge<-FineWood%>%
  select(SY,Island,Site,Plot,Transect_num,Transect_m, Wood_fine_num, Wood_med_num)

#now i have total diameter of debris for each transect, even if that coarse debris count is zero  
CoarseMerge<-Woodmerge%>%
  left_join(totaldebris, by = c("SY", "Site","Plot", "Transect_num"))

CoarseMerge[is.na(CoarseMerge)] <- 0


#now mutate to get volume for each decay class (units=m3 woody debris/m2), then I multiply by conversion factors to determine mass for decay class) 
CoarseMergeVolume<-CoarseMerge%>%
  mutate(IVol=(pi^2 *I/ (8 * Transect_m)), #m3/m2
         SVol=(pi^2*S/ (8 * Transect_m)), #m3/m2
         RVol=(pi^2*R/(8*Transect_m)),     #m3/m2
         IVol_ha=IVol*10000, #m3/ha  Volume-m3/ha
         SVol_ha=SVol*10000, #m3/ha
         RVol_ha=RVol*10000, #m3/ha
         BMI=IVol_ha*0.35, #t/ha
         BMS=SVol_ha*0.5,  #t/ha
         BMR=RVol_ha*0.2,  #t/ha
         CoarseMass=BMI+BMS+BMR, #total t/ha
         CoarseVol=IVol_ha+SVol_ha+RVol_ha) #total m3/ha


#calculate mass of fine and medium debris
#<1cm (2-4m) assume diameter 0.005m (0.5cm)
#1-7.5cm (2-6m) assume diameter 0.0425 (4.25cm)

#need to determine the number of meters sampled,if transect lengths less than 4 meters for fine and 6 meters for coarse then subtract two from them, otherwise use 2m for fine and 4m for coarse
SmallMedCoarseVolume<-CoarseMergeVolume%>%
  mutate(Fine_m=if_else(Transect_m<4, Transect_m-2, 2))%>% #fine debris 2-4m (if shorter than 4 want true length, else 2m)
  mutate(Med_m=if_else(Transect_m<6, Transect_m-2,4))%>% #medium debris 2-6m
  mutate(FVol=(pi^2*((0.005^2)*Wood_fine_num)/ (8 * Fine_m))*10000, #m2/ha
         MVol=(pi^2*((0.0425^2)*Wood_med_num)/(8*Med_m))*10000, #m2/ha
         FMass=FVol*0.5,#t/ha
         MMass=MVol*0.5, #t/ha
         TotalMass=FMass+MMass+CoarseMass,#t/ha
         TotalVolume=FVol+MVol+CoarseVol)#m2/ha

return(SmallMedCoarseVolume)
}

WoodyBarPlot <- function(WoodSummary) {
  
  WoodSummary$Plot<-as.factor(WoodSummary$Plot)

MeanMassPlot<-WoodSummary%>%
  mutate(SmallMass=FMass+MMass)%>%
  group_by(SY, Site)%>%
  summarise(MeanCoarse=mean(CoarseMass), MeanFine=-mean(SmallMass), SEMCoarse=std.error(CoarseMass), SEMFine=std.error(SmallMass),.groups = "drop")%>%
  pivot_longer(
    cols = c(MeanCoarse, MeanFine, SEMCoarse, SEMFine),
    names_to = c(".value", "type"),
    names_pattern = "(Mean|SEM)(Coarse|Fine)"
  )%>%
  ggplot(aes(x=Site, y=Mean, fill=type))+
  geom_col(stat="identity")+
    geom_errorbar(aes(
    ymin = Mean - SEM,
    ymax = Mean + SEM), 
    width=0.2, 
    position = position_identity()
  ) +
  geom_hline(yintercept = 0)+
  facet_wrap(~SY, ncol=1)+
  theme_Publication()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  scale_fill_manual(values=c("#889A34","#B5651D"))+
  ylab("Mean Woody Debris Mass")

return(MeanMassPlot)
}


WoodyBoxPlot <- function(WoodSummary) {
  WoodSummary$SY<-as.factor(WoodSummary$SY)
  
  TotalMassPlot<-WoodSummary%>%
    ggplot(aes(x=Site, y=TotalMass,fill=SY))+
    geom_boxplot(outlier.size=0.75)+
    geom_point(
      aes(group = SY),
      position = position_dodge(width = 0.75),
      size = 0.75
    )+
    theme_Publication()+
    theme(axis.text.x = element_text(angle = 45, hjust = 1))+
    ylab("Woody Debris (tonnes/ha)")+
    scale_fill_manual(values=c("#889A34","#B5651D"), name="Sample Year")
  
  TotalMassPlot
  
}
