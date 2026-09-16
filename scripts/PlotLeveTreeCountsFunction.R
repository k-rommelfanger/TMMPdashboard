library(readr)
library(tidyverse)
library(patchwork)
library(readxl)



colors<-c("#4d4b4a","darkgrey","salmon4","#c19a6b","#9b111e","#c21e56")
St_Thomas<-c("Brewers Bay", "Perseverance Bay", "Vessup Bay", "Compass Point", "Magens Bay", "Mandahl Bay", "STEER Fringe", "STEER Basin")
St_Croix<-c("Krause Lagoon", "Salt River", "Great Pond", "Southgate")
St_John<-c("Lameshur Bay", "Princess Bay","Francis Bay", "Turner Bay","Reef Bay","Water Creek", "Mary Creek")
Basin<-c("Magens Bay", "STEER Basin")
Fringe<-c("Brewers Bay","Krause Lagoon","Mandahl Bay","Mary Creek","Princess Bay","Salt River","STEER Fringe","Turner Bay","Vessup Bay","Water Creek")
SaltPond<-c("Compass Point","Francis Bay","Great Pond","Lameshur Bay","Perseverance Bay","Reef Bay","Southgate")

#"maroon","pink"

TreeDistribution<-function(ForestType){

Trees<-tree_measurements%>%
  group_by(SY,Site, Plot, Species, Mortality)%>%
  summarise(N=n())%>%
  filter(!is.na(Mortality))%>%
  filter(!is.na(Site))%>%
  filter(Site %in% ForestType)%>%
  filter(Species=="RHMA"|Species=="LARA"|Species=="AVGE")%>%
  mutate(
    Mortality=recode(Mortality,
                     "Dying"="Alive")
  )%>%
  unite(col=X, sep=" ",Site, Plot, remove=FALSE)%>%
  unite(col=SpMortality, sep=" ", Species, Mortality, remove=FALSE)%>%
  ggplot()+
  geom_col(aes(x=X, y=N, fill=SpMortality, position="stack"))+
  theme_Publication()+
  theme(axis.text.x = element_text(angle=45, vjust = 1, hjust=1))+
  scale_fill_manual(values=colors, name="Species & Mortality")+
  labs(x="Site", y="Total Count of Trees")+
  facet_wrap(~SY, ncol=1)

return(Trees)

}
