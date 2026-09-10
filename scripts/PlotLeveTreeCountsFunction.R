library(readr)
library(tidyverse)
library(patchwork)
TreeMeasurements <- read_csv("TreeMeasurements.csv")

colors<-c("#4d4b4a","darkgrey","salmon4","#c19a6b","#9b111e","#c21e56")


#"maroon","pink"

Trees24<-TreeMeasurements%>%
  filter(SY=="2024")%>%
  group_by(SY,Site, Plot, Species, Mortality)%>%
  summarise(N=n())%>%
  filter(!is.na(Mortality))%>%
  filter(!is.na(Site))%>%
  filter(Species=="RHMA"|Species=="LARA"|Species=="AVGE")%>%
  mutate(
    Mortality=recode(Mortality,
                     "Dying"="Alive")
  )%>%
  unite(col=X, sep=" ",Site, Plot, remove=FALSE)%>%
  unite(col=SpMortality, sep=" ", Species, Mortality, remove=FALSE)%>%
  ggplot()+
  geom_col(aes(x=X, y=N, fill=SpMortality, position="stack"))+
  theme(axis.text.x = element_text(angle=45, vjust = 1, hjust=1))+
  scale_fill_manual(values=colors)

Trees24



Trees22<-TreeMeasurements%>%
  filter(SY=="2022")%>%
  group_by(SY,Site, Plot, Species, Mortality)%>%
  summarise(N=n())%>%
  filter(!is.na(Mortality))%>%
  filter(!is.na(Site))%>%
  filter(Species=="RHMA"|Species=="LARA"|Species=="AVGE")%>%
  mutate(
    Mortality=recode(Mortality,
                     "Dying"="Alive")
  )%>%
  unite(col=X, sep=" ",Site, Plot, remove=FALSE)%>%
  unite(col=SpMortality, sep=" ", Species, Mortality, remove=FALSE)%>%
  ggplot()+
  geom_col(aes(x=X, y=N, fill=SpMortality, position="stack"))+
  theme(axis.text.x = element_text(angle=45, vjust = 1, hjust=1))+
  scale_fill_manual(values=colors)

Trees22

Treeees<-Trees22/Trees24
Treeees