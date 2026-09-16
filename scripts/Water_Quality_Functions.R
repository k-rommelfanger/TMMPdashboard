#Libraries
library(tidyverse)
library(lubridate)
library(patchwork)
library(readr)


salinityplot <- function(yvalue, ylab){
wqclassification <- wqcleanedraw %>% 
  select(Island:Date_Survey, Water_depth:Syringe_used, Notes) %>% 
  mutate(Forest_Type = case_when(
    Site %in% c("Great Pond", "Southgate", "Francis Bay", "Lameshur Bay", "Reef Bay", "Compass Point", "Perseverance Bay") ~ "Salt Pond",
    Site %in% c("Krause Lagoon", "Salt River", "Mary Creek", "Princess Bay", "Turner Bay", "Water Creek", "Brewers Bay", "Mandahl Bay", "STEER Fringe", "Vessup Bay") ~ "Fringe",
    Site %in% c("STEER Basin", "Magens Bay") ~ "Basin"
  )) %>% 
  unite(col=SitePlot, sep=" ",Site, Plot, remove=FALSE) %>% 
  group_by(SY) %>% 
  distinct() %>% 
  mutate(
    Plot=recode(Plot,
                "C-OUT"="C",
                "B-IN"="B",
                "B-OUT"="B",
                "E-IN"="C")) 


wqclassification$SY <- as.factor(wqclassification$SY)

# 1. Rearrange by explicitly stating a manual order
wqclassification$Site <- factor(wqclassification$Site, levels = c("Magens Bay", "STEER Basin", "Brewers Bay", "Krause Lagoon", "Mandahl Bay", "Mary Creek", "Princess Bay", "Salt River", "STEER Fringe", "Turner Bay", "Vessup Bay", "Water Creek", "Compass Point", "Francis Bay", "Great Pond", "Lameshur Bay", "Perseverance Bay", "Reef Bay", "Southgate"))

#Forest Type rectangles
ftrect_df <- data.frame(
  xmin = c(0.5, 2.5, 12.5),
  xmax = c(2.5, 12.5, 19.5),
  ymin = c(-Inf, -Inf, -Inf),
  ymax = c(Inf, Inf, Inf),
  Forest_Type = c("Basin", "Fringe", "Salt Pond") # Optional: add grouping
)

#Ocean salinity (35ppt)
oceanhline_data <- data.frame(y = (35), type = factor(2), 
                              stringsAsFactors = FALSE)

#The Salinity Graph#
salinitygraph <- ggplot(data = wqclassification) +
  geom_rect(data = ftrect_df, aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax, fill = Forest_Type), alpha = 0.4) +
  geom_hline(data = oceanhline_data, 
             aes(yintercept = y, linetype = type)) +
  scale_linetype_manual(values = 2, 
                        labels = ("35ppt"),
                        name = "Average Ocean Salinity") +
  scale_fill_manual(values = c("Basin" = "lightblue", "Fringe" = "coral", "Salt Pond" = "lightyellow", "35ppt" = "grey3", name = "Average Ocean Salinity")) +
  geom_boxplot(aes(x = Site, y = {{yvalue}})) +
  geom_jitter(aes(x = Site, y = {{yvalue}}, color = Plot, group = Site, shape = SY), size = 1.5, width = 0.1, height = 0) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(x = "Site", y = ylab, shape = "Year", fill = "Forest Type")
return(salinitygraph)
}

salinityplot(yvalue = Salinity_ppt, ylab = "Salinity (ppt)")


##################################################################################


Restoftheplots <- function(yvalue, ylab){
  wqclassification <- wqcleanedraw %>% 
    select(Island:Date_Survey, Water_depth:Syringe_used, Notes) %>% 
    mutate(Forest_Type = case_when(
      Site %in% c("Great Pond", "Southgate", "Francis Bay", "Lameshur Bay", "Reef Bay", "Compass Point", "Perseverance Bay") ~ "Salt Pond",
      Site %in% c("Krause Lagoon", "Salt River", "Mary Creek", "Princess Bay", "Turner Bay", "Water Creek", "Brewers Bay", "Mandahl Bay", "STEER Fringe", "Vessup Bay") ~ "Fringe",
      Site %in% c("STEER Basin", "Magens Bay") ~ "Basin"
    )) %>% 
    unite(col=SitePlot, sep=" ",Site, Plot, remove=FALSE) %>% 
    group_by(SY) %>% 
    distinct() %>% 
    mutate(
      Plot=recode(Plot,
                  "C-OUT"="C",
                  "B-IN"="B",
                  "B-OUT"="B",
                  "E-IN"="C")) 
  
  
  wqclassification$SY <- as.factor(wqclassification$SY)
  
  # 1. Rearrange by explicitly stating a manual order
  wqclassification$Site <- factor(wqclassification$Site, levels = c("Magens Bay", "STEER Basin", "Brewers Bay", "Krause Lagoon", "Mandahl Bay", "Mary Creek", "Princess Bay", "Salt River", "STEER Fringe", "Turner Bay", "Vessup Bay", "Water Creek", "Compass Point", "Francis Bay", "Great Pond", "Lameshur Bay", "Perseverance Bay", "Reef Bay", "Southgate"))
  
  #Forest Type rectangles
  ftrect_df <- data.frame(
    xmin = c(0.5, 2.5, 12.5),
    xmax = c(2.5, 12.5, 19.5),
    ymin = c(-Inf, -Inf, -Inf),
    ymax = c(Inf, Inf, Inf),
    Forest_Type = c("Basin", "Fringe", "Salt Pond") # Optional: add grouping
  )
  
  
  #The Salinity Graph#
  Metricgraph <- ggplot(data = wqclassification) +
    geom_rect(data = ftrect_df, aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax, fill = Forest_Type), alpha = 0.4) +
    scale_fill_manual(values = c("Basin" = "lightblue", "Fringe" = "coral", "Salt Pond" = "lightyellow")) +
    geom_boxplot(aes(x = Site, y = {{yvalue}})) +
    geom_jitter(aes(x = Site, y = {{yvalue}}, color = Plot, group = Site, shape = SY), size = 1.5, width = 0.1, height = 0) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    labs(x = "Site", y = ylab, shape = "Year", fill = "Forest Type")
  return(Metricgraph)
}

#Temperature
Restoftheplots(yvalue = Temp, ylab = "Temperature (C)")
