#Libraries
library(tidyverse)
library(lubridate)
library(patchwork)
library(readr)
#____________________________________________________________________________________________________
# Function: salinityplot
# Purpose: Plot the distribution of salinity across TMMP sites separated into the different forest types. Allow for visualization of trends across sample plots within a site and across years
#
# Parameters:
#   - yvalue: Salinity (ppt), the column header for salinity
#   -ylab: "Salinity (ppt)", the y lab in quotes
#
# Returns:
#   - A ggplot object showing a boxplot of salinity levels. TMMP sites are organized by forest type, with the plot background split into three colors reprenting a different forest type classification. All individual salinity measurements are represented with points overlayed onto the boxplot. Individual points are colored by suplot (ABC) and shaped by collection year. There is a horizontal bar at 35 that represents average ocean salinity.
#
#Adding More Sample Years: All that needs to be done is reupload the new data in the qmd file. In the function Just double check that the new shape for the new sample year is distinct, may need to set shapes manually if not
#Adding New Sample Sites: 1. Add the new sample site into it's forest type classification in lines 26, 27, or 28. 2. Add sites into Alphabetical list separated by forest type in line 44 3. Adjust the size of the forst type rectangles lines 47-53


salinityplot <- function(yvalue, ylab){
wqclassification <- wqcleanedraw %>% 
  select(Island:Date_Survey, Water_depth:Syringe_used, Notes) %>% 
  mutate(Forest_Type = case_when(
    Site %in% c("Great Pond", "Southgate", "Francis Bay", "Lameshur Bay", "Reef Bay", "Compass Point", "Perseverance Bay") ~ "Salt Pond",
    Site %in% c("Krause Lagoon", "Salt River", "Mary Creek", "Princess Bay", "Turner Bay", "Water Creek", "Brewers Bay", "Mandahl Bay", "STEER Fringe", "Vessup Bay") ~ "Fringe",
    Site %in% c("STEER Basin", "Magens Bay") ~ "Basin"
  )) %>% #this is where you will need to add new sites in future years
  unite(col=SitePlot, sep=" ",Site, Plot, remove=FALSE) %>% 
  group_by(SY) %>% 
  distinct() %>% #when computers dont suck
  mutate(
    Plot=recode(Plot,
                "C-OUT"="C",
                "B-IN"="B",
                "B-OUT"="B",
                "E-IN"="C"))  #we don't need this section cleaning data, moving forward data will be clean, we will pull this from the function later when computers don't suck!


wqclassification$SY <- as.factor(wqclassification$SY)

# 1. Rearrange by explicitly stating a manual order
wqclassification$Site <- factor(wqclassification$Site, levels = c("Magens Bay", "STEER Basin", "Brewers Bay", "Krause Lagoon", "Mandahl Bay", "Mary Creek", "Princess Bay", "Salt River", "STEER Fringe", "Turner Bay", "Vessup Bay", "Water Creek", "Compass Point", "Francis Bay", "Great Pond", "Lameshur Bay", "Perseverance Bay", "Reef Bay", "Southgate")) #also need to add any new sites here!! On the x axis, each site is represented by a number, Magens=1, STEER Basin=2 ect. 

#Forest Type rectangles (if more sites are added the rectangles that break up the forest type colors need to be adjusted, 0.5 after the whole number that represents the last site in each forest type group. Arranged Alphabetically )
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
#____________________________________________________________________________________________________
# Function: Restoftheplots
# Purpose: Plot the distribution of WQ measurements across TMMP sites separated into the different forest types. Allow for visualization of trends across sample plots within a site and across years
#
# Parameters:
#   - yvalue: WQ value of interest, the column header for your WQ value of interest
#   -ylab: WQ Value and units ("XX (units)" ) the y lab in quotes
#
# Returns:
#   - A ggplot object showing a boxplot of WQ levels. TMMP sites are organized by forest type, with the plot background split into three colors reprenting a different forest type classification. All individual measurements are represented with points overlayed onto the boxplot. Individual points are colored by suplot (ABC) and shaped by collection year. There is a horizontal bar at 35 that represents average ocean salinity.
#
#Adding More Sample Years: All that needs to be done is reupload the new data in the qmd file. In the function Just double check that the new shape for the new sample year is distinct, may need to set shapes manually if not
#Adding New Sample Sites: 1. Add the new sample site into it's forest type classification in lines 97, 98, or 99. 2. Add sites into Alphabetical list separated by forest type in line 115 3. Adjust the size of the forest type rectangles lines 119-125


Restoftheplots <- function(yvalue, ylab){
  wqclassification <- wqcleanedraw %>% 
    select(Island:Date_Survey, Water_depth:Syringe_used, Notes) %>% 
    mutate(Forest_Type = case_when(
      Site %in% c("Great Pond", "Southgate", "Francis Bay", "Lameshur Bay", "Reef Bay", "Compass Point", "Perseverance Bay") ~ "Salt Pond",
      Site %in% c("Krause Lagoon", "Salt River", "Mary Creek", "Princess Bay", "Turner Bay", "Water Creek", "Brewers Bay", "Mandahl Bay", "STEER Fringe", "Vessup Bay") ~ "Fringe",
      Site %in% c("STEER Basin", "Magens Bay") ~ "Basin"
    )) %>% #this is where you will need to add new sites in future years 
    unite(col=SitePlot, sep=" ",Site, Plot, remove=FALSE) %>% 
    group_by(SY) %>% 
    distinct() %>% 
    mutate(
      Plot=recode(Plot,
                  "C-OUT"="C",
                  "B-IN"="B",
                  "B-OUT"="B",
                  "E-IN"="C")) #prolly pull this once better computer
  
  
  wqclassification$SY <- as.factor(wqclassification$SY)
  
  # 1. Rearrange by explicitly stating a manual order
  wqclassification$Site <- factor(wqclassification$Site, levels = c("Magens Bay", "STEER Basin", "Brewers Bay", "Krause Lagoon", "Mandahl Bay", "Mary Creek", "Princess Bay", "Salt River", "STEER Fringe", "Turner Bay", "Vessup Bay", "Water Creek", "Compass Point", "Francis Bay", "Great Pond", "Lameshur Bay", "Perseverance Bay", "Reef Bay", "Southgate"))#also need to add any new sites here!! On the x axis, each site is represented by a number, Magens=1, STEER Basin=2 ect. 
  
  #Forest Type rectangles (if more sites are added the rectangles that break up the forest type colors need to be adjusted, 0.5 after the whole number that represents the last site in each forest type group. Arranged Alphabetically )
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
