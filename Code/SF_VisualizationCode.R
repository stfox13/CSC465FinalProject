#Load libraries
if (!require(pacman)) install.packages("pacman")
pacman::p_load(RCurl, fiftystater, dplyr, plyr, ggplot2, mapproj, reshape2, maptools,choroplethr, statebins,
               scales, stringr, forcats, grid, gridExtra, RGraphics, RColorBrewer, tidyr, choroplethrMaps, sf
)

# Density plot by Age Range ====
vac_data_age <- read.csv(text=getURL("https://raw.githubusercontent.com/stfox13/CSC465FinalProject/master/Datasets/vacinationdata_final.csv"),header=T)

#output directory
output <- "/Users/sidneyfox/Documents/DePaul/Winter2018/CSC465/Homework/Homework03/output/"

#Replace NA with 0
vac_data_age[is.na(vac_data_age)] <- 0

#Filter dataset to States by Population defined by age (as opposed to race / ethnicity)
vac_data_age <- vac_data_age %>% 
  filter(PopulationType=="Age" & RegionType=="State") %>% 
  mutate(Year=as.integer(substr(DateRange, 1, 4))) %>%
  select(RegionName, PopulationRange, Sample, Metric, Year)

#Calculate the mean by State and Year
vac_age_mean <- as.data.frame(
  ddply(vac_data_age, .(RegionName, Year),
        summarize,
        MeanVacRate=as.numeric(mean(Metric)/100),
        MeanSample=as.numeric(mean(Sample)/100))
)

#Construct plot
q1c <- ggplot(vac_age_mean, aes(x = MeanVacRate)) +
  geom_density(fill = "#006d2c", colour = "black", alpha = 0.5) +
  scale_x_continuous(name = "Mean Vaccination Rate",
                     breaks = seq(0, 1, .2),
                     limits=c(0, 1)
                     , labels = percent) +
  scale_y_continuous(name = "Mean Sample",
                     breaks = seq(0, 15, 2.5),
                     limits=c(0,15)) +
  ggtitle("Density Plot of Vaccination Rate") +
  theme_linedraw() +
  theme(panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
        axis.text.x=element_text(angle=66, hjust = 1)) +
  facet_grid(Year ~ .)

#Name output file:
outputfile <- paste(output, "Q01C_PopulationRange_2_DensityPlot.jpg", sep = "")

print(q1c)
ggsave(outputfile, width = 12, height = 8, dpi = 300)

#Might need this?
#q1c + coord_flip()

# Q1D Violin Plot ====

#Read in data
vac_data_age <- read.csv(text=getURL("https://raw.githubusercontent.com/stfox13/CSC465FinalProject/master/Datasets/vacinationdata_final.csv"),header=T)

#Replace NA with 0
vac_data_age[is.na(vac_data_age)] <- 0


vac_data_age <- vac_data_age %>% 
  filter(PopulationType=="Age" & RegionType=="State") %>% 
  mutate(Year=as.integer(substr(DateRange, 1, 4))) %>%
  select(RegionName, PopulationRange, Sample, Metric, Year)

vac_age_mean <- as.data.frame(
  ddply(vac_data_age, .(RegionName, Year),
        summarize,
        MeanVacRate=as.numeric(mean(Metric)/100),
        MeanSample=as.numeric(mean(Sample)/100))
)

q1d <- ggplot(vac_age_mean, aes(factor(Year), MeanVacRate, fill=factor(Year))) + 
  geom_violin(alpha=0.6) + 
  geom_jitter(height = 0, width = 0.1) +
  scale_x_discrete(name = "Reporting Year") +
  scale_y_continuous(name = "Mean Vaccination Rate", breaks = seq(0, 1, .1),limits=c(0, 1), labels = percent) +
  scale_fill_brewer(palette = "PuRd", name = "Year")+
  ggtitle("Violin Plot of Mean Vaccination Rate By Year") +
  theme_linedraw() +
  theme(panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"), legend.position = "bottom")

#Name output file:
outputfile <- paste(output, "Q01D_MeanVacRate_ViolinPlot.jpg", sep = "")

print(q1d)
ggsave(outputfile, width = 12, height = 8, dpi = 300)

ggplot(economics_long, aes(date, value)) + 
  geom_line() + 
  facet_wrap(~variable, scales = "free_y", ncol = 1)


# Looking Into Adding Brewer Color to Tim C Viz ----
#Load libraries
library(RCurl)
library(fiftystater)
library(dplyr)
library(plyr)
library(ggplot2)
library(mapproj)
library(scales)
library(stringr)
library(forcats)
library(grid)
library(gridExtra)
library(RGraphics)
library(RColorBrewer)

#Set output directory
output <- "/Users/sidneyfox/Documents/DePaul/Winter2018/CSC465/FinalProject/Visualizations/"

tc <- read.csv("/Users/sidneyfox/Documents/DePaul/Winter2018/CSC465/FinalProject/Datasets/TimC.csv") 

#Read in state lat. and long. coordinates
data("fifty_states")

#Summarize data by state:
tc_sum <- as.data.frame(ddply(tc, .(State1), 
                                summarize,  
                                TotalFoodService = sum(FoodService),
                                MeanFoodService = mean(FoodService)))

tc_97 <- tc %>%
  filter(Year==1997) %>% 
  select(State, FoodService)

tc_02 <- tc %>%
  filter(Year==2002) %>% 
  select(State, FoodService)

tc_07 <- tc %>%
  filter(Year==2007) %>% 
  select(State, FoodService)

# map_id creates the aesthetic mapping to the state name column in your data
# Sum
tc_plot_sum <- ggplot(tc_sum, aes(map_id = State1)) + 
  geom_map(aes(fill = TotalFoodService), map = fifty_states) + 
  expand_limits(x = fifty_states$long, y = fifty_states$lat) +
  coord_map() +
  scale_fill_gradientn("", colours = brewer.pal(9, "RdYlBu"), guide = guide_legend(),  label=comma) +
  scale_x_continuous(breaks = NULL) + 
  scale_y_continuous(breaks = NULL) +
  labs(x = "", y = "", title = "Total Food Service by State", cex=0.75) +
  theme(legend.position = "bottom", 
        panel.background = element_blank())

# map_id creates the aesthetic mapping to the state name column in your data
# Mean
tc_plot_mean <- ggplot(tc_sum, aes(map_id = State1)) + 
  geom_map(aes(fill = MeanFoodService), map = fifty_states) + 
  expand_limits(x = fifty_states$long, y = fifty_states$lat) +
  coord_map() +
  scale_fill_gradientn("", colours = brewer.pal(9, "RdYlBu"), guide = guide_legend(), label=comma) +
  scale_x_continuous(breaks = NULL) + 
  scale_y_continuous(breaks = NULL) +
  labs(x = "", y = "", title = "Mean Food Service by State", cex=0.75) +
  theme(legend.position = "bottom", 
        panel.background = element_blank())

# map_id creates the aesthetic mapping to the state name column in your data
# 1997 Data
tc_plot_97 <- ggplot(tc_97, aes(map_id = State1)) + 
  geom_map(aes(fill = FoodService), map = fifty_states) + 
  expand_limits(x = fifty_states$long, y = fifty_states$lat) +
  coord_map() +
  scale_fill_gradientn("", colours = brewer.pal(9, "RdYlBu"), guide = guide_legend(), label=comma) +
  scale_x_continuous(breaks = NULL) + 
  scale_y_continuous(breaks = NULL) +
  labs(x = "", y = "", title = "Food Service by State\nYear: 1997", cex=0.75) +
  theme(legend.position = "bottom", 
        panel.background = element_blank())

# map_id creates the aesthetic mapping to the state name column in your data
# 2002 Data
tc_plot_02 <- ggplot(tc_02, aes(map_id = State1)) + 
  geom_map(aes(fill = FoodService), map = fifty_states) + 
  expand_limits(x = fifty_states$long, y = fifty_states$lat) +
  coord_map() +
  scale_fill_gradientn("", colours = brewer.pal(9, "RdYlBu"), guide = guide_legend(), label=comma) +
  scale_x_continuous(breaks = NULL) + 
  scale_y_continuous(breaks = NULL) +
  labs(x = "", y = "", title = "Food Service by State\nYear: 2002", cex=0.75) +
  theme(legend.position = "bottom", 
        panel.background = element_blank())

# map_id creates the aesthetic mapping to the state name column in your data
# 2007 Data
tc_plot_07 <- ggplot(tc_07, aes(map_id = State1)) + 
  geom_map(aes(fill = FoodService), map = fifty_states) + 
  expand_limits(x = fifty_states$long, y = fifty_states$lat) +
  coord_map() +
  scale_fill_gradientn("", colours = brewer.pal(9, "RdYlBu"), guide = guide_legend(), label=comma) +
  scale_x_continuous(breaks = NULL) + 
  scale_y_continuous(breaks = NULL) +
  labs(x = "", y = "", title = "Food Service by State\nYear: 2007", cex=0.75) +
  theme(legend.position = "bottom", 
        panel.background = element_blank())

#Name output file:
outputfile <- paste(output, "TotalFoodService_Map.jpg", sep = "")
#Print plot:
print(tc_plot_sum)
#Save plot:
ggsave(outputfile, width = 12, height = 8, dpi = 300)

#Name output file:
outputfile <- paste(output, "MeanFoodService_Map.jpg", sep = "")
#Print plot:
print(tc_plot_mean)
#Save plot:
ggsave(outputfile, width = 12, height = 8, dpi = 300)

#Name output file:
outputfile <- paste(output, "FoodService_1997_Map.jpg", sep = "")
#Print plot:
print(tc_plot_97)
#Save plot:
ggsave(outputfile, width = 12, height = 8, dpi = 300)

#Name output file:
outputfile <- paste(output, "FoodService_2002_Map.jpg", sep = "")
#Print plot:
print(tc_plot_02)
#Save plot:
ggsave(outputfile, width = 12, height = 8, dpi = 300)

#Name output file:
outputfile <- paste(output, "FoodService_2007_Map.jpg", sep = "")
#Print plot:
print(tc_plot_07)
#Save plot:
ggsave(outputfile, width = 12, height = 8, dpi = 300)



tc_97 %>% 
  select(state=as.vector(tc_97$State), value=FoodService) %>%
  statebins_continuous(legend_position="bottom", legend_title="Food Service per state", 
                       brewer_pal="RdPu", text_color="black", font_size=3)

# Tile Map ----
if (!require(pacman)) install.packages("pacman")
pacman::p_load(RCurl, fiftystater, dplyr, plyr, ggplot2, mapproj, statebins,
               scales, stringr, forcats, grid, gridExtra, RGraphics, RColorBrewer
)

tc_97 <- tc %>%
  filter(Year==1997) %>% 
  select(State1, FoodService)

tc_97$state <- str_to_title(tc_97$State1)


statebins(tc_97, value_col="FoodService", legend_title = "Food Service per State - 1997",
          brewer_pal = "RdPu", text_color = "black", font_size=3) +
          theme_statebins()
          theme_statebins(legend_position = "bottom")

