### R Session 1 ###
### 29 October 2023 ###

### By: Ananya Iyengar ###

### Objectives for this lecture: Graphing!

#Setting the Working Directory

setwd("C:/Users/anniy/OneDrive/Desktop/r_stephens23/r_stephens23")

#############################################################

#loading packages

library(dplyr)
library(ggplot2)
library(readxl)
library(viridis)
library(plotly)

#Creating a variable to divide population of the community by total SC population in tamil nadu 

tn <- tn %>% mutate(pop_prop_sc = pop_total/14438445)

#Removing the observation for all "All Scheduled Castes"

tn <- tn %>% filter(sc_code != "000")

#Choosing the Top-10 largest castes by population

tn_top10 <- tn %>% arrange(desc(prim_total)) %>% slice(1:10)

#A Basic Graph! Learning the introductory functions of ggplot2

tn_top10 %>% 
  ggplot(aes(x = pop_prop_sc, y = prim_total)) + 
  geom_point(alpha = 0.6, shape = 21, color = "black") + 
  scale_y_log10() +
  theme(axis.text.x = element_text(size = 5.5), axis.title = element_text(size = 9)) +
  xlab("Community Population as a Fraction of Total SC Population ") + 
  ylab("Log of Quantum of Primary School Attainment") +
  ggtitle("Community Size vs Primary Education: Top 10 SC in Tamil Nadu") +
  labs(caption = "Source: Data from Census 2011 Appendices A-10, A-11, SC-08, ST-08.") + 
  theme(plot.title = element_text(size=10), plot.caption = element_text(size = 6)) +
  guides(size = "none") + theme_gray()
  

#Introducing colour! 


#We will now use the "edu" data set! 


#Let's pick a state: I am going to pick Andhra Pradesh!! 

ap <- edu %>% filter(state == "andhra pradesh")

#Selecting only state-wide data and removing observations that we do not need 

ap <- ap %>% filter(region7111 == 1) %>%
  filter(scst2011 != "scst" & scst2011 != "no") #comment on the usage of the & operator here! 

#Population proportions! 

ap <- ap %>% mutate(population_proportions = totalp/19796151)

#Now we can make a point graph! 

ap %>%
  ggplot(aes(x = population_proportions, y = primaryp, fill = as.factor(scst2011), size = totalp)) + 
  geom_point(shape = 21, alpha = 0.6) + 
  xlab("Community Population as a Fraction of Total SC/ST Population ") + 
  ylab("Log of Quantum of Primary School Attainment") +
  labs(fill = "SC/ST") + scale_fill_discrete(labels = c("SC", "ST")) + 
  ggtitle("Community Size vs Primary Education: Top 10 SC and ST in AP") +
  labs(caption = "Source: Data from Census 2011") + 
  theme(plot.title = element_text(size=10), plot.caption = element_text(size = 6)) +
  guides(size = "none")  + xlim(c(0,0.4))

#What if i want to compare two different districts of Andhra Pradesh side by side, say East and West Godavari! 


eg <- edu %>% filter(region7111 == 4) %>%
  filter(scst2011 != "scst" & scst2011 != "no") %>%
  mutate(population_proportions = totalp/1158464)

wg <- edu %>% filter(region7111 == 5) %>% 
  filter(scst2011 != "scst" & scst2011 != "no") %>%
  mutate(population_proportions = totalp/920770)

#Why merged separately? 

godavari <- rbind(eg, wg) #about the Rbind command

typeof(godavari) #LIST!

godavari <- as.data.frame(godavari) #This is important!!

#Now, introduction to facet_wrap command!

godavari %>%
  ggplot(aes(x = population_proportions, y = primaryp, fill = as.factor(scst2011), size = totalp)) + 
  geom_point(shape = 21, alpha = 0.6) + 
  xlab("Community Population as a Fraction of Total SC/ST Population ") + 
  ylab("Log of Quantum of Primary School Attainment") +
  labs(fill = "SC/ST") + scale_fill_discrete(labels = c("SC", "ST")) + 
  ggtitle("Community Size vs Primary Education: Top 10 SC and ST in Andhra Pradesh") +
  labs(caption = "Source: Data from Census 2011") + 
  theme(plot.title = element_text(size=10), plot.caption = element_text(size = 6)) +
  guides(size = "none") + facet_wrap(~ regionname7111)




#Colouring!!! How to make your graphs look aesthetic! Using the simple data frames!

#Importing the data 

sector_wise_contribution <- read_excel("R1/sector_wise_contribution.xlsx")

#We use the viridis package. why?

area_chart <- ggplot(sector_wise_contribution) + 
  theme_gray() + 
  geom_area(aes(x = year, y = agri, fill = state ), colour = "white", size = 0.5, alpha = 0.6) +
  xlab("Year") + ylab("Agriculture NVA") + ggtitle("Statewise Contribution to Agriculture NVA") + 
  labs(caption = "Data: RBI") + scale_fill_viridis(discrete = T)

area_chart

#Using the plotly package to make interactive charts!

ggplotly(area_chart)


















