#Title: Yuma Metal(loid) Data Analysis
#Author: Jenna K Honan
#Start Date: June 29, 2022

#Importing Data
library(readxl)
Yuma_Metals_Project_EMR_Editted_23Aug2021_mental_health_ID_removed <- read_excel("~/Desktop/Yuma Metals Project_EMR_Editted 23Aug2021 mental health & ID removed.xlsx", 
                                                                                 sheet = "Sqrt 2")
View(Yuma_Metals_Project_EMR_Editted_23Aug2021_mental_health_ID_removed)

#Renaming for ease of use
Metals <- as.data.frame(Yuma_Metals_Project_EMR_Editted_23Aug2021_mental_health_ID_removed)
remove(Yuma_Metals_Project_EMR_Editted_23Aug2021_mental_health_ID_removed)

#Notes: Difference Between Metals and Metalloids
  #Metals have unique metallic properties such as shiny appearance, high density, higher melting points and electric conductivity. 
  #However, metalloids possess both metal properties and non-metal properties. 
  #Metals are located in the left side of the periodic table while metalloids are in the middle of metals and non-metals.

#Reformating Data
Metals$Date <- as.Date(as.numeric(Metals$Date), origin = "1899-12-30")
Metals$DOB <- as.Date(as.numeric(Metals$DOB), origin = "1899-12-30")

#Visualization of Data - Exploratory
#Not sure if these concentrations are from biological or environmental samples.
library(reshape2)
data.melt <- melt(Metals,id.vars='ID', measure.vars=c('Mn55','Cu65','Cd111', 'Hg202', 'Pb208', 'U238'))
library(ggplot2)
ggplot(data.melt) +
  geom_boxplot(aes(y=log10(value), fill=variable))+
  theme(axis.ticks.x = element_blank(),
        axis.text.x = element_blank(),
        panel.background = element_rect(fill="#ffffff"),
        axis.line = element_line(color="#000000"))+
  ylab("Log10 Concentration (Units)")+
  labs(fill='Metals')

#There are additional columns with metal concentrations for arsenic and iron that come later after the hormone level columns.
data.melt2 <- melt(Metals,id.vars='ID', measure.vars=c('As_75', 'Fe_57'))
ggplot(data.melt2) +
  geom_boxplot(aes(y=log10(value), fill=variable))+
  theme(axis.ticks.x = element_blank(),
        axis.text.x = element_blank(),
        panel.background = element_rect(fill="#ffffff"),
        axis.line = element_line(color="#000000"))+
  ylab("Log10 Concentration (Units)")+
  labs(fill='Metals')

#Concentrations by date, including all.
data.melt3 <- melt(Metals,id.vars=c('ID','Date'), measure.vars=c('Mn55','Cu65','Cd111', 'Hg202', 'Pb208', 'U238', 'As_75', 'Fe_57'))
ggplot(subset(data.melt3, data.melt3$Date>"2014-01-01"), aes(y=log10(value), x=Date, color=variable))+
  geom_point()+
  geom_smooth(se=F)+
  theme(panel.background = element_rect(fill="#ffffff"),
        axis.text.x=element_text(angle=60, hjust=1),
        axis.line = element_line(color="#000000"))+
  scale_x_date(date_breaks = "1 month", date_labels =  "%b")+
  ylab("Log10 Concentration (Units)")+
  xlab("Date of Sample Collection, 2018")+
  labs(color='Metals')
#There are some dates here from 2013, but the concentration values are blank, so I removed them from the graphical display.

#Looking at concentration levels of hormones in blood samples
library(gridExtra)
TSH <- ggplot(Metals) +geom_boxplot(aes(y=TSH)) +theme(axis.ticks.x = element_blank(),axis.text.x = element_blank()) +theme_classic() +ylab("TSH (units)")
T3 <- ggplot(Metals) +geom_boxplot(aes(y=TotalT3)) +theme(axis.ticks.x = element_blank(),axis.text.x = element_blank()) +theme_classic() +ylab("Total T3 (units)")
T4 <- ggplot(Metals) +geom_boxplot(aes(y=TotalT4)) +theme(axis.ticks.x = element_blank(),axis.text.x = element_blank()) +theme_classic() +ylab("Total T4 (units)")
Cortisol <- ggplot(Metals) +geom_boxplot(aes(y=Cortisol)) +theme(axis.ticks.x = element_blank(),axis.text.x = element_blank()) +theme_classic() +ylab("Cortisol (units)")

grid.arrange(TSH, Cortisol, T3, T4)

#Looking at demographics of respondents
#average age and range
typeof(Metals$age)
Metals$age <- as.numeric(Metals$age)
library(dplyr)
knitr::kable(Metals %>%
               summarise(mean=round(mean(age, na.rm=T), digits=1),
                         median=round(median(age, na.rm=T), digits=1),
                         min=round(min(age, na.rm=T), digits=1),
                         max=round(max(age, na.rm=T), digits=1),
                         NAs=sum(is.na(age))))

#height, weight, and BMI
Metals$emr_height_inches <- as.numeric(Metals$emr_height_inches)
Metals$emr_weight <- as.numeric(Metals$emr_weight)
Metals$emr_bmi <- as.numeric(Metals$emr_bmi)
knitr::kable(summary(round(Metals[c("emr_height_inches", "emr_weight", "emr_bmi")]), digits=1), col.names = c("Height (inches)", "Weight (lbs)", "BMI"))

### Determining if Differences Exist in EMR versus Measured Data
t.test(Metals$emr_bmi, Metals$bmi)
t.test(Metals$emr_bmi, Metals$bmi, paired = T)
#Some data is missing from the EMRs for BMI. 
#The BMIs between the the EMR and measurements are different in general.
#Also there are differences between only the data that are paired.

#Need to figure out what data is missing
#First thing is to see if any of the Participant IDs are duplicated across any rows
length(unique(Metals$ID)) == nrow(Metals)
  #There are no duplicate IDs

#Looking for negative or NA concentrations
sum(Metals$Mn55<0, na.rm = T)
sum(Metals$Mn55>=0, na.rm = T)
sum(is.na(Metals$Mn55))

#Looking at the distributions of the data





