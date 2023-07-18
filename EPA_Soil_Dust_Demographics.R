#This is for EPA Soil / Dust Ingestion Project

#Importing Data

#Set the default time zone
Sys.setenv(TZ="America/Phoenix") 

#Set the working directory
setwd("~/Desktop/")

##Date Specific Import for EPA Project Data
library(readxl)
EPA <- read.csv("EPASTARSoilAndDustIn_DATA_2022-12-13_0742.csv")

#Creating Demographics Table 
library(dplyr)

#Important columns: age_calc_c1, gender_c1, moouthing_c1, latino_c1, race_c1___1 to race_c1___6, race_c1___89, race_c1___99, all the same for c1 to c4
EPA %>%
  filter(child_health_and_activity_questionnaire_complete>0) %>%
  summarise(number_child=sum(number_children, na.rm=T))

EPA %>%
  summarise(number_child=sum(number_children, na.rm=T))

Incompletes <- EPA %>% 
  filter(child_health_and_activity_questionnaire_complete==0)

All_Demog <- EPA %>%
  filter(child_health_and_activity_questionnaire_complete>0) %>%
  summarise("Filler"='filler',
            "Number of Children"=sum(number_children, na.rm=T),
            "6 months to 1 year"=sum(age_calc_c1>=0.5&age_calc_c1<1, na.rm = T)+sum(age_calc_c2>=0.5&age_calc_c2<1, na.rm = T)+sum(age_calc_c3>=0.5&age_calc_c3<1, na.rm = T)+sum(age_calc_c4>=0.5&age_calc_c4<1, na.rm = T),
            "1 year to 2 years"=sum(age_calc_c1>=1&age_calc_c1<2, na.rm = T)+sum(age_calc_c2>=1&age_calc_c2<2, na.rm = T)+sum(age_calc_c3>=1&age_calc_c3<2, na.rm = T)+sum(age_calc_c4>=1&age_calc_c4<2, na.rm = T),
            "2 years to 3 years"=sum(age_calc_c1>=2&age_calc_c1<3, na.rm = T)+sum(age_calc_c2>=2&age_calc_c2<3, na.rm = T)+sum(age_calc_c3>=2&age_calc_c3<3, na.rm = T)+sum(age_calc_c4>=2&age_calc_c4<3, na.rm = T),
            "3 years to 6 years"=sum(age_calc_c1>=3&age_calc_c1<6, na.rm = T)+sum(age_calc_c2>=3&age_calc_c2<6, na.rm = T)+sum(age_calc_c3>=3&age_calc_c3<6, na.rm = T)+sum(age_calc_c4>=3&age_calc_c4<6, na.rm = T),
            "Male"=sum(gender_c1==1, na.rm = T)+sum(gender_c2==1, na.rm = T)+sum(gender_c3==1, na.rm = T)+sum(gender_c4==1, na.rm = T),
            "Female"=sum(gender_c1==2, na.rm = T)+sum(gender_c2==2, na.rm = T)+sum(gender_c3==2, na.rm = T)+sum(gender_c4==2, na.rm = T),
            "Other"=sum(gender_c1==3, na.rm = T)+sum(gender_c2==3, na.rm = T)+sum(gender_c3==3, na.rm = T)+sum(gender_c4==3, na.rm = T),
            "Don't know Gender"=sum(gender_c1==89, na.rm = T)+sum(gender_c2==89, na.rm = T)+sum(gender_c3==89, na.rm = T)+sum(gender_c4==89, na.rm = T),
            "Refused Gender"=sum(gender_c1==99, na.rm = T)+sum(gender_c2==99, na.rm = T)+sum(gender_c3==99, na.rm = T)+sum(gender_c4==99, na.rm = T),
            "Not Hispanic"=sum(latino_c1==1, na.rm = T)+sum(latino_c2==1, na.rm = T)+sum(latino_c3==1, na.rm = T)+sum(latino_c4==1, na.rm = T),
            "Yes Puero Rican"=sum(latino_c1==2, na.rm = T)+sum(latino_c2==2, na.rm = T)+sum(latino_c3==2, na.rm = T)+sum(latino_c4==2, na.rm = T),
            "Yes Cuban"=sum(latino_c1==3, na.rm = T)+sum(latino_c2==3, na.rm = T)+sum(latino_c3==3, na.rm = T)+sum(latino_c4==3, na.rm = T),
            "Yes Mexican"=sum(latino_c1==4, na.rm = T)+sum(latino_c2==4, na.rm = T)+sum(latino_c3==4, na.rm = T)+sum(latino_c4==4, na.rm = T),
            "Yes Another Hispanic"=sum(latino_c1==5, na.rm = T)+sum(latino_c2==5, na.rm = T)+sum(latino_c3==5, na.rm = T)+sum(latino_c4==5, na.rm = T),
            "Don't know Hispanic"=sum(latino_c1==89, na.rm = T)+sum(latino_c2==89, na.rm = T)+sum(latino_c3==89, na.rm = T)+sum(latino_c4==89, na.rm = T),
            "Refused Hispanic"=sum(latino_c1==99, na.rm = T)+sum(latino_c2==99, na.rm = T)+sum(latino_c3==99, na.rm = T)+sum(latino_c4==99, na.rm = T),
            "White"=sum(race_c1___1==1, na.rm = T)+sum(race_c2___1==1, na.rm = T)+sum(race_c3___1==1, na.rm = T)+sum(race_c4___1==1, na.rm = T),
            "Black or African American"=sum(race_c1___2==1, na.rm = T)+sum(race_c2___2==1, na.rm = T)+sum(race_c3___2==1, na.rm = T)+sum(race_c4___2==1, na.rm = T),
            "American Indian or Alaska Native"=sum(race_c1___3==1, na.rm = T)+sum(race_c2___3==1, na.rm = T)+sum(race_c3___3==1, na.rm = T)+sum(race_c4___3==1, na.rm = T),
            "Asian"=sum(race_c1___4==1, na.rm = T)+sum(race_c2___4==1, na.rm = T)+sum(race_c3___4==1, na.rm = T)+sum(race_c4___4==1, na.rm = T),
            "Native Hawaiian or Pacific Islander"=sum(race_c1___5==1, na.rm = T)+sum(race_c2___5==1, na.rm = T)+sum(race_c3___5==1, na.rm = T)+sum(race_c4___5==1, na.rm = T),
            "Some other race"=sum(race_c1___6==1, na.rm = T)+sum(race_c2___6==1, na.rm = T)+sum(race_c3___6==1, na.rm = T)+sum(race_c4___6==1, na.rm = T),
            "Don't know"=sum(race_c1___89==1, na.rm = T)+sum(race_c2___89==1, na.rm = T)+sum(race_c3___89==1, na.rm = T)+sum(race_c4___89==1, na.rm = T),
            "Refused"=sum(race_c1___99==1, na.rm = T)+sum(race_c2___99==1, na.rm = T)+sum(race_c3___99==1, na.rm = T)+sum(race_c4___99==1, na.rm = T))

All_Demog_by_City <- EPA %>%
  filter(child_health_and_activity_questionnaire_complete>0) %>%
  group_by(city) %>%
  summarise("Number of Children"=sum(number_children, na.rm=T),
            "6 months to 1 year"=sum(age_calc_c1>=0.5&age_calc_c1<1, na.rm = T)+sum(age_calc_c2>=0.5&age_calc_c2<1, na.rm = T)+sum(age_calc_c3>=0.5&age_calc_c3<1, na.rm = T)+sum(age_calc_c4>=0.5&age_calc_c4<1, na.rm = T),
            "1 year to 2 years"=sum(age_calc_c1>=1&age_calc_c1<2, na.rm = T)+sum(age_calc_c2>=1&age_calc_c2<2, na.rm = T)+sum(age_calc_c3>=1&age_calc_c3<2, na.rm = T)+sum(age_calc_c4>=1&age_calc_c4<2, na.rm = T),
            "2 years to 3 years"=sum(age_calc_c1>=2&age_calc_c1<3, na.rm = T)+sum(age_calc_c2>=2&age_calc_c2<3, na.rm = T)+sum(age_calc_c3>=2&age_calc_c3<3, na.rm = T)+sum(age_calc_c4>=2&age_calc_c4<3, na.rm = T),
            "3 years to 6 years"=sum(age_calc_c1>=3&age_calc_c1<6, na.rm = T)+sum(age_calc_c2>=3&age_calc_c2<6, na.rm = T)+sum(age_calc_c3>=3&age_calc_c3<6, na.rm = T)+sum(age_calc_c4>=3&age_calc_c4<6, na.rm = T),
            "Male"=sum(gender_c1==1, na.rm = T)+sum(gender_c2==1, na.rm = T)+sum(gender_c3==1, na.rm = T)+sum(gender_c4==1, na.rm = T),
            "Female"=sum(gender_c1==2, na.rm = T)+sum(gender_c2==2, na.rm = T)+sum(gender_c3==2, na.rm = T)+sum(gender_c4==2, na.rm = T),
            "Other"=sum(gender_c1==3, na.rm = T)+sum(gender_c2==3, na.rm = T)+sum(gender_c3==3, na.rm = T)+sum(gender_c4==3, na.rm = T),
            "Don't know Gender"=sum(gender_c1==89, na.rm = T)+sum(gender_c2==89, na.rm = T)+sum(gender_c3==89, na.rm = T)+sum(gender_c4==89, na.rm = T),
            "Refused Gender"=sum(gender_c1==99, na.rm = T)+sum(gender_c2==99, na.rm = T)+sum(gender_c3==99, na.rm = T)+sum(gender_c4==99, na.rm = T),
            "Not Hispanic"=sum(latino_c1==1, na.rm = T)+sum(latino_c2==1, na.rm = T)+sum(latino_c3==1, na.rm = T)+sum(latino_c4==1, na.rm = T),
            "Yes Puero Rican"=sum(latino_c1==2, na.rm = T)+sum(latino_c2==2, na.rm = T)+sum(latino_c3==2, na.rm = T)+sum(latino_c4==2, na.rm = T),
            "Yes Cuban"=sum(latino_c1==3, na.rm = T)+sum(latino_c2==3, na.rm = T)+sum(latino_c3==3, na.rm = T)+sum(latino_c4==3, na.rm = T),
            "Yes Mexican"=sum(latino_c1==4, na.rm = T)+sum(latino_c2==4, na.rm = T)+sum(latino_c3==4, na.rm = T)+sum(latino_c4==4, na.rm = T),
            "Yes Another Hispanic"=sum(latino_c1==5, na.rm = T)+sum(latino_c2==5, na.rm = T)+sum(latino_c3==5, na.rm = T)+sum(latino_c4==5, na.rm = T),
            "Don't know Hispanic"=sum(latino_c1==89, na.rm = T)+sum(latino_c2==89, na.rm = T)+sum(latino_c3==89, na.rm = T)+sum(latino_c4==89, na.rm = T),
            "Refused Hispanic"=sum(latino_c1==99, na.rm = T)+sum(latino_c2==99, na.rm = T)+sum(latino_c3==99, na.rm = T)+sum(latino_c4==99, na.rm = T),
            "White"=sum(race_c1___1==1, na.rm = T)+sum(race_c2___1==1, na.rm = T)+sum(race_c3___1==1, na.rm = T)+sum(race_c4___1==1, na.rm = T),
            "Black or African American"=sum(race_c1___2==1, na.rm = T)+sum(race_c2___2==1, na.rm = T)+sum(race_c3___2==1, na.rm = T)+sum(race_c4___2==1, na.rm = T),
            "American Indian or Alaska Native"=sum(race_c1___3==1, na.rm = T)+sum(race_c2___3==1, na.rm = T)+sum(race_c3___3==1, na.rm = T)+sum(race_c4___3==1, na.rm = T),
            "Asian"=sum(race_c1___4==1, na.rm = T)+sum(race_c2___4==1, na.rm = T)+sum(race_c3___4==1, na.rm = T)+sum(race_c4___4==1, na.rm = T),
            "Native Hawaiian or Pacific Islander"=sum(race_c1___5==1, na.rm = T)+sum(race_c2___5==1, na.rm = T)+sum(race_c3___5==1, na.rm = T)+sum(race_c4___5==1, na.rm = T),
            "Some other race"=sum(race_c1___6==1, na.rm = T)+sum(race_c2___6==1, na.rm = T)+sum(race_c3___6==1, na.rm = T)+sum(race_c4___6==1, na.rm = T),
            "Don't know"=sum(race_c1___89==1, na.rm = T)+sum(race_c2___89==1, na.rm = T)+sum(race_c3___89==1, na.rm = T)+sum(race_c4___89==1, na.rm = T),
            "Refused"=sum(race_c1___99==1, na.rm = T)+sum(race_c2___99==1, na.rm = T)+sum(race_c3___99==1, na.rm = T)+sum(race_c4___99==1, na.rm = T))

Demo_Table <- cbind(t(All_Demog), t(All_Demog_by_City))
library(writexl)
Demo_Table <- as.data.frame(Demo_Table)

Hisp_Demog <- EPA %>%
  filter(child_health_and_activity_questionnaire_complete>0 & (latino_c1==2 | latino_c2==2 | latino_c3==2 |latino_c4==2 | 
                                                                 latino_c1==3 | latino_c2==3 | latino_c3==3 | latino_c4==3 |
                                                                 latino_c1==4 | latino_c2==4 | latino_c3==4 | latino_c4==4 |
                                                                 latino_c1==5 | latino_c2==5 | latino_c3==5 | latino_c4==5)) %>%
  summarise("Filler"='filler',
            "Number of Children"=sum(number_children, na.rm=T),
            "White"=sum(race_c1___1==1, na.rm = T)+sum(race_c2___1==1, na.rm = T)+sum(race_c3___1==1, na.rm = T)+sum(race_c4___1==1, na.rm = T),
            "Black or African American"=sum(race_c1___2==1, na.rm = T)+sum(race_c2___2==1, na.rm = T)+sum(race_c3___2==1, na.rm = T)+sum(race_c4___2==1, na.rm = T),
            "American Indian or Alaska Native"=sum(race_c1___3==1, na.rm = T)+sum(race_c2___3==1, na.rm = T)+sum(race_c3___3==1, na.rm = T)+sum(race_c4___3==1, na.rm = T),
            "Asian"=sum(race_c1___4==1, na.rm = T)+sum(race_c2___4==1, na.rm = T)+sum(race_c3___4==1, na.rm = T)+sum(race_c4___4==1, na.rm = T),
            "Native Hawaiian or Pacific Islander"=sum(race_c1___5==1, na.rm = T)+sum(race_c2___5==1, na.rm = T)+sum(race_c3___5==1, na.rm = T)+sum(race_c4___5==1, na.rm = T),
            "Some other race"=sum(race_c1___6==1, na.rm = T)+sum(race_c2___6==1, na.rm = T)+sum(race_c3___6==1, na.rm = T)+sum(race_c4___6==1, na.rm = T),
            "Don't know"=sum(race_c1___89==1, na.rm = T)+sum(race_c2___89==1, na.rm = T)+sum(race_c3___89==1, na.rm = T)+sum(race_c4___89==1, na.rm = T),
            "Refused"=sum(race_c1___99==1, na.rm = T)+sum(race_c2___99==1, na.rm = T)+sum(race_c3___99==1, na.rm = T)+sum(race_c4___99==1, na.rm = T))

Non_Hisp_Demog <- EPA %>%
  filter(child_health_and_activity_questionnaire_complete>0 & (latino_c1==1 | latino_c2==1 | latino_c3==1 |latino_c4==1)) %>%
  summarise("Filler"='filler',
            "Number of Children"=sum(number_children, na.rm=T),
            "White"=sum(race_c1___1==1, na.rm = T)+sum(race_c2___1==1, na.rm = T)+sum(race_c3___1==1, na.rm = T)+sum(race_c4___1==1, na.rm = T),
            "Black or African American"=sum(race_c1___2==1, na.rm = T)+sum(race_c2___2==1, na.rm = T)+sum(race_c3___2==1, na.rm = T)+sum(race_c4___2==1, na.rm = T),
            "American Indian or Alaska Native"=sum(race_c1___3==1, na.rm = T)+sum(race_c2___3==1, na.rm = T)+sum(race_c3___3==1, na.rm = T)+sum(race_c4___3==1, na.rm = T),
            "Asian"=sum(race_c1___4==1, na.rm = T)+sum(race_c2___4==1, na.rm = T)+sum(race_c3___4==1, na.rm = T)+sum(race_c4___4==1, na.rm = T),
            "Native Hawaiian or Pacific Islander"=sum(race_c1___5==1, na.rm = T)+sum(race_c2___5==1, na.rm = T)+sum(race_c3___5==1, na.rm = T)+sum(race_c4___5==1, na.rm = T),
            "Some other race"=sum(race_c1___6==1, na.rm = T)+sum(race_c2___6==1, na.rm = T)+sum(race_c3___6==1, na.rm = T)+sum(race_c4___6==1, na.rm = T),
            "Don't know"=sum(race_c1___89==1, na.rm = T)+sum(race_c2___89==1, na.rm = T)+sum(race_c3___89==1, na.rm = T)+sum(race_c4___89==1, na.rm = T),
            "Refused"=sum(race_c1___99==1, na.rm = T)+sum(race_c2___99==1, na.rm = T)+sum(race_c3___99==1, na.rm = T)+sum(race_c4___99==1, na.rm = T))

Hisp_Demog_City <- EPA %>%
  filter(child_health_and_activity_questionnaire_complete>0 & (latino_c1==2 | latino_c2==2 | latino_c3==2 |latino_c4==2 | 
                                                                 latino_c1==3 | latino_c2==3 | latino_c3==3 | latino_c4==3 |
                                                                 latino_c1==4 | latino_c2==4 | latino_c3==4 | latino_c4==4 |
                                                                 latino_c1==5 | latino_c2==5 | latino_c3==5 | latino_c4==5)) %>%
  group_by(city) %>%
  summarise("Number of Children"=sum(number_children, na.rm=T),
            "White"=sum(race_c1___1==1, na.rm = T)+sum(race_c2___1==1, na.rm = T)+sum(race_c3___1==1, na.rm = T)+sum(race_c4___1==1, na.rm = T),
            "Black or African American"=sum(race_c1___2==1, na.rm = T)+sum(race_c2___2==1, na.rm = T)+sum(race_c3___2==1, na.rm = T)+sum(race_c4___2==1, na.rm = T),
            "American Indian or Alaska Native"=sum(race_c1___3==1, na.rm = T)+sum(race_c2___3==1, na.rm = T)+sum(race_c3___3==1, na.rm = T)+sum(race_c4___3==1, na.rm = T),
            "Asian"=sum(race_c1___4==1, na.rm = T)+sum(race_c2___4==1, na.rm = T)+sum(race_c3___4==1, na.rm = T)+sum(race_c4___4==1, na.rm = T),
            "Native Hawaiian or Pacific Islander"=sum(race_c1___5==1, na.rm = T)+sum(race_c2___5==1, na.rm = T)+sum(race_c3___5==1, na.rm = T)+sum(race_c4___5==1, na.rm = T),
            "Some other race"=sum(race_c1___6==1, na.rm = T)+sum(race_c2___6==1, na.rm = T)+sum(race_c3___6==1, na.rm = T)+sum(race_c4___6==1, na.rm = T),
            "Don't know"=sum(race_c1___89==1, na.rm = T)+sum(race_c2___89==1, na.rm = T)+sum(race_c3___89==1, na.rm = T)+sum(race_c4___89==1, na.rm = T),
            "Refused"=sum(race_c1___99==1, na.rm = T)+sum(race_c2___99==1, na.rm = T)+sum(race_c3___99==1, na.rm = T)+sum(race_c4___99==1, na.rm = T))

Non_Hisp_Demog_City <- EPA %>%
  filter(child_health_and_activity_questionnaire_complete>0 & (latino_c1==1 | latino_c2==1 | latino_c3==1 |latino_c4==1)) %>%
  group_by(city) %>%
  summarise("Number of Children"=sum(number_children, na.rm=T),
            "White"=sum(race_c1___1==1, na.rm = T)+sum(race_c2___1==1, na.rm = T)+sum(race_c3___1==1, na.rm = T)+sum(race_c4___1==1, na.rm = T),
            "Black or African American"=sum(race_c1___2==1, na.rm = T)+sum(race_c2___2==1, na.rm = T)+sum(race_c3___2==1, na.rm = T)+sum(race_c4___2==1, na.rm = T),
            "American Indian or Alaska Native"=sum(race_c1___3==1, na.rm = T)+sum(race_c2___3==1, na.rm = T)+sum(race_c3___3==1, na.rm = T)+sum(race_c4___3==1, na.rm = T),
            "Asian"=sum(race_c1___4==1, na.rm = T)+sum(race_c2___4==1, na.rm = T)+sum(race_c3___4==1, na.rm = T)+sum(race_c4___4==1, na.rm = T),
            "Native Hawaiian or Pacific Islander"=sum(race_c1___5==1, na.rm = T)+sum(race_c2___5==1, na.rm = T)+sum(race_c3___5==1, na.rm = T)+sum(race_c4___5==1, na.rm = T),
            "Some other race"=sum(race_c1___6==1, na.rm = T)+sum(race_c2___6==1, na.rm = T)+sum(race_c3___6==1, na.rm = T)+sum(race_c4___6==1, na.rm = T),
            "Don't know"=sum(race_c1___89==1, na.rm = T)+sum(race_c2___89==1, na.rm = T)+sum(race_c3___89==1, na.rm = T)+sum(race_c4___89==1, na.rm = T),
            "Refused"=sum(race_c1___99==1, na.rm = T)+sum(race_c2___99==1, na.rm = T)+sum(race_c3___99==1, na.rm = T)+sum(race_c4___99==1, na.rm = T))

Ethnicity_Demo_Table <- cbind(t(Hisp_Demog), t(Non_Hisp_Demog), t(Non_Hisp_Demog_City), t(Non_Hisp_Demog_City))
Ethnicity_Demo_Table <- as.data.frame(Ethnicity_Demo_Table)
colnames(Ethnicity_Demo_Table) <- c("All_Hispanic", "All_Non_Hispanic", "Greensboro_Hispanic", "Miami_Hispanic", "Tucson_Hispanic", "Greensboro_Non_Hispanic", "Miami_Non_Hispanic", "Tucson_Non_Hispanic")

Ethnicity_Demo_Table <- Ethnicity_Demo_Table[-c(1),]


write_xlsx(Demo_Table, "EPA_Demo_Table.xlsx")
write_xlsx(Ethnicity_Demo_Table, "Ethnicity_Demo_Table.xlsx")

#Number of children with autism
Behavioral_Health <- EPA %>% 
  summarise("Developmental Delays"=sum(diagnoses_c1___6==1, na.rm = T)+sum(diagnoses_c2___6==1, na.rm = T)+sum(diagnoses_c3___6==1, na.rm = T)+sum(diagnoses_c4___6==1, na.rm = T),
            "Learning Disorders"=sum(diagnoses_c1___7==1, na.rm = T)+sum(diagnoses_c2___7==1, na.rm = T)+sum(diagnoses_c3___7==1, na.rm = T)+sum(diagnoses_c4___7==1, na.rm = T),
            "Behavioral / Mental Health"=sum(diagnoses_c1___8==1, na.rm = T)+sum(diagnoses_c2___8==1, na.rm = T)+sum(diagnoses_c3___8==1, na.rm = T)+sum(diagnoses_c4___8==1, na.rm = T),
            "ADHD/ADD"=sum(diagnoses_c1___9==1, na.rm = T)+sum(diagnoses_c2___9==1, na.rm = T)+sum(diagnoses_c3___9==1, na.rm = T)+sum(diagnoses_c4___9==1, na.rm = T),
            "ASD"=sum(diagnoses_c1___10==1, na.rm = T)+sum(diagnoses_c2___10==1, na.rm = T)+sum(diagnoses_c3___10==1, na.rm = T)+sum(diagnoses_c4___10==1, na.rm = T))
#These may have some overlap (i.e., those who selected ASD may also have selected 'Developmental Delays' etc.)
