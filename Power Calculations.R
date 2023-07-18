# Calculating Statistical Power

All_Data %>%
  filter(hypothyroid=="1"|
           thyroid_cancer=="1"|
           goiter=="1"|
           emr_hypothyroidism=="1"|
           emr_thyroid_cancer=="1"|
           emr_goiter=="1") %>% 
  summarise(n=length(ID))



#https://www.programmingr.com/examples/neat-tricks/sample-r-function/how-to-seize-pwr-statistical-power-analysis-in-r/
  
install.packages("pwr")
install.packages("WebPower")
install.packages("epiR")

#power of >= 0.80
#alpha of 0.05
#sample size based on number of participants who responded to that question (missing responses not included)
#calculating estimated effect size or min correlation coefficient.

library(pwr)
library(WebPower)
library(epiR)
library(EnvStats)

#Wow this is complicated... Here I'm now using G*Power...
H_0 <- sum(All_Data$hypo_recode==1&All_Data$Result_P_ng_mL>=geoMean(as.numeric(All_Data$Result_P_ng_mL), na.rm = T), na.rm = T)/295
H_0 <- sum(All_Data$thyroid_prob_D==1&All_Data$Result_P_ng_mL>=geoMean(as.numeric(All_Data$Result_P_ng_mL), na.rm = T), na.rm = T)/295
H_0 <- sum(All_Data$emr_thyroid_problem==1&All_Data$Result_P_ng_mL>=geoMean(as.numeric(All_Data$Result_P_ng_mL), na.rm = T), na.rm = T)/248
H_0 <- sum(All_Data$emr_hypothyroidism==1&All_Data$Result_P_ng_mL>=geoMean(as.numeric(All_Data$Result_P_ng_mL), na.rm = T), na.rm = T)/248

H_0 <- sum(All_Data$emr_thyroid_problem_fromothercolumns==1&All_Data$Result_P_ng_mL>=geoMean(as.numeric(All_Data$Result_P_ng_mL), na.rm = T), na.rm = T)/248

All_Data %>% 
  filter(!is.na(Result_P_ng_mL)) %>% 
  summarise(geomean=geoMean(as.numeric(Result_P_ng_mL)),
            geosd=geoSD(as.numeric(Result_P_ng_mL)))

geoMean(as.numeric(!is.na(All_Data$Result_P_ng_mL)))
sum(!is.na(All_Data$Result_P_ng_mL)<0)

#Just needed to know the Pr(Y=1|X=1) H0 value.


#Values for hormone numbers
sum(!is.na(All_Data$TSH))
sum(!is.na(All_Data$TotalT4))
sum(!is.na(All_Data$TotalT3))
sum(!is.na(All_Data$fT3))
sum(!is.na(All_Data$fT4))

#4 predictors, u = 3 df, v = 295-4=291, alpha = 0.05, power=0.8, 
pwr.f2.test(u = 3,
            v = 291,
            f2 = NULL,
            power = 0.8)









#Other stuff below that didn't work. Best for crude models:

A <- sum(All_Data$hypo_recode==1&All_Data$Result_P_ng_mL>=1, na.rm = T)
B <- sum(All_Data$hypo_recode==0&All_Data$Result_P_ng_mL>=1, na.rm = T)
C <- sum(All_Data$hypo_recode==1&All_Data$Result_P_ng_mL<1, na.rm = T)
D <- sum(All_Data$hypo_recode==0&All_Data$Result_P_ng_mL<1, na.rm = T)

(A*D)/(B*C)

#Perchlorate and Self Report Hypothyroidism
length(Perchlorate$`Sample ID`) #295
sum(!is.na(All_Data$hypothyroid)) #321

wp.logistic(n = 295, 
            p0 = (sum(All_Data$hypo_recode==1&All_Data$Result_P_ng_mL<1, na.rm = T)/sum(All_Data$Result_P_ng_mL<1, na.rm = T)), 
            p1 = (sum(All_Data$hypo_recode==1&All_Data$Result_P_ng_mL>=1, na.rm = T)/sum(All_Data$Result_P_ng_mL>=1, na.rm = T)), 
            alpha = 0.05,
            power = NULL,
            alternative = c("two.sided"),
            family = c("lognormal"))

epi.sscc(OR = NA, p1 = 0.49, p0 = 0.36, n = 295, power = 0.8, r = 1, 
         phi.coef = 0, design = 1, sided.test = 2, nfractional = FALSE, 
         conf.level = 0.95, method = "unmatched", fleiss = FALSE)

#Perchlorate and Self Report Thyroid Disease
length(Perchlorate$`Sample ID`) #295
sum(!is.na(All_Data$thyroid_prob)) #320

wp.logistic(n = 295, 
            p0 = (sum(All_Data$thyroid_prob==1&All_Data$Result_P_ng_mL<1, na.rm = T)/sum(All_Data$Result_P_ng_mL<1, na.rm = T)), 
            p1 = (sum(All_Data$thyroid_prob==1&All_Data$Result_P_ng_mL>=1, na.rm = T)/sum(All_Data$Result_P_ng_mL>=1, na.rm = T)), 
            alpha = 0.05,
            power = NULL,
            alternative = c("two.sided"),
            family = c("lognormal"))

epi.sscc(OR = NA, p1 = 0.31, p0 = 0.24, n = 295, power = 0.8, r = 1, 
         phi.coef = 0, design = 1, sided.test = 2, nfractional = FALSE, 
         conf.level = 0.95, method = "unmatched", fleiss = FALSE)

#Perchlorate and EMR Hypothyroidism
length(Perchlorate$`Sample ID`) #295
sum(!is.na(All_Data$emr_hypothyroidism)) #248

wp.logistic(n = 248, 
            p0 = (sum(All_Data$emr_hypothyroidism==1&All_Data$Result_P_ng_mL<1, na.rm = T)/sum(!is.na(All_Data$emr_hypothyroidism))), 
            p1 = (sum(All_Data$emr_hypothyroidism==1&All_Data$Result_P_ng_mL>=1, na.rm = T)/sum(!is.na(All_Data$emr_hypothyroidism))), 
            alpha = 0.05,
            power = NULL,
            alternative = c("two.sided"),
            family = c("lognormal"))

epi.sscc(OR = NA, p1 = 0.41, p0 = 0.04, n = 248, power = 0.8, r = 1, 
         phi.coef = 0, design = 1, sided.test = 2, nfractional = FALSE, 
         conf.level = 0.95, method = "unmatched", fleiss = FALSE)

#Perchlorate and EMR Thyroid Disease
length(Perchlorate$`Sample ID`) #295
sum(!is.na(All_Data$emr_thyroid_problem)) #248

wp.logistic(n = 248, 
            p0 = (sum(All_Data$emr_thyroid_problem==1&All_Data$Result_P_ng_mL<1, na.rm = T)/sum(!is.na(All_Data$emr_thyroid_problem))), 
            p1 = (sum(All_Data$emr_thyroid_problem==1&All_Data$Result_P_ng_mL>=1, na.rm = T)/sum(!is.na(All_Data$emr_thyroid_problem))), 
            alpha = 0.05,
            power = NULL,
            alternative = c("two.sided"),
            family = c("lognormal"))

epi.sscc(OR = NA, p1 = 0.31, p0 = 0.24, n = 295, power = 0.8, r = 1, 
         phi.coef = 0, design = 1, sided.test = 2, nfractional = FALSE, 
         conf.level = 0.95, method = "unmatched", fleiss = FALSE)








#Perchlorate and BMI
pwr.r.test(r=NULL, n=323, sig.level=0.05, power = 0.80, alternative = "two.sided") 

#Perchlorate and Obesity (based on BMI)
pwr.r.test(r=NULL, n=322, sig.level=0.05, power = 0.80, alternative = "two.sided")

#PC1, PC2, and CVD
pwr.chisq.test(w=NULL, N=314, df=(314-5), sig.level=0.05, power = 0.80) #df: age, gender, ethnicity, income, smoking

#PC1, PC2, and DM
pwr.chisq.test(w=NULL, N=252, df=(252-5), sig.level=0.05, power = 0.80) #df: age, gender, ethnicity, income, insulin/meds

#PC1, PC2 and BMI
pwr.r.test(r=NULL, n=323, sig.level=0.05, power = 0.80, alternative = "two.sided")

#PC1, PC2 and Obesity
pwr.chisq.test(w=NULL, N=251, df=(251-4), sig.level=0.05, power = 0.80) #df: age, gender, ethnicity, income

#Perchlorate and Obesity / FLD, CPS
pwr.r.test(r=NULL, n=550, sig.level=0.05, power = 0.80, alternative = "two.sided")

#Perchlorate and Perchlorate, CPS and Yuma Projects
pwr.2p2n.test(h=NULL, n1=550, n2=323, sig.level = 0.05, power = 0.80, alternative = "two.sided")

#Perchlorate and Perchlorate, NHANES and Yuma Projects
pwr.2p2n.test(h=NULL, n1=2979, n2=323, sig.level = 0.05, power = 0.80, alternative = "two.sided")




