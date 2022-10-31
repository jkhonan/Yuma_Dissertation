# Calculating Statistical Power

#https://www.programmingr.com/examples/neat-tricks/sample-r-function/how-to-seize-pwr-statistical-power-analysis-in-r/
  
install.packages("pwr")

#power of 0.80
#alpha of 0.05
#sample size based on number of participants who responded to that question (missing responses not included)
#calculating estimated effect size or min correlation coefficient.

library(pwr)

#Perchlorate and Hypothyroidism
pwr.chisq.test(w=NULL, N=293, df=(293-5), sig.level=0.05, power = 0.80) #df: age, gender, ethnicity, income, thyroid medication

#Perchlorate and Thyroid Disease
pwr.chisq.test(w=NULL, N=318, df=(318-5), sig.level=0.05, power = 0.80) #df: age, gender, ethnicity, income, thyroid medication

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




