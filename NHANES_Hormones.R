#NHANES Check for Hormones
#LBXTSH1 - Thyroid stimulating hormone (uIU/mL)
#LBXTT3 - Triiodothyronine (T3), total (ng/dL) -> ng/mL
#LBXT3F - Triiodothyronine (T3), free (pg/mL)
#LBXTT4 - Thyroxine, total (T4) (ug/dL) -> ng/mL
#LBXT4F - Thyroxine, free (ng/dL) -> pg/mL

#Cortisol? Can't find it. 

library(nhanesA)
nhanesSearchVarName('LBXTSH1')
Thyroid_NHANES <- nhanes("THYROD_G", includelabels = T)

Thyroid_NHANES$LBXTT3_ng_mL <- Thyroid_NHANES$LBXTT3/100
Thyroid_NHANES$LBXTT4_ng_mL <- Thyroid_NHANES$LBXTT4*10
Thyroid_NHANES$LBXT4F_pg_mL <- Thyroid_NHANES$LBXT4F*10

Age2_NHANES <- nhanes("DEMO_G", includelabels = T)

Thyroid_Adult_NHANES <- merge(Thyroid_NHANES, Age2_NHANES, by = "SEQN")

Thyroid_Adult_NHANES <- subset(Thyroid_Adult_NHANES, Thyroid_Adult_NHANES$RIDAGEYR>17)

hist1a_NHANES <- ggplot(Thyroid_Adult_NHANES, aes(x=LBXTSH1))+
  geom_histogram(aes(y=..density..), fill="lightblue", color="black")+
  geom_density(alpha=.5, fill="darkred")+
  xlab("TSH (mIU/mL)")+
  ylab("")+
  theme(panel.background = element_rect(fill="#ffffff"),
        text = element_text(size=15))

hist2_NHANES <- ggplot(Thyroid_Adult_NHANES, aes(x=LBXTT3_ng_mL))+
  geom_histogram(aes(y=..density..), fill="lightblue", color="black")+
  geom_density(alpha=.5, fill="darkred")+
  xlab("Total T3 (ng/mL)")+
  ylab("")+
  theme(panel.background = element_rect(fill="#ffffff"),
        text = element_text(size=15))

hist3_NHANES <- ggplot(Thyroid_Adult_NHANES, aes(x=LBXT3F))+
  geom_histogram(aes(y=..density..), fill="lightblue", color="black")+
  geom_density(alpha=.5, fill="darkred")+
  xlab("Free T3 (pg/mL)")+
  ylab("")+
  theme(panel.background = element_rect(fill="#ffffff"),
        text = element_text(size=15))

hist4_NHANES <- ggplot(Thyroid_Adult_NHANES, aes(x=LBXTT4_ng_mL))+
  geom_histogram(aes(y=..density..), fill="lightblue", color="black")+
  geom_density(alpha=.5, fill="darkred")+
  xlab("Total T4 (ng/mL)")+
  ylab("")+
  theme(panel.background = element_rect(fill="#ffffff"),
        text = element_text(size=15))

hist5_NHANES <- ggplot(Thyroid_Adult_NHANES, aes(x=LBXT4F_pg_mL))+
  geom_histogram(aes(y=..density..), fill="lightblue", color="black")+
  geom_density(alpha=.5, fill="darkred")+
  xlab("Free T4 (pg/mL)")+
  ylab("")+
  theme(panel.background = element_rect(fill="#ffffff"),
        text = element_text(size=15))

tiff("Hormone_Hists_NHANES.tiff", units="in", width=10, height=5, res=300) #Only to save in high def.
ggarrange(hist1a_NHANES, hist2_NHANES, hist3_NHANES, hist4_NHANES, hist5_NHANES, ncol = 3, nrow=2)
dev.off()

#Yuma
All_Data %>% 
  summarise(n=sum(!is.na(TSH)|!is.na(fT4)|!is.na(fT3)|!is.na(TotalT4)|!is.na(TotalT3)),
            geoMeanTSH=geoMean(TSH, na.rm = T),
            geoSDTSH=geoSD(TSH, na.rm = T),
            medianTSH=median(TSH, na.rm = T),
            maxTSH=max(TSH, na.rm = T),
            geoMeanfT4=geoMean(fT4, na.rm = T),
            geoSDfT4=geoSD(fT4, na.rm = T),
            medianfT4=median(fT4, na.rm = T),
            maxfT4=max(fT4, na.rm = T),
            geoMeanfT3=geoMean(fT3, na.rm = T),
            geoSDfT3=geoSD(fT3, na.rm = T),
            medianfT3=median(fT3, na.rm = T),
            maxfT3=max(fT3, na.rm = T),
            geoMeanTT4=geoMean(TotalT4, na.rm = T),
            geoSDTT4=geoSD(TotalT4, na.rm = T),
            medianTT4=median(TotalT4, na.rm = T),
            maxTT4=max(TotalT4, na.rm = T),
            geoMeanTT3=geoMean(TotalT3, na.rm = T),
            geoSDTT3=geoSD(TotalT3, na.rm = T),
            medianTT3=median(TotalT3, na.rm = T),
            maxTT3=max(TotalT3, na.rm = T))

All_Data %>% 
  summarise(n=sum(!is.na(TSH)|!is.na(fT4)|!is.na(fT3)|!is.na(TotalT4)|!is.na(TotalT3)),
            MinTSH=min(TSH, na.rm = T),
            MinfT4=min(fT4, na.rm = T),
            MinfT3=min(fT3, na.rm = T),
            MinTT4=min(TotalT4, na.rm = T),
            MinTT3=min(TotalT3, na.rm = T))

#Yuma Without Meds
All_Data %>% 
  filter(thyroid_med==0) %>% 
  summarise(n=sum(!is.na(TSH)|!is.na(fT4)|!is.na(fT3)|!is.na(TotalT4)|!is.na(TotalT3)),
            geoMeanTSH=geoMean(TSH, na.rm = T),
            geoSDTSH=geoSD(TSH, na.rm = T),
            medianTSH=median(TSH, na.rm = T),
            maxTSH=max(TSH, na.rm = T),
            geoMeanfT4=geoMean(fT4, na.rm = T),
            geoSDfT4=geoSD(fT4, na.rm = T),
            medianfT4=median(fT4, na.rm = T),
            maxfT4=max(fT4, na.rm = T),
            geoMeanfT3=geoMean(fT3, na.rm = T),
            geoSDfT3=geoSD(fT3, na.rm = T),
            medianfT3=median(fT3, na.rm = T),
            maxfT3=max(fT3, na.rm = T),
            geoMeanTT4=geoMean(TotalT4, na.rm = T),
            geoSDTT4=geoSD(TotalT4, na.rm = T),
            medianTT4=median(TotalT4, na.rm = T),
            maxTT4=max(TotalT4, na.rm = T),
            geoMeanTT3=geoMean(TotalT3, na.rm = T),
            geoSDTT3=geoSD(TotalT3, na.rm = T),
            medianTT3=median(TotalT3, na.rm = T),
            maxTT3=max(TotalT3, na.rm = T))

All_Data %>% 
  filter(thyroid_med==0) %>% 
  summarise(n=sum(!is.na(TSH)|!is.na(fT4)|!is.na(fT3)|!is.na(TotalT4)|!is.na(TotalT3)),
            MinTSH=min(TSH, na.rm = T),
            MinfT4=min(fT4, na.rm = T),
            MinfT3=min(fT3, na.rm = T),
            MinTT4=min(TotalT4, na.rm = T),
            MinTT3=min(TotalT3, na.rm = T))

#NHANES
Thyroid_Adult_NHANES %>% 
  summarise(n=sum(!is.na(LBXTSH1)|!is.na(LBXT4F_pg_mL)|!is.na(LBXT3F)|!is.na(LBXTT4_ng_mL)|!is.na(LBXTT3_ng_mL)),
            geoMeanTSH=geoMean(as.numeric(LBXTSH1), na.rm = T),
            geoSDTSH=geoSD(as.numeric(LBXTSH1), na.rm = T),
            medianTSH=median(as.numeric(LBXTSH1), na.rm = T),
            maxTSH=max(as.numeric(LBXTSH1), na.rm = T),
            geoMeanfT4=geoMean(as.numeric(LBXT4F_pg_mL), na.rm = T),
            geoSDfT4=geoSD(as.numeric(LBXT4F_pg_mL), na.rm = T),
            medianfT4=median(as.numeric(LBXT4F_pg_mL), na.rm = T),
            maxfT4=max(as.numeric(LBXT4F_pg_mL), na.rm = T),
            geoMeanfT3=geoMean(as.numeric(LBXT3F), na.rm = T),
            geoSDfT3=geoSD(as.numeric(LBXT3F), na.rm = T),
            medianfT3=median(as.numeric(LBXT3F), na.rm = T),
            maxfT3=max(as.numeric(LBXT3F), na.rm = T),
            geoMeanTT4=geoMean(as.numeric(LBXTT4_ng_mL), na.rm = T),
            geoSDTT4=geoSD(as.numeric(LBXTT4_ng_mL), na.rm = T),
            medianTT4=median(as.numeric(LBXTT4_ng_mL), na.rm = T),
            maxTT4=max(as.numeric(LBXTT4_ng_mL), na.rm = T),
            geoMeanTT3=geoMean(as.numeric(LBXTT3_ng_mL), na.rm = T),
            geoSDTT3=geoSD(as.numeric(LBXTT3_ng_mL), na.rm = T),
            medianTT3=median(as.numeric(LBXTT3_ng_mL), na.rm = T),
            maxTT3=max(as.numeric(LBXTT3_ng_mL), na.rm = T))


Thyroid_Adult_NHANES %>% 
  summarise(n=sum(!is.na(LBXTSH1)|!is.na(LBXT4F_pg_mL)|!is.na(LBXT3F)|!is.na(LBXTT4_ng_mL)|!is.na(LBXTT3_ng_mL)),
            MinTSH=min(as.numeric(LBXTSH1), na.rm = T),
            MinfT4=min(as.numeric(LBXT4F_pg_mL), na.rm = T),
            MinfT3=min(as.numeric(LBXT3F), na.rm = T),
            MinTT4=min(as.numeric(LBXTT4_ng_mL), na.rm = T),
            MinTT3=min(as.numeric(LBXTT3_ng_mL), na.rm = T))

All_Data_No_Meds_Tho <- subset(All_Data, All_Data$thyroid_med==0)

#TSH
t.test(x=log10(All_Data$TSH), mu=mean(log10(as.numeric(Thyroid_Adult_NHANES$LBXTSH1)), na.rm = T))
t.test(x=log10(All_Data_No_Meds_Tho$TSH), mu=mean(log10(as.numeric(Thyroid_Adult_NHANES$LBXTSH1)), na.rm = T))

#Total T3
t.test(x=log10(All_Data$TotalT3), mu=mean(log10(as.numeric(Thyroid_Adult_NHANES$LBXTT3_ng_mL)), na.rm = T))
t.test(x=log10(All_Data_No_Meds_Tho$TotalT3), mu=mean(log10(as.numeric(Thyroid_Adult_NHANES$LBXTT3_ng_mL)), na.rm = T))

#Free T3
t.test(x=log10(All_Data$fT3), mu=mean(log10(as.numeric(Thyroid_Adult_NHANES$LBXT3F)), na.rm = T))
t.test(x=log10(All_Data_No_Meds_Tho$fT3), mu=mean(log10(as.numeric(Thyroid_Adult_NHANES$LBXT3F)), na.rm = T))

#Total T4
t.test(x=log10(All_Data$TotalT4), mu=mean(log10(as.numeric(Thyroid_Adult_NHANES$LBXTT4_ng_mL)), na.rm = T))
t.test(x=log10(All_Data_No_Meds_Tho$TotalT4), mu=mean(log10(as.numeric(Thyroid_Adult_NHANES$LBXTT4_ng_mL)), na.rm = T))

#Free T4
t.test(x=log10(All_Data$fT4), mu=mean(log10(as.numeric(Thyroid_Adult_NHANES$LBXT4F_pg_mL)), na.rm = T))
t.test(x=log10(All_Data_No_Meds_Tho$fT4), mu=mean(log10(as.numeric(Thyroid_Adult_NHANES$LBXT4F_pg_mL)), na.rm = T))


#Now looking at NHANES Hg to make sure the original student got the numbers right.
nhanesSearchVarName('HRDHG')
Hg_NHANES <- nhanes("LAB22", includelabels = T)
mean(Hg_NHANES$HRDHG, na.rm=T)
geoMean(as.numeric(Hg_NHANES$HRDHG), na.rm=T)

#Oops, just want adults
nhanesSearchVarName('RIDAGEYR')
Age_NHANES <- nhanes("DEMO", includelabels = T)

Hg_Adult_NHANES <- merge(Hg_NHANES, Age_NHANES, by = "SEQN")

Hg_Adult_NHANES <- subset(Hg_Adult_NHANES, Hg_Adult_NHANES$RIDAGEYR>17)

mean(Hg_Adult_NHANES$HRDHG, na.rm=T)
geoMean(as.numeric(Hg_Adult_NHANES$HRDHG), na.rm=T)
median(Hg_Adult_NHANES$HRDHG, na.rm = T)
