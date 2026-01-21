
TTPAUC28  <- 400 # computed dynamically from the TTP curve
hazard_fn_auc28 <- function(t){ exp(-11.8 -0.0270*t +
                                      2.45*log(t+1) +
                                      0.0353*((TTPAUC28-400)/10) ) }

Ftauc28 <- apply_survival_function(seq(0,120,0.1),hazard_fn_auc28,
                                   supplied_fn_type="h",
                                   fn_type_to_apply="F")
Stauc28 <- apply_survival_function(seq(0,120,0.1), hazard_fn_auc28,
                                   supplied_fn_type="h",
                                   fn_type_to_apply="S")
htauc28 <- apply_survival_function(seq(0,120,0.1), hazard_fn_auc28,
                                   supplied_fn_type="h",
                                   fn_type_to_apply="h")

df1<- data.frame(Time=seq(0,120,0.1),
           F_t=Ftauc28,
           S_t=Stauc28,
           h_t=htauc28)
df1$AUC <- 400
TTPAUC28  <- 600 # computed dynamically from the TTP curve

Ftauc28 <- apply_survival_function(seq(0,120,0.1),hazard_fn_auc28,
                                   supplied_fn_type="h",
                                   fn_type_to_apply="F")
Stauc28 <- apply_survival_function(seq(0,120,0.1), hazard_fn_auc28,
                                   supplied_fn_type="h",
                                   fn_type_to_apply="S")
htauc28 <- apply_survival_function(seq(0,120,0.1), hazard_fn_auc28,
                                   supplied_fn_type="h",
                                   fn_type_to_apply="h")


df2<- data.frame(Time=seq(0,120,0.1),
                 F_t=Ftauc28,
                 S_t=Stauc28,
                 h_t=htauc28)
df2$AUC <- 600

TTPAUC28  <- 200 # computed dynamically from the TTP curve
Ftauc28 <- apply_survival_function(seq(0,120,0.1),hazard_fn_auc28,
                                   supplied_fn_type="h",
                                   fn_type_to_apply="F")
Stauc28 <- apply_survival_function(seq(0,120,0.1), hazard_fn_auc28,
                                   supplied_fn_type="h",
                                   fn_type_to_apply="S")
htauc28 <- apply_survival_function(seq(0,120,0.1), hazard_fn_auc28,
                                   supplied_fn_type="h",
                                   fn_type_to_apply="h")

df3<- data.frame(Time=seq(0,120,0.1),
                 F_t=Ftauc28,
                 S_t=Stauc28,
                 h_t=htauc28)
df3$AUC <- 200

dft<- rbind(df1,df2,df3)
dft$AUC<-as.factor(dft$AUC)
ggplot(dft %>% 
         gather(key,value,S_t,h_t,F_t,factor_key = TRUE))+
  geom_line(aes(x=Time,y=value,linetype=AUC))+
  labs(y="Survival Analysis Functions",
       linetype="TTP AUC",
       x="Time since Trial Start (days)")+
  theme_bw()+
  facet_wrap(~key,scales="free")+
  guides(linetype=guide_legend(reverse=TRUE))+
  scale_x_continuous(breaks=c(0,14,28,42,56,70,84,98,
                              98+14))
  

