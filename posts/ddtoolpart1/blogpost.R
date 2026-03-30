library(rms)
library(risksetROC)

dd <- datadist(esrd)
options(datadist='dd')


fitSURVfinal <- cph(Surv(time, status)   ~   egfr_bsl+tkv_bsl_log+ age_bsl+ egfr_bsl:age_bsl + tkv_bsl_log:age_bsl,data=esrd, x = TRUE, y=TRUE)
fitSURVnointer <- cph(Surv(time, status) ~   egfr_bsl+ tkv_bsl_log + age_bsl,data=esrd, x = TRUE, y=TRUE)

finalmodelpredictions <-  Predict(fitSURVfinal,
                                  tkv_bsl_log=log(c(500,1000,1500)),
                                  age_bsl    =c(20,40,60),
                                  egfr_bsl   =c(40,50,60)) %>% 
  as.data.frame() %>% 
  mutate(egfr_bsl_2 = paste("eGFR:",egfr_bsl,"mL/min"),
         age_bsl_2 = paste("Age:",age_bsl,"years"))


finalmodelpredictionsnointeraction <-  Predict(fitSURVnointer,
                                  tkv_bsl_log=log(c(500,1000,1500)),
                                  age_bsl    =c(20,40,60),
                                  egfr_bsl   =c(40,50,60)) %>% 
  as.data.frame() %>% 
  mutate(egfr_bsl_2 = paste("eGFR:",egfr_bsl,"mL/min"),
         age_bsl_2 = paste("Age:",age_bsl,"years"))



finalmodelpredictionsnointeraction$model <- "No\nInteractions"
finalmodelpredictions$model <- "Interactions:\negfr:age_bsl\ntkv:age"

finalmodelpredictions <- rbind(finalmodelpredictions,finalmodelpredictionsnointeraction)
a<- ggplot(finalmodelpredictions,aes(x=exp(tkv_bsl_log),y=yhat,
                                   color= as.factor(age_bsl_2),
                                   fill = as.factor(age_bsl_2)))+
  geom_hline(yintercept = log(1),lty=2)+
  geom_ribbon(aes(ymin=lower,ymax=upper,group=interaction(age_bsl)),alpha=0.2,
              color=NA)+
  geom_line()+
  facet_grid(model~egfr_bsl_2,switch="y")+
  labs(y="Log Relative Hazard Ratio",
       x="Baseline Kidney Volume (mL) - Log10 Scale",
       fill="Baseline Age",color="Baseline Age")+
  theme_bw(base_size = 14)+
  theme(legend.position="top",strip.placement = "outside",
        strip.background = element_rect(fill="#475c6b"),
        strip.text =       element_text(face = "bold",color = "white",angle = 0),
        strip.text.y.left =       element_text(face = "bold",color = "white",angle = 0,
                                               hjust=0,vjust=1),
        strip.text.x = element_text(size = 14))+
  scale_x_log10(breaks=c(500,700,1000,1500),guide = guide_axis(n.dodge = 2))+
    scale_color_manual(values=tableau10)+
  scale_fill_manual(values=tableau10)


b<- ggplot(finalmodelpredictions,aes(x=egfr_bsl_2,y=yhat,
                                 color= as.factor(exp(tkv_bsl_log)),
                                 fill = as.factor(exp(tkv_bsl_log) )))+
  geom_hline(yintercept = log(1),lty=2)+
  
  geom_ribbon(aes(ymin=lower,ymax=upper,group=interaction(tkv_bsl_log)),alpha=0.2,
              color=NA)+
  geom_line(aes(group=as.factor(exp(tkv_bsl_log) )))+
  facet_grid(model~age_bsl_2,switch="y")+
  labs(y="Log Relative Hazard Ratio",
       x="Baseline eGFR",
       fill="Baseline Kidney Volume (mL)",color="Baseline Kidney Volume (mL)")+
  theme_bw(base_size = 14)+
  theme(legend.position="top",strip.placement = "outside",
        strip.background = element_rect(fill="#475c6b"),
        strip.text =       element_text(face = "bold",color = "white",angle = 0),
        strip.text.y.left =       element_text(face = "bold",color = "white",angle = 0,
                                               hjust=0,vjust=1),
        strip.text.x = element_text(size = 14))+
  scale_x_discrete( guide = guide_axis(n.dodge = 3))+
  scale_color_manual(values=tableau10[4:6])+
  scale_fill_manual(values=tableau10[4:6])

a|(b+theme(strip.background.y.left = element_blank(),
           strip.text.y.left = element_blank(),
           axis.title.y.left = element_blank()))

library(survival)
library(partykit)
set.seed(165461)

simple_tree <- ctree(Surv(time, status) ~ egfr_bsl+tkv_bsl+ age_bsl, 
                     data = esrd,
                     control = ctree_control(
                       alpha = 0.01,      # Stricter significance (default 0.05)
                       maxdepth = 3,      # Limits the tree to 3 levels deep
                       minsplit = 50,     # Must have 50 obs to try a split
                       minbucket = 20     # Each leaf must have at least 20 obs
                     ))
plot(simple_tree)


