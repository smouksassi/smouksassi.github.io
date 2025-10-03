library(lme4)       
library(ggplot2)
library(pbapply)
library(combinat)

# Simulate some data for demonstration
set.seed(031230)
NID <- 8
IDtime <- seq(0,10,1)
time <-  rep(IDtime,NID)
ID <- factor(rep(1:NID, each = length(IDtime)))
modeldata <- data.frame(ID=ID,time=time,
                        cov = ifelse(as.numeric(ID)<NID/2,1,2))
modeldata$slope <- -2+rep(rnorm(NID,sd= 2*0.2) ,each = length(IDtime))
modeldata$int   <- 100+rep(rnorm(NID,sd= 100*0.2) ,each = length(IDtime))
modeldata$contresponse   <-  (modeldata$int+(modeldata$cov==2)*10) + 
                      (modeldata$slope *(1+(modeldata$cov==2)*1.25) )*modeldata$time +
                  rnorm(nrow(modeldata), sd = 0.3)
ggplot(modeldata,aes(time,contresponse))+
  geom_line(aes(group=ID))+
  geom_point()+
  facet_grid(~cov)

sample(c(1,1,1,1,2,2,2,1),NID,replace = TRUE)


model0 <- lmer(contresponse ~ time +
                 (1 | ID) + (0+time | ID),
               data=modeldata,
               REML = FALSE)
model1 <- lmer(contresponse ~ time + cov +
                 (1 | ID) + (0+time | ID),
               data=modeldata,
               REML = FALSE)
model2 <- lmer(contresponse ~ time + time:cov+
                 (1 | ID) + (0+time | ID),
               data=modeldata,
               REML = FALSE)
anova(model1,model0)
lrt_stat_m1_m0 <- as.numeric(2 * (logLik(model1) - logLik(model0)))  
p_value__m1_m0 <- pchisq(lrt_stat_m1_m0, df = 1, lower.tail = FALSE)    
p_value__m1_m0
anova(model2,model0)
lrt_stat_m2_m0 <- as.numeric(2 * (logLik(model2) - logLik(model0)))  
p_value__m2_m0 <- pchisq(lrt_stat_m2_m0, df = 1, lower.tail = FALSE)    
p_value__m2_m0



n_permutations <- 1000  # Number of permutations but we know there is unique 56 combinations
lrt_null_distribution <- pbapply::pbreplicate(n_permutations, {
  permcov <- modeldata %>%
  distinct(ID,cov)%>%
  mutate(cov_permuted2= sample(cov,replace = FALSE))#
  modeldataperm <- left_join(modeldata,permcov)
  model0_perm <- model0 
  model2_perm <- lmer(contresponse ~ time+ cov_permuted2 +
                        (1 | ID) + (0+time | ID)   ,
                      data = modeldataperm,
                      REML = FALSE)
  as.numeric(2 * (logLik(model2_perm) - logLik(model0)))
})

ggplot(data.frame(lrt=lrt_null_distribution,perm=1:length(lrt_null_distribution)),
       aes(lrt)) +
  geom_step(stat="ecdf",aes(col="permutation based"))+
  geom_step(data=data.frame(lrt= rchisq(n = 10000, df = 1)),
            stat="ecdf",aes(col="theoretical"))+
  geom_hline(yintercept = 0.95)+
  geom_vline(xintercept = 3.84)
p_value_permutation <- mean(lrt_null_distribution >= lrt_stat)
p_value_permutation
p_value__m1_m0


data_vector <- modeldata %>% 
  distinct(ID,cov) %>% 
  pull(cov)
list_of_permutations <- combinat::permn(data_vector)
matrix_of_permutations <- do.call(rbind, list_of_permutations)
unique_permutations <- unique(matrix_of_permutations)
print(unique_permutations)

n_permutations <- 56  # Number of permutations but we know there is unique 56 combinations
lrt_null_distribution <- NULL 
for (i in 1:n_permutations) {
  print(i)
  permcov <- modeldata %>%
    distinct(ID,cov)%>%
    mutate(cov_permuted2= unique_permutations[i,])#
  modeldataperm <- left_join(modeldata,permcov)
  model0_perm <- model0 
  model2_perm <- lmer(contresponse ~ time+ cov_permuted2 +
                        (1 | ID) + (0+time | ID)   ,
                      data = modeldataperm,
                      REML = FALSE)
  lrt_null_distribution[i] <-as.numeric(2 * (logLik(model2_perm) - logLik(model0)))
}

ggplot(data.frame(lrt=lrt_null_distribution,perm=1:length(lrt_null_distribution)),
       aes(lrt)) +
  geom_step(stat="ecdf",aes(col="permutation based"))+
  geom_step(data=data.frame(lrt= rchisq(n = 10000, df = 1)),
            stat="ecdf",aes(col="theoretical"))+
  geom_hline(yintercept = 0.95)+
  geom_vline(xintercept = 3.84)

p_value_permutation <- mean(lrt_null_distribution >= lrt_stat)
p_value_permutation
p_value__m1_m0



#

Th.start  <- c(lKe = -2.4, lKa = 0.45, lCl = -3.2)
nm1  <- nlmer(conc ~ SSfol(Dose , Time ,lKe , lKa , lCl) ~
                0+lKe+lKa+lCl +(lKe+lKa+lCl|Subject),
              nAGQ=0,
              Theoph,
              start = Th.start)
nm0  <- nlmer(conc ~ SSfol(Dose , Time ,lKe , lKa , lCl) ~
                0+lKe+lKa+lCl +(lKa+lCl|Subject),
              nAGQ=0,
              Theoph,
              start = Th.start)
varCompTest(nm1,nm0)






model00 <- lmer(contresponse ~ time +
                 (1 | ID)   ,
               data=modeldata,
               REML = FALSE)

model0 <- lmer(contresponse ~ time +
                 (1 | ID) + (0+time | ID)   ,
               data=modeldata,
               REML = FALSE)

anova(model0,model00)


model2 <- lmer(contresponse ~ time+ time:cov +
                 (1 | ID) + (0+time | ID)   ,
               data=modeldata,
               REML = FALSE)


lmer(contresponse ~ time+ time:cov +
       (1 | ID) + (0+time +cov| ID)   ,
     data=modeldata,
     REML = FALSE)



anova(model2,model0)
lrt_stat <- as.numeric(2 * (logLik(model2) - logLik(model0)))  
p_value_lrt <- pchisq(lrt_stat, df = 1, lower.tail = FALSE)    


model0 <- lmer(contresponse ~ time +
                 (1 | ID)    ,
               data=modeldata, REML = FALSE)
model1 <- lmer(contresponse ~ time +
                      (1 | ID) + (0+time | ID)   ,
                    data=modeldata, REML = FALSE)
anova(model1,model0)
n_permutations <- 100  # Number of permutations
lrt_null_distribution <- pbapply::pbreplicate(n_permutations, {
# permcov <- modeldata %>% 
# distinct(ID,cov)%>% 
# mutate(cov_permuted= sample(cov),
#        cov_permuted2= sample(cov)
#        )
# #modeldataperm <- left_join(modeldata,permcov)
  model0_perm <- model0 
  # model2_perm <- lmer(contresponse ~ time + cov_permuted2
  #                  (1 | ID) + (0+time | ID)   ,
  #                data=modeldataperm, REML = FALSE)
  model2_perm <- lmer(contresponse ~ time +
                   (1 | ID) + (0+time | ID)   ,
                 data=modeldataperm, REML = FALSE)
  
  as.numeric(2 * (logLik(model2_perm) - logLik(model0)))
})



ggplot(data.frame(lrt=lrt_null_distribution,perm=1:1000),
       aes(lrt)) +
  geom_step(stat="ecdf",aes(col="permutation based"))+
  geom_step(data=data.frame(lrt= rchisq(n = 10000, df = 1)),
            stat="ecdf",aes(col="theoretical"))+
  geom_hline(yintercept = 0.95)+
  geom_vline(xintercept = 3.84)
  



p_value_permutation <- mean(lrt_null_distribution >= lrt_stat)


hist(lrt_null_distribution, breaks = 30,
     main = "Null Distribution of LRT Statistic",
     xlab = "LRT Statistic", col = "lightblue")
abline(v = lrt_stat, col = "red", lwd = 2, lty = 2)
text(lrt_stat, 50, labels = paste("Observed\nLRT"), col = "red", pos = 4)



ggplot(data.frame(lrt=lrt_null_distribution,perm=1:1000),
       aes(x=lrt)) +
geom_histogram(bins = 30,alpha=0.1,color="red")+
  geom_vline(xintercept = lrt_stat, col = "red", lwd = 2, lty = 2)




set.seed(13215432)
NID <- 20
IDtime <- seq(0,10,1)
time <-  rep(IDtime,NID)
ID <- factor(rep(1:NID, each = length(IDtime)))
modeldata <- data.frame(ID=ID,time=time,
                        cov = ifelse(as.numeric(ID)<NID/2,1,2))
modeldata$slope <- 2+rep(rnorm(NID,sd= 2*0.3) ,each = length(IDtime))
modeldata$int   <- 5+rep(rnorm(NID,sd= 5*0) ,each = length(IDtime))
modeldata$contresponse   <-  modeldata$int+
  modeldata$slope*modeldata$time +
  rnorm(nrow(modeldata), sd = 0.3)
ggplot(modeldata,aes(time,contresponse))+
  geom_line(aes(group=ID))+
  geom_point()+
  facet_grid(~cov)


library(varTestnlme)
library(nlme)
m1 <- lme(contresponse ~ 1 + time, random = ~ 1 + time | ID, data = modeldata, method = "ML")
m0 <- lme(contresponse ~ 1 + time, random = ~ 1 | ID, data = modeldata, method = "ML")

anova(m0,m1)
varCompTest(m1,m0 )

modeldata$TIME<- modeldata$time
lme(contresponse ~ 1 +  TIME ,
    random = pdSymm(~ 1 + TIME), data = modeldata, method = "ML")

lme(distance ~ 1 +  age , 
    random = pdSymm(Subject ~ 1 + age), data = Orthodont, method = "ML")


library(nlme)
m1 <- lme(distance ~ 1 + Sex + age + age*Sex, 
          random = pdSymm(Subject ~ 1 + age), data = Orthodont, method = "ML")
m0 <- lme(distance ~ 1 + Sex, random = ~ 1 | Subject, data = Orthodont, method = "ML")
varCompTest(m1,m0)
