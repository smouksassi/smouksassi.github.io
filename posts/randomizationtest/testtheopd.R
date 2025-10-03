
library(ggplot2)
library(lme4)
theopd <- read.csv("theopd.csv")

ggplot(theopd,aes(THEO,PEFR))+
  geom_point()+
  geom_smooth(se=F)+
  facet_grid(~SEX)


nform <- ~ e0 + emax*input/(ec50+input)
nfun <- deriv(nform,namevec=c("e0","emax","ec50"),
              function.arg=c("input","e0","emax","ec50"))
nm1 <- nlme(PEFR  ~ nfun(THEO, e0, emax, ec50),
             fixed = e0 + emax + ec50 ~ 1,
             random = pdDiag(e0+ emax ~ 1),
             theopd,
             start = c(e0 = 100, emax = 100, ec50 = 8),
            control = nlmeControl(msMaxIter = 20, msVerbose = TRUE))
nm1

nm1r <- nlmer( dv  ~ nfun(THEO, e0, emax, ec50) ~ 
                (e0 | ID) + (emax | ID),
            theopd,nAGQ = 1L,
            start = c(e0 = 140, emax = 170, ec50 = 8))
nm1r





theopd <-    groupedData( PEFR ~ THEO | ID,
               data = as.data.frame( theopd ),
               FUN = mean,
               outer = ~ SEX  )

fm1 <- nlme(PEFR ~ SSasymp(THEO, Asym, R0, lrc),
     data = theopd,
     fixed = Asym + R0 + lrc ~ 1,
     random =  pdDiag(Asym + R0 ~ 1),
     start = c(Asym = 103, R0 = -8.5, lrc = -3.3))




summary(fm1)

fm1 <- nlme(PEFR ~ exp(le0) +(exp(lemax)*THEO/(exp(lec50)+THEO) ) ,
            data = theopd,
            fixed = le0 + lemax + lec50 ~ 1,
            random =  pdDiag(e0~ 1),
            start = c(le0 = log(139), lemax = log(190),
                      lec50 = log(8)))

