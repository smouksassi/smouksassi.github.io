library(export)
vpcdatabasebininfo<- bininfo(vpcdatabase)

p1 <- ggplot(vpcdatabase$stats,aes(x=xbin,y=md,group=qname))+
  geom_ribbon(aes(ymin=lo,ymax=hi,fill=qname),alpha=0.1,col=NA ) +
  geom_line(aes (fill=qname,col=qname)) +
  geom_line(data=vpcdatabase$stats,aes(x=xbin,y=y,linetype="Obs"))+
  facet_wrap(~STUDID+ARM,
             scales="free",labeller = label_wrap_gen(multi_line=FALSE) ) +
  scale_colour_manual(name="Predictions Intervals\nMedian (lines)\n95% CI (areas)",
                      breaks=c("q0.05","q0.5","q0.95","Obs"),
                      values=c("red","blue","red","black"))+
  scale_fill_manual  (name="Predictions Intervals\nMedian (lines)\n95% CI (areas)",
                      breaks=c("q0.05","q0.5","q0.95","Obs"),
                      values=c("red","blue","red","black"))+
  scale_linetype_manual(name="Observed (lines)",
                        breaks=c("q0.05","q0.5","q0.95","Obs"),
                        values=c("solid","solid","solid","dashed")) +
  theme_bw()+
  theme(legend.position="right",axis.text.x = element_text(angle=40,
                                                           vjust=1,
                                                           hjust=1))+
  ylab("Obs/Simulated")+
  xlab("Days on Treatment (bins)")



p2 <- ggplot(vpcdatabase$stats,aes(x=bin,y=md,group=qname))+
  geom_ribbon(aes(ymin=lo,ymax=hi,fill=qname),alpha=0.1,col=NA ) +
  geom_line(aes (fill=qname,col=qname)) +
  geom_line(data=vpcdatabase$stats,aes(x=bin,y=y,linetype="Obs"))+
  facet_wrap(~STUDID+ARM,
             scales="free",labeller = label_wrap_gen(multi_line=FALSE) ) +
  scale_colour_manual(name="Predictions Intervals\nMedian (lines)\n95% CI (areas)",
                      breaks=c("q0.05","q0.5","q0.95","Obs"),
                      values=c("red","blue","red","black"))+
  scale_fill_manual  (name="Predictions Intervals\nMedian (lines)\n95% CI (areas)",
                      breaks=c("q0.05","q0.5","q0.95","Obs"),
                      values=c("red","blue","red","black"))+
  scale_linetype_manual(name="Observed (lines)",
                        breaks=c("q0.05","q0.5","q0.95","Obs"),
                        values=c("solid","solid","solid","dashed")) +
  theme_bw()+
  theme(legend.position="right",axis.text.x = element_text(angle=40,
                                                           vjust=1,
                                                           hjust=1))+
  ylab("Obs/Simulated")+
  xlab("Days on Treatment (bins)")

p1
p2

filen <- tempfile(pattern = "ggplot")
graph2ppt(x=p1, file=filen, width=9, 
          aspectr=sqrt(2), append = TRUE) 


graph2ppt(x=p1+
            scale_x_continuous(
              breaks=c(0,10,20,30,40,50,60,70,80,120,225),
              labels=c("0","10","20","30","40","50","60","70","[43,225]","[43,225]",
                       "[43,225]")), file=filen, width=9, 
          aspectr=sqrt(2), append = TRUE) 

graph2ppt(x=p2, file=filen, width=9, 
          aspectr=sqrt(2), append = TRUE) 




p3<- ggplot(vpcdatabase$stats,aes(x=bin,y=md,group=qname))+
  geom_ribbon(aes(ymin=lo,ymax=hi,fill=qname),alpha=0.1,col=NA ) +
  geom_line(aes(linetype=qname,col=qname,fill=qname)) +
  geom_line(data=bootobservedPIallPI,
            aes(linetype="Obs",col="Obs",fill="Obs"))+
  geom_ribbon(data=bootobservedPIallPI,aes(ymin=lo,ymax=hi,fill="Obs",
                                           group=qname),
              alpha=0.1,col=NA)+
  facet_wrap(~STUDID+ARM,
             scales="free",labeller = label_wrap_gen(multi_line=FALSE) ) +
  scale_colour_manual(name="Predictions Intervals\nMedian (lines)\n95% CI (areas)",
                      breaks=c("q0.05","q0.5","q0.95","Obs"),
                      values=c("red","blue","red","black"))+
  scale_fill_manual  (name="Predictions Intervals\nMedian (lines)\n95% CI (areas)",
                      breaks=c("q0.05","q0.5","q0.95","Obs"),
                      values=c("red","blue","red","black"))+
  scale_linetype_manual (name="Predictions Intervals\nMedian (lines)\n95% CI (areas)",
                         breaks=c("q0.05","q0.5","q0.95","Obs"),
                         values=c("solid","solid","solid","dashed")) +
  theme_bw()+
  theme(legend.position="right",axis.text.x = element_text(angle=40,
                                                           vjust=1,
                                                           hjust=1))+
  ylab("Obs/Simulated")+
  xlab("Days on Treatment (bins)")

p3

graph2ppt(x=p3, file=filen, width=9, 
          aspectr=sqrt(2), append = TRUE) 

forestplot
graph2ppt(x=forestplot, file=filen, width=9, 
          aspectr=sqrt(2), append = TRUE) 



