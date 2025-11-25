library(dplyr)
library(ggplot2)
library(export)






  
  
NDATA <- read.csv("NDATA.csv")
oddsDATA <- read.csv("oddsDATA.csv")
oddsDATArct <- read.csv("oddsDATArct.csv")
tmp <- read.csv("tmp.csv")

figure2data <- read.csv("figuredata.csv")
figure2data$xmid  <- exp(figure2data$est)
figure2data$xlow  <- exp(figure2data$est - 1.96 *figure2data$se)
figure2data$xhigh <- exp(figure2data$est + 1.96 *figure2data$se)
figure2data <- figure2data %>%
  mutate(midlabel = format(round(xmid,2), nsmall = 2),
         lowerlabel = format(round(xlow,2), nsmall = 2),
         upperlabel = format(round(xhigh,2), nsmall = 2),
         LABEL = paste0(midlabel, " [", lowerlabel, "-", upperlabel, "]"))
figure2data$randomized <- figure2data$type 
figure2data$ynumeric <- ifelse(figure2data$type =="randomized",1,2)
figure2data$facet <- as.factor(figure2data$facet)
figure2data$facet  <- reorder(figure2data$facet ,figure2data$N)
figure2data <- figure2data %>% 
  arrange(facet,N)
figure2data$facetorder <- rep(1:9,each=2)
figure2data$randomized<- gsub(" ","~", figure2data$randomized)

source("plot_function.r")

#### column by column we build the MA plot

p_col_1 <- ggplot(data=figure2data)+
  geom_text(aes(label=rct,y=ynumeric),x=0,hjust=0,vjust="inward",
            size=6, parse = TRUE)+
  facet_wrap(facet~.,strip.position = "top",ncol=1,scales = "free_y")+
  labs(title= expression(bold(paste(" "))))+
  scale_x_continuous(expand = c(0.1,0,0.1,0),limits=c(0,0.6),breaks = NULL)+
  theme1

p_col_2 <- ggplot(data=figure2data[,],)+
  geom_text(aes(label=N,y=ynumeric),
            x=0.5,hjust=0.5,size=6,vjust="inward")+
  facet_wrap(facet~.,strip.position = "top",ncol=1,scales = "free_y")+
  labs(title=" ",
       subtitle= "Studies\nn")+
  scale_x_continuous(expand = c(0,0,0,0),limits=c(0,1),breaks = NULL)+
  theme2

p_col_3 <-  ggplot(data=figure2data[,],)+
  geom_text(aes(label=paste(Nendpoint,Nevent,sep=" / "),y=ynumeric),
            x=0.5,hjust=0.5,size=6,vjust="inward")+
  facet_wrap(facet~.,strip.position = "top",ncol=1,scales = "free_y")+
  labs(title=" ",
       subtitle= "Treatment:\nN/n")+
  scale_x_continuous(expand = c(0,0,0,0),limits=c(0,1),breaks = NULL)+
  theme2

p_col_4 <- ggplot(data=figure2data[,],)+
  geom_text(aes(label=paste(Ncontrol ,Neventcontrol,sep=" / "),y=ynumeric),
            x=0.5,hjust=0.5,size=6,vjust="inward")+
  facet_wrap(facet~.,strip.position = "top",ncol=1,scales = "free_y")+
  labs(title=" ",
       subtitle= "Control:\nN/n")+
  scale_x_continuous(expand = c(0,0,0,0),limits=c(0,1),breaks = NULL)+
  theme2

p_tau <- ggplot(data=figure2data[,],)+
  geom_text(aes(label=paste( round(tau,2)),y=ynumeric),
            x=0.5,hjust=0.5,size=6,vjust="inward")+
  facet_wrap(facet~.,strip.position = "top",ncol=1,scales = "free_y")+
  labs(title=" ",
       subtitle= expression(bold(tau)))+
  scale_x_continuous(expand = c(0,0,0,0),limits=c(0,1),breaks = NULL)+
  theme2

p_diamonds <- plot_forest_diamond(data = figure2data[,], logx=TRUE, greektau = TRUE,
                                  xlimits = c(0.19,2.01),
                                  xbreaks = c(1/8,1/4,1/2,1,2),
                                  xlabels = c("1/8","1/4","1/2","1","2"),
                                  expandleft = 0,expandright=0,stripx.text.color="transparent")+
  labs(title=" ",
       subtitle= " \nMeta Analysis")+
  theme(
    plot.subtitle =   element_text(hjust=0.5,face="bold",
                                   color="#3764b0",size=18))

p_95 <- ggplot(data=figure2data[,],)+
  geom_text(aes(label=LABEL,y=ynumeric),x=0.5,hjust=0.5, vjust="inward",size=6)+
  facet_wrap(facet~.,strip.position = "top",ncol=1,scales = "free_y")+
  labs(title=" ",
       subtitle = expression(bold(paste("Estimates [95% CI]"))))+
  scale_x_continuous(expand = c(0,0,0,0),limits=c(0,1),breaks = NULL)+
  theme2

p_pvalue <- ggplot(data=figure2data[,],)+
  geom_text(aes(label=ifelse(p<0.01,"< 0.01",p),y=ynumeric),x=0.5,hjust=0.5,size=6,vjust="inward")+
  facet_wrap(facet~.,strip.position = "top",ncol=1,scales = "free_y")+
  labs(title=" ",
       subtitle= expression(bold(paste("p-value"))))+
  scale_x_continuous(expand = c(0,0,0,0),limits=c(0,1),breaks=NULL)+
  theme2


foresplot<- egg::ggarrange(p_col_1,p_col_2,p_tau,p_col_3,p_col_4,p_diamonds,p_95,p_pvalue,
                           widths=c(0.1,0.1,0.1,0.2,0.2,0.6,0.2,0.1))

ggsave("forestplot_redo.png",foresplot,width =20 ,height = 10.4, dpi = 300)


foresplot<- egg::ggarrange(p_col_1,p_col_2,p_tau,p_col_3,p_col_4,p_diamonds,p_95,p_pvalue,
                           widths=c(0.1,0.1,0.1,0.2,0.2,0.6,0.2,0.1))

ggsave("forestplot.png",foresplot,width =20 ,height = 10.4, dpi = 300)

# graph2ppt(foresplot,file = "metaanalysi.pptx", vector.graphic = TRUE,
#           margins = c(top = 0, right = 0, bottom = 0, left = 0),
#           center = TRUE,
#           aspectr  = 20/10.4,
#           offx = 0,
#           offy = 0,
#           upscale = TRUE,append = TRUE )



NDATA$`Cum N`<-NDATA$Cum.N

tociplot <- plot_odds_time(data1 = NDATA,
                           data2 = oddsDATA,
                           data3 = oddsDATArct,
                           class = "tocilizumab",
                           severity = "all",
                           data4 = tmp,
                           trt.disease="tocilizumab",
                           plot_title = "Tocilizumab",
                           fill_col = c("steelblue","#73ae95"),
                           xbreaks = c(200,300,400,500,600,700,800),
                           nudge_N_labels= -2000)


NDATA_plot  <- NDATA %>%
  filter(class == !!class, severity == !!severity)
oddsDATA_plot = oddsDATA%>%
  filter(class == !!class, severity == !!severity)

tmp2<-tmp[tmp$trt.disease==trt.disease,]
tmp2$RCT <- ifelse(tmp2$randomization=="yes","RCT","RWS")
tmp2$RCT <- reorder(factor(tmp2$RCT),tmp2$n.endpoint,function(x) median(x,na.rm = TRUE)) 
tmp2$RCT <- factor(tmp2$RCT , levels = c("RWS","RCT"))


oddstime<- ggplot(oddsDATA_plot,aes(pandemicday   ,exp(estimate )   ))+
  geom_blank(data=NDATA_plot,aes(pandemicday,y=1),inherit.aes = FALSE)+
  geom_point(data=tmp2,aes(pub.day,or,size=size,col=RCT,shape=RCT))+
  geom_stepribbon(aes(x=pandemicday,
                      ymin = ifelse(ymin<0.25,0.25,ymin),
                      ymax = ifelse(ymax>4.1,4.1,ymax) ),
                  alpha=0.5,color="transparent",fill=fill_col[1])+
  geom_stepribbon(data=oddsDATArct_plot,aes(x=pandemicday,
                                            ymin = ifelse(ymin<0.25,0.25,ymin),
                                            ymax = ifelse(ymax>4.1,4.1,ymax) ),
                  alpha=0.5,color="transparent",fill=fill_col[2])+
  geom_step(aes(x=pandemicday,y = ifelse(exp(estimate) >4.1,4.1,exp(estimate))),
            size=2,col=fillcol2,alpha=0.5)+
  geom_step(data=oddsDATArct_plot,
            aes(x=pandemicday,y = ifelse(exp(estimate) >4.1,4.1,exp(estimate))),
            size=2,col="#73ae95",alpha=1)+
  geom_hline(yintercept=1,linetype="dashed",size=1,alpha=0.5)+
  theme_bw(base_size = 22)+
  theme(
    legend.position = c(0.9,0.93),legend.key=element_blank(),
    legend.background = element_blank(),legend.title = element_blank(),
    axis.title.x.bottom = element_blank())+
  labs(title=plot_title)+
  labs(x = "Pandemic Day",y="Odds-ratio for death")+
  coord_cartesian(ylim=c(0.25,4.1),xlim=c(150,800),expand = TRUE,clip = "on") +
  guides(size= "none",color = guide_legend(override.aes = list(size=3)))+
  scale_color_manual(values=c("#b04782","#73ae95"))+
  scale_size( range = c(1,6))+
  scale_x_continuous(expand = expansion(add = c(5, 40), mult= c(0, 0)),breaks = xbreaks )+
  scale_y_log10(breaks=c(0.25,0.5,1,2,3,4),labels=c(" 1/4"," 1/2","1","2","  3","  4"),expand = expansion(add = c(0, 0), mult= c(0, 0)))



NDATA_plot$RCT <- factor(NDATA_plot$Ntype , levels = c("RWS","RCT"))

a<- ggplot(NDATA_plot,aes(pandemicday,`Cum N`, color=Ntype))+
  geom_blank(data=oddsDATA_plot,aes(pandemicday,y=0),inherit.aes = FALSE)+
  geom_step(alpha=0.5,linewidth = 2)+
  geom_text(data=NDATA_plot %>%
              group_by(Ntype) %>%
              filter(!is.na(N))%>%
              slice(n()), aes(x=pandemicday,
                              y= `Cum N`,
                              label=paste0(Ntype,": ",`Cum N`)),
            hjust=1,show.legend = FALSE,
            nudge_y = -2000,size=6)+
  guides( shape = "none",color="none",
          linewidth = guide_legend(position = "top"))+
  labs(x = "Pandemic Day",y="Cumulative N",col="")+
  scale_y_continuous(labels = scales::label_number(scale_cut = cut_short_scale()))+
  scale_color_manual(values=c("#b04782","#73ae95"))+
  scale_x_continuous(expand = expansion(add = c(5, 40), mult= c(0, 0)),
                     breaks =  c(200,300,400,500,600,700,800))+
  coord_cartesian(xlim=c(150,800),clip="off")+
  theme_bw(base_size = 16)+
  theme(legend.position = "right",
        legend.box = "horizontal",
        #legend.direction = "horizontal",
        legend.spacing.y =unit(0.01, 'cm'),
        legend.background = element_rect(fill="transparent"),
        legend.box.just = "left")



b<- ggplot(NDATA_plot,aes(pandemicday,`Cum N`, color=Ntype))+
  geom_blank(data=oddsDATA_plot,aes(pandemicday,y=0),inherit.aes = FALSE)+
  geom_step(alpha=0.5,)+#aes(linewidth = `Cum N`)
  geom_point(alpha=0.5,aes(size=N ,shape=Ntype))+
  geom_text(data=NDATA_plot %>%
              group_by(Ntype) %>%
              filter(!is.na(N))%>%
              slice(n()), aes(x=pandemicday,
                              y= `Cum N`,
                              label=paste0(Ntype,": ",`Cum N`)),
            hjust=1,show.legend = FALSE,
            nudge_y = -2000,size=6)+
  guides( shape = "none",color="none",
          size = "none")+
  labs(x = "Pandemic Day",y="Cumulative N",col="")+
  scale_y_continuous(labels = scales::label_number(scale_cut = cut_short_scale()))+
  scale_color_manual(values=c("#b04782","#73ae95"))+
  scale_x_continuous(expand = expansion(add = c(5, 40), mult= c(0, 0)),
                     breaks =  c(200,300,400,500,600,700,800))+
  coord_cartesian(xlim=c(150,800),clip="off")+
  theme_bw(base_size = 16)+
  theme(legend.position = "right",
        legend.box = "horizontal",
        #legend.direction = "horizontal",
        legend.spacing.y =unit(0.01, 'cm'),
        legend.background = element_rect(fill="transparent"),
        legend.box.just = "left")


c <- ggplot(NDATA_plot,aes(pandemicday,forcats::fct_rev(RCT), color=RCT))+
  geom_line(alpha=0.2,aes(linewidth = sqrt(`Cum N`)))+
  geom_point(alpha=0.5,aes(size=N ,shape=Ntype))+
  geom_text(data=NDATA_plot %>%
              group_by(Ntype) %>%
              filter(!is.na(N))%>%
              slice(n()), aes(x=pandemicday,
                              y= RCT,
                              label=paste0(Ntype,": ",`Cum N`)),
            hjust=1,show.legend = FALSE,
            nudge_y =-0.1,size=6)+
  guides( shape = "none",color="none",
          size = guide_legend(position = "right",nrow = 2))+
  labs(x = "Pandemic Day",y="",col="",linewidth = "sqrt(cumsum(N))")+
  scale_color_manual(values=c("#b04782","#73ae95"))+
  scale_shape_manual(values=c("triangle","circle"))+
  
  scale_x_continuous(expand = expansion(add = c(5, 40), mult= c(0, 0)),
                     breaks =  c(200,300,400,500,600,700,800))+
  coord_cartesian(xlim=c(150,800),clip="off")+
  theme_bw(base_size = 16)+
  theme(legend.position = "none",
        legend.box = "vertical",
        #legend.direction = "horizontal",
        legend.spacing.y =unit(0.01, 'cm'),
        legend.background = element_rect(fill="transparent"),
        legend.box.just = "left")


a|b|c

(oddstime+coord_cartesian(clip="off"))/c + plot_layout(
  heights = c(1,0.2)
)|
(oddstime+coord_cartesian(clip="off"))/b + plot_layout(
  heights = c(1,0.2)
)