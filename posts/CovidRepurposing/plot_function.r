
theme1 <- theme(
  strip.clip = "off", #overflow to next column won't be cut
  strip.text.x=element_text(hjust=0,face="bold",color="#3764b0",size=16),
  plot.tag =   element_text(hjust=0,face="bold",color="#3764b0",size=16),
  plot.subtitle =  element_text(hjust=0.5  ,face="bold",color="#3764b0",size=16),
  plot.title =     element_text(hjust=0.999,face="bold",color="#3764b0",size=16),
  plot.caption=element_text(face="bold",color="black",size=12), 
  plot.tag.position =   c(0,0.99),
  legend.position = "none", # c(0.8,0.95)
  axis.title  = element_text(size=16),
  axis.text.x = element_text(size=16),
  axis.title.y.left = element_blank(), axis.text.y.left = element_blank(),
  axis.ticks.y.left = element_blank(), axis.line.y.left = element_blank(),
  panel.background = element_rect(),panel.grid.major = element_blank(),
  panel.grid.minor = element_blank(),
  strip.background = element_blank(),
)

theme2 <- theme1 +
  theme(
    strip.text.x=element_text(hjust=0,face="bold",color="transparent", size=16)
  )


data_to_diamond <- function(data = figure2data,
                            diamondheight =0.2){
  plotdata <- data 
  diamonddataset <- data.frame(
    x = c(plotdata$xlow    , plotdata$xmid,
          plotdata$xhigh   , plotdata$xmid),
    y = c(plotdata$ynumeric, plotdata$ynumeric + diamondheight,
          plotdata$ynumeric, plotdata$ynumeric - diamondheight),
    ynumeric   = rep(plotdata$ynumeric,4),
    randomized = rep(plotdata$randomized,4),
    rct        = rep(plotdata$rct,4),
    facet      = rep(plotdata$facet,4),
    facetorder = rep(plotdata$facetorder,4),
    names      = c("xmin", "ymax", "xmax", "ymax")
  ) %>% 
    arrange(ynumeric)
  diamonddataset
}



plot_forest_diamond <- function(data = figure2data,logx=TRUE, greektau=TRUE,
                                xlimits =c(1/8,3),stripx.text.color ="#3764b0",
                                expandleft = 0.4 ,expandright = 0.4,
                                xbreaks = waiver(),
                                xlabels = waiver()) 
{
  
  diamonddataset<- data_to_diamond(data)
  plotdata<- data
  segmentdata <- data.frame(x=xlimits[1],xend=xlimits[2],y=-Inf,yend=-Inf,
                            facet=unique(plotdata$facet[plotdata$facetorder==max(plotdata$facetorder)]))
  segmentdata$facet <- as.factor(segmentdata$facet)
  plotdata$facet <- reorder(as.factor(plotdata$facet),plotdata$facetorder)
  diamonddataset$facet <- reorder(as.factor(diamonddataset$facet),diamonddataset$facetorder)
  
  plot<- ggplot(plotdata)+
    facet_wrap(facet~.,strip.position = "top",ncol=1,scales = "free_y")+
    geom_vline(xintercept= 1,size=2,color="gray",alpha=0.9)+
    geom_polygon(data= diamonddataset,
                 aes(x = x, y = y,group=as.factor(ynumeric ),
                     fill=randomized),alpha=0.8)+ 
    geom_segment(data=segmentdata,
                 aes(x=x,xend=xend,y=y,yend=yend))+
    labs(
      tag = " ",
      title= expression(bold(paste(" "))),
      subtitle =  " \n ",
      caption= expression(bold(paste(" ")))
    )+
    scale_x_continuous("Odds Ratio of Death",
                       trans=ifelse(logx,"log","identity"),
                       expand = c(expandleft,0,expandright,0),#c(m_lower, a_lower, m_uppper, a_upper)
                       breaks= xbreaks,
                       labels = xlabels,
                       limits = xlimits
    )
  plot <-  plot +  theme(
    plot.tag =   element_text(hjust=0,face="bold",color="#3764b0",size=22),
    plot.tag.position =   c(0,0.99),legend.position = "none",
    axis.title  = element_text(size=22), axis.text.x = element_text(size=16),
    axis.title.y.left = element_blank(),axis.text.y.left = element_blank(),axis.ticks.y.left = element_blank(),
    panel.background = element_rect(),panel.grid.major = element_blank(),panel.grid.minor = element_blank(),
    plot.title =   element_text(hjust=0.999,face="bold",color="#3764b0",size=22),
    plot.caption=element_text(face="bold",color="black",size=14),
    strip.background = element_blank(),
    strip.text.x=element_text(hjust=0,face="bold", color=stripx.text.color,size=10),
    axis.line.y.left = element_blank()
  )+
    scale_fill_manual(values=c("#b04782","#73ae95","#2297e6","#28e2e5"))+
    coord_cartesian(clip="off")
  plot
}

plot_odds_time <- function(data1 = NDATA,
                           data2 = oddsDATA,
                           data3 = oddsDATArct,
                           data4 = tmp,
                           class = "cort",
                           severity = "severecrit",
                           trt.disease="glucocorticoid severe/critical",
                           plot_title = "Glucocorticoid - severe/crit",
                           fill_col = c("steelblue","#73ae95"),
                           xbreaks = c(200,300,400,500,600,700,800),
                           nudge_N_labels= -1000
)
{
  NDATA_plot  <- data1 %>%
    filter(class == !!class, severity == !!severity)
  oddsDATA_plot<- data2 %>%
    filter(class == !!class, severity == !!severity)%>%
    dplyr::select(class,severity,pandemicday,estimate,sevalue,ymin,ymax)
  oddsDATArct_plot <- data3 %>%
    filter(class == !!class, severity == !!severity)%>%
    dplyr::select(pandemicday,estimate,sevalue,ymin,ymax)
  tmp2 <- data4[data4$trt.disease==trt.disease,]
  
  tmp2$RCT <- ifelse(tmp2$randomization=="yes","RCT","RWS")
  tmp2$RCT <- reorder(factor(tmp2$RCT),tmp2$n.endpoint,function(x) median(x,na.rm = TRUE)) 
  tmp2$RCT <- factor(tmp2$RCT , levels = c("RWS","RCT"))
  
  odds_plot <- ggplot(oddsDATA_plot,aes(pandemicday   ,exp(estimate )   ))+
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
    scale_y_log10(breaks=c(0.25,0.5,1,2,3,4),labels=c(" 1/4"," 1/2","1","2","  3","  4"),
                  expand = expansion(add = c(0, 0), mult= c(0, 0)))
  
  
  N_plot <- ggplot(NDATA_plot,aes(pandemicday,`Cum N`, color=Ntype))+
    geom_blank(data=oddsDATA_plot,aes(pandemicday,y=0),inherit.aes = FALSE)+
    geom_step(alpha=1,size=2)+
    geom_text(data=NDATA_plot %>% group_by(Ntype) %>% 
                slice(n()), aes(x=pandemicday,y= `Cum N`,label=paste0(Ntype,": ",`Cum N`)),hjust=1,show.legend = FALSE,
              nudge_y = nudge_N_labels,size=6)+
    theme_bw(base_size = 22)+
    guides( shape = "none")+
    theme(legend.position = "none",
          legend.box = "horizontal",
          legend.direction = "vertical",
          legend.spacing.y =unit(0.01, 'cm'),
          legend.background = element_rect(fill="transparent"),
          legend.box.just = "left")+
    coord_cartesian(xlim=c(150,800))+
    labs(x = "Pandemic Day",y="Cumulative N",col="")+
    scale_y_continuous(labels = scales::label_number(scale_cut = cut_short_scale()))+
    scale_color_manual(values=c("#73ae95","#b04782"))+
    scale_x_continuous(expand = expansion(add = c(5, 40), mult= c(0, 0)),breaks = xbreaks)
  
  (odds_plot/N_plot) +
    plot_layout(heights = c(0.8,0.5))
  
}




