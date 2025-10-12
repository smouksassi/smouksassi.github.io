library(GGally)
library(ggplot2)

 c("#4682AC",  "#FDBB2F", "#EE3124", "#336343",
   "#7059a6", "#803333", "#2F71FD", "#093B6D",
   "#EF761B", "#279594")

my_custom_color <- function(r, cutoff = 0, reverse = FALSE,
                            col1 = "#4682AC", col2="#EE3124") {
  if(reverse)
  {rvalue <- ifelse( r >= cutoff, "blue","red")}
  if(!reverse)
  {rvalue <- ifelse( r < cutoff, "blue","red")}
  color<- ifelse( rvalue == "blue",
  rgb(
    red   = col2rgb(col1)[1],
    green = col2rgb(col1)[2],
    blue  = col2rgb(col1)[3],
    alpha = abs(r) * 110,
    max = 255
  ),
  rgb(
    red   = col2rgb(col2)[1],
    green = col2rgb(col2)[2],
    blue  = col2rgb(col2)[3],
    alpha = abs(r) * 110,
    max = 255
  )
  )
  color
} 
# my_custom_color(0.5 , cutoff = 0.9)   "#4682AC37"
# my_custom_color(-0.9, reverse = TRUE) "#EE312463"

my_custom_smooth <- function(data, mapping,
                             method ="loess",
                             cutoff = 0, reverse = FALSE,
                             col1 = "#4682AC",
                             col2 = "#EE3124",
                             panel_border_colour = "#FDBB2F",
                             pValue_cutoff = 0.05,
                             ...) {
  x <- rlang::eval_tidy(mapping$x, data)
  y <- rlang::eval_tidy(mapping$y, data)
  lmModel <- lm(y ~ x, data = data, na.action = na.omit)
  fs <- summary(lmModel)$fstatistic
  pValue <- pf(fs[1], fs[2], fs[3], lower.tail = FALSE)
  ct <- cor.test(x,y)
  r <- unname(ct$estimate)
  rt <- format(r, digits=2)[1]
  tt <- as.character(rt)
  sig <- symnum(
    ct$p.value, corr = FALSE, na = FALSE,
    cutpoints = c(0, 0.001, 0.01, 0.05, 0.1, 1),
    symbols = c("***", "**", "*", ".", " ")
  )
  r <- unname(ct$estimate)

  polygoncol <- my_custom_color(r, cutoff = cutoff, reverse =  reverse, col1 = col1 , col2 = col2)
  pointcol   <- my_custom_color(r, cutoff = cutoff, reverse = !reverse, col1 = col1 , col2 = col2)

    p <- ggplot(data = data, mapping = mapping) +
    geom_point(color = I(pointcol)) + 
    geom_smooth(method = method, color = I("black"),se=FALSE, ...)
  
  if (pValue < pValue_cutoff) {
    p <- p + theme(
      panel.border = element_rect(
        color = panel_border_colour, 
        size = 2,
        linetype = "dotted",
        fill = "transparent"
      )
    )
  }
  
  p <- p + theme(
    panel.background = element_rect(fill= polygoncol)
  )
  
  p
}

my_custom_cor_color <- function(data, mapping,
                                color = I("black"),
                                sizeRange = c(1, 5),
                                cutoff = 0, reverse = FALSE,
                                col1 = "#4682AC",
                                col2 = "#EE3124",
                                panel_border_colour = "#FDBB2F",
                                pValue_cutoff = 0.05,
                                ...) {
  x <- rlang::eval_tidy(mapping$x, data)
  y <- rlang::eval_tidy(mapping$y, data)
  
  ct <- cor.test(x,y)
  
  r <- unname(ct$estimate)
  rt <- format(r, digits=2)[1]
  pvalue<- round(ct$p.value,3)
  pvalue<- ifelse(pvalue<0.001,"p < 0.001",paste("p =",as.character(pvalue)))
  rt <-paste(rt,"\n",pvalue,sep="")
  tt <- as.character(rt)
  sig <- symnum(
    ct$p.value, corr = FALSE, na = FALSE,
    cutpoints = c(0, 0.001, 0.01, 0.05, 0.1, 1),
    symbols = c("***", "**", "*", ".", " ")
  )
  
  # plot the cor value
  p <- ggally_text(
    label = rt, 
    mapping = aes(),
    xP    = 0.5,
    yP    = 0.5, 
    size  = 6,
    color = color,
    ...
  )+ 
    geom_text(
      aes_string(
        x = 0.8,
        y = 0.8
      ),
      label = sig, 
      size = 5,
      color = color,
      ...
    ) + 
    
    theme(
      panel.background=element_rect(fill="transparent", color = "black", linetype = 0),
      panel.grid.minor=element_blank(),
      panel.grid.major=element_blank()
    ) 
  
  
  if (round(ct$p.value,3) < pValue_cutoff) {
    p <- p + theme(
      panel.border = element_rect(
        color = panel_border_colour, 
        size = 2,
        linetype = "dotted",
        fill = "transparent"
      )
    )
  }
  
  
  polygoncol <-  my_custom_color(r,
                                 cutoff = cutoff,
                                 reverse = reverse,
                                 col1 = col1,
                                 col2 = col2)

  p <- p + theme(
    panel.background = element_rect(fill= polygoncol)
  )
  
  p
}

my_custom_densityDiag <- function (data, mapping, rescale = TRUE,
                                   density = TRUE,
                                   histogram = TRUE,...) 
{
  mapping <- mapping_color_to_fill(mapping)
  p <- ggplot(data, mapping) + scale_y_continuous()
  if(density){
    if (identical(rescale, TRUE)) {
      p <- p + stat_density(aes(y = ..scaled..),
                            position = "identity", 
                            geom = "line", ...)
      }
    else {
      p <- p + geom_density(...)
    }
  }
  if(histogram){
    if (identical(rescale, TRUE)) {
      # p <- p + stat_bin(aes(y = ..scaled..),
      #                       position = "dodge",
      #                       geom = "bar",...)
      p <- p + geom_histogram(fill="#4682AC",...)
      
    }
    else {
      p <- p + geom_histogram(...)
    }
  }
  
  
  p <- p + theme(
    panel.background = element_rect(fill= "transparent")
  )
  p
}


smooth_with_hline <- function(data, mapping,method="loess", ..., hlinecolor="red") {
  x <- eval_data_col(data, mapping$x)
  y <- eval_data_col(data, mapping$y)
  ggally_smooth(data, mapping,method = method, ...) +
    ggplot2::geom_hline(color=hlinecolor,
                        data = data.frame(y = 0),
                        mapping = ggplot2::aes(yintercept = y),
                        inherit.aes = FALSE 
    )
}

boxplot_with_hline <- function(data, mapping, ..., hlinecolor="red",
                               outliers = TRUE) {
  x <- eval_data_col(data, mapping$x)
  y <- eval_data_col(data, mapping$y)
  xVal <- mapping_string(mapping$x)
  ggplot(data = data)+
    geom_boxplot(mapping,  outliers = outliers)+
    ggplot2::geom_hline(color=hlinecolor,
                        data = data.frame(y = 0),
                        mapping = ggplot2::aes(yintercept = y),
                        inherit.aes = FALSE # do not inherit anything from the ...
    )
}

give.n <- function(x){
  return(c(y = -Inf,
           label = length(x))) 
}


boxplot_with_hline_n <- function(data, mapping, ..., hlinecolor="red",
                               outliers = TRUE) {
  x <- eval_data_col(data, mapping$x)
  y <- eval_data_col(data, mapping$y)
  xVal <- mapping_string(mapping$x)
  ggplot(data = data)+
    geom_boxplot(mapping,  outliers = outliers,
                 position = position_dodge(preserve = "single"))+
    stat_summary(mapping,
                 fun.data = give.n, geom = "text", fun.y = median,
                 vjust = 0,
                 position = position_dodge(width =0.75,
                                           preserve = "single"))+
    ggplot2::geom_hline(color=hlinecolor,
                        data = data.frame(y = 0),
                        mapping = ggplot2::aes(yintercept = y),
                        inherit.aes = FALSE # do not inherit anything from the ...
    )
}

#https://www.r-bloggers.com/multivariate-ordinal-categorical-data-generation/
#https://github.com/ggobi/ggally/issues/139

