---
  title: "Visualizing Dose Exposure Response"
author: "Samer Mouksassi"
date: "2025-08-18"
categories: [news, visualization]
image: "image.jpg"
format:
  html:
  code-fold: true
---
  
  In this post I will provide the code and thinking process behind the figure shared in the last blog post.

{r}
#| message: false
#| warning: false
#| paged-print: true
suppressMessages(library(ggplot2))
suppressMessages(library(ggquickeda))
suppressMessages(library(patchwork))

ICGI<- read.csv("ICGI.csv")
ICGI$responder <- ifelse(ICGI$ICGI==1,"responder",
                         "not responder")
ICGI$DOSE <- as.factor(ICGI$DOSE)
p1 <-  ggplot(ICGI, aes(AUC, ICGI)) +
  geom_point(
    aes(shape=responder),
    position = position_jitter(height = 0.08),
    size = 3,
    alpha = 0.2
  ) +
  geom_smooth(
    method = "glm",
    method.args = list(family = "binomial"),
    se = TRUE
  ) +
  theme_bw(base_size = 16) +
  guides(shape=guide_legend(reverse=TRUE))+
  labs(x="AUC (µg*h/mL)",y="Probability of Endpoint", shape = "")

p2 <-
  ggplot(ICGI, aes(AUC, DOSE)) +
  geom_point(
    data = ICGI[ICGI$responder == "responder", ],
    position = position_nudge(y = -0.25),
    size = 3,
    alpha = 0.2,
    shape = "circle"
  ) +
  geom_point(
    data = ICGI[ICGI$responder != "responder", ],
    position = position_nudge(y = 0.25),
    size = 3,
    alpha = 0.2,
    shape = "triangle"
  ) +
  theme_bw(base_size = 16) +
  labs(y = "Dose (mg)")
(p1+
    theme(legend.position = "inside",
          legend.position.inside = c(0.8,0.55),
          legend.background = element_rect(fill = "transparent",
                                           colour = "black"),
          legend.title = element_blank(),
          axis.text.x.bottom = element_blank(),
          axis.title.x.bottom = element_blank())+
    scale_y_continuous(breaks= seq(0,1,0.2),
                       labels = scales::percent_format())
)/p2


