library(ggplot2)
library(farver)

ggplot(mpg, aes(class,fill=class)) +
  geom_bar(aes(
    fill  = stage(class, after_scale = alpha(fill, 0.8))
  ),lwd=1) +
  geom_text(
    aes(
      y = after_stat(count),
      label = after_stat(count),
      color = stage(class, after_scale = ifelse(
        decode_colour(alpha(color, 0.8),"rgb", "hcl")[, "l"] > 50,
        "black","white")
      )      ),
    stat = "count", vjust = 1, size = 7,
    show.legend = FALSE
  )+
  scale_color_viridis_d()+
  scale_fill_viridis_d()


