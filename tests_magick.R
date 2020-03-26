palette = tmaptools::get_brewer_pal('YlOrRd', 5) %>% as.vector()
breaks = classInt::classIntervals(casos_nacional$casos_confirmados, n = 5, style = 'jenks')

library(ggplot2)
g2 = ggplot(casos_nacional) +
  # geom_line(aes(x = fecha, y = casos_confirmados)) +
  geom_col(aes(x = fecha, y = casos_confirmados, fill = casos_confirmados)) +
  scale_fill_gradientn(colors = palette, breaks = breaks$brks[2:6]) +
  scale_x_date(date_breaks = '2 day', date_labels = "%d/%m") +
  xlab('') + ylab('') +
  theme_void() +
  theme(
    # axis.text = element_text(size = 40),
    legend.position = "none", 
    # panel.grid = element_line(color = 'grey90', linetype = 'dotted'), 
    plot.background = element_rect(fill = 'transparent')
  ) 

library(gganimate)
g2_anim = g2 + transition_manual(fecha, cumulative = T) 
anim = animate(g2_anim)

library(magick)
a = image_read('covid_prov.gif')
b = image_read(anim) %>% image_scale('40%')

new_gif <- image_composite(
  image = a[1],  composite_image = b[1],  
  offset = "+500+450",
)
for(i in 2:length(a)){
  combined <- image_composite(image = a[i],  composite_image = b[i],  offset = "+500+450")

  new_gif <- c(new_gif, combined)
}

new_gif
