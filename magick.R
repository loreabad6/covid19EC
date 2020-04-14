library(dplyr)
# Leer datos recopilados por Pablo Reyes (https://twitter.com/PabloRA19/status/1241964053406285825)
# "https://raw.githubusercontent.com/loreabad6/COVID19_EC/master/covid_ec.csv"
casos = read.csv('https://raw.githubusercontent.com/pablora19/COVID19_EC/master/covid_ec.csv', sep = ',') %>% 
  mutate(fecha_hora = paste(fecha, hora)) %>%
  mutate(fecha = as.Date(fecha, format = '%d/%m/%Y')) %>% 
  mutate(fecha_hora = as.POSIXct(fecha_hora, format = '%d/%m/%Y %H:%M'))

# Convertir NaN en NA
is.nan.data.frame <- function(x){
  do.call(cbind, lapply(x, is.nan))
}
casos[is.nan(casos)] <- NA

# Corregir errores:
## Codigos de cantones no tienen el zero inicial
## Crear columna con codigo de provincia para union
casos_canton = casos %>% 
  # mutate(id_canton = ifelse(nombre_canton == 'salitre', 919, id_canton)) %>% 
  mutate(id_canton = as.factor(sprintf("%04d", id_canton))) %>% 
  mutate(id_prov = substr(id_canton, start = 1, stop = 2))

# Extraer info relevante a nivel nacional
casos_nacional = casos_canton %>% group_by(fecha) %>% 
  mutate(casos_confirmados = sum(casos_confirmados)) %>% 
  summarize_at(vars(7:15), first)

library(dplyr)
palette = tmaptools::get_brewer_pal('YlOrRd', 5) %>% as.vector()
breaks = classInt::classIntervals(casos_nacional$casos_confirmados, n = 5, style = 'jenks')

library(ggplot2)
g2 = ggplot(casos_nacional, aes(x = fecha, y = casos_confirmados)) +
  # geom_line(aes(x = fecha, y = casos_confirmados)) +
  geom_col(aes(fill = casos_confirmados)) +
  geom_text(
    aes(label = casos_confirmados, color = casos_confirmados), 
    nudge_y = 500, size = 7, fontface = 'bold', angle = 90
  ) +
  scale_fill_gradientn(colors = palette, breaks = breaks$brks[2:6]) +
  scale_color_gradientn(colors = palette, breaks = breaks$brks[2:6]) +
  scale_x_date(date_breaks = '2 day', date_labels = "%d/%m") +
  xlab('') + ylab('') +
  theme_void() +
  theme(legend.position = "none", plot.background = element_rect(fill = 'white', color = 'white')) 

library(gganimate)
g2_anim = g2 + transition_manual(fecha, cumulative = T) 
anim = animate(g2_anim, height = 500, width = 1200)

library(magick)
a = image_read('covid_prov.gif')
b = image_read(anim) #%>% image_scale('40%')

covid_cases <- image_append(c(a[1],b[1]), stack = T)
for(i in 2:length(a)){
  combined <- image_append(c(a[i],b[i]), stack = T)
  
  covid_cases <- c(covid_cases, combined)
}

covid_cases

image_write(covid_cases, 'covid_cases.gif')
