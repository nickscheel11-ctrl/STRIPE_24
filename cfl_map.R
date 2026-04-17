library(ggplot2)

map <- ggplot() + 
  geom_sf(data = mendota_outline, linewidth = 1, col = "black")+ 
  theme(
    axis.text.x = element_blank(),
    axis.text.y = element_blank()) +
  theme(
    axis.text.x = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks = element_blank())+
  theme(panel.background = element_blank())

map

cfl <-  map+ 
  geom_label(aes(-89.403, 43.077, label= "CFL"), size= 7)

cfl
