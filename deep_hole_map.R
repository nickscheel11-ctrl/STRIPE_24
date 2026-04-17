
mendota_outline <- st_read("~/Desktop/stripe_data/mendota_sf/805400.shp")
sf <- ggplot()+ geom_sf(data = , linewidth = 1, col = "gray40")+ 
  theme(
    axis.text.x = element_blank(),
    axis.text.y = element_blank()) +
  theme(
    axis.text.x = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks = element_blank())+
  theme(panel.background = element_blank())+
  ggplot(dh_loc, aes(lon, lat, label = dh_loc(type)))+
geom_label()

map + annotate("text", label = "David Buoy",
               x = -89.4045,
               y = 43.0995)
  
dh_loc <- data.frame(
  type = c("Deep Hole"),
  lat   = 43.0995,
  lon   = -89.4045)
