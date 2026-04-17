
install.packages('cowplot')
#Mussel length histogram

library(ggplot2)
library(readr)
library(dplyr)
library(lattice)
library(cowplot)
library(patchwork)
library(ggrepel)
library(grid)



getwd()
setwd("~/Desktop/stripe_2024/data")
lengths <- read_csv("lengths20240626.csv")

hist(lengths$`length(mm)`, breaks=20, 
     main="ZM Length Near the CFL, June 2024",
     xlab= "Length(mm)",
     ylab= "Count",
     col="red")

ggsave("length_hist.jpg", units="in", 
       width=5, height=4, dpi=300)

#boathouse
bh_lengths <- read_csv("zmdens_boathouse.csv")

bh <- histogram(bh_lengths$`length(mm)`, breaks=10, 
     main="Boathouse",
     type="density",
     xlab= "Length(mm)",
     ylab= "",
    )

ggsave("bh_hist.jpg", units="in", 
       width=5, height=4, dpi=300)

#maple bluff

mb_lengths <- read_csv("zmdens_maplebluff.csv")

mb <- histogram(mb_lengths$`length(mm)`, breaks=10, 
          main="Maple Bluff",
          type="density",
          xlab= "Length(mm)",
          ylab= "Relative Abundance",
)
ggsave("mb_hist.tiff", units="in", 
       width=5, height=4, dpi=300)

#cfl dock

cfl_lengths <- read_csv("cfldensity.csv")

cfl <- histogram(cfl_lengths$`length(mm)`, breaks=10, 
          main="CFL",
          type="density",
          xlab= "Length(mm)",
          ylab= "Relative Abundance",
)
ggsave("mb_hist.tiff", units="in", 
       width=5, height=4, dpi=300)

#govs island

gov_lengths <- read_csv("govsisland_density.csv")

gi <- histogram(gov_lengths$`length(mm)`, breaks=15, 
          main="Governor's Island",
          type="density",
          xlab= "Length(mm)",
          ylab= "",
)

ggsave("mb_hist.tiff", units="in", 
       width=5, height=4, dpi=300)

#all zm (so far) dens

all_lengths <- read_csv("all_zm_dens.csv")

histogram(all_lengths$`length(mm)`, breaks=15, 
          main="Mendota",
          type="density",
          xlab= "Length(mm)",
          ylab= "Relative Abundance",
)
ggsave("all_hist.tiff", units="in", 
       width=5, height=4, dpi=300)

##map zm density

#shape file

sf <- ggplot()+ geom_sf(data = mendota_outline, linewidth = 1, col = "gray40")+ 
  theme(
    axis.text.x = element_blank(),
    axis.text.y = element_blank()) +
  theme(
    axis.text.x = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks = element_blank())+
  theme(panel.background = element_blank())

#map
dens <- data.frame(
  location = c("boathouse", "maple_bluff", "cfl_dock",
               "gov_island", "wally_bauman", "fox_bluff", "marshall_park"),
  lat = c(43.079468, 43.115111, 43.077493,
          43.122833, 43.089203, 43.109571, 43.094165),
  lon = c(-89.411975, -89.384826, -89.402896, 
          -89.402083, -89.441666, -89.458060, -89.482045 ),
  zm_dens = c(476, 449, 3619, 1444, 912, 0, 49  ))

sf + geom_point(data= dens, color= "red", 
                 aes(x=lon, y=lat, size=zm_dens))+ 
  labs(size = "Zebra Mussels/m²")

#map just locations for histogram

hist_loc <- 
  
  
sf + geom_point(data= dens, color= "red", 
                aes(x=lon, y=lat, size=5))

ggsave("map.tiff", units="in", 
       width=5, height=4, dpi=300)

#wally bauman

gov_lengths <- read_csv("wb_lengths.csv")

wb <- histogram(gov_lengths$`length(mm)`, breaks=12, 
          main="Wally Bauman Park",
          type="density",
          xlab= "Length(mm)",
          ylab= "")

ggsave("wb.tiff", units="in", 
       width=5, height=4, dpi=300)

#marshall park

gov_lengths <- read_csv("marshall_lengths.csv")

mp <- histogram(gov_lengths$`length(mm)`, breaks=12, 
          main="Marshall Park",
          type="density",
          xlab= "Length(mm)",
          ylab= "")

ggsave("mp.tiff", units="in", 
       width=7, height=4, dpi=300)

#panel
p1 <- mb
p2 <- wb
p3 <- gi
p4 <- cfl
p5 <- mp
p6 <- bh


#calculate wet weight from length
wet <- wdtare$wetmass.tared.g <- (wdtare$wet.mass.g - wdtare$tare.mass.g)
wdtare$pct_moisture <- ((wt-dt)/wt)

all_lengths$wetmass.g <- (0.0001 + ((all_lengths$`length(mm)`)^ 2.85))/10000

# cowplot
histogram(all_lengths$`wetmass.g`, breaks=15, 
          main="Mendota 2024",
          type="density",
          xlab= "wet_mass_g",
          ylab= "Relative Abundance")

plot_grid(p1, p2, p3, p4, p5, p6, labels="AUTO")


p1 + p2 + p3 + p4 + p5 + p6

hist_loc <-  data.frame(
  location = c("boathouse", "maple_bluff", "cfl_dock",
               "gov_island", "wally_bauman", "marshall_park"),
  lat = c(43.079468, 43.115111, 43.077493,
          43.122833, 43.089203, 43.094165),
  lon = c(-89.411975, -89.384826, -89.402896, 
          -89.402083, -89.441666, -89.482045 ))

print(hist_loc)

sf + geom_point(data= hist_loc, color= "red", 
                aes(x=lon, y=lat ), size=5 )+ 
  labs(size = "Zebra Mussels/m²")+
  geom_label(data= hist_loc, , 
             aes(x=lon, y=lat ), )

sf + geom_label_repel (data= hist_loc, 
                       aes (x=lon, y=lat, 
                                      label= location
    ))


  

#map just locations for histogram


  
  
  sf + geom_point(data= dens, color= "red", 
                  aes(x=lon, y=lat, size=5))
  
  library(ggplot2)
  library(ggrepel)
  library(grid)
  
  sf +
    geom_point(data = hist_loc, aes(x = lon, y = lat),
               color = "red", size =3, shape = 9, stroke= 1) +
    geom_label_repel(
      data = hist_loc,
      aes(x = lon, y = lat, label = location),
      arrow = arrow(length = unit(0.02, "npc")),
      segment.color = "gray30",  # arrow line color
      segment.size = 0.5         # arrow line thickness
    )
  
  
  R.version
  