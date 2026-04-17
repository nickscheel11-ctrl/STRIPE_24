
#Mussel length histogram

library(ggplot2)
library(readr)
library(dplyr)
library(lattice)
library(cowplot)
library(patchwork)
library(ggrepel)
library(grid)


setwd("~/Desktop/stripe_2024/data")
lengths <- read_csv("lengths20240626.csv")


#boathouse
bh_lengths <- read_csv("zmdens_boathouse.csv")

bh_lengths <- bh_lengths %>%
  mutate(location = "boathouse")

bh <- histogram(bh_lengths$`length(mm)`, breaks=10, 
                main="Boathouse",
                type="density",
                xlab= "Length(mm)",
                ylab= "",
)

bh
ggsave("bh_hist.jpg", units="in", 
       width=5, height=4, dpi=300)

#maple bluff

mb_lengths <- read_csv("zmdens_maplebluff.csv")

mb_lengths <- mb_lengths %>%
  mutate(location = "maple_bluff")

mb <- histogram(mb_lengths$`length(mm)`, breaks=10, 
                main="Maple Bluff",
                type="density",
                xlab= "Length(mm)",
                ylab= "Relative Abundance",
)
mb
ggsave("mb_hist.tiff", units="in", 
       width=5, height=4, dpi=300)

#cfl dock

cfl_lengths <- read_csv("cfldensity.csv")

cfl_lengths <- cfl_lengths %>%
  mutate(location = "cfl")

cfl <- histogram(cfl_lengths$`length(mm)`, breaks=10, 
                 main="CFL",
                 type="density",
                 xlab= "Length(mm)",
                 ylab= "Relative Abundance",
)

cfl
ggsave("mb_hist.tiff", units="in", 
       width=5, height=4, dpi=300)

#govs island

gov_lengths <- read_csv("govsisland_density.csv")

gov_lengths <- gov_lengths %>%
  mutate(location = "govs_island")

gi <- histogram(gov_lengths$`length(mm)`, breaks=15, 
                main="Governor's Island",
                type="density",
                xlab= "Length(mm)",
                ylab= "",
)

gi

ggsave("mb_hist.tiff", units="in", 
       width=5, height=4, dpi=300)

#wally bauman

wb_lengths <- read_csv("wb_lengths.csv")

wb_lengths <- wb_lengths %>%
  mutate(location = "wally_bauman")

wb <- histogram(wb_lengths$`length(mm)`, breaks=12, 
                main="Wally Bauman Park",
                type="density",
                xlab= "Length(mm)",
                ylab= "")

wb

ggsave("wb.tiff", units="in", 
       width=5, height=4, dpi=300)

#marshall park

mp_lengths <- read_csv("marshall_lengths.csv")

mp_lengths <- mp_lengths %>%
  mutate(location = "marshall_park")

mp <- histogram(gov_lengths$`length(mm)`, breaks=12, 
                main="Marshall Park",
                type="density",
                xlab= "Length(mm)",
                ylab= "")
mp

ggsave("mp.tiff", units="in", 
       width=7, height=4, dpi=300)


#panel
p1 <- mb
p2 <- wb
p3 <- gi
p4 <- cfl
p5 <- mp
p6 <- bh

# cowplot
histogram(all_lengths$`wetmass.g`, breaks=15, 
          main="Mendota 2024",
          type="density",
          xlab= "wet_mass_g",
          ylab= "Relative Abundance")

plot_grid(p1, p2, p3, p4, p5, p6, labels="AUTO")


# Combine all data frames into one
all_lengths <- bind_rows(bh_lengths, mb_lengths,
                         cfl_lengths, gov_lengths, 
                         wb_lengths, mp_lengths)

# Save to a new CSV file
write.csv(all_lengths, "combined_lengths.csv", row.names = FALSE)

#calculate wet weight from length

all_lengths$wetmass.g <- (0.0001 + ((all_lengths$`length(mm)`)^ 2.85))/10000

#biomass_histogram

biom_2024 <- histogram(all_lengths$`wetmass.g`, breaks=12, 
                main="Lake Mendota 2024",
                type="density",
                xlab= "Biomass (g)",
                ylab= "Relative Abundance (%)")
biom_2024







