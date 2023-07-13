##Figure 1 - percent nutrient supply from SSF 

library(tidyverse)
library(ggpubr)
library(sf)

# Clear workspace
rm(list = ls())

#Load world map
world <- rnaturalearth::ne_countries("small", returnclass = "sf")

# Extract French Guiana
fguiana <-world %>% 
  sf::st_cast(to="POLYGON") %>% 
  filter(gu_a3=="FRA") %>% 
  mutate(id=1:n()) %>% 
  select(id) %>% 
  filter(id==1)

#World centroids
world_lg <- rnaturalearth::ne_countries(scale="large", returnclass = "sf") %>% 
  mutate(area_sqkm=sf::st_area(.)/(1000*1000)) %>%
  mutate(area_sqkm=as.numeric(area_sqkm)) %>% 
  sf::st_centroid() %>% 
  select(continent, subunit, su_a3, area_sqkm) %>% 
  rename(country=subunit, iso3=su_a3) 

# Small nation centroids
world_tiny <- rnaturalearth::ne_countries(type="tiny_countries", returnclass = "sf") %>% 
  select(continent, subunit, su_a3) %>% 
  rename(country=subunit, iso3=su_a3) %>% 
  mutate(area_sqkm=10)

# Merge centroids
world_centers <- bind_rows(world_lg, world_tiny)

# Base theme
base_theme <- theme(axis.text=element_blank(),
                    axis.title=element_blank(),
                    legend.text=element_text(size=10),
                    legend.title=element_text(size=10),
                    strip.text=element_blank(),
                    plot.title=element_text(size=7),
                    panel.grid.major = element_blank(), 
                    panel.grid.minor = element_blank(),
                    panel.background = element_blank(), 
                    axis.line = element_line(colour = "black"),
                    # Legend
                    legend.position=c(0.11,0.35),
                    legend.background = element_rect(fill=alpha('blue', 0)))


##relative to seafood supply - boxplot
total_nut_supply <- read_csv("Outputs/total_nut_supply.csv") %>% 
  select(-consump) %>% 
  spread(production_sector, nut_supply) %>% 
  mutate_all(~replace(., is.na(.), 0)) %>% 
  mutate(SSF = Artisanal + Subsistence,
         total = Artisanal + Industrial + Recreational + Subsistence + aquaculture + Import,
         perc_SSF = 100*SSF/total,
         perc_artisanal = 100*Artisanal/total,
         perc_subsistence = 100*Subsistence/total,
         perc_industrial = 100*Industrial/total,
         perc_import = 100*Import/total,
         perc_Recreational = 100*Recreational/total,
         perc_aquaculture = 100*aquaculture/total,
         nutrient = recode(nutrient, "Omega-3 fatty acids" = "DHA+EPA")) %>% 
  select(iso3c, nutrient, c(perc_SSF:perc_aquaculture)) %>% 
  reshape2::melt(id.vars = c("iso3c", "nutrient")) %>% 
  rename(percent_consump = value,
         sector = variable) %>% 
  mutate(sector = recode(sector, 
                         perc_SSF = "Small-scale",
                         perc_artisanal = "Artisanal",
                         perc_subsistence = "Subsistence",
                         perc_industrial = "Industrial",
                         perc_import = "Imports",
                         perc_Recreational = "Recreational",
                         perc_aquaculture = "Mariculture")) %>% 
  filter(!nutrient == "Protein",
         !sector %in% c("Small-scale"))

total_nut_supply$sector = factor(total_nut_supply$sector, 
                                            levels= c("Imports",
                                                      "Recreational", 
                                                      "Mariculture",
                                                      "Industrial",
                                                      "Subsistence",
                                                      "Artisanal"))

total_nut_supply$nutrient = factor(total_nut_supply$nutrient, 
                                 levels= c("Zinc", 
                                           "Iron",
                                           "Vitamin B12",
                                           "DHA+EPA",
                                           "Vitamin A",
                                           "Calcium"))
# p1 = ggplot(data = total_nut_supply) +
#   geom_boxplot(aes(x = percent_consump,
#                    y = sector,
#                    fill = nutrient,
#                    color = nutrient),
#                alpha = 0.7) +
#   guides(alpha = "none", color = "none") +
#   labs(x = "Nutrient Supply (%)", y = "", fill = "") +
#   theme_bw() +
#   #xlim(0, 100)+
#   #labs(title = "A")
#   theme(legend.position = "top",
#         panel.grid = element_blank(),
#         text = element_text(size=15, color = "black"),
#         axis.text = element_text(size = 15),
#         plot.margin = ggplot2::margin(t = 0, r = 0.5, b = 0.45, l = 0, "cm"),
#         legend.box.margin = ggplot2::margin(t = 0.5, r = 3.5, b = -0.3, l = 0, "cm"),
#         legend.title = element_text(face = "bold", size = 18),
#         legend.text = element_text(size = 14))

p1 = ggplot(data = total_nut_supply) +
  geom_boxplot(aes(x = percent_consump,
                   y = sector,
                   fill = nutrient,
                   color = nutrient),
               alpha = 0.7) +
  guides(alpha = "none", color = "none", fill = guide_legend(nrow=2)) +
  labs(x = "Nutrient Supply (%)", y = "", fill = "") +
  theme_bw() +
  #xlim(0, 100)+
  #labs(title = "A")
  theme(legend.position = "top",
        panel.grid = element_blank(),
        text = element_text(size=15, color = "black"),
        axis.text = element_text(size = 15),
        plot.margin = ggplot2::margin(t = 0, r = 0.5, b = 0.45, l = 0, "cm"),
        legend.box.margin = ggplot2::margin(t = 0.5, r = 3.5, b = -0.3, l = 0, "cm"),
        legend.title = element_text(face = "bold", size = 18),
        legend.text = element_text(size = 18))

p1

ggsave(p1, filename = "Figures/Figure 2 - w Imports.jpeg", 
       height = 8, 
       width = 6)

##relative to seafood supply - map
total_nut_supply <- read_csv("Outputs/total_nut_supply.csv") %>% 
  select(-consump) %>% 
  spread(production_sector, nut_supply) %>% 
  mutate_all(~replace(., is.na(.), 0)) %>% 
  mutate(SSF = Artisanal + Subsistence,
         total = Artisanal + Industrial + Recreational + Subsistence + aquaculture + Import,
         perc_SSF = 100*SSF/total,
         nutrient = recode(nutrient, "Omega-3 fatty acids" = "DHA+EPA")) %>% 
  group_by(iso3c) %>% 
  summarise(perc_SSF = mean(perc_SSF, na.rm = T))

# Format data
mpa_dta_sf <- world %>% 
  left_join(total_nut_supply, by=c("gu_a3"="iso3c"))
  
# Spatialize tiny
sdata_pt <- world_centers %>% 
  left_join(total_nut_supply, by=c("iso3"="iso3c")) %>% 
  # Reduce to ones with data
  filter(!is.na(perc_SSF)) %>% 
  arrange(area_sqkm) %>% 
  # Reduce to small
  filter(area_sqkm<=2.5*10^4 & continent!="Europe")
  

# Open Access reef area
p2 <- ggplot(mpa_dta_sf) +
  geom_sf(mapping=aes(fill=perc_SSF), lwd=0.1) +
  # Plot small places
  geom_sf(data=sdata_pt, mapping=aes(fill=perc_SSF), shape=21, size=3, stroke=0.3) +
  # Plot French Guiana
  geom_sf(data=fguiana, lwd=0.1, color="grey30", fill="grey80") +
  # Crop out Antarctica
  coord_sf(y=c(-55, NA)) +
  # Legend and labels
  scale_fill_gradient2(name="SSF\nnutrient\nsupply (%)", 
                       #breaks=c(2.5, 5.01, 7.31), labels=c("15", "150", "1500"),
                       low="navy", high="darkred", mid="white", midpoint=0) +
  guides(fill = guide_colorbar(ticks.colour = "black", frame.colour = "black", barwidth = 1, barheight = 4)) +
  # Theme
  labs(title = "Seafood") +
  theme_bw() + base_theme +
  theme(legend.position="right",
        axis.text = element_blank(),
        axis.title=element_blank(), 
        legend.background = element_rect(fill=alpha('blue', 0)),
        legend.title = element_text(size = 15),
        plot.title = element_text(size = 18, hjust = 0.5),
        legend.text = element_text(size = 15),
        plot.margin = ggplot2::margin(t = 1, r = 0, b = 0, l = 0, "cm"))
  

##Relative to ASF
percent_nutrient_supply <- read_csv("Outputs/percent_nutrient_supply_coastal.csv") %>% 
    filter(!nutrient == "DHA+EPA") %>% 
    group_by(iso3c) %>% 
    summarise(perc_SSF_ASF = mean(perc_SSF_ASF, na.rm = T))

ggplot(data = percent_nutrient_supply) +
    geom_boxplot(aes(y = perc_SSF_ASF))

#percent_nutrient_supply$perc_SSF_ASF[percent_nutrient_supply$perc_SSF_ASF>15] = 20


# Format data
mpa_dta_sf <- world %>% 
  left_join(percent_nutrient_supply, by=c("gu_a3"="iso3c"))

# Spatialize tiny
sdata_pt <- world_centers %>% 
  left_join(percent_nutrient_supply, by=c("iso3"="iso3c")) %>% 
  # Reduce to ones with data
  filter(!is.na(perc_SSF_ASF)) %>% 
  arrange(area_sqkm) %>% 
  # Reduce to small
  filter(area_sqkm<=2.5*10^4 & continent!="Europe")


# Open Access reef area
p3 <- ggplot(mpa_dta_sf) +
  geom_sf(mapping=aes(fill=perc_SSF_ASF), lwd=0.1) +
  # Plot small places
  geom_sf(data=sdata_pt, mapping=aes(fill=perc_SSF_ASF), shape=21, size=3, stroke=0.3) +
  # Plot French Guiana
  geom_sf(data=fguiana, lwd=0.1, color="grey30", fill="grey80") +
  # Crop out Antarctica
  coord_sf(y=c(-55, NA)) +
  # Legend and labels
  scale_fill_gradient2(name="SSF\nnutrient\nsupply (%)", 
                       #breaks=c(0, 10, 19), labels=c("0", "10", ">20"),
                       low="navy", high="darkred", mid="white", midpoint=0) +
  guides(fill = guide_colorbar(ticks.colour = "black", frame.colour = "black", barwidth = 1, barheight = 4)) +
  # Theme
  labs(title = "Animal sourced foods") +
  theme_bw() + base_theme +
  theme(legend.position="right",
        axis.text = element_blank(),
        axis.title=element_blank(), 
        legend.background = element_rect(fill=alpha('blue', 0)),
        legend.title = element_text(size = 15),
        plot.title = element_text(size = 18, hjust = 0.5),
        legend.text = element_text(size = 15),
        plot.margin = ggplot2::margin(t = 0, r = 0, b = 1.5, l = 0, "cm"))

# Merge maps
g = ggarrange(p2, p3,
              ncol=1,
              labels = c("B", "C"),
              font.label = list(size = 21, color = "black", face = "bold"),
              vjust = c(3.4, 1.3))
g
p = ggarrange(p1, g, widths = c(7,11.1), heights = c(1,10), labels = c("A", ""),
              font.label = list(size = 21, color = "black", face = "bold"),
              vjust = 5)
p
ggsave(p, filename = "Figures/Figure 2 - final.jpeg", 
       height = 6.2, 
       width = 10)

# Export  
ggsave(p, filename = "Figures/Figure 2 - final.pdf", 
       width=10, height=6.2, units="in", dpi=600, device=cairo_pdf)


###Stats
library(countrycode)
##Seafood
total_nut_supply <- read_csv("Outputs/total_nut_supply.csv") %>% 
  select(-consump) %>% 
  spread(production_sector, nut_supply) %>% 
  mutate_all(~replace(., is.na(.), 0)) %>% 
  mutate(SSF = Artisanal + Subsistence,
         total = Artisanal + Industrial + Recreational + Subsistence + aquaculture + Import,
         perc_SSF = 100*SSF/total,
         nutrient = recode(nutrient, "Omega-3 fatty acids" = "DHA+EPA"),
         country = countrycode(iso3c, 'iso3c', 'country.name')) %>% 
  group_by(iso3c, country, nutrient) %>% 
  summarise(perc_SSF = mean(perc_SSF, na.rm = T))

mean(total_nut_supply$perc_SSF, na.rm = T)

stats_continent = total_nut_supply %>% 
  mutate(country = countrycode(iso3c, 'iso3c', 'country.name'),
         continent = countrycode(iso3c, 'iso3c', 'region23')) %>% 
  group_by(continent) %>% 
  summarise(perc_SSF = mean(perc_SSF))

stats_nut = total_nut_supply %>% 
  group_by(nutrient) %>% 
  summarise(perc_SSF = mean(perc_SSF, na.rm = T))

##ASF
percent_nutrient_supply <- read_csv("Outputs/percent_nutrient_supply_coastal.csv") %>% 
  filter(!nutrient == "DHA+EPA") %>% 
  mutate(country = countrycode(iso3c, 'iso3c', 'country.name')) %>% 
  group_by(iso3c, country, nutrient) %>% 
  summarise(perc_SSF_ASF = mean(perc_SSF_ASF, na.rm = T))

mean(percent_nutrient_supply$perc_SSF_ASF, na.rm = T)

stats_continent = total_nut_supply %>% 
  mutate(continent = countrycode(iso3c, 'iso3c', 'region23')) %>% 
  group_by(continent) %>% 
  summarise(perc_SSF = mean(perc_SSF))

stats_nut = percent_nutrient_supply %>% 
  group_by(nutrient) %>% 
  summarise(perc_SSF = mean(perc_SSF_ASF, na.rm = T),
            max_SSF = max(perc_SSF_ASF, na.rm = T),
            min_SSF = min(perc_SSF_ASF, na.rm = T))

