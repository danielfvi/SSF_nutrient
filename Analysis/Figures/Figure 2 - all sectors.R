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


##relative to seafood supply - map
total_nut_supply <- read_csv("Outputs/total_nut_supply.csv") %>% 
  select(-consump) %>% 
  spread(production_sector, nut_supply) %>% 
  mutate_all(~replace(., is.na(.), 0)) %>% 
  rename("Aquaculture" = "aquaculture") %>% 
  mutate(total = Artisanal + Industrial + Recreational + Subsistence + Aquaculture + Import,
         nutrient = recode(nutrient, "Omega-3 fatty acids" = "DHA+EPA")) %>% 
  reshape2::melt(id.vars = c("iso3c", "nutrient", "total")) %>% 
  mutate(perc_cont = 100*value/total) %>% 
  group_by(iso3c, variable) %>% 
  summarise(perc_cont = mean(perc_cont, na.rm = T))

# Format data
mpa_dta_sf <- world %>% 
  left_join(total_nut_supply, by=c("gu_a3"="iso3c"))
  
# Spatialize tiny
sdata_pt <- world_centers %>% 
  left_join(total_nut_supply, by=c("iso3"="iso3c")) %>% 
  # Reduce to ones with data
  filter(!is.na(perc_cont)) %>% 
  arrange(area_sqkm) %>% 
  # Reduce to small
  filter(area_sqkm<=2.5*10^4 & continent!="Europe")
  
plot_sectors = function(sector){

  if(sector %in% c("Artisanal", "Industrial", "Import")){
    legend.pos = "left"
  }else{
    legend.pos = "right"
  }
  
  # Open Access reef area
  p2 <- ggplot(mpa_dta_sf %>% filter(variable == sector)) +
    geom_sf(mapping=aes(fill=perc_cont), lwd=0.1) +
    # Plot small places
    geom_sf(data=sdata_pt %>% filter(variable == sector),
            mapping=aes(fill=perc_cont), shape=21, size=3, stroke=0.3) +
    # Plot French Guiana
    geom_sf(data=fguiana, lwd=0.1, color="grey30", fill="grey80") +
    # Crop out Antarctica
    coord_sf(y=c(-55, NA)) +
    # Legend and labels
    scale_fill_gradient2(name="Nutrient\nsupply (%)", 
                         #breaks=c(2.5, 5.01, 7.31), labels=c("15", "150", "1500"),
                         low="navy", high="darkred", mid="white", midpoint=0) +
    guides(fill = guide_colorbar(ticks.colour = "black", frame.colour = "black", barwidth = 1, barheight = 4)) +
    # Theme
    labs(title = sector) +
    theme_bw() + base_theme +
    theme(legend.position=legend.pos,
          axis.text = element_blank(),
          axis.title=element_blank(), 
          legend.background = element_rect(fill=alpha('blue', 0)),
          legend.title = element_text(size = 9),
          plot.title = element_text(size = 13))
    return(p2)
}

plot4 = plot_sectors(sector = "Aquaculture")
plot5 = plot_sectors(sector = "Import")
plot1 = plot_sectors(sector = "Artisanal")
plot2 = plot_sectors(sector = "Subsistence")
plot3 = plot_sectors(sector = "Industrial")
plot6 = plot_sectors(sector = "Recreational")

# Merge maps
g <- gridExtra::grid.arrange(plot1, plot2,
                             plot3, plot4,
                             plot5, plot6, ncol=2)
g
# Export
ggsave(g, filename = "Figures/Figure SX - all sectors.pdf", 
       width=10, height=6.2, units="in", dpi=600, device=cairo_pdf)

ggsave(g, filename = "Figures/Figure SX - all sectors.jpeg", 
       height = 6.2, 
       width = 10)

####################Relative to ASF
##Relative to ASF - Industrial
percent_nutrient_supply <- read_csv("Outputs/percent_nutrient_supply_coastal.csv") %>% 
    filter(!nutrient == "DHA+EPA") %>%
    rename("Aquaculture" = "aquaculture") %>% 
    dplyr::select(-SSF, -SSF_per_capita, -total, -perc_SSF, -perc_SSF_ASF, -Other) %>% 
    reshape2::melt(id.vars = c("iso3c", "nutrient", "ASF", "pop")) %>% 
    mutate(per_capita = value/pop/365,
          perc_cont = 100*per_capita/ASF,
          perc_cont = if_else(perc_cont>100, 100, perc_cont)) %>% 
    group_by(iso3c, variable) %>% 
    summarise(perc_cont = mean(perc_cont, na.rm = T))

# Format data
mpa_dta_sf <- world %>% 
  left_join(percent_nutrient_supply, by=c("gu_a3"="iso3c"))

# Spatialize tiny
sdata_pt <- world_centers %>% 
  left_join(percent_nutrient_supply, by=c("iso3"="iso3c")) %>% 
  # Reduce to ones with data
  filter(!is.na(perc_cont)) %>% 
  arrange(area_sqkm) %>% 
  # Reduce to small
  filter(area_sqkm<=2.5*10^4 & continent!="Europe")

plot_sectors = function(sector){
  
  if(sector %in% c("Artisanal", "Industrial", "Import")){
    legend.pos = "left"
  }else{
    legend.pos = "right"
  }
  
  # Open Access reef area
  p2 <- ggplot(mpa_dta_sf %>% filter(variable == sector)) +
    geom_sf(mapping=aes(fill=perc_cont), lwd=0.1) +
    # Plot small places
    geom_sf(data=sdata_pt %>% filter(variable == sector),
            mapping=aes(fill=perc_cont), shape=21, size=3, stroke=0.3) +
    # Plot French Guiana
    geom_sf(data=fguiana, lwd=0.1, color="grey30", fill="grey80") +
    # Crop out Antarctica
    coord_sf(y=c(-55, NA)) +
    # Legend and labels
    scale_fill_gradient2(name="Nutrient\nsupply (%)", 
                         #breaks=c(2.5, 5.01, 7.31), labels=c("15", "150", "1500"),
                         low="navy", high="darkred", mid="white", midpoint=0) +
    guides(fill = guide_colorbar(ticks.colour = "black", frame.colour = "black", barwidth = 1, barheight = 4)) +
    # Theme
    labs(title = sector) +
    theme_bw() + base_theme +
    theme(legend.position=legend.pos,
          axis.text = element_blank(),
          axis.title=element_blank(), 
          legend.background = element_rect(fill=alpha('blue', 0)),
          legend.title = element_text(size = 9),
          plot.title = element_text(size = 13))
  return(p2)
}

plot4 = plot_sectors(sector = "Aquaculture")
plot5 = plot_sectors(sector = "Import")
plot1 = plot_sectors(sector = "Artisanal")
plot2 = plot_sectors(sector = "Subsistence")
plot3 = plot_sectors(sector = "Industrial")
plot6 = plot_sectors(sector = "Recreational")

# Merge maps
g <- gridExtra::grid.arrange(plot1, plot2,
                             plot3, plot4,
                             plot5, plot6, ncol=2)
g
# Export
ggsave(g, filename = "Figures/Figure SX - all sectors - ASF.pdf", 
       width=10, height=6.2, units="in", dpi=600, device=cairo_pdf)

ggsave(g, filename = "Figures/Figure SX - all sectors - ASF.jpeg", 
       height = 6.2, 
       width = 10)

