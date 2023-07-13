##Figure 1 - percent nutrient supply from SSF 

library(tidyverse)
library(ggpubr)
library(sf)

# Clear workspace
rm(list = ls())

total_nut_supply <- read_csv("Outputs/total_nut_supply.csv") %>% 
  select(-consump) %>% 
  spread(production_sector, nut_supply) %>% 
  mutate_all(~replace(., is.na(.), 0)) %>% 
  mutate(SSF = Artisanal + Subsistence,
         total = Artisanal + Industrial + Recreational + Subsistence + aquaculture + Import,
         perc_SSF = 100*SSF/total,
         nutrient = recode(nutrient, "Omega-3 fatty acids" = "DHA+EPA")) %>% 
  filter(nutrient == "DHA+EPA") %>% 
  rename(perc_SSF_ASF = perc_SSF) %>% 
  select(iso3c, nutrient, perc_SSF_ASF)

percent_nutrient_supply <- read_csv("Outputs/percent_nutrient_supply_coastal.csv") %>% 
  filter(!nutrient == "DHA+EPA") %>% 
  select(iso3c, nutrient, perc_SSF_ASF) %>% 
  rbind(total_nut_supply)

ggplot(data = percent_nutrient_supply) +
  geom_boxplot(aes(y = perc_SSF_ASF)) +
  facet_wrap(~nutrient, scales = "free")

#percent_nutrient_supply$perc_SSF_ASF[percent_nutrient_supply$nutrient=="Calcium" & percent_nutrient_supply$perc_SSF_ASF>25] = 25
#percent_nutrient_supply$perc_SSF_ASF[percent_nutrient_supply$nutrient=="DHA+EPA" & percent_nutrient_supply$perc_SSF_ASF>25] = 25
#percent_nutrient_supply$perc_SSF_ASF[percent_nutrient_supply$nutrient=="Protein" & percent_nutrient_supply$perc_SSF_ASF>10] = 10
percent_nutrient_supply$perc_SSF_ASF[percent_nutrient_supply$nutrient=="Zinc" & percent_nutrient_supply$perc_SSF_ASF>50] = 50
#percent_nutrient_supply$perc_SSF_ASF[percent_nutrient_supply$nutrient=="Vitamin B12" & percent_nutrient_supply$perc_SSF_ASF>25] = 25
percent_nutrient_supply$perc_SSF_ASF[percent_nutrient_supply$nutrient=="Iron" & percent_nutrient_supply$perc_SSF_ASF>75] = 75
percent_nutrient_supply$perc_SSF_ASF[percent_nutrient_supply$nutrient=="Vitamin A" & percent_nutrient_supply$perc_SSF_ASF>40] = 40


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

# Set breaks and labels
breaks_list <- list("DHA+EPA"=c(0, 50, 98),
                    "Vitamin B12"=c(0, 50, 95),
                    "Iron"=c(0, 40, 74.8),
                    "Zinc"=c(0, 25, 49.8),
                    "Calcium"=c(0, 50, 98),
                    "Vitamin A"=c(0, 20, 39.8))

labels_list <- list("DHA+EPA"=c("0", "50", "100"),
                    "Vitamin B12"=c("0", "50", "95"),
                    "Iron"=c("0", "40", ">75"),
                    "Zinc"=c("0", "25", ">50"),
                    "Calcium"=c("0", "50", "100"),
                    "Vitamin A"=c("0", "20", ">40"))

### per capita seafood consumption
plot_func = function(nut){
#nut = "Iron"
  if(nut %in% c("DHA+EPA", "Iron", "Calcium")){
    legend.pos = "left"
  }else{
    legend.pos = "right"
  }
  
  # Format data
  mpa_dta_sf <- world %>% 
    left_join(percent_nutrient_supply %>% filter(nutrient == nut), by=c("gu_a3"="iso3c"))
  
  # Spatialize tiny
  sdata_pt <- world_centers %>% 
    left_join(percent_nutrient_supply %>% filter(nutrient == nut), by=c("iso3"="iso3c")) %>% 
    # Reduce to ones with data
    filter(!is.na(perc_SSF_ASF)) %>% 
    arrange(area_sqkm) %>% 
    # Reduce to small
    filter(area_sqkm<=2.5*10^4 & continent!="Europe")
  
  # Build title
  nutr_do_use <- nut
  if(nut=="Vitamin A, RAE"){
    nutr_do_use <- "Vitamin A"
  }
  if(nut=="Vitamin B12"){
    nutr_do_use <- expression("Vitamin B"["12"])
  }
  
  # Get breaks and labels
  breaks <- breaks_list[[nut]]
  labels <- labels_list[[nut]]
  
  # Open Access reef area
  p <- ggplot(mpa_dta_sf) +
    geom_sf(mapping=aes(fill=perc_SSF_ASF), lwd=0.1) +
    # Plot small places
    geom_sf(data=sdata_pt, mapping=aes(fill=perc_SSF_ASF), shape=21, size=3, stroke=0.3) +
    # Plot French Guiana
    geom_sf(data=fguiana, lwd=0.1, color="grey30", fill="grey80") +
    # Crop out Antarctica
    coord_sf(y=c(-55, NA)) +
    # Legend and labels
    scale_fill_gradient2(name="SSF\nnutrient\nsupply (%)", 
                         breaks=breaks, labels=labels,
                         low="navy", high="darkred", mid="white", midpoint=0) +
    guides(fill = guide_colorbar(ticks.colour = "black", frame.colour = "black", barwidth = 0.8, barheight = 3)) +
    # Theme
    labs(title = nutr_do_use) +
    theme_bw() + base_theme +
    theme(legend.position=legend.pos,
          axis.text = element_blank(),
          axis.title=element_blank(), 
          legend.background = element_rect(fill=alpha('blue', 0)),
          legend.title = element_text(size = 9),
          plot.title = element_text(size = 13))
  
  return(p)
}

plot1 <- plot_func(nut = "DHA+EPA")
plot2 <- plot_func(nut = "Vitamin B12")
plot3 <- plot_func(nut = "Iron")
plot4 <- plot_func(nut = "Zinc")
plot5 <- plot_func(nut = "Calcium")
plot6 <- plot_func(nut = "Vitamin A")

# Merge maps
g <- gridExtra::grid.arrange(plot1, plot2,
                             plot3, plot4,
                             plot5, plot6, ncol=2)
g
# Export
ggsave(g, filename = "Figures/Figure SX - SSF nutrient supply ASF - coastal.pdf", 
       width=10, height=6.2, units="in", dpi=600, device=cairo_pdf)

ggsave(g, filename = "Figures/Figure SX - SSF nutrient supply ASF - coastal.jpeg", 
       height = 6.2, 
       width = 10)
