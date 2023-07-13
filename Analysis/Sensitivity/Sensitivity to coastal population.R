
library(tidyverse)

# Clear workspace
rm(list = ls())

##Read population information
total_nut_supply <- read_csv("Outputs/total_nut_supply.csv") %>% 
  select(-consump) %>% 
  mutate(nutrient = recode(nutrient, "DHA+EPA" = "Omega-3 fatty acids"))

coastal_pop_all <- read_csv("data/coastal_population_all_V2.csv") 

####Read GND############
BaseNutrients <- read_csv("~/Fisheries Nutrition Modeling/data/NutrientsBaseRev3.csv") %>% mutate(scenario = "base") %>% 
  rename("output" = "OUTPUT,0",
         "iso3c" = "...2",
         "food_abrev" = "...3",
         "nutrient" = "...4") %>% 
  #separate(X10, c("X11", "nutrient", "total"), "\\..",remove=T) %>%
  separate(elements, c("nutrient_long", "units", "X12"), "\\[", remove=F) %>% 
  mutate(units = gsub("]", "", units),
         nutrient = recode(nutrient,
                           "CA" = "Calcium",
                           "FE" = "Iron",
                           "MFAT" = "Monounsaturated fatty acids",
                           "O3" = "Omega-3 fatty acids",
                           "PFAT" = "Polyunsaturated fatty acids",
                           "SFAT" = "Saturated fatty acids",
                           "TFAT" = "Fat",
                           "VitA1" = "Vitamin A",
                           "VitB" = "Vitamin B12",
                           "ZN" = "Zinc",
                           "PROT" = "Protein",
                           "DES" = "Calories")) %>% 
  dplyr::select(-X12)


##Change to long format
NutrientsScen_long = reshape2::melt(BaseNutrients, id.vars=c("output", "countries", "products", "elements",     
                                                             "nutrient_long", "units", "iso3c",        
                                                             "food_abrev", "nutrient", "scenario")) %>% 
  rename("year" = "variable") %>% 
  mutate(year = gsub("A", "", year)) %>% 
  filter(nutrient %in% c("Iron", "Zinc", "Protein", "Vitamin A", "Vitamin B12", "Omega-3 fatty acids", "Calcium"))

countries_with_bug <- read_csv("~/Fisheries Nutrition Modeling/countries_with_bug.csv")
countries_bug = as.vector(countries_with_bug$iso)

NutrientsScen_long = NutrientsScen_long %>% 
  filter(!iso3c %in% countries_bug)

o3_suply = NutrientsScen_long %>% 
  filter(!products=="Total food",
         nutrient == "Omega-3 fatty acids") %>% 
  group_by(products) %>% 
  summarise(value = sum(value, na.rm = T))

NutrientsScen_long_other = NutrientsScen_long %>% 
  filter(!products=="Total food") %>% 
  rename("nutrient_supply" = "value") %>% 
  mutate(production_sector = case_when(products == "Fish" ~ "ASF",
                                       products == "Beef and Veal" ~ "ASF",
                                       products == "Eggs" ~ "ASF",
                                       products == "Pork" ~ "ASF",
                                       products == "Poultry" ~ "ASF",
                                       products == "Sheep" ~ "ASF",
                                       products == "Butter" ~ "Other",
                                       products == "Fruits and vegetables" ~ "Other",
                                       products == "Milk" ~ "Other",
                                       products == "Other Coarse Grains" ~ "Other",
                                       products == "Other Oilseeds" ~ "Other",
                                       products == "#N/A" ~ "Other",
                                       products == "Pulses" ~ "Other",
                                       products == "Sugar" ~ "Other",
                                       products == "Rice" ~ "Other",
                                       products == "Vegetable oils" ~ "Other",
                                       products == "Maize" ~ "Other",
                                       products == "Wheat" ~ "Other",
                                       products == "Roots tubers" ~ "Other")) %>%
  filter(year == 2017) %>% 
  group_by(iso3c, production_sector, nutrient) %>% 
  summarise(nut_supply = sum(nutrient_supply, na.rm = T)) %>% 
  ungroup()

iso_group <- read_csv("data/iso_group.csv")
countries_ISO <- read_csv("data/countries_ISO.csv")

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

total_nut_supply_O3 <- read_csv("Outputs/total_nut_supply.csv") %>% 
  spread(production_sector, nut_supply) %>% 
  mutate_all(~replace(., is.na(.), 0)) %>% 
  mutate(SSF = Artisanal + Subsistence,
         total = Artisanal + Industrial + Recreational + Subsistence + aquaculture + Import,
         perc_SSF = 100*SSF/total,
         nutrient = recode(nutrient, "Omega-3 fatty acids" = "DHA+EPA")) %>% 
  filter(nutrient == "DHA+EPA") %>% 
  select(iso3c, nutrient, perc_SSF)

sensit_func = function(coastal_distance, plot_lab){
  
#plot_lab = "10 km"
#coastal_distance = "km_30"

  if(coastal_distance %in% c("km_10", "km_30", "km_50")){
    legend.pos = "left"
  }else{
    legend.pos = "right"
  }
  
coastal_pop <- coastal_pop_all %>% 
  select(iso3c, {{coastal_distance}}) %>% 
  rename(pop = {{coastal_distance}})

nutrient_supply_final_v2 = rbind(NutrientsScen_long_other, total_nut_supply) %>% 
  spread(production_sector, nut_supply)

####FIll NAs 
missing_nutrient_supply = nutrient_supply_final_v2 %>% 
  filter(is.na(Other))

NutrientsScen_long_other_EUN = NutrientsScen_long_other %>% 
  filter(iso3c=="EUN") %>% 
  rename(group=iso3c)

NutrientsScen_long_other_group = NutrientsScen_long_other %>% 
  left_join(iso_group) %>% 
  group_by(group, production_sector, nutrient) %>% 
  summarise(nut_supply = sum(nut_supply, na.rm = T)) %>% 
  ungroup() %>% 
  drop_na(group) %>% 
  rbind(NutrientsScen_long_other_EUN)

missing_countries = data.frame(iso3c = unique(missing_nutrient_supply$iso3c)) %>% 
  left_join(countries_ISO) %>% 
  distinct(iso3c, .keep_all = T) %>% 
  left_join(iso_group) %>% 
  left_join(NutrientsScen_long_other_group) %>% 
  dplyr::select(-missing_countries, -group)

NutrientsScen_long_other_v2 = rbind(NutrientsScen_long_other, missing_countries)

nutrient_supply_final_v2 = rbind(NutrientsScen_long_other_v2, total_nut_supply) %>% 
  drop_na(production_sector) %>% 
  spread(production_sector, nut_supply) %>% 
  left_join(coastal_pop) %>% 
  drop_na(Other) %>% 
  drop_na(pop) %>% 
  mutate_all(~replace(., is.na(.), 0)) %>% 
  mutate(SSF = Artisanal + Subsistence,
         SSF_per_capita = SSF/pop/365,
         total = Other + ASF + SSF_per_capita,
         ASF = ASF + SSF_per_capita,
         perc_SSF = 100*SSF_per_capita/total,
         perc_SSF = if_else(perc_SSF>100, 100, perc_SSF),
         perc_SSF_ASF = 100*SSF_per_capita/ASF,
         perc_SSF_ASF = if_else(perc_SSF_ASF>100, 100, perc_SSF_ASF), 
         nutrient = recode(nutrient, "Omega-3 fatty acids" = "DHA+EPA"))

#write.csv(nutrient_supply_final_v2, "Outputs/percent_nutrient_supply_coastal.csv", row.names=FALSE)

percent_nutrient_supply <- nutrient_supply_final_v2 %>% 
  filter(!nutrient == "DHA+EPA") %>% 
  select(iso3c, nutrient, perc_SSF) %>% 
  rbind(total_nut_supply_O3) %>% 
  group_by(iso3c) %>% 
  summarise(perc_SSF = mean(perc_SSF, na.rm = T))

##Plot 1 - average contribution of SSF to nutrient supply
# Format data
ssf_sf <- world %>% 
  left_join(percent_nutrient_supply, by=c("gu_a3"="iso3c"))

# Spatialize tiny
sdata_ssf_pt <- world_centers %>% 
  left_join(percent_nutrient_supply, by=c("iso3"="iso3c")) %>% 
  # Reduce to ones with data
  filter(!is.na(perc_SSF)) %>% 
  arrange(area_sqkm) %>% 
  # Reduce to small
  filter(area_sqkm<=2.5*10^4 & continent!="Europe")

plot1 <- ggplot(ssf_sf) +
  geom_sf(mapping=aes(fill=perc_SSF), lwd=0.1) +
  # Plot small places
  geom_sf(data=sdata_ssf_pt, mapping=aes(fill=perc_SSF), shape=21, size=3, stroke=0.3) +
  # Plot French Guiana
  geom_sf(data=fguiana, lwd=0.1, color="grey30", fill="grey80") +
  # Crop out Antarctica
  coord_sf(y=c(-55, NA)) +
  # Legend and labels
  scale_fill_gradient2(name="Mean SSF\nnutrient\nsupply (%)", 
                       breaks=seq(0, 60, 20), labels=c("0", "20", "40", "60"), 
                       limits = c(0, 60),
                       low="navy", high="darkred", mid="white", midpoint=0) +
  guides(fill = guide_colorbar(ticks.colour = "black", frame.colour = "black", barwidth = 0.8, barheight = 3)) +
  # Theme
  labs(title = plot_lab) +
  theme_bw() + base_theme +
  theme(legend.position=legend.pos,
        axis.text = element_blank(),
        axis.title=element_blank(), 
        legend.background = element_rect(fill=alpha('blue', 0)),
        legend.title = element_text(size = 9),
        plot.title = element_text(face = "bold", size = 13))
#plot1 

return(plot1)
}


p1 = sensit_func(coastal_distance = "km_10", plot_lab = "10 km")
p2 = sensit_func(coastal_distance = "km_20", plot_lab = "20 km")
p3 = sensit_func(coastal_distance = "km_30", plot_lab = "30 km")
p4 = sensit_func(coastal_distance = "km_40", plot_lab = "40 km")
p5 = sensit_func(coastal_distance = "km_50", plot_lab = "50 km")
p6 = sensit_func(coastal_distance = "Total_pop", plot_lab = "Entire population")

g <- gridExtra::grid.arrange(p1, p2,
                             p3, p4,
                             p5, p6, ncol=2)
g
# Export
ggsave(g, filename = "Figures/Figure SX - Sensitivity to coastal population.pdf", 
       width=10, height=6.2, units="in", dpi=600, device=cairo_pdf)

ggsave(g, filename = "Figures/Figure SX - Sensitivity to coastal population.jpeg", 
       height = 6.2, 
       width = 10)

