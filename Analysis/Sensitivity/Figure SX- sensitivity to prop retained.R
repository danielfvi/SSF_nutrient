##Figure 1 - percent nutrient supply from SSF 

library(tidyverse)
library(ggpubr)
library(sf)

# Clear workspace
rm(list = ls())

total_nut_supply <- read_csv("Outputs/total_nut_supply.csv") %>% 
  spread(production_sector, nut_supply) %>% 
  mutate_all(~replace(., is.na(.), 0)) %>% 
  mutate(SSF = Artisanal + Subsistence,
         total = Artisanal + Industrial + Recreational + Subsistence + aquaculture + Import,
         perc_SSF = 100*SSF/total,
         nutrient = recode(nutrient, "Omega-3 fatty acids" = "DHA+EPA")) %>% 
  filter(nutrient == "DHA+EPA") %>% 
  select(iso3c, nutrient, perc_SSF)

percent_nutrient_supply <- read_csv("Outputs/percent_nutrient_supply_coastal.csv") %>% 
  filter(!nutrient == "DHA+EPA") %>% 
  select(iso3c, nutrient, perc_SSF) %>% 
  rbind(total_nut_supply) %>% 
  group_by(iso3c) %>% 
  summarise(perc_SSF = mean(perc_SSF, na.rm = T))

percent_nutrient_supply_allretained <- read_csv("Outputs/percent_nutrient_supply_coastal_allretained.csv") %>% 
  filter(!nutrient == "DHA+EPA") %>% 
  select(iso3c, nutrient, perc_SSF) %>% 
  rbind(total_nut_supply) %>% 
  group_by(iso3c) %>% 
  summarise(perc_SSF = mean(perc_SSF, na.rm = T))

sevs = read_csv("data/2017_perc_pop_deficient.csv") %>% 
  group_by(iso3) %>% 
  summarise(intake = mean(perc_deficient))

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
  scale_fill_gradient2(name="Mean SSF                       \nnutrient\nsupply (%)", 
                       #breaks=seq(0, 10, 2), labels=c("0", "2", "4", "6", "8", ">10"),
                       low="navy", high="darkred", mid="white", midpoint=0) +
  guides(fill = guide_colorbar(ticks.colour = "black", frame.colour = "black", barwidth = 0.8, barheight = 3)) +
  # Theme
  labs(title = "A") +
  theme_bw() + base_theme +
  theme(legend.position="right",
        axis.text = element_blank(),
        axis.title=element_blank(), 
        legend.background = element_rect(fill=alpha('blue', 0)),
        legend.title = element_text(size = 9),
        plot.title = element_text(face = "bold", size = 13))
#plot1 

## Plot 2 - scores of SSF supply and inadequate intake
ecdf_fun <- function(x,perc) ecdf(x)(perc)

SSF_scores = percent_nutrient_supply %>% 
  left_join(sevs, by = c("iso3c" = "iso3")) %>% 
  mutate(q_SSF = ecdf_fun(percent_nutrient_supply$perc_SSF, perc_SSF),
         q_intake = ecdf_fun(sevs$intake, intake),
         is_dep = if_else(q_SSF>0.70, 1, 0),
         is_def = if_else(q_intake>0.70, 2, 0),
         combined_score = is_def + is_dep,
         category = case_when(combined_score == 0 ~ "Less vulnerable and less reliant",
                              combined_score == 1 ~ "Less vulnerable but reliant",
                              combined_score == 2 ~ "Vulnerable but less reliant",
                              combined_score == 3 ~ "Vulnerable and reliant")) %>% 
  select(iso3c, category)

factor.final = c("Less vulnerable and less reliant",
                 "Less vulnerable but reliant",
                 "Vulnerable but less reliant",
                 "Vulnerable and reliant")
col.final = c("khaki1","chocolate2", "salmon2", "red3")

SSF_scores$category = factor(SSF_scores$category, levels = factor.final)

# Format data
score_pop_sf <- world %>% 
  left_join(SSF_scores, by=c("gu_a3"="iso3c"))

# Spatialize tiny
sdata_score_pt <- world_centers %>% 
  left_join(SSF_scores, by=c("iso3"="iso3c")) %>% 
  # Reduce to ones with data
  filter(!is.na(category)) %>% 
  arrange(area_sqkm) %>% 
  # Reduce to small
  filter(area_sqkm<=2.5*10^4 & continent!="Europe")

plot2 <- ggplot(score_pop_sf) +
  geom_sf(mapping=aes(fill=category), lwd=0.1) +
  # Plot small places
  geom_sf(data=sdata_score_pt, mapping=aes(fill=category), shape=21, size=3, stroke=0.3) +
  # Plot French Guiana
  geom_sf(data=fguiana, lwd=0.1, color="grey30", fill="grey80") +
  # Crop out Antarctica
  coord_sf(y=c(-55, NA)) +
  # Legend and labels
  labs(fill = "") +
  scale_fill_manual(values = col.final, na.value = "grey87",
                    labels = c("Less vulnerable\nand less reliant",
                               "Less vulnerable\nbut reliant",
                               "Vulnerable but\nless reliant",
                               "Vulnerable and\nreliant")) +  # Theme
  guides(fill = guide_legend(byrow = TRUE,
                             override.aes=list(colour = NA))) + 
  labs(title = "A") +
  theme_bw() + base_theme +
  theme(legend.position="bottom",
        axis.text = element_blank(),
        axis.title=element_blank(), 
        legend.background = element_rect(fill=alpha('blue', 0)),
        legend.title = element_text(size = 9),
        plot.title = element_text(face = "bold", size = 13))
#plot2

##Plot 3 - average contribution of SSF to nutrient supply - all retained
# Format data
ssf_sf <- world %>% 
  left_join(percent_nutrient_supply_allretained, by=c("gu_a3"="iso3c"))

# Spatialize tiny
sdata_ssf_pt <- world_centers %>% 
  left_join(percent_nutrient_supply_allretained, by=c("iso3"="iso3c")) %>% 
  # Reduce to ones with data
  filter(!is.na(perc_SSF)) %>% 
  arrange(area_sqkm) %>% 
  # Reduce to small
  filter(area_sqkm<=2.5*10^4 & continent!="Europe")

plot3 <- ggplot(ssf_sf) +
  geom_sf(mapping=aes(fill=perc_SSF), lwd=0.1) +
  # Plot small places
  geom_sf(data=sdata_ssf_pt, mapping=aes(fill=perc_SSF), shape=21, size=3, stroke=0.3) +
  # Plot French Guiana
  geom_sf(data=fguiana, lwd=0.1, color="grey30", fill="grey80") +
  # Crop out Antarctica
  coord_sf(y=c(-55, NA)) +
  # Legend and labels
  scale_fill_gradient2(name="Mean SSF                       \nnutrient\nsupply (%)", 
                       #breaks=seq(0, 10, 2), labels=c("0", "2", "4", "6", "8", ">10"),
                       low="navy", high="darkred", mid="white", midpoint=0) +
  guides(fill = guide_colorbar(ticks.colour = "black", frame.colour = "black", barwidth = 0.8, barheight = 3)) +
  # Theme
  labs(title = "B") +
  theme_bw() + base_theme +
  theme(legend.position="right",
        axis.text = element_blank(),
        axis.title=element_blank(), 
        legend.background = element_rect(fill=alpha('blue', 0)),
        legend.title = element_text(size = 9),
        plot.title = element_text(face = "bold", size = 13))
#plot3

## Plot 4 - scores of SSF supply and inadequate intake
SSF_scores = percent_nutrient_supply_allretained %>% 
  left_join(sevs, by = c("iso3c" = "iso3")) %>% 
  mutate(q_SSF = ecdf_fun(percent_nutrient_supply$perc_SSF, perc_SSF),
         q_intake = ecdf_fun(sevs$intake, intake),
         is_dep = if_else(q_SSF>0.70, 1, 0),
         is_def = if_else(q_intake>0.70, 2, 0),
         combined_score = is_def + is_dep,
         category = case_when(combined_score == 0 ~ "Less vulnerable and less reliant",
                              combined_score == 1 ~ "Less vulnerable but reliant",
                              combined_score == 2 ~ "Vulnerable but less reliant",
                              combined_score == 3 ~ "Vulnerable and reliant")) %>% 
  select(iso3c, category)

factor.final = c("Less vulnerable and less reliant",
                 "Less vulnerable but reliant",
                 "Vulnerable but less reliant",
                 "Vulnerable and reliant")
col.final = c("khaki1","chocolate2", "salmon2", "red3")

SSF_scores$category = factor(SSF_scores$category, levels = factor.final)

# Format data
score_pop_sf <- world %>% 
  left_join(SSF_scores, by=c("gu_a3"="iso3c"))

# Spatialize tiny
sdata_score_pt <- world_centers %>% 
  left_join(SSF_scores, by=c("iso3"="iso3c")) %>% 
  # Reduce to ones with data
  filter(!is.na(category)) %>% 
  arrange(area_sqkm) %>% 
  # Reduce to small
  filter(area_sqkm<=2.5*10^4 & continent!="Europe")

plot4 <- ggplot(score_pop_sf) +
  geom_sf(mapping=aes(fill=category), lwd=0.1) +
  # Plot small places
  geom_sf(data=sdata_score_pt, mapping=aes(fill=category), shape=21, size=3, stroke=0.3) +
  # Plot French Guiana
  geom_sf(data=fguiana, lwd=0.1, color="grey30", fill="grey80") +
  # Crop out Antarctica
  coord_sf(y=c(-55, NA)) +
  # Legend and labels
  labs(fill = "") +
  scale_fill_manual(values = col.final, na.value = "grey87",
                    labels = c("Less vulnerable\nand less reliant",
                               "Less vulnerable\nbut reliant",
                               "Vulnerable but\nless reliant",
                               "Vulnerable and\nreliant")) +  # Theme
  guides(fill = guide_legend(byrow = TRUE,
                             override.aes=list(colour = NA))) + 
  labs(title = "B") +
  theme_bw() + base_theme +
  theme(legend.position="none",
        plot.margin = ggplot2::margin(t = 0, r = 1, b = 0, l = 1, "cm"),
        axis.text = element_blank(),
        axis.title=element_blank(), 
        legend.background = element_rect(fill=alpha('blue', 0)),
        legend.title = element_text(size = 9),
        plot.title = element_text(face = "bold", size = 13))
#plot3

g <- gridExtra::grid.arrange(plot1,
                             plot3,
                             ncol=1)

g1 <- gridExtra::grid.arrange(plot2,
                             plot4,
                             ncol=1)
g
# Export
ggsave(g, filename = "Figures/Figure SX - sensitivity to prop retained.pdf", 
       width=10, height=6.2, units="in", dpi=600, device=cairo_pdf)

ggsave(g, filename = "Figures/Figure SX - sensitivity to prop retained.jpeg", 
       height = 6.2, 
       width = 10)

###Stats
library(countrycode)

SSF_scores = percent_nutrient_supply %>% 
  left_join(sevs, by = c("iso3c" = "iso3")) %>% 
  mutate(q_SSF = ecdf_fun(percent_nutrient_supply$perc_SSF, perc_SSF),
         q_intake = ecdf_fun(sevs$intake, intake),
         is_dep = if_else(q_SSF>0.70, 1, 0),
         is_def = if_else(q_intake>0.70, 2, 0),
         combined_score = is_def + is_dep,
         category = case_when(combined_score == 0 ~ "Less vulnerable and less reliant",
                              combined_score == 1 ~ "Less vulnerable but reliant",
                              combined_score == 2 ~ "Vulnerable but less reliant",
                              combined_score == 3 ~ "Vulnerable and reliant"),
         country = countrycode(iso3c, 'iso3c', 'country.name'))

stats_nut = SSF_scores %>% 
  count(category) %>% 
  mutate(perc = 100*n/sum(n))

stats_continent = SSF_scores %>% 
  mutate(country = countrycode(iso3c, 'iso3c', 'country.name'),
         continent = countrycode(iso3c, 'iso3c', 'region23')) %>% 
  group_by(continent) %>% 
  count(category) %>% 
  mutate(perc = 100*n/sum(n))

stats_continent_SSF = SSF_scores %>% 
  mutate(country = countrycode(iso3c, 'iso3c', 'country.name'),
         continent = countrycode(iso3c, 'iso3c', 'region23')) %>% 
  group_by(continent) %>% 
  count(is_dep) %>% 
  mutate(perc = 100*n/sum(n))

stats_country = percent_nutrient_supply %>% 
  mutate(country = countrycode(iso3c, 'iso3c', 'country.name'),
         continent = countrycode(iso3c, 'iso3c', 'region23')) %>% 
  group_by(country, continent, nutrient) %>% 
  summarise(perc_SSF = mean(perc_SSF, na.rm = T))




