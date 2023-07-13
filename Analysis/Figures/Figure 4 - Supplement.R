##Figure 1 - percent nutrient supply from SSF 

library(tidyverse)
library(ggpubr)
library(sf)
library(ggrepel)

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
  select(iso3c, nutrient, perc_SSF)

percent_nutrient_supply <- read_csv("Outputs/percent_nutrient_supply_coastal.csv") %>% 
  filter(!nutrient == "DHA+EPA") %>%
  drop_na(perc_SSF) %>% 
  select(iso3c, nutrient, perc_SSF) %>% 
  rbind(total_nut_supply) %>% 
  group_by(iso3c) %>% 
  summarise(perc_SSF = mean(perc_SSF, na.rm = T))

percent_nutrient_supply_stats <- read_csv("Outputs/percent_nutrient_supply_coastal.csv") %>% 
  filter(!nutrient == "DHA+EPA") %>%
  drop_na(perc_SSF) %>% 
  select(iso3c, nutrient, perc_SSF) %>% 
  #rbind(total_nut_supply) %>% 
  group_by(iso3c) %>% 
  summarise(perc_SSF_min = min(perc_SSF, na.rm = T),
            perc_SSF_max = max(perc_SSF, na.rm = T),
            perc_SSF_sd = sd(perc_SSF, na.rm=T))

percent_nutrient_supply = percent_nutrient_supply %>% 
  left_join(percent_nutrient_supply_stats) %>% 
  mutate(perc_SSF_low = perc_SSF - perc_SSF_sd,
         perc_SSF_high = perc_SSF + perc_SSF_sd,
         perc_SSF_low = if_else(perc_SSF_low<0, 0, perc_SSF_low))

sevs = read_csv("data/2017_perc_pop_deficient.csv") %>% 
  group_by(iso3) %>% 
  summarise(intake_sd = sd(perc_deficient),
            intake = mean(perc_deficient)) %>% 
  mutate(intake_low = intake - intake_sd,
         intake_high = intake + intake_sd,
         intake_low = if_else(intake_low<0, 0, intake_low))

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

##Plot 1 - average contribution of SSF to nutrient supply vs inadequate intake
ecdf_fun <- function(x,perc) ecdf(x)(perc)

factor.final = c("Less vulnerable and less reliant",
                 "Less vulnerable but reliant",
                 "Vulnerable but less reliant",
                 "Vulnerable and reliant")
col.final = c("khaki1","chocolate2", "salmon2", "red3")

SSF_intake = percent_nutrient_supply %>% 
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
         is_3 = if_else(combined_score == 3, 1, 0.8))

SSF_intake$category = factor(SSF_intake$category, levels = factor.final)

plot1 = ggplot(data = SSF_intake, aes(y=perc_SSF, x=intake, label = iso3c, color = category)) +
  geom_linerange(aes(ymin = perc_SSF_low, ymax = perc_SSF_high), size = 1, color = "black", alpha = 0.03, position = position_dodge(0.5)) +
  geom_linerange(aes(xmin = intake_low, xmax = intake_high), size = 1, color = "black", alpha = 0.03, position = position_dodge(0.5)) +
  #geom_point(shape=21) +
  geom_point(aes(alpha = is_3), shape=19, size = 4.5) +
  scale_alpha(range = c(0.6, 1))+
  geom_text_repel(min.segment.length = 0.5, seed = 42, box.padding = 0.5, size = 5, color = "black", max.overlaps = Inf) +
  #geom_vline(xintercept = 0, linetype="dashed") + 
  #geom_hline(yintercept = 1, linetype="dashed") +
  scale_color_manual(values = col.final, na.value = "grey87",
                    labels = c("Less vulnerable\nand less reliant",
                               "Less vulnerable\nbut reliant",
                               "Vulnerable but\nless reliant",
                               "Vulnerable and\nreliant")) +
  guides(alpha = "none", color = "none") +
  labs(y="Contribution of SSF to total\nnutrient supply (%)", 
       x = "Prevalence of inadequate intake (%)",
       title = "A") +
  ylim(0, 100) +
  #xlim(0, 20)+
  theme_classic() +
  theme(axis.title = element_text(size = 23),
        axis.text = element_text(size = 18),
        plot.title = element_text(face = "bold", size = 23))

plot1

ggsave(plot1, filename = "Figures/Figure 4 - Supplement.jpeg", 
       height = 10, 
       width = 15)

## Plot 2 - scores of SSF supply and inadequate intake
SSF_scores = SSF_intake %>% 
  select(iso3c, category) 

factor.final = c("Less vulnerable and less reliant",
                 "Less vulnerable but reliant",
                 "Vulnerable but less reliant",
                 "Vulnerable and reliant")
col.final = c("khaki1","yellow4", "salmon2", "red3")

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

plot3 <- ggplot(score_pop_sf) +
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
  guides(fill = guide_legend(title = "",
                             byrow = TRUE,
                             override.aes=list(colour = NA))) + 
  labs(title = "   B") +
  theme_bw() + base_theme +
  theme(legend.position="right",
        axis.text = element_blank(),
        axis.title=element_blank(), 
        legend.background = element_rect(fill=alpha('blue', 0)),
        legend.title = element_text(size = 9),
        legend.text = element_text(size = 18),
        plot.title = element_text(face = "bold", size = 23),
        plot.margin = ggplot2::margin(t = -1, r = 0.25, b = 0.1, l = 2.3, "cm"))
#plot3
g = ggarrange(plot1, plot3, common.legend = T, ncol = 1)
g
# Export
ggsave(g, filename = "Figures/Figure 4 - final.pdf", 
       width=10, height=10, units="in", dpi=600, device=cairo_pdf)

ggsave(g, filename = "Figures/Figure 4 - final.jpeg", 
       height = 10, 
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
         country = countrycode(iso3c, 'iso3c', 'country.name'),
         continent = countrycode(iso3c, 'iso3c', 'region23'))

x = SSF_scores %>% 
  filter(combined_score ==3)

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


vulnerable_countries = SSF_scores %>% 
  filter(intake>50) %>% 
  count(category) %>% 
  mutate(perc = 100*n/sum(n))

sevs = read_csv("data/2017_perc_pop_deficient.csv") %>% 
  group_by(nutrient) %>% 
  summarise(def = sum(ndeficient, na.rm=T),
            intake = mean(perc_deficient, na.rm=T))
