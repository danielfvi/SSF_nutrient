##Step 3 - Calculate nutrient supply

library(tidyverse)

# Clear workspace
rm(list = ls())

#Read nutrient information
spp_nutrients_SAU <- read_csv("Outputs/spp_nutrient_final.csv") %>% 
  rename(scientific_name = species) %>% 
  dplyr::select(scientific_name, nutrient, value)

spp_category <- read_csv("Outputs/spp_nutrient_final.csv") %>% 
  mutate(genus_cat = recode(genus_cat, "Marine Fish; Other" = "Marine fish; Other")) %>% 
  group_by(genus_cat, nutrient) %>% 
  summarise(value = median(value, na.rm = T))

edible_portion <- read_csv("Outputs/edible_portion.csv")
total_consumption <- read_csv("Outputs/total_consumption_2019.csv")

# x = total_consumption %>% 
#   filter(iso3c == "CHL") %>% 
#   group_by(production_sector) %>% 
#   summarise(pred_consumption = sum(pred_consumption))
# ggplot(data = x) + 
#   geom_bar(aes(y = production_sector, x = pred_consumption), stat = "identity") + 
#   labs(title = "CHL", x = "Predicted consumption", y = "Sector") + 
#   theme_bw()

###Multiply by edible portion##########
total_consumption = total_consumption %>%
  mutate(scientific_name = tolower(scientific_name))

total_consumption_edible = left_join(total_consumption, edible_portion) %>% 
  mutate(edible_portion = pred_consumption*edible) %>% 
  rename(pred_consumption_whole = pred_consumption,
         pred_consumption = edible_portion)

####Merge with nutrient content#######

##Assign those with scientific names
consump_sci = total_consumption_edible %>% 
  filter(!is.na(scientific_name))

consumption_sci = left_join(consump_sci, spp_nutrients_SAU)

##Assign those without scientific name
consump_cat = total_consumption_edible %>% 
  filter(is.na(scientific_name))

consumption_cat = left_join(consump_cat, spp_category)

consumption_nutrition = rbind(consumption_sci, consumption_cat) %>% 
  mutate(nut_supply = value*pred_consumption*10000) %>% 
  filter(nut_supply>0)

total_consump = consumption_nutrition %>% 
  group_by(iso3c, production_sector) %>% 
  summarize(consump = sum(pred_consumption, na.rm = T)) %>% 
  ungroup() 

total_nut_supply = consumption_nutrition %>% 
  group_by(iso3c, nutrient, production_sector) %>% 
  summarize(nut_supply = sum(nut_supply, na.rm = T)) %>% 
  ungroup() %>% 
  left_join(total_consump)

write.csv(total_nut_supply, "Outputs/total_nut_supply.csv", row.names = FALSE)

