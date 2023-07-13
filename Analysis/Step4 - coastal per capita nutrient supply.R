##Step 4 - Calculate per capita nutrient supply

library(tidyverse)

# Clear workspace
rm(list = ls())

##Read population information

coastal_pop <- read_csv("data/coastal_population_all_V2.csv") %>% 
  select(iso3c, km_20) %>% 
  rename(pop = km_20)

total_nut_supply <- read_csv("Outputs/total_nut_supply.csv") %>% 
  select(-consump) %>% 
  mutate(nutrient = recode(nutrient, "DHA+EPA" = "Omega-3 fatty acids"))

####Read GND############
BaseNutrients <- read_csv("data/NutrientsBaseRev3.csv") %>% mutate(scenario = "base") %>% 
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

countries_with_bug <- read_csv("data/countries_with_bug.csv")
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
  filter(year == 2019) %>% 
  group_by(iso3c, production_sector, nutrient) %>% 
  summarise(nut_supply = sum(nutrient_supply, na.rm = T)) %>% 
  ungroup()


nutrient_supply_final_v2 = rbind(NutrientsScen_long_other, total_nut_supply) %>% 
  spread(production_sector, nut_supply)

####FIll NAs 
missing_nutrient_supply = nutrient_supply_final_v2 %>% 
  filter(is.na(Other))

NutrientsScen_long_other_EUN = NutrientsScen_long_other %>% 
  filter(iso3c=="EUN") %>% 
  rename(group=iso3c)

iso_group <- read_csv("data/iso_group.csv")
countries_ISO <- read_csv("data/countries_ISO.csv")

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

write.csv(nutrient_supply_final_v2, "Outputs/percent_nutrient_supply_coastal.csv", row.names=FALSE)
