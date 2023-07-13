##Step 2 - adjust for trade

library(tidyverse)

# Clear workspace
rm(list = ls())

##Read and clean FAO food balance import/export data
FAO_commod <- read_csv("data/FAO_FBS_2021.csv")

FAO_commod = reshape2::melt(FAO_commod, id.vars = names(FAO_commod)[1:5], 
                            measure.vars = names(FAO_commod)[6:ncol(FAO_commod)])

FAO_commod = FAO_commod %>% 
  rename(year = variable, tonnes = value, species_group = FAOSTAT_group) %>% 
  filter(!tonnes=="...") %>% 
  mutate(tonnes = as.double(tonnes)) %>% 
  drop_na(tonnes) %>% 
  drop_na(iso3c)

##Create Genus category
FAO_commod = FAO_commod %>%
  mutate(genus_cat = case_when(#Cephalopods
    species_group=="Cephalopods" ~ "Cephalopods",
    #Demersal fish
    species_group=="Demersal fish" ~ "Demersal Fish",
    #Pelagic fish
    species_group=="Pelagic fish" ~ "Pelagic Fish",
    #Crustaceans
    species_group=="Crustaceans" ~ "Crustaceans",
    #Marine Fish; Other
    species_group=="Marine fish nei" ~ "Marine Fish; Other",
    #Moluscs; Other
    species_group=="Molluscs excl. cephalopods" ~ "Moluscs; Other",
    #Freshwater
    species_group=="Freshwater & diadromous fish" ~ "Freshwater Fish",
    #Other aquatic animals (Aquatic Animals; Others)
    species_group=="Aquatic animals nei" ~ "Aquatic Animals; Others"))

FAO_commod = FAO_commod %>% 
  filter(!genus_cat %in% c("Freshwater Fish"))

FAO_commod$year = as.character(FAO_commod$year)
FAO_commod$year = as.numeric(FAO_commod$year)

FAO_commod = FAO_commod %>% 
  filter(year==2017) 

##Remove predicted reexports
FAO_reexports <- read_csv("data/FAO_reexports.csv") %>% 
  dplyr::select(iso3c, genus_cat, Reexports)

FAO_commod_wide = FAO_commod %>% 
  spread(element, tonnes) %>% 
  left_join(FAO_reexports) %>% 
  mutate(imports_reex = `Food imports` - Reexports,
         exports_reex = `Food exports` - Reexports,
         delta_exports = `Food exports` - Production,
         pred_reexports = if_else(delta_exports>0, delta_exports, 0),
         pred_imports = imports_reex - pred_reexports,
         pred_exports = exports_reex - pred_reexports,
         imports_pos = if_else(pred_imports<0, 0, pred_imports),
         exports_pos = if_else(pred_exports<0, 0, pred_exports),
         imports = if_else(is.na(imports_pos), `Food imports`, pred_imports),
         exports = if_else(is.na(exports_pos), `Food imports`, pred_exports),
         imports = if_else(is.na(imports), 0, imports),
         exports = if_else(is.na(exports), 0, exports),
         imports = if_else(imports<0, 0, imports),
         exports = if_else(exports<0, 0, exports),
         perc_reexp = 100*pred_reexports/`Food exports`)

FAO_FBS = reshape2::melt(FAO_commod_wide, 
                         id.vars = c(names(FAO_commod_wide)[1:6]),
                         measure.vars = c("imports", "exports")) %>% 
  rename(element = variable,
         tonnes = value) %>% 
  drop_na(tonnes)

FAO_export = FAO_FBS %>% filter(element=="exports")
FAO_import = FAO_FBS %>% filter(element=="imports")

FB_export = FAO_export %>% 
  group_by(iso3c) %>%
  summarise(tonnes_FB = sum(tonnes))

FB_import = FAO_import %>% 
  group_by(iso3c) %>%
  summarise(tonnes_FB = sum(tonnes))

#Percent of production in each new category per country and production source
total_commercial_prod <- read_csv("Outputs/total_commercial_prod_2019.csv")

# x = total_commercial_prod %>%
#   filter(iso3c == "CHL") %>%
#   group_by(production_sector) %>%
#   summarise(tonnes = sum(tonnes))
# ggplot(data = x) + 
#   geom_bar(aes(y = production_sector, x = tonnes), stat = "identity") + 
#   labs(title = "CHL", x = "Production (tonnes)", y = "Sector") + 
#   theme_bw()

countries = unique(total_commercial_prod$iso3c)
genus = unique(total_commercial_prod$genus_cat)

for(j in 1:length(countries)){
  for(i in 1:length(genus)){
    x = total_commercial_prod %>% 
      filter(iso3c == countries[j],
             genus_cat == genus[i])
    total_tonnes = sum(x$tonnes)
    x = x %>% 
      mutate(prop_catch = tonnes/total_tonnes)
    y = FAO_export %>% 
      filter(iso3c == countries[j],
             genus_cat == genus[i])
    y = y$tonnes[1]
    #print(sau_countries[j])
    #print(genus_categories[i])
    #print(y)
    x = x %>% 
      mutate(pred_exp_catch = prop_catch*y)
    x$pred_exp_catch[is.na(x$pred_exp_catch)]=0
    x = x %>% 
      mutate(pred_consumed_catch = tonnes - pred_exp_catch)
    if(j==1&i==1){
      total_consump = x}else{
        total_consump = rbind(total_consump, x)}
  }
}

##############Set negatives to zero#############
total_consump = total_consump %>% 
  mutate(pred_consumption = if_else(pred_consumed_catch<0,0,pred_consumed_catch),
         negative_values = if_else(pred_consumed_catch<0,pred_consumed_catch,0))

total_consumption = total_consump %>% 
  dplyr::select(country, iso3c, year, genus_cat, scientific_name, common_name, production_sector, pred_consumption)

##Add recreational and subsistance
consumption_catch <- read_csv("Outputs/consumption_catch_2019.csv")

total_consumption = rbind(total_consumption, consumption_catch)

total_consumption = total_consumption %>% 
  dplyr::select(iso3c, year, genus_cat, common_name, scientific_name, production_sector, pred_consumption)

#########Add imported fish#############

##Calculate predicted imoprt consumption by spp
FAO_import_consump = FAO_import %>%
  mutate(production_sector = "Import",
         common_name = NA,
         scientific_name = NA) %>%
  rename("pred_consumption" = "tonnes") %>% 
  dplyr::select(iso3c, year, genus_cat, common_name, scientific_name, production_sector, pred_consumption)

total_consumption = rbind(total_consumption, FAO_import_consump)

write.csv(total_consumption, "Outputs/total_consumption_2019.csv", row.names = F)

