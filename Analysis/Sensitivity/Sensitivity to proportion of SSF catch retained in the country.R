####Sensitivity to proportion retained in country

library(tidyverse)

# Clear workspace
rm(list = ls())

##Read data

##############Clean FAO data
##########Aquaculture data####################

##Read and clean FAO production data
#FAO_prod = read_csv("C:/Users/Daniel/Google Drive/MPAs and Human Nutrition/Data/FAO/FAO_global_production.csv")

FAO_AQUAC_2021 <- read_csv("data/FAO_AQUAC_2021.csv")

FAO_prod = reshape2::melt(FAO_AQUAC_2021, id.vars = names(FAO_AQUAC_2021)[1:13], 
                          measure.vars = names(FAO_AQUAC_2021)[20:ncol(FAO_AQUAC_2021)])

FAO_prod = FAO_prod %>% 
  dplyr::rename(year = variable, tonnes = value) %>% 
  drop_na(tonnes) %>% 
  filter(tonnes>0)

##Create Genus category
FAO_prod = FAO_prod %>%
  mutate(genus_cat = case_when(#Cephalopods
    FAOSTAT_group=="Cephalopods" ~ "Cephalopods",
    #Demersal fish
    FAOSTAT_group=="Demersal Marine Fish" ~ "Demersal Fish",
    #Pelagic fish
    FAOSTAT_group=="Pelagic Marine Fish" ~ "Pelagic Fish",
    #Crustaceans
    FAOSTAT_group=="Crustaceans" ~ "Crustaceans",
    #Marine Fish; Other
    FAOSTAT_group=="Marine Fish NEI" ~ "Marine Fish; Other",
    #Moluscs; Other
    FAOSTAT_group=="Molluscs excl. Cephalopods" ~ "Moluscs; Other",
    #Freshwater
    FAOSTAT_group=="Freshwater and Diadromous Fish" ~ "Freshwater Fish",
    #Aquatic Plants
    FAOSTAT_group=="Aquatic Plants" ~ "Aquatic Plants",
    #Other aquatic animals (Aquatic Animals; Others)
    FAOSTAT_group=="Aquatic Animals NEI" ~ "Aquatic Animals; Others"))


##Remove freshwater species
FAO_prod_MAR = FAO_prod %>% 
  filter(environment=="Marine")

FAO_prod_MAR$year = as.character(FAO_prod_MAR$year)
FAO_prod_MAR$year = as.numeric(FAO_prod_MAR$year)

FAO_aquac = FAO_prod_MAR %>% 
  filter(year==2019) %>% 
  mutate(Production_source="Aquaculture production")

##Read FAO import/export data
#FAO_trade = read_csv("C:/Users/Daniel/Google Drive/MPAs and Human Nutrition/Data/FAO/FAO_commodities_quantity.csv")

FAO_trade <- read_csv("data/FAO_trade_2020.csv")

FAO_trade = reshape2::melt(FAO_trade, id.vars = names(FAO_trade)[1:10], 
                           measure.vars = names(FAO_trade)[11:ncol(FAO_trade)])

FAO_trade = FAO_trade %>% 
  dplyr::rename(year = variable, tonnes = value) %>% 
  drop_na(tonnes) %>%
  filter(year=="2019") %>% 
  separate(FAOSTAT_group, c("FAOSTAT_group_clean", "processing"), ",", remove=FALSE) %>% 
  mutate(genus_cat = case_when(#Cephalopods
    FAOSTAT_group_clean=="Cephalopods" ~ "Cephalopods",
    #Demersal fish
    FAOSTAT_group_clean=="Demersal fish" ~ "Demersal Fish",
    FAOSTAT_group_clean=="Demersal" ~ "Demersal Fish",
    FAOSTAT_group_clean=="Demersal frozen" ~ "Demersal Fish",
    #Pelagic fish
    FAOSTAT_group_clean=="Pelagic fish" ~ "Pelagic Fish",
    FAOSTAT_group_clean=="Pelagic" ~ "Pelagic Fish",
    #Crustaceans
    FAOSTAT_group_clean=="Crustaceans" ~ "Crustaceans",
    #Marine Fish; Other
    FAOSTAT_group_clean=="Marine fish nei" ~ "Marine Fish; Other",
    #Moluscs; Other
    FAOSTAT_group_clean=="Molluscs excl. cephalopods" ~ "Moluscs; Other",
    FAOSTAT_group_clean=="Molluscs excl. ceph." ~ "Moluscs; Other",
    #Freshwater
    FAOSTAT_group_clean=="Freshwater & diadromous fish" ~ "Freshwater Fish",
    FAOSTAT_group_clean=="Freshwater & diadromous" ~ "Freshwater Fish",
    #Other aquatic animals (Aquatic Animals; Others)
    FAOSTAT_group_clean=="Aquatic animals nei" ~ "Aquatic Animals; Others")) %>% 
  drop_na(genus_cat) %>% 
  filter(tonnes>0)

#Convert to live weight
FAO_trade_fillets = FAO_trade %>% 
  filter(str_detect(common_name, fixed('fillet', ignore_case=TRUE))) %>% 
  mutate(tonnes = tonnes*2)

FAO_trade_whole = FAO_trade %>% 
  filter(!str_detect(common_name, fixed('fillet', ignore_case=TRUE)))


FAO_trade = rbind(FAO_trade_fillets, FAO_trade_whole)

####Calculate freshwater species in each category
FAO_freshwater = FAO_trade %>% 
  group_by(iso3c, genus_cat, ISSCAAP_group, Trade_flow) %>% 
  summarise(tonnes = sum(tonnes))

###########Other calculations
FAO_reexports = FAO_trade %>% 
  group_by(country, iso3c, genus_cat, Trade_flow) %>% 
  summarise(tonnes = sum(tonnes)) %>% 
  filter(!Trade_flow == "Processed production") %>% 
  spread(Trade_flow, tonnes) %>% 
  mutate(Reexports = replace_na(Reexports, 0),
         perc_imp_reexport = 100*Reexports/Imports,
         perc_exports_reexport = 100*Reexports/Exports) %>% 
  arrange(desc(perc_exports_reexport))

write.csv(FAO_reexports, "Outputs/FAO_reexports.csv", row.names = F)

##Freshwater crustaceans
FAO_crustaceans_total = FAO_trade %>% 
  filter(genus_cat == "Crustaceans",
         Trade_flow == "Exports") %>% 
  group_by(country, iso3c) %>% 
  summarise(tonnes = sum(tonnes))

FAO_crustaceans_freshwater = FAO_trade %>% 
  filter(genus_cat == "Crustaceans",
         Trade_flow == "Exports",
         ISSCAAP_group == "Freshwater crustaceans") %>% 
  group_by(country, iso3c) %>% 
  summarise(tonnes_freshwater = sum(tonnes)) %>% 
  left_join(FAO_crustaceans_total) %>% 
  mutate(prop.exports.freshwater = 100*tonnes_freshwater/tonnes) %>% 
  arrange(desc(prop.exports.freshwater))

##############################SAU data
##Read SAU data
#SAU = read_csv("C:/Users/Daniel/Google Drive/MPAs and Human Nutrition/Data/SAU data/complete data/SAU raw database by EEZ 2014.csv")

SAU_2019 <- read_csv("data/ChrisGolden_202212151042.csv") 

countries_ISO <- read_csv("data/countries_ISO.csv")

SAU = left_join(SAU_2019, countries_ISO, by=c("fishing_entity"="missing_countries")) %>% 
  drop_na(iso3c)

SAU = SAU %>%
  mutate(genus_cat = case_when(
    #Cephalopods
    functional_group=="cephalopods" ~ "Cephalopods",
    #Demersal fish
    functional_group=="demersalsm" ~ "Demersal Fish",
    functional_group=="demersalmd" ~ "Demersal Fish",
    functional_group=="demersallg" ~ "Demersal Fish", 
    functional_group=="bathydemersalsm" ~ "Demersal Fish",
    functional_group=="bathydemersalmd" ~ "Demersal Fish",
    functional_group=="bathydemersallg" ~ "Demersal Fish",
    functional_group=="flatfishsm md" ~ "Demersal Fish",
    functional_group=="flatfishlg" ~ "Demersal Fish",
    #Pelagic fish
    functional_group=="pelagicsm" ~ "Pelagic Fish",
    functional_group=="pelagicmd" ~ "Pelagic Fish",
    functional_group=="pelagiclg" ~ "Pelagic Fish", 
    functional_group=="benthopelagicsm" ~ "Pelagic Fish",
    functional_group=="benthopelagicmd" ~ "Pelagic Fish",
    functional_group=="benthopelagiclg" ~ "Pelagic Fish",
    functional_group=="bathypelagicsm" ~ "Pelagic Fish",
    functional_group=="bathypelagicmd" ~ "Pelagic Fish",
    functional_group=="bathypelagiclg" ~ "Pelagic Fish",
    #Crustaceans
    functional_group=="shrimp" ~ "Crustaceans",
    functional_group=="lobsters,crab" ~ "Crustaceans",
    functional_group=="krill" ~ "Crustaceans",
    #Reef fish
    functional_group=="reef-associatedsm" ~ "Demersal Fish",
    functional_group=="reef-associatedmd" ~ "Demersal Fish",
    functional_group=="reef-associatedlg" ~ "Demersal Fish", 
    #Sharks and rays
    functional_group=="raysm md" ~ "Demersal Fish",
    functional_group=="raylg" ~ "Demersal Fish",
    functional_group=="sharksm-md" ~ "Demersal Fish",
    functional_group=="sharklg" ~ "Pelagic Fish",
    #Moluscs; Other
    functional_group=="otherdeminvert" ~ "Moluscs; Other",
    functional_group=="jellyfish" ~ "Aquatic Animals; Others"))

##Set Unidentified fish as Marine fish; Other
SAU = SAU %>%
  mutate(genus_cat = if_else(
    scientific_name=="Marine fishes not identified", "Marine fish; Other", genus_cat))


####################Merge SAU and aquaculture data###########
commercial_catch = SAU %>% 
  filter(sector_type %in% c("Industrial"),
         !end_use_name %in% c("Fishmeal and fish oil", "Other"))

commercial_catch = commercial_catch %>%
  rename(country = fishing_entity,
         fishing_sector = sector_type,
         tonnes = catch) %>% 
  dplyr::select(country, iso3c, year, genus_cat, scientific_name, common_name, fishing_sector, 
                end_use_name, tonnes) %>%
  rename(production_sector = fishing_sector)

commercial_catch_agg = commercial_catch %>%
  group_by(country, iso3c, year, genus_cat, scientific_name,
           common_name, production_sector) %>%
  summarize(tonnes = sum(tonnes)) %>% 
  ungroup()

commercial_catch_agg = commercial_catch_agg %>% 
  dplyr::select(country, iso3c, year, genus_cat, scientific_name, 
                common_name, production_sector, tonnes)

FAO_aquac = FAO_aquac %>%
  mutate(production_sector = "aquaculture") %>% 
  dplyr::select(country, iso3c, year, genus_cat, scientific_name, common_name, production_sector, 
                tonnes)

FAO_aquac$tonnes = as.numeric(FAO_aquac$tonnes)
FAO_aquac_agg = FAO_aquac %>% 
  group_by(country, iso3c, year, genus_cat, scientific_name, 
           common_name, production_sector) %>% 
  summarize(tonnes = sum(tonnes)) %>% 
  ungroup()

total_commercial_prod = rbind(commercial_catch_agg, FAO_aquac_agg)

total_commercial_prod = as.data.frame(total_commercial_prod)

##Recreational and subsistance catch
consumption_catch = SAU %>% 
  filter(!sector_type %in% c("Industrial"),
         !end_use_name %in% c("Fishmeal and fish oil", "Other")) %>% 
  rename(country = fishing_entity,
         pred_consumption = catch,
         production_sector = sector_type) %>% 
  dplyr::select(country, iso3c, year, genus_cat, scientific_name, common_name, production_sector, pred_consumption)

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
#total_commercial_prod <- read_csv("Outputs/total_commercial_prod.csv")

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

#Read nutrient information
spp_category <- read_csv("Outputs/spp_nutrient_final.csv") %>% 
  mutate(genus_cat = recode(genus_cat, "Marine Fish; Other" = "Marine fish; Other")) %>% 
  group_by(genus_cat, nutrient) %>% 
  summarise(value = median(value, na.rm = T))

spp_nutrients_SAU <- read_csv("Outputs/spp_nutrient_final.csv") %>% 
  rename(scientific_name = species) %>% 
  dplyr::select(scientific_name, nutrient, value)

edible_portion <- read_csv("Outputs/edible_portion.csv")

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

total_nut_supply = consumption_nutrition %>% 
  group_by(iso3c, nutrient, production_sector) %>% 
  summarize(nut_supply = sum(nut_supply, na.rm = T)) %>% 
  ungroup() %>% 
  mutate(nutrient = recode(nutrient, "DHA+EPA" = "Omega-3 fatty acids"))

##Read population information

coastal_pop <- read_csv("data/coastal_population_all_V2.csv") %>% 
  select(iso3c, km_20) %>% 
  rename(pop = km_20)

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

write.csv(nutrient_supply_final_v2, "Outputs/percent_nutrient_supply_coastal_allretained.csv", row.names=FALSE)

