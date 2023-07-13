##Step 1 - Clean FAO and SAU data

library(tidyverse)

# Clear workspace
rm(list = ls())

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

write.csv(FAO_crustaceans_freshwater, "Outputs/FAO_crustaceans_freshwater.csv", row.names = F)

##############################SAU data
##Read SAU data
#SAU = read_csv("C:/Users/Daniel/Google Drive/MPAs and Human Nutrition/Data/SAU data/complete data/SAU raw database by EEZ 2014.csv")

SAU_2019 <- read_csv("data/SAU_2023.csv") 

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
  filter(sector_type %in% c("Industrial", "Artisanal"),
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

###Proportion of artisanal catch retained in the country
#If 0 retained than entire catch is part of the international trade
#prop.retained = 0.5

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

write.csv(total_commercial_prod, "Outputs/total_commercial_prod_2019.csv", row.names = F)

##Recreational and subsistance catch
consumption_catch = SAU %>% 
  filter(!sector_type %in% c("Industrial", "Artisanal"),
         !end_use_name %in% c("Fishmeal and fish oil", "Other")) %>% 
  rename(country = fishing_entity,
         pred_consumption = catch,
         production_sector = sector_type) %>% 
  dplyr::select(country, iso3c, year, genus_cat, scientific_name, common_name, production_sector, pred_consumption)

write.csv(consumption_catch, "Outputs/consumption_catch_2019.csv", row.names = F)
