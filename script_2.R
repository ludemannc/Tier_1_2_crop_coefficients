#Description of script: ---
#This script creates crop nutrient coefficients (for harvest index and N,P and K nutrient concentrations of crop products and crop residues).
#Then a sub-selection of these data (nutrient concentrations of crop products) are turned into a csv file format for the UN FAO Soil Nutrient Budget.

#This script was written in R version 4.1.0 with a 64-bit computer.

#Libraries----
library(readr)
library(tidyverse)
library(reshape2)
library(countrycode)

#Read files----
df <- as.data.frame(read_csv("data/standardised/Combined_crop_data.csv"))
df_countries <- as.data.frame(read_csv("data/standardised/Transposed_UN_country_codes_summary.csv"))
df_countries <- rbind(colnames(df_countries), df_countries) #Ensure we have a row with list of all countries in 'World' original region. Only do this if you want means of world Tier 1 and region Tier 2 values. 


#Avoid double ups to use of same primary sources of data.----
#The FAO_2020 data uses IPNI data so to avoid double ups I will include the FAO_2020 data and will exclude any other data that comes from IPNI.
df_FAO_2020 <- filter(df, #Create FAO_2020 datafame with all FAO data so we can delete all data that refers to IPNI in a second data frame and column bind them together. This gets around the issue of how to filter out IPNI data from other sources.
                      reference_where_data_were_collated==
                        "FAO(2020) FAOSTAT Domain Soil Nutrient Budget Metadata, release December 2020 https://fenixservices.fao.org/faostat/static/documents/ESB/ESB_e.pdf") 

#Exclude all FAO_2020 or IPNI data in the second dataframe----
df1 <- filter(df, 
              !reference_where_data_were_collated==
                "FAO(2020) FAOSTAT Domain Soil Nutrient Budget Metadata, release December 2020 https://fenixservices.fao.org/faostat/static/documents/ESB/ESB_e.pdf",#Exclude FAO_2020 data to avoid double ups when I rbind them later on.
              !primary_reference_of_dataset == #Exclude IPNI as primary reference as IFA_2020 uses IPNI values too.
                "IPNI + Comifer",
              !primary_reference_of_dataset ==#Exclude IPNI as primary reference as IFA_2020 uses IPNI values too.
                "https://www.ipni.net/app/calculator",
              !primary_reference_of_dataset ==#Exclude IPNI as primary reference as IFA_2020 uses IPNI values too.
                "IPNI",
              !primary_reference_of_dataset==
                "IPNI (2014) IPNI Estimates of Nutrient Uptake and Removal, available at: http://www.ipni.net/ipniweb/portal.nsf/0/CBDC9962624CDFCD85257AC60050BBD2/$FILE/Metric%204_1%20&%204_5%200115.pdf")

#Now row bind them together so we excluded most double ups in referrals to same primary sources of data----
df1 <- rbind(df_FAO_2020, 
             df1)

#Create data frame with mean dry matter percentages for each item-crop_component combination. (This allows us to later use these to fill in gaps of nutrient concentrations on a fresh weight or dry matter basis)
df_DM <- filter(df1, 
                variable =="DM_pc")

df_DM_means <- df_DM %>% 
  group_by(item, item_code, item_group_code, crop_component) %>% 
  select(-factor,-cpc_code,-latitude,-longitude)%>% 
  summarise(across(where(is.numeric),mean,na.rm=TRUE)) %>% 
  rename("DM_pc_mean"=value)

#Filter by variables of interest (harvest index and nutrient concentrations per unit of fresh or dry matter crop product).
df2 <- df1 %>% filter(variable %in% c("HI",
                                      "DM_pc",
                                      "N_pc_fresh",
                                      "P_pc_fresh",
                                      "K_pc_fresh",
                                      "S_pc_fresh",
                                      "N_pc",
                                      "P_pc",
                                      "K_pc",
                                      "S_pc"))

#Summarise variables to the original_regions and pivot wider
df2 <- df2 %>% 
  group_by(item, item_code, item_group_code, original_region,crop_component, variable) %>% 
  select(-factor,-cpc_code,-latitude,-longitude)%>% 
  summarise(across(where(is.numeric),mean,na.rm=TRUE)) %>% 
  pivot_wider(names_from =variable, values_from=value)

#Merge mean dry matter percentage values (across all data for each crop component and original_region) to main data frame
df2 <- merge(df2, df_DM_means,by.x=c("item","item_code","item_group_code","crop_component"), by.y=c("item","item_code","item_group_code","crop_component"), all.x = TRUE)

#Coalesce the DM_pc and DM_pc_mean columns into one putting priority on original DM_pc value from original_region and if that is not available fill in with DM_pc_mean values from across all data for that item,
#and de-select DM_pc_mean column as it is now extraneous.
df2$DM_pc <- coalesce(df2$DM_pc, df2$DM_pc_mean) 
df2 <- df2%>% select(-DM_pc_mean)

#Fill in gaps in dry matter nutrient concentrations with fresh weight nutrient concentrations interpolated with mean DM%'s and vice versa-----
# Then coalesce columns to get only one set of fresh weight and one set of dry matter nutrient concentration values for each item, original_region and crop component.
df_regions <- mutate(df2, 
                     N_pc_1= N_pc_fresh*(100/DM_pc),
                     P_pc_1= P_pc_fresh*(100/DM_pc),
                     K_pc_1= K_pc_fresh*(100/DM_pc),
                     S_pc_1= S_pc_fresh*(100/DM_pc),
                     N_pc_fresh_1= N_pc*(DM_pc/100),
                     P_pc_fresh_1= P_pc*(DM_pc/100),
                     K_pc_fresh_1= K_pc*(DM_pc/100),
                     S_pc_fresh_1= S_pc*(DM_pc/100),
                     Test = N_pc,
                     N_pc_coalesced=coalesce(N_pc,N_pc_1),
                     N_pc_fresh_coalesced=coalesce(N_pc_fresh,N_pc_fresh_1),
                     P_pc_coalesced=coalesce(P_pc,P_pc_1),
                     P_pc_fresh_coalesced=coalesce(P_pc_fresh,P_pc_fresh_1),
                     K_pc_coalesced=coalesce(K_pc,K_pc_1),
                     K_pc_fresh_coalesced=coalesce(K_pc_fresh,K_pc_fresh_1),
                     S_pc_coalesced=coalesce(S_pc,S_pc_1),
                     S_pc_fresh_coalesced=coalesce(S_pc_fresh,S_pc_fresh_1),
                     N_pc=N_pc_coalesced,                         #If you wish to not use coalesced data then # out these lines.
                     N_pc_fresh=N_pc_fresh_coalesced,             #If you wish to not use coalesced data then # out these lines.
                     P_pc=P_pc_coalesced,                         #If you wish to not use coalesced data then # out these lines.
                     P_pc_fresh=P_pc_fresh_coalesced,             #If you wish to not use coalesced data then # out these lines.
                     K_pc=K_pc_coalesced,                         #If you wish to not use coalesced data then # out these lines.
                     K_pc_fresh=K_pc_fresh_coalesced,             #If you wish to not use coalesced data then # out these lines.
                     S_pc=S_pc_coalesced,                         #If you wish to not use coalesced data then # out these lines.
                     S_pc_fresh=S_pc_fresh_coalesced             #If you wish to not use coalesced data then # out these lines.
) 

df_regions <- df_regions %>%  select(item, item_code,item_group_code,original_region,crop_component,DM_pc,HI,
                                     N_pc,N_pc_1,N_pc_fresh,N_pc_fresh_1,N_pc_coalesced,N_pc_fresh_coalesced,
                                     P_pc,P_pc_1,P_pc_fresh,P_pc_fresh_1,P_pc_coalesced,P_pc_fresh_coalesced,
                                     K_pc,K_pc_1,K_pc_fresh,K_pc_fresh_1,K_pc_coalesced,K_pc_fresh_coalesced,
                                     S_pc,S_pc_1,S_pc_fresh,S_pc_fresh_1,S_pc_coalesced,S_pc_fresh_coalesced) #Keep this as separate data frame in case you want to check the coalesced values next to each other in columns.
#Create simplified data frame----
df_regions_1 <- select(df_regions, 
                       item, item_code, item_group_code, original_region, crop_component,
                       DM_pc,HI,N_pc,P_pc, K_pc,S_pc,
                       N_pc_fresh,P_pc_fresh, K_pc_fresh,S_pc_fresh)

#Convert spaces into "_"'s in original_region column so they match well with df_countries data frame. 
df_regions_1$original_region <- gsub(" ", "_", df_regions_1$original_region)

#Create a data frame with original_regions in one column with associated (melted) country names for each original_region----
df_countries <- melt(df_countries,id="World")
df_countries <- na.omit(df_countries[]) %>% select(-variable) %>% rename(original_region=World, country=value) #Exclude NAs and rename columns
df_regions_1 <- merge(df_regions_1, df_countries, by.x=c("original_region"), by.y=c("original_region"), all.x = T)

#Create other columns of interest
df_regions_1 <- df_regions_1 %>%   mutate("N_kg_per_t_fresh_wt"=N_pc_fresh*10,
                                          "P_kg_per_t_fresh_wt"=P_pc_fresh*10,
                                          "K_kg_per_t_fresh_wt"=K_pc_fresh*10,
                                          "S_kg_per_t_fresh_wt"=S_pc_fresh*10)

#Create Tier 1 estimates for each country based on values from 'World' original_region.----
df_world_coeffs <- filter(df_regions_1,original_region=="World" ) #Filter to only 'World' values

df_world_coeffs <- df_world_coeffs %>% 
  group_by(item, item_code, item_group_code, crop_component) %>% 
  summarise(across(where(is.numeric),mean,na.rm=TRUE)) 

#Create Tier 2 estimates for each country based on values from all original_regions except for the 'World'.----
df_country_coeffs <- filter(df_regions_1,!original_region=="World" ) #Filter to exclude 'World' values

df_country_coeffs <- df_country_coeffs %>% 
  group_by(item, item_code, item_group_code, crop_component, country) %>% 
  summarise(across(where(is.numeric),mean,na.rm=TRUE))

#Add country codes to reduce ambiguity in names.
df_country_coeffs$iso3_code <- countrycode(df_country_coeffs$country, origin = 'country.name', destination = 'iso3c')

#Reorder columns so all character columns are to the right.
df_country_coeffs <- df_country_coeffs %>%relocate(iso3_code, .before = DM_pc)

#Alter World (Tier 1) and Country (Tier 2) data frames so they can be combined into one. ----
#Add Tier columns to data frames
df_world_coeffs$tier <- 1
df_country_coeffs$tier <- 2

#Add region information
df_world_coeffs$region <- "World"
df_country_coeffs$region <- df_country_coeffs$country

#Add NAs to country and iso3 code columns for df_world_coeffs
df_world_coeffs$country <- NA
df_world_coeffs$iso3_code <- NA

#Rbind the data frames together. 
Tier_1_and_2_nutrient_coefficients <- rbind(df_world_coeffs, df_country_coeffs)

#Shift columns.
Tier_1_and_2_nutrient_coefficients <- Tier_1_and_2_nutrient_coefficients %>%relocate(region:iso3_code, .before = item)%>%
  relocate(tier, .before = region)

#Create meta-data frame for Tier 1 and 2 data frame. 
Meta_data_Tier_1_and_2_nutrient_coefficients <- as.data.frame(colnames(Tier_1_and_2_nutrient_coefficients))

Meta_data_Tier_1_and_2_nutrient_coefficients<- rename(Meta_data_Tier_1_and_2_nutrient_coefficients, "Parameter"="colnames(Tier_1_and_2_nutrient_coefficients)")

Meta_data_Tier_1_and_2_nutrient_coefficients$Description <- c("Tier of data, whereby Tier 1 indicates values to represent the world and Tier 2 indicated values to represent countries",
                                                              "Region values represent",
                                                              "Country name based on UN standard English categories",
                                                              "Three letter ISO3 UN code to signify country or region",
                                                              "Item (crop) name as per UN FAO standards",  
                                                              "Item (crop) code as per UN FAO standards",
                                                              "Item group (crop) code as per UN FAO standards",
                                                              "Crop component defined as either 'Crop_products' or 'Crop_residues'",
                                                              "Dry matter percentage of fresh weight",
                                                              "Harvest index",
                                                              "Nitrogen concentration on a dry matter basis",
                                                              "Elemental phosphorus concentration on a dry matter basis",
                                                              "Elemental potassium concentration on a dry matter basis",
                                                              "Elemental sulphur concentration on a dry matter basis",
                                                              "Nitrogen concentration on a fresh weight basis",
                                                              "Elemental phosphorus concentration on a fresh weight basis",
                                                              "Elemental potassium concentration on a fresh weight basis",
                                                              "Elemental sulphur concentration on a fresh weight basis",
                                                              "Nitrogen concentration on a fresh weight basis-per metric tonne",
                                                              "Elemental phosphorus concentration on a fresh weight basis-per metric tonne",
                                                              "Elemental potassium concentration on a fresh weight basis-per metric tonne",
                                                              "Elemental sulphur concentration on a fresh weight basis-per metric tonne")


Meta_data_Tier_1_and_2_nutrient_coefficients$Format <- c("Character",
                                                         "Character",
                                                         "Character",
                                                         "Character",
                                                         "Character",
                                                         "Numeric",
                                                         "Numeric",
                                                         "Character",
                                                         "Numeric",
                                                         "Numeric",
                                                         "Numeric",
                                                         "Numeric",
                                                         "Numeric",
                                                         "Numeric",
                                                         "Numeric",
                                                         "Numeric",
                                                         "Numeric",
                                                         "Numeric",
                                                         "Numeric",
                                                         "Numeric",
                                                         "Numeric",
                                                         "Numeric")

Meta_data_Tier_1_and_2_nutrient_coefficients$Units <- c("Character",
                                                        "Character",
                                                        "Character",
                                                        "Character",
                                                        "Character",
                                                        "Numeric, integer",
                                                        "Numeric, integer",
                                                        "Character",
                                                        "Dry matter as a percentage of fresh weight",
                                                        "Harvest index as proportion of above ground biomass as crop products",
                                                        "Nitrogen as a percentage of dry matter",
                                                        "Elemental phosphorus as a percentage of dry matter",
                                                        "Elemental potassium as a percentage of dry matter",
                                                        "Elemental sulphur as a percentage of dry matter",
                                                        "Nitrogen as a percentage of fresh weight",
                                                        "Elemental phosphorus as a percentage of fresh weight",
                                                        "Elemental potassium as a percentage of fresh weight",
                                                        "Elemental sulphur as a percentage of fresh weight",
                                                        "Nitrogen in kilograms per metric tonne of fresh weight",
                                                        "Elemental phosphorus in kilograms per metric tonne of fresh weight",
                                                        "Elemental potassium in kilograms per metric tonne of fresh weight",
                                                        "Elemental sulphur in kilograms per metric tonne of fresh weight")




#Create data file for FAO as a sub-selection of Tier 1 data for nutrient concentrations of crop products for N, P and K.----
#Filter to 1714 item_group_code items and to 'World' (Tier 1) values for 'Crop_products'. Select only columns of interest to FAO for Soil Nutrient Budget.
World_nutrient_coefficients_for_UN_FAO <- Tier_1_and_2_nutrient_coefficients %>% 
  filter(tier==1, item_group_code==1714, crop_component=="Crop_products") %>% 
  select(item, item_code, item_group_code, N_kg_per_t_fresh_wt, P_kg_per_t_fresh_wt,K_kg_per_t_fresh_wt,DM_pc,N_pc, P_pc, K_pc)

#Create meta-data file for World_nutrient_coefficients_for_UN_FAO
Meta_data_World_nutrient_coefficients_for_UN_FAO <- as.data.frame(colnames(World_nutrient_coefficients_for_UN_FAO))

Meta_data_World_nutrient_coefficients_for_UN_FAO<- rename(Meta_data_World_nutrient_coefficients_for_UN_FAO, "Parameter"="colnames(World_nutrient_coefficients_for_UN_FAO)")

Meta_data_World_nutrient_coefficients_for_UN_FAO$Description <- c("Item (crop) name as per UN FAO standards",  
                                                                  "Item (crop) code as per UN FAO standards",
                                                                  "Item group (crop) code as per UN FAO standards",
                                                                  "Nitrogen concentration on a fresh weight basis",
                                                                  "Elemental phosphorus concentration on a fresh weight basis",
                                                                  "Elemental potassium concentration on a fresh weight basis",
                                                                  "Dry matter percentage of fresh weight",
                                                                  "Nitrogen concentration on a dry matter basis",
                                                                  "Elemental phosphorus concentration on a dry matter basis",
                                                                  "Elemental potassium concentration on a dry matter basis")

Meta_data_World_nutrient_coefficients_for_UN_FAO$Format <- c("Character",
                                                             "Numeric",
                                                             "Numeric",
                                                             "Numeric",
                                                             "Numeric",
                                                             "Numeric",
                                                             "Numeric",
                                                             "Numeric",
                                                             "Numeric",
                                                             "Numeric")

Meta_data_World_nutrient_coefficients_for_UN_FAO$Units <- c("Character",
                                                            "Numeric, integer",
                                                            "Numeric, integer",                                                        
                                                            "Nitrogen (in kilograms) per metric tonne of fresh weight of crop product",
                                                            "Elemental phosphorus (in kilograms) per metric tonne of fresh weight of crop product",
                                                            "Elemental potassium (in kilograms) per metric tonne of fresh weight of crop product",
                                                            "Dry matter as a percentage of fresh weight of crop product",
                                                            "Nitrogen as a percentage of dry matter of crop product",
                                                            "Elemental phosphorus as a percentage of dry matter of crop product",
                                                            "Elemental potassium as a percentage of dry matter of crop product")

#Save file as csv file.----
write.csv(Tier_1_and_2_nutrient_coefficients,"data/standardised/Tier_1_and_2_crop_coefficients.csv", row.names = FALSE)
write.csv(Meta_data_Tier_1_and_2_nutrient_coefficients, "data/standardised/Meta_data_Tier_1_and_2_crop_coefficients.csv")
write.csv(World_nutrient_coefficients_for_UN_FAO,"data/standardised/World_crop_coefficients_for_UN_FAO.csv", row.names = FALSE)
write.csv(Meta_data_World_nutrient_coefficients_for_UN_FAO, "data/standardised/Meta_data_World_crop_coefficients_for_UN_FAO.csv")