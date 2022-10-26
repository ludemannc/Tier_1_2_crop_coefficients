#Description of script: ---
##This script standardises and combines data collated from various sources in the literature for crop nutrient concentration,
#harvest index, and crop residue concentration. It combines it into one file and then that file is used to perform analysis
# with FAO crop production data to estimate total nutrient removal at the global and country level. 
    
#This script was written in R version 4.1.0 with a 64-bit computer.
getwd()
#Libraries----
library(readr)
library(dplyr)
library(reshape2) #melt function
library(stringr)
library(stringr)
library(countrycode)

#Settings----
#Ensure all values are decimals rather than scientific notation. See: https://stackoverflow.com/questions/44725001/convert-scientific-notation-to-numeric-preserving-decimals/44725320
options(scipen = 999)

#Factors for converting P2O5 to elemental P and K2O to elemental K. 
P2O5_to_P <- 0.44 # Based on: https://www.yara.co.uk/crop-nutrition/farmers-toolbox/conversion-calculator/
K2O_to_K <-  0.83  #Based on: https://www.yara.co.uk/crop-nutrition/farmers-toolbox/conversion-calculator/

#Read files----#Files are named by last name of first author and date of publication. If the data relate to anything other than just nutrient concentrations of crops then
#this is included in the name (eg manure or animal feed)
Bouwman_2017 <- read_csv("data/raw/Bouwman_2017_SI_Table_S12_S14_13072022.csv")
Conant_2013 <- read_delim("data/raw/Conant_et_al_2013Supp_Tble_2_23062020.csv", 
           delim = ";", escape_double = FALSE, trim_ws = TRUE) #This was delimited by ":" so had to be imported this way.
EU_N_Panel_2016_N_animal_feed<- read_csv("data/raw/EU_N_Panel_2016_Annex_1_A1_N_animal_feed.csv")
EU_N_Panel_2016 <- read_csv("data/raw/EU_N_Panel_2016_Annex_1_A3_Crop_N_P_content.csv")
FAO_2004 <- read_csv("data/raw/FAO_2004_table_15_12072022.csv")
IFA_2020 <- read_csv("data/raw/IFA_2020_NUE_nutrient_removal_13072022.csv")
IPCC_2018 <- read_csv("data/raw/IPCC_2019_Chpt11_Table11.1A_11072022.csv")
IPNI_2012 <- read_csv("data/raw/IPNI_2012_Metric_nutrient_uptake_removal_tables_12072022.csv")
Koopmans_1998 <- read_csv("data/raw/Koopmans&Koppejan_1998_Annex_1.csv")
Lassaletta_2014 <- read_csv("data/raw/Lassaletta_et_al_2014Supp_Mat_1_24062020A.csv")
Zhang_2021 <- read_csv("data/raw/Zhang_2021_source_data_fig_5_N_P_detailed_crops_16082022.csv")
Unkovich_2010 <- read_csv("data/raw/Unkovich_2010_Adv_in_Agron.csv")
USDA_1992 <- as.data.frame(read_csv("data/raw/USDA_1992_Ag_waste_mgmt_field_handbook_chpt_6.csv"))
Panagos_2022 <- read_csv("data/raw/Panagos_2022_JofCP&FS.csv")
FAO_2020 <- read_csv("data/raw/FAO_2020_Nutrient_Removal_With_Fodder.csv")
Swain_2022 <- read.csv("data/raw/Swain_2022_crop_nutrient_data.csv")
Bessembinder_1995 <- read_csv("data/raw/Bessembinder_1995.csv") 
Angeleska_2022 <- as.data.frame(read_csv("data/raw/Angeleska_2022.csv"))
Nijhof_1987 <- read_csv("data/raw/Nijhof_1987_26082022.csv")
Roy_2016 <- read_csv("data/raw/Roy_2016.csv")
Norton_2011 <- read_csv("data/raw/Norton_2011.csv")
Kumssa_2022 <- read_csv("data/raw/Kumssa_2022.csv")
FAO_2022_item_codes <- as.data.frame(read_csv("data/raw/FAO_2022_item_codes.csv"))
##############################################################################################################################################################################

#Check for double ups in usage of data----
#EU_N_Panel_2016 uses the same data as from FAO_2004 except FAO_2004 includes more nutrients. Therefore do not use EU_N_Panel_2016 data. 
#IFA_2020 uses a lot of data from IPNI_2012. However, there are some differences (eg Wheat N concentration). So I will add both datasets for comparative purposes. 
#Lassaletta_2014 uses a lot of data from FAO 2011 which is presumably similar to FAO_2004. But I will keep both datasets for comparative purposes. 
#In summary we only need to exclude EU_N_Panel_2016 from further use in the aggregated dataset. 

#Convert dataframes into standardised format for row binding together later on.----
#Where possible convert nutrient concentration values to a percentage of dry matter basis and standardise column names and melt data. 
#Bouwman_2017----
Bouwman_2017$Original_region <- "World" #These data reflect world estimates.
Bouwman_2017$Original_crop <- Bouwman_2017$Crop
Bouwman_2017$Original_crop_component <- Bouwman_2017$Crop_component #Align column names
Bouwman_2017$N_pc <- Bouwman_2017$Crop_component_N_concentration_kg_N_kg_DM*100 #Align units for nutrient concentration
Bouwman_2017$P_pc <- Bouwman_2017$Crop_component_P_concentration_kg_elemental_P_kg_DM*100
Bouwman_2017$DM_pc <- Bouwman_2017$Crop_component_DM_concentration_kg_DM_kg_fresh_weight*100
Bouwman_2017$Primary_reference_of_dataset <- Bouwman_2017$Reference_where_data_were_collated

Bouwman_2017<- select(Bouwman_2017, Original_region, Reference_where_data_were_collated,
                       Website_of_source_of_collated_data,Primary_reference_of_dataset,
                      Original_crop, Original_crop_component,
                      N_pc, P_pc,DM_pc)


#Melt data
Bouwman_2017<- melt(Bouwman_2017, id=c("Original_region", "Reference_where_data_were_collated",
                                       "Website_of_source_of_collated_data","Primary_reference_of_dataset",
                                       "Original_crop",
                                       "Original_crop_component"))

#Conant_2013----
Conant_2013$Original_region <- Conant_2013$`World_region_Conant_(2013)`
Conant_2013$Original_crop <- Conant_2013$Crop
Conant_2013$Original_crop_component <- NA
Conant_2013$Reference_where_data_were_collated <- paste(Conant_2013$Author," ",Conant_2013$Pub_year)
Conant_2013$Website_of_source_of_collated_data <- Conant_2013$DOI
Conant_2013$Primary_reference_of_dataset <- Conant_2013$Reference_where_data_were_collated
Conant_2013$CR_removed_pc <- (1-Conant_2013$`Crop_residue_returned$Proportion`)*100

Conant_2013<- select(Conant_2013, Original_region, Reference_where_data_were_collated,
                      Website_of_source_of_collated_data,Primary_reference_of_dataset,
                     Original_crop, Original_crop_component,
                     CR_removed_pc)

Conant_2013<- melt(Conant_2013, id=c("Original_region", "Reference_where_data_were_collated",
                                       "Website_of_source_of_collated_data","Primary_reference_of_dataset",
                                     "Original_crop","Original_crop_component"))

#EU_N_Panel_2016_N_animal_feed----
EU_N_Panel_2016_N_animal_feed$Original_region <- EU_N_Panel_2016_N_animal_feed$Region
EU_N_Panel_2016_N_animal_feed$Material_N_pc_fresh <- EU_N_Panel_2016_N_animal_feed$Default_N_content_kg_N_Mg_fresh_weight/1000*100#Convert nutrie
EU_N_Panel_2016_N_animal_feed$Original_crop <- NA
EU_N_Panel_2016_N_animal_feed$Original_crop_component <- NA

EU_N_Panel_2016_N_animal_feed<- select(EU_N_Panel_2016_N_animal_feed, Original_region, Reference_where_data_were_collated,
                                Website_of_source_of_collated_data,Primary_reference_of_dataset,
                                Original_crop, Original_crop_component,Material,
                                Material_N_pc_fresh)

EU_N_Panel_2016_N_animal_feed<- melt(EU_N_Panel_2016_N_animal_feed, id=c("Original_region", "Reference_where_data_were_collated",
                                                           "Website_of_source_of_collated_data","Primary_reference_of_dataset",
                                                           "Original_crop","Original_crop_component", "Material"))

#Delete "Material_" from variable information.
EU_N_Panel_2016_N_animal_feed$variable <- gsub("Material_","",EU_N_Panel_2016_N_animal_feed$variable)

#FAO_2004----
FAO_2004$Reference_where_data_were_collated <- FAO_2004$Reference
FAO_2004$Website_of_source_of_collated_data <- "https://www.fao.org/3/y5749e/y5749e00.htm#Contents"
FAO_2004$Primary_reference_of_dataset <-FAO_2004$Reference_where_data_were_collated
FAO_2004$Original_crop <- FAO_2004$Crops
FAO_2004$CP_N_pc_fresh <- FAO_2004$Crop_product_concentration_N_kg_N_tonne/1000*100 #Convert to percentage
FAO_2004$CP_P_pc_fresh <- FAO_2004$Crop_product_elemental_P_concentration_kg_P_tonne/1000*100 #Convert to percentage
FAO_2004$CP_K_pc_fresh <- FAO_2004$Crop_product_elemental_K_concentration_kg_K_tonne/1000*100 #Convert to percentage

FAO_2004$CR_N_pc_fresh <- FAO_2004$Crop_residue_concentration_N_kg_N_tonne/1000*100 #Convert to percentage
FAO_2004$CR_P_pc_fresh <- FAO_2004$Crop_residue_elemental_P_concentration_kg_P_tonne/1000*100 #Convert to percentage
FAO_2004$CR_K_pc_fresh <- FAO_2004$Crop_residue_elemental_K_concentration_kg_K_tonne/1000*100 #Convert to percentage

Crop_residue_removal_Ghana_Kenya_Mali <- select(FAO_2004,Reference_where_data_were_collated,
                                                Website_of_source_of_collated_data,
                                                Primary_reference_of_dataset,
                                                Original_crop,
                                                Crop_residue_proportion_removed_pc_Ghana,
                                                Crop_residue_proportion_removed_pc_Kenya,
                                                Crop_residue_proportion_removed_pc_Mali)

Crop_residue_removal_Ghana_Kenya_Mali$Original_crop_component <- "Crop_residues"

#Melt so the data can be differentiated by region/country
Crop_residue_removal_Ghana_Kenya_Mali<- melt(Crop_residue_removal_Ghana_Kenya_Mali, 
                                             id=c("Reference_where_data_were_collated",
                                                  "Website_of_source_of_collated_data","Primary_reference_of_dataset",
                                                   "Original_crop","Original_crop_component"))

#Convert factors to characters for regular expression function.
Crop_residue_removal_Ghana_Kenya_Mali$variable <- as.character(Crop_residue_removal_Ghana_Kenya_Mali$variable)


#Assign categories to certain text. 
Crop_residue_removal_Ghana_Kenya_Mali<-Crop_residue_removal_Ghana_Kenya_Mali %>% mutate(Original_region=case_when(
                                                str_detect(variable,regex("Ghana", ignore_case=TRUE))~"Ghana",
                                                str_detect(variable,regex("Kenya", ignore_case=TRUE))~"Kenya",
                                                str_detect(variable,regex("Mali", ignore_case=TRUE))~"Mali",
                                                TRUE~variable))

Crop_residue_removal_Ghana_Kenya_Mali$CR_removed_pc <- Crop_residue_removal_Ghana_Kenya_Mali$value

#Select required columns
Crop_residue_removal_Ghana_Kenya_Mali <- select(Crop_residue_removal_Ghana_Kenya_Mali,
                                                Original_region, Reference_where_data_were_collated,
                                                Website_of_source_of_collated_data,Primary_reference_of_dataset,
                                                Original_crop,Original_crop_component,CR_removed_pc)


#Melt so the data can be differentiated by region/country
Crop_residue_removal_Ghana_Kenya_Mali<- melt(Crop_residue_removal_Ghana_Kenya_Mali, 
                                             id=c("Original_region",
                                                  "Reference_where_data_were_collated",
                                                  "Website_of_source_of_collated_data","Primary_reference_of_dataset",
                                                  "Original_crop","Original_crop_component"))

#Standardise the crop nutrient concentration data for FAO_2004.
FAO_2004$Original_region <- "World"

#Select columns of interest 
FAO_2004 <- select(FAO_2004, 
                   Original_region,
                   Reference_where_data_were_collated,
                   Website_of_source_of_collated_data,
                   Primary_reference_of_dataset,
                   Original_crop, 
                   CP_N_pc_fresh,
                   CP_P_pc_fresh,
                   CP_K_pc_fresh,
                   CR_N_pc_fresh,
                   CR_P_pc_fresh,
                   CR_K_pc_fresh)

#Add Original_crop_component information. 
#Melt data frame to enable use of regular expression function. 
FAO_2004<- melt(FAO_2004, 
               id=c("Original_region",
                    "Reference_where_data_were_collated",
                    "Website_of_source_of_collated_data","Primary_reference_of_dataset",
                    "Original_crop"))

#Convert variables to character for regular expression function
FAO_2004$variable <- as.character(FAO_2004$variable)

#Assign crop component categories to certain text. 
FAO_2004<-FAO_2004 %>% mutate(Original_crop_component=case_when(
  str_detect(variable,regex("CP_", ignore_case=TRUE))~"Crop_products",
  str_detect(variable,regex("CR_", ignore_case=TRUE))~"Crop_residues",
  TRUE~variable))

#Remove CP_ and CR_ information in variable column.
FAO_2004$variable <- gsub("CP_","",FAO_2004$variable)
FAO_2004$variable <- gsub("CR_","",FAO_2004$variable)

FAO_2004<- select(FAO_2004, Original_region, Reference_where_data_were_collated,
                      Website_of_source_of_collated_data,Primary_reference_of_dataset,
                      Original_crop, Original_crop_component,variable,value)

#IFA_2020----
#Delete extraneous percentage values in Primary_reference_of_dataset column.
IFA_2020$Primary_reference_of_dataset <- gsub(".*%","",IFA_2020$Primary_reference_of_dataset) # Remove all before and up to "%":

#Standardise units
IFA_2020 <- mutate(IFA_2020, 
                   "Original_crop"=Crop_FAO,
                   "Original_crop_component"="Crop_products",
                   "Fixed_N_kg_N_metric_t_fresh_crop_product"=as.numeric(`Biological_Fixation_kg_N _t`),
                   "N_pc_fresh"=as.numeric(Crop_product_N_concentration_kg_N_t)/1000*100,
                   "P_pc_fresh"=as.numeric(Crop_product_kg_P2O5_t)/1000*100*P2O5_to_P,
                   "K_pc_fresh"=as.numeric(Crop_product_kg_K2O_t)/1000*100*K2O_to_K,
                   "Recycled_N_kg_N_t_fresh_crop_product"=as.numeric(Recycled_N_kg_N_t),
                   "Recycled_P_kg_P_t_fresh_crop_product"=as.numeric(Recycled_P2O5_kg_P2O5_t)*P2O5_to_P,
                   "Recycled_K_kg_K_t_fresh_crop_product"=as.numeric(Recycled_K2O_kg_K2O_t)*K2O_to_K)

IFA_2020$Original_region <- "World" #Add Original_region information.

#Select columns of interest
IFA_2020 <- select(IFA_2020, 
                   Original_region, Reference_where_data_were_collated,
                   Website_of_source_of_collated_data,Primary_reference_of_dataset,
                   Original_crop, Original_crop_component,
                   N_pc_fresh,
                   P_pc_fresh,
                   K_pc_fresh,
                   Fixed_N_kg_N_metric_t_fresh_crop_product,
                   Recycled_N_kg_N_t_fresh_crop_product,
                   Recycled_P_kg_P_t_fresh_crop_product,
                   Recycled_K_kg_K_t_fresh_crop_product)

#Melt for desired format
IFA_2020<- melt(IFA_2020, 
                       id=c("Original_region",
                            "Reference_where_data_were_collated",
                            "Website_of_source_of_collated_data","Primary_reference_of_dataset",
                            "Original_crop","Original_crop_component"))

#IPCC_2018----
IPCC_2018$Original_crop <- IPCC_2018$Crop #Rename
IPCC_2018$N_pc <-IPCC_2018$NAGT_kg_N_per_kg_DM*100#Convert  N content of above-ground residues (NAGT_kg_N_per_kg_DM) to a percentage of dry matter basis. 
IPCC_2018$HI <- 1/(1+IPCC_2018$RAGT_Unitless) #Converted RAGT ratio to harvest index value. 
IPCC_2018$Original_region <- "World"
IPCC_2018$Reference_where_data_were_collated <- paste(IPCC_2018$Author_Last_name,"(",IPCC_2018$Pub_year_Years,")",IPCC_2018$Source_title_Various)
IPCC_2018$Website_of_source_of_collated_data <- IPCC_2018$DOI_DOI
IPCC_2018$Primary_reference_of_dataset <- "Various, from IPCC (2019) literature review"
IPCC_2018$DM_pc <- IPCC_2018$DRY_Proportion*100

#Select columns of interest
IPCC_2018 <- select(IPCC_2018, 
                   Original_region, Reference_where_data_were_collated,
                   Website_of_source_of_collated_data,Primary_reference_of_dataset,
                   Original_crop, N_pc,
                   HI, 
                   DM_pc)

#Melt for desired format
IPCC_2018<- melt(IPCC_2018, 
                 id=c("Original_region",
                      "Reference_where_data_were_collated",
                      "Website_of_source_of_collated_data","Primary_reference_of_dataset",
                      "Original_crop"))
IPCC_2018$variable <- as.character(IPCC_2018$variable) #Needs to be in character format for case_when function. 

#Assign crop component categories to certain text. 
IPCC_2018<-IPCC_2018 %>% mutate(Original_crop_component=case_when(
  str_detect(variable,regex("DM_pc", ignore_case=TRUE))~"Crop_products",
  str_detect(variable,regex("HI", ignore_case=TRUE))~"Crop_residues",
  str_detect(variable,regex("N_pc", ignore_case=TRUE))~"Crop_residues",  
  TRUE~variable))

#IPNI_2012----
#Note if nutrient concentration is a percentage of dry matter or fresh weight basis. 
IPNI_2012<-IPNI_2012 %>% mutate(DM_or_fresh_basis=case_when(
  str_detect(Crop,regex("(DM)", ignore_case=TRUE))~"",
  str_detect(Crop,regex("(dry)", ignore_case=TRUE))~"",
  TRUE~"_fresh"))

#Standardise units
IPNI_2012 <- mutate(IPNI_2012, 
                   "Original_crop"=Crop,
                   "Original_region"="World",
                   "Primary_reference_of_dataset"=Reference_where_data_were_collated,
                   "N_pc"=as.numeric(Nitrogen_concentration_kg_metric_tonne)/1000*100,
                   "P_pc"=as.numeric(P2O5_concentration_kg_metric_tonne)/1000*100*P2O5_to_P,
                   "K_pc"=as.numeric(K2O_concentration_kg_metric_tonne)/1000*100*K2O_to_K,
                   "S_pc"=as.numeric(S_concentration_kg_metric_tonne)/1000*100)

#Create column with Crop_products or Crop_residues information. 
IPNI_2012<-IPNI_2012 %>% mutate(Original_crop_component=case_when(
  str_detect(Crop,regex("straw|Stover", ignore_case=TRUE))~"Crop_residues",
  str_detect(Crop,regex("Potato above-ground stems & leaves", ignore_case=TRUE))~"Crop_residues",
  str_detect(Crop,regex("hay", ignore_case=TRUE))~"Crop_residues",
  TRUE~"Crop_products"))

#Select columns of interest
IPNI_2012 <- select(IPNI_2012, 
                    Original_region, Reference_where_data_were_collated,
                    Website_of_source_of_collated_data,Primary_reference_of_dataset,
                    Original_crop, Original_crop_component,
                    DM_or_fresh_basis, 
                    N_pc,
                    P_pc,
                    K_pc,
                    S_pc)

#Melt for rdesired format
IPNI_2012<- melt(IPNI_2012, 
                id=c("Original_region",
                     "Reference_where_data_were_collated",
                     "Website_of_source_of_collated_data","Primary_reference_of_dataset",
                     "Original_crop","Original_crop_component", "DM_or_fresh_basis"))

#Create fnal variable column information that accounts for whether the data are on a fresh weight basis. 
IPNI_2012$variable <- paste(IPNI_2012$variable, IPNI_2012$DM_or_fresh_basis)

#Delete extraneous DM_or_fresh_basis column
IPNI_2012 <- select(IPNI_2012, -DM_or_fresh_basis)

#Koopmans_1998----
#Create DM_pc column
Koopmans_1998$Original_region <- Koopmans_1998$Region
Koopmans_1998$Original_crop <- Koopmans_1998$Crop
Koopmans_1998$Original_crop_component <- Koopmans_1998$Component
Koopmans_1998$DM_pc <- 100-Koopmans_1998$Moisture_content_pc
Koopmans_1998$C_pc_fresh <- Koopmans_1998$Carbon_pc
Koopmans_1998$N_pc_fresh <- Koopmans_1998$Nitrogen_pc
Koopmans_1998$HI <- Koopmans_1998$Harvest_index

#Select columns of interest
Koopmans_1998 <- select(Koopmans_1998, 
                        Original_region, Reference_where_data_were_collated,
                        Website_of_source_of_collated_data,Primary_reference_of_dataset,
                        Original_crop, Original_crop_component,
                        DM_pc,C_pc_fresh,N_pc_fresh, HI)

#Separate Millet/rye/oats into separate categories with same values.
Koopmans_Millet_rye_oats <- filter(Koopmans_1998, Original_crop =="Millet/rye/oats")

#Anti_join Koopmans_Millet_rye_oats data from Koopmans_1998 so we can replace it later with the data differentiated by Millet, rye and oats.
Koopmans_1998 <- anti_join(Koopmans_1998, Koopmans_Millet_rye_oats)
                        
#Create dataframe with Millet, Rye and Oats differentiated. 
Koopmans_Millet <- Koopmans_Millet_rye_oats
Koopmans_Millet$Original_crop <- "Millet"

Koopmans_Rye <- Koopmans_Millet_rye_oats
Koopmans_Rye$Original_crop <- "Rye"

Koopmans_Oats <- Koopmans_Millet_rye_oats
Koopmans_Oats$Original_crop <- "Oats"

Koopmans_1998 <- rbind(Koopmans_1998, 
                       Koopmans_Millet,
                       Koopmans_Rye,
                       Koopmans_Oats)

#Melt for desired format
Koopmans_1998<- melt(Koopmans_1998, 
                 id=c("Original_region",
                      "Reference_where_data_were_collated",
                      "Website_of_source_of_collated_data","Primary_reference_of_dataset",
                      "Original_crop","Original_crop_component"))

#Lassaletta_2014----
#Add columns to align to those added to other data sets
Lassaletta_2014$Original_region <- "World"
Lassaletta_2014$Reference_where_data_were_collated <- "Lassaletta et al (2014) Biogeochemistry, supplementary material 1"
Lassaletta_2014$Website_of_source_of_collated_data <- "https://link.springer.com/article/10.1007/s10533-013-9923-4#Sec18"
Lassaletta_2014$Primary_reference_of_dataset <- Lassaletta_2014$Reference
Lassaletta_2014$Original_crop <- Lassaletta_2014$ITEM
Lassaletta_2014$Original_crop_component <- "Crop_products"
Lassaletta_2014$N_pc_fresh <- Lassaletta_2014$N_content_pc

#Select columns of interest
Lassaletta_2014 <- select(Lassaletta_2014, 
                        Original_region, Reference_where_data_were_collated,
                        Website_of_source_of_collated_data,Primary_reference_of_dataset,
                        Original_crop, Original_crop_component,
                        N_pc_fresh)

#Melt for desired format
Lassaletta_2014<- melt(Lassaletta_2014, 
                     id=c("Original_region",
                          "Reference_where_data_were_collated",
                          "Website_of_source_of_collated_data","Primary_reference_of_dataset",
                          "Original_crop","Original_crop_component"))

#Zhang_2021----
#Clean Original_crop and Original_crop_component information so there are no ' 's
Zhang_2021$Original_crop <- gsub("'","",Zhang_2021$Original_crop)
Zhang_2021$Original_crop_component <- gsub("'","",Zhang_2021$Original_crop_component)
Zhang_2021$Crop_aggregate_category <- gsub("'","",Zhang_2021$Crop_aggregate_category)
 
#Create variable column with N_pc instead of kg_nutrient_kg_DM and N_pc_fresh instead of kg_nutrient_kg_fresh for the different nutrients etc.
#Create column with unit and nutrient information
Zhang_2021$Nutrient_Unit <- paste(Zhang_2021$Nutrient, Zhang_2021$Unit)

Zhang_2021 <-Zhang_2021 %>% mutate(variable=case_when(
  str_detect(Nutrient_Unit,regex("N kg_nutrient_kg_fresh_weight", ignore_case=TRUE))~"N_pc_fresh",
  str_detect(Nutrient_Unit,regex("N kg_nutrient_kg_DM", ignore_case=TRUE))~"N_pc", 
  str_detect(Nutrient_Unit,regex("P kg_nutrient_kg_fresh_weight", ignore_case=TRUE))~"P_pc_fresh",
  str_detect(Nutrient_Unit,regex("P kg_nutrient_kg_DM", ignore_case=TRUE))~"P_pc",
  TRUE~Nutrient_Unit)) 

#Convert units from kg per kg of fresh weight or dry matter to a percentage.
Zhang_2021$value <- Zhang_2021$Crop_nutrient_concentration_kg_nutrient*100

#Select columns of interest for collation later on
Zhang_2021 <- select(Zhang_2021,
                     Original_region,Reference_where_data_were_collated,
                     Website_of_source_of_collated_data,Primary_reference_of_dataset,
                     Original_crop,Original_crop_component,variable,value)

#To avoid double ups in data, filter Zhang_2021 data to only Bodirsky (2012) and Feedipedia because Lassaletta et al (2014), IPNI (2014) and Bouwman 2017 have already been standardised from source reference,
Zhang_2021 <- Zhang_2021 %>% filter(Primary_reference_of_dataset %in% c("Feedipedia", "Bodirsky (2012)") )

#Unkovich_2010----
#Rename columns of interest
Unkovich_2010 <-  dplyr::rename(Unkovich_2010, Original_region= Region, 
                                Original_crop_component=Crop_component)

#Create variable column with N_pc, CHO_pc, Ash_pc etc instead of Percentage_dry_matter_as_nitrogen,Percentage_dry_matter_as_carbohydrates,Percentage_dry_matter_as_ash etc
Unkovich_2010 <-Unkovich_2010 %>% mutate(variable=case_when(
  str_detect(Variable,regex("Percentage_dry_matter_as_carbohydrates", ignore_case=TRUE))~"CHO_pc",
  str_detect(Variable,regex("Percentage_dry_matter_as_protein", ignore_case=TRUE))~"Protein_pc",
  str_detect(Variable,regex("Percentage_dry_matter_as_nitrogen", ignore_case=TRUE))~"N_pc",  
  str_detect(Variable,regex("Percentage_dry_matter_as_lipid", ignore_case=TRUE))~"Lipid_pc",
  str_detect(Variable,regex("Percentage_dry_matter_as_ash", ignore_case=TRUE))~"Ash_pc",
  str_detect(Variable,regex("Moisture_percentage_crop_products", ignore_case=TRUE))~"Water_pc", 
  str_detect(Variable,regex("Mean_Harvest_index|Mean_HI", ignore_case=TRUE))~"HI",  
  TRUE~Variable)) 

Unkovich_2010$Value <- as.numeric(Unkovich_2010$Value)

#Create DM_pc variable based on Water_pc 
#Create separate data frame and make changes
Unkovich_2010_DM_pc <- filter(Unkovich_2010,variable == "Water_pc" )
Unkovich_2010_DM_pc <-mutate(Unkovich_2010_DM_pc, 
                             Variable="Dry_matter_percentage_crop_products",
                             variable="DM_pc",
                             Value=100-Value)

#Row bind data frames together
Unkovich_2010 <- rbind(Unkovich_2010 , Unkovich_2010_DM_pc)

#Create value column
Unkovich_2010$value <- Unkovich_2010$Value

#Add an Original_crop_component column
Unkovich_2010$Original_crop_component <- "Crop_products"

#Select columns of interest
Unkovich_2010 <- select(Unkovich_2010,
                     Original_region,Reference_where_data_were_collated,
                     Website_of_source_of_collated_data,Primary_reference_of_dataset,
                     Original_crop,Original_crop_component,variable,value)

#USDA_1992----
USDA_1992 <- as.data.frame(read_csv("data/raw/USDA_1992_Ag_waste_mgmt_field_handbook_chpt_6.csv"))
#Deselect Dry_wt._lb_bushel and Typical_yield_per_acre_plant_part columns 
USDA_1992 <- select(USDA_1992, -Dry_wt._lb_bushel,-Typical_yield_per_acre_plant_part,-Original_crop_category)


#Rename columns as appropriate
USDA_1992 <-  dplyr::rename(USDA_1992, 
                            Original_region= Region,
                            Original_crop_component=Crop_component,
                            N_pc=N_pc_DM,
                            P_pc=P_pc_DM,
                            K_pc=K_pc_DM,
                            Ca_pc=Ca_pc_DM,
                            Mg_pc=Mg_pc_DM,
                            S_pc=S_pc_DM,
                            Cu_pc=Cu_pc_DM,
                            Mn_pc=Mn_pc_DM,
                            Zn_pc=Zn_pc_DM)

USDA_1992 <- melt(USDA_1992,id=c("Original_region","Reference_where_data_were_collated",
                                   "Website_of_source_of_collated_data", "Primary_reference_of_dataset",
                                   "Original_crop","Original_crop_component"))

#Panagos_2002----
#Rename columns
Panagos_2022 <- rename(Panagos_2022, Original_region=Region,
                       Original_crop_component=Crop_component,
                       P_pc=P_pc_DM,
                       HI=Harvest_index,
                       P_pc_sd=sd_P_pc_DM,
                       P_pc_no_samples=No_samples_P_pc_DM,
                       DM_pc_sd=sd_DM_pc)

#Select columns of interest
Panagos_2022 <- select(Panagos_2022, 
                       Original_region, Reference_where_data_were_collated, 
                       Website_of_source_of_collated_data, Primary_reference_of_dataset,
                       Original_crop, Original_crop_component, P_pc,
                       P_pc_sd, P_pc_no_samples, DM_pc,DM_pc_sd,
                       HI, Ratio_residues_removed_from_field)

#Melt data
Panagos_2022 <- melt(Panagos_2022,id=c("Original_region","Reference_where_data_were_collated",
                                 "Website_of_source_of_collated_data", "Primary_reference_of_dataset",
                                 "Original_crop","Original_crop_component"))

#FAO_2020----
#Delete N, P, K and S columns as they show the total kg of nutrient per 1 metric tonne of crop products plus 1 metric tonne of crop residues. 
#These are not useful for this analysis given they do not account for harvest index. 
FAO_2020 <-select(FAO_2020, 
                  -N,-P,-K,-S)

#Convert values from kg nutrient per metric tonne of fresh weight to a percentage of fresh weight basis.
FAO_2020 <- mutate(FAO_2020, 
                   Original_crop = Item,
                   Comment=Crop,
                   N_pc_CP_fresh=N_Grain/10, #CP= crop products
                   N_pc_CR_fresh=N_Plant/10, #CR = crop residues
                   P_pc_CP_fresh=P_Grain/10, #CP= crop products
                   P_pc_CR_fresh=P_Plant/10, #CR = crop residues
                   K_pc_CP_fresh=K_Grain/10, #CP= crop products
                   K_pc_CR_fresh=K_Plant/10, #CR = crop residues
                   S_pc_CP_fresh=S_Grain/10, #CP= crop products
                   S_pc_CR_fresh=S_Plant/10, #CR = crop residues
                   Original_region= "World") 

#Select columns of interest
FAO_2020 <- select(FAO_2020, Original_region, Reference_where_data_were_collated, 
                   Website_of_source_of_collated_data, Primary_reference_of_dataset,
                   Original_crop, 
                   N_pc_CP_fresh,N_pc_CR_fresh,
                   P_pc_CP_fresh,P_pc_CR_fresh,
                   K_pc_CP_fresh,K_pc_CR_fresh,
                   S_pc_CP_fresh,S_pc_CR_fresh)

#Melt data
FAO_2020 <- melt(FAO_2020, 
                 id=c("Original_region","Reference_where_data_were_collated",
                      "Website_of_source_of_collated_data", "Primary_reference_of_dataset",
                      "Original_crop"))

FAO_2020$variable <- as.character(FAO_2020$variable)

#Create Original_crop_component column 
FAO_2020 <-FAO_2020 %>% mutate(Original_crop_component=case_when(
  str_detect(variable,regex("CP", ignore_case=TRUE))~"Crop_products",
  str_detect(variable,regex("CR", ignore_case=TRUE))~"Crop_residues",  
  TRUE~variable)) 

#Create Original_crop_component column 
FAO_2020 <-FAO_2020 %>% mutate(Original_crop_component=case_when(
  str_detect(variable,regex("N_pc_CP_fresh|P_pc_CP_fresh|K_pc_CP_fresh|S_pc_CP_fresh", ignore_case=TRUE))~"Crop_products",
  str_detect(variable,regex("N_pc_CR_fresh|P_pc_CR_fresh|K_pc_CR_fresh|S_pc_CR_fresh", ignore_case=TRUE))~"Crop_residues",  
  TRUE~variable)) 

#Delete the (now) extraneous _CP and _CR from text in the variable column. 
FAO_2020$variable <- gsub("_CP|_CR","",FAO_2020$variable)

#Swain_2022----
#Select columns of interest
Swain_2022 <- select(Swain_2022, 
                     Original_region, 
                     Reference_where_data_were_collated,
                     Website_of_source_of_collated_data,
                     Primary_reference_of_dataset,
                     Original_crop,
                     Original_crop_component,
                     variable,
                     value)

#Bessembinder_1995----
#Select columns of interest
Bessembinder_1995 <- select(Bessembinder_1995, 
                     Original_region, 
                     Reference_where_data_were_collated,
                     Website_of_source_of_collated_data,
                     Primary_reference_of_dataset,
                     Original_crop,
                     Original_crop_component,
                     variable,
                     value)

#Angeleska_2022----
#Select columns of interest
Angeleska_2022 <- select(Angeleska_2022, 
                            Original_region, 
                            Reference_where_data_were_collated,
                            Website_of_source_of_collated_data,
                            Primary_reference_of_dataset,
                            Original_crop,
                            Original_crop_component,
                            variable,
                            value)

#Nijhof_1987----
#Set as data frame
Nijhof_1987 <- as.data.frame(Nijhof_1987)

#De-select columns that aren't of interest
Nijhof_1987 <- select(Nijhof_1987, 
                      -Author_last_name,
                      -Source_title,
                      -Pub_year,
                      -Pub_page_number)

#Convert Sub_tropics in 'Original_region' to 'World' given it is difficult to differentiate countries into sub-tropical and non- sub-tropical countries.
Nijhof_1987$Original_region <-"World"

#Melt data
Nijhof_1987 <- melt(Nijhof_1987, 
                 id=c("Original_region","Reference_where_data_were_collated",
                      "Website_of_source_of_collated_data", "Primary_reference_of_dataset",
                      "Original_crop", "Original_crop_component"))

#Roy_2016----
Roy_2016 <- as.data.frame(Roy_2016)

#Deselect columns that are not of interest
Roy_2016 <- select(Roy_2016,
                   -Pub_page_Number,
                   -Fe_uptake_kg_per_t,
                   -Mn_uptake_kg_per_t,
                   -Cu_uptake_kg_per_t,
                   -B_uptake_kg_per_t,
                   -Mo_uptake_kg_per_t)

Roy_2016$Zn_content_kg_per_t <-as.numeric(Roy_2016$Zn_content_kg_per_t)

#Create columns with standardised units.
Roy_2016 <- mutate(Roy_2016, 
                   "N_pc"=N_content_kg_per_t/10,
                   "P_pc"=P2O5_content_kg_per_t/10*P2O5_to_P,
                   "K_pc"=K2O_content_kg_per_t/10*K2O_to_K,
                   "Ca_pc"=Ca_content_kg_per_t/10,
                   "Mg_pc"=Mg_content_kg_per_t/10,
                   "S_pc"=S_content_kg_per_t/10,
                   "Cu_pc"=Cu_content_kg_per_t/10,
                   "Mn_pc"=Mn_content_kg_per_t/10,
                   "Zn_pc"=Zn_content_kg_per_t/10) %>% 
  select(-N_content_kg_per_t,
         -P2O5_content_kg_per_t,
         -K2O_content_kg_per_t,
         -Ca_content_kg_per_t,
         -Mg_content_kg_per_t,
         -S_content_kg_per_t,
         -Cu_content_kg_per_t,
         -Mn_content_kg_per_t,
         -Zn_content_kg_per_t)

Roy_2016 <- melt(Roy_2016, 
                    id=c("Original_region","Reference_where_data_were_collated",
                         "Website_of_source_of_collated_data", "Primary_reference_of_dataset",
                         "Original_crop", "Original_crop_component"))

#Norton_2011----
Norton_2011 <- as.data.frame(Norton_2011)

#Add factor to convert nutrient concentrations in milligrams per kg to percentage of dry matter.
Mg_kg_to_pc_factor <- 10000

#Create columns with standardised names/units
Norton_2011 <- mutate(Norton_2011, 
                      P_pc=P_mg_kg_DM/Mg_kg_to_pc_factor,
                      K_pc=K_mg_kg_DM/Mg_kg_to_pc_factor,
                      S_pc=S_mg_kg_DM/Mg_kg_to_pc_factor,
                      Ca_pc=Ca_mg_kg_DM/Mg_kg_to_pc_factor,
                      Mg_pc=Mg_mg_kg_DM/Mg_kg_to_pc_factor,
                      Na_pc=Na_mg_kg_DM/Mg_kg_to_pc_factor,
                      Fe_pc=Fe_mg_kg_DM/Mg_kg_to_pc_factor,
                      Mn_pc=Mn_mg_kg_DM/Mg_kg_to_pc_factor,
                      B_pc=B_mg_kg_DM/Mg_kg_to_pc_factor,
                      Cu_pc=Cu_mg_kg_DM/Mg_kg_to_pc_factor,
                      Zn_pc=Zn_mg_kg_DM/Mg_kg_to_pc_factor,
                      Al_pc=Al_mg_kg_DM/Mg_kg_to_pc_factor,
                      P_pc_sd=P_mg_kg_DM_sd/Mg_kg_to_pc_factor,
                      K_pc_sd=K_mg_kg_DM_sd/Mg_kg_to_pc_factor,
                      S_pc_sd=S_mg_kg_DM_sd/Mg_kg_to_pc_factor,
                      Ca_pc_sd=Ca_mg_kg_DM_sd/Mg_kg_to_pc_factor,
                      Mg_pc_sd=Mg_mg_kg_DM_sd/Mg_kg_to_pc_factor,
                      Na_pc_sd=Na_mg_kg_DM_sd/Mg_kg_to_pc_factor,
                      Fe_pc_sd=Fe_mg_kg_DM_sd/Mg_kg_to_pc_factor,
                      Mn_pc_sd=Mn_mg_kg_DM_sd/Mg_kg_to_pc_factor,
                      B_pc_sd=B_mg_kg_DM_sd/Mg_kg_to_pc_factor,
                      Cu_pc_sd=Cu_mg_kg_DM_sd/Mg_kg_to_pc_factor,
                      Zn_pc_sd=Zn_mg_kg_DM_sd/Mg_kg_to_pc_factor,
                      Al_pc_sd=Al_mg_kg_DM_sd/Mg_kg_to_pc_factor)

#De-select columns that are not of interest (columns that are in extraneous units)
Norton_2011 <- select(Norton_2011,-contains("mg_kg_DM")) %>% select(-Pub_page_Number,
                                                                    -Year_sowing,
                                                                    -Number_years_data,
                                                                    -Sites_years,
                                                                    -Number_values)

#Create column with merged Country_state_site information for Original_region.
Norton_2011$Original_region <- paste(Norton_2011$Original_region,Norton_2011$Original_state,Norton_2011$Original_site)

#Delete extraneous State and Site columns
Norton_2011 <- select(Norton_2011,-Original_state,-Original_site)

#Melt data
Norton_2011 <- melt(Norton_2011,id=c("Original_region","Reference_where_data_were_collated",
                                     "Website_of_source_of_collated_data", "Primary_reference_of_dataset",
                                     "Original_crop","Original_crop_component"))

#Kumssa_2022----
#Select columns of interest (exclude soil test results)
Kumssa_2022 <- select(Kumssa_2022, 
                         Original_region, 
                         Reference_where_data_were_collated,
                         Website_of_source_of_collated_data,
                         Primary_reference_of_dataset,
                         Original_crop,
                         Original_crop_component,
                         File, 
                         ID,
                         Crop_ICP_Run,
                         Crop_ICP_Run_Se,
                        Latitude,
                        Longitude,
                        SamplingStart,
                        SamplingEnd,
                        GrainSource,
                        Site,
                        Ag_grain,
                        Al_grain,
                        As_grain,
                        B_grain,
                        Ba_grain,
                        Be_grain,
                        Ca_grain,
                        Cd_grain,
                        Co_grain,
                        Cr_grain,
                        Cs_grain,
                        Cu_grain,
                        Fe_grain,
                        K_grain,
                        Li_grain,
                        Mg_grain,
                        Mn_grain,
                        Mo_grain,
                        Ni_grain,
                        P_grain,
                        Pb_grain,
                        Rb_grain,
                        S_grain,
                        Se_grain,
                        Sr_grain,
                        Tl_grain,
                        U_grain,
                        V_grain,
                        Zn_grain)

#Mutate variables so they are in percentage of dry matter rather than milligrams of nutrient per kilogram of dry matter.
Kumssa_2022  <- mutate(Kumssa_2022, 
                      "Ag_pc"=Ag_grain/Mg_kg_to_pc_factor,
                      "Al_pc"=Al_grain/Mg_kg_to_pc_factor,
                      "As_pc"=As_grain/Mg_kg_to_pc_factor,
                      "B_pc"=B_grain/Mg_kg_to_pc_factor,
                      "Ba_pc"=Ba_grain/Mg_kg_to_pc_factor,
                      "Be_pc"=Be_grain/Mg_kg_to_pc_factor,
                      "Ca_pc"=Ca_grain/Mg_kg_to_pc_factor,
                      "Cd_pc"=Cd_grain/Mg_kg_to_pc_factor,
                      "Co_pc"=Co_grain/Mg_kg_to_pc_factor,
                      "Cr_pc"=Cr_grain/Mg_kg_to_pc_factor,
                      "Cs_pc"=Cs_grain/Mg_kg_to_pc_factor,
                      "Cu_pc"=Cu_grain/Mg_kg_to_pc_factor,
                      "Fe_pc"=Fe_grain/Mg_kg_to_pc_factor,
                      "K_pc"=K_grain/Mg_kg_to_pc_factor,
                      "Li_pc"=Li_grain/Mg_kg_to_pc_factor,
                      "Mg_pc"=Mg_grain/Mg_kg_to_pc_factor,
                      "Mn_pc"=Mn_grain/Mg_kg_to_pc_factor,
                      "Mo_pc"=Mo_grain/Mg_kg_to_pc_factor,
                      "Ni_pc"=Ni_grain/Mg_kg_to_pc_factor,
                      "P_pc"=P_grain/Mg_kg_to_pc_factor,
                      "Pb_pc"=Pb_grain/Mg_kg_to_pc_factor,
                      "Rb_pc"=Rb_grain/Mg_kg_to_pc_factor,
                      "S_pc"=S_grain/Mg_kg_to_pc_factor,
                      "Se_pc"=Se_grain/Mg_kg_to_pc_factor,
                      "Sr_pc"=Sr_grain/Mg_kg_to_pc_factor,
                      "Tl_pc"=Tl_grain/Mg_kg_to_pc_factor,
                      "U_pc"=U_grain/Mg_kg_to_pc_factor,
                      "V_pc"=V_grain/Mg_kg_to_pc_factor,
                      "Zn_pc"=Zn_grain/Mg_kg_to_pc_factor)

#De-select unnecessary columns
Kumssa_2022 <- select(Kumssa_2022,
                      -Ag_grain,
                      -Al_grain,
                      -As_grain,
                      -B_grain,
                      -Ba_grain,
                      -Be_grain,
                      -Ca_grain,
                      -Cd_grain,
                      -Co_grain,
                      -Cr_grain,
                      -Cs_grain,
                      -Cu_grain,
                      -Fe_grain,
                      -K_grain,
                      -Li_grain,
                      -Mg_grain,
                      -Mn_grain,
                      -Mo_grain,
                      -Ni_grain,
                      -P_grain,
                      -Pb_grain,
                      -Rb_grain,
                      -S_grain,
                      -Se_grain,
                      -Sr_grain,
                      -Tl_grain,
                      -U_grain,
                      -V_grain,
                      -Zn_grain)

#Summarise results per unique location and sample date.
Kumssa_2022 <- Kumssa_2022 %>% group_by(Original_region,
                                        Reference_where_data_were_collated,
                                        Website_of_source_of_collated_data,
                                        Primary_reference_of_dataset,
                                        Original_crop,
                                        Original_crop_component,
                                        #File,
                                        #SamplingStart,
                                        #SamplingEnd,
                                        Latitude,
                                        Longitude) %>% select(-Crop_ICP_Run,-Crop_ICP_Run_Se) %>% summarise(
    across(where(is.numeric), mean,na.rm=TRUE)) 

#Melt data
Kumssa_2022 <- melt(Kumssa_2022,id=c("Original_region","Reference_where_data_were_collated",
                                       "Website_of_source_of_collated_data", "Primary_reference_of_dataset",
                                       "Original_crop","Original_crop_component",
                                     "Latitude","Longitude"))

#Combine all the crop related datasets together----
Crop_df <- rbind(Bouwman_2017,
                 Conant_2013,
                 Crop_residue_removal_Ghana_Kenya_Mali,
                 FAO_2004,
                 IFA_2020,
                 IPCC_2018,
                 IPNI_2012,
                 Koopmans_1998,
                 Lassaletta_2014, 
                 Zhang_2021,
                 Unkovich_2010,
                 USDA_1992,
                 Panagos_2022,
                 FAO_2020,
                 Swain_2022,
                 Bessembinder_1995,
                 Angeleska_2022,
                 Nijhof_1987,
                 Roy_2016,
                 Norton_2011)

#Add Latitude and Longitude columns to Crop_df data frame so Kumssa_2022 data can maintain these info in the final dataset.
Crop_df$Latitude <- as.numeric("NA")
Crop_df$Longitude <- as.numeric("NA")

#Bind Crop_df with Kumssa_2022 data.
Crop_df <- rbind(Crop_df,
                 Kumssa_2022)

#Remove spaces in variable column
Crop_df$variable <- gsub(" ","",Crop_df$variable)

#Exclude rows that do not have data in value column
Crop_df <- dplyr::filter(Crop_df, !is.na(value))

#Create a summary of references used
Crop_df_references <- unique(Crop_df %>% select(Original_region, Reference_where_data_were_collated,
                                         Website_of_source_of_collated_data,Primary_reference_of_dataset))

#Get country code information
UN_countries_codes <- read_csv("data/raw/UN_countries_codes.csv")

#Create columns for each unique region so we can align countries to regions.
UN_countries_codes$World <- "World"
UN_countries_codes$Canada <- NA
UN_countries_codes$Central_America <- NA
UN_countries_codes$East_Africa <- NA
UN_countries_codes$East_Asia <- NA
UN_countries_codes$Eastern_Europe <- NA
UN_countries_codes$Former_USSR <- NA
UN_countries_codes$Japan <- NA
UN_countries_codes$Middle_East <- NA
UN_countries_codes$North_Africa <- NA
UN_countries_codes$Oceania <- NA
UN_countries_codes$OECD_Europe <- NA
UN_countries_codes$Southern_Africa <- NA
UN_countries_codes$South_America <- NA
UN_countries_codes$South_Asia <- NA
UN_countries_codes$Southeast_Asia <- NA
UN_countries_codes$USA <- NA
UN_countries_codes$West_Africa <- NA
UN_countries_codes$Ghana <- NA
UN_countries_codes$Kenya <- NA
UN_countries_codes$Mali <- NA
UN_countries_codes$South_and_South_East_Asia <- NA
UN_countries_codes$Australia <- NA
UN_countries_codes$United_States_of_America <- NA
UN_countries_codes$European_Union_and_United_Kingdom <- NA
UN_countries_codes$India_Odisha <- NA
UN_countries_codes$Atlantic_Zone_of_Costa_Rica <- NA
UN_countries_codes$North_Macedonia <- NA
#UN_countries_codes$Sub_tropics <- NA
UN_countries_codes$North_America <- NA
UN_countries_codes$India <- NA
UN_countries_codes$Australia_New_South_Wales_South_East <- NA
UN_countries_codes$Australia_New_South_Wales_South_West <- NA
UN_countries_codes$Australia_South_Australia_Lower_EP <- NA
UN_countries_codes$Australia_South_Australia_Mid_North <- NA
UN_countries_codes$Australia_South_Australia_Murray_Mallee <- NA
UN_countries_codes$Australia_South_Australia_South_East <- NA
UN_countries_codes$Australia_South_Australia_Upper_EP <- NA
UN_countries_codes$Australia_South_Australia_Yorke_Penn. <- NA
UN_countries_codes$Australia_Victoria_Mallee <- NA
UN_countries_codes$Australia_Victoria_North_Central <- NA
UN_countries_codes$Australia_Victoria_North_East <- NA
UN_countries_codes$Australia_Victoria_Wimmera <- NA
UN_countries_codes$Ethiopia <- NA
UN_countries_codes$Malawi <- NA

#Add region names to applicable countries If countries in each region were not explicit in the source document then the UN regions were used from: https://www.fao.org/faostat/en/#definitions under Country/Region
UN_countries_codes$Canada[38] <-"Canada"

UN_countries_codes$Central_America[22] <- c("Central America") #According to UN, Central America included c("Belize","Costa Rica","El Salvador","Guatemala","Honduras","Mexico","Nicaragua","Panama")
UN_countries_codes$Central_America[52] <- c("Central America") #According to UN, Central America included c("Belize","Costa Rica","El Salvador","Guatemala","Honduras","Mexico","Nicaragua","Panama")
UN_countries_codes$Central_America[64] <- c("Central America") #According to UN, Central America included c("Belize","Costa Rica","El Salvador","Guatemala","Honduras","Mexico","Nicaragua","Panama")
UN_countries_codes$Central_America[88] <- c("Central America") #According to UN, Central America included c("Belize","Costa Rica","El Salvador","Guatemala","Honduras","Mexico","Nicaragua","Panama")
UN_countries_codes$Central_America[96] <- c("Central America") #According to UN, Central America included c("Belize","Costa Rica","El Salvador","Guatemala","Honduras","Mexico","Nicaragua","Panama")
UN_countries_codes$Central_America[141] <- c("Central America") #According to UN, Central America included c("Belize","Costa Rica","El Salvador","Guatemala","Honduras","Mexico","Nicaragua","Panama")
UN_countries_codes$Central_America[158] <- c("Central America") #According to UN, Central America included c("Belize","Costa Rica","El Salvador","Guatemala","Honduras","Mexico","Nicaragua","Panama")
UN_countries_codes$Central_America[169] <- c("Central America") #According to UN, Central America included c("Belize","Costa Rica","El Salvador","Guatemala","Honduras","Mexico","Nicaragua","Panama")

UN_countries_codes$East_Africa[35] <- c("East Africa") #According to UN, East Africa included: c("Burundi","Chagos Archipelago","Comoros","Dijbouti","Eritrea",""Ethiopia,"Ethiopia PDR","French Southern Territories","Kenya","Madagascar","Malawi","Mauritius","Mayotte","Réunion","Rwanda","Seychelles","Somalia","South Sudan","Uganda","United Republic of Tanzania","Zambia","Zimbabwe")
UN_countries_codes$East_Africa[48] <- c("East Africa") #According to UN, East Africa included: c("Burundi","Chagos Archipelago","Comoros","Dijbouti","Eritrea",""Ethiopia,"Ethiopia PDR","French Southern Territories","Kenya","Madagascar","Malawi","Mauritius","Mayotte","Réunion","Rwanda","Seychelles","Somalia","South Sudan","Uganda","United Republic of Tanzania","Zambia","Zimbabwe")
UN_countries_codes$East_Africa[59] <- c("East Africa") #According to UN, East Africa included: c("Burundi","Chagos Archipelago","Comoros","Dijbouti","Eritrea",""Ethiopia,"Ethiopia PDR","French Southern Territories","Kenya","Madagascar","Malawi","Mauritius","Mayotte","Réunion","Rwanda","Seychelles","Somalia","South Sudan","Uganda","United Republic of Tanzania","Zambia","Zimbabwe")
UN_countries_codes$East_Africa[66] <- c("East Africa") #According to UN, East Africa included: c("Burundi","Chagos Archipelago","Comoros","Dijbouti","Eritrea",""Ethiopia,"Ethiopia PDR","French Southern Territories","Kenya","Madagascar","Malawi","Mauritius","Mayotte","Réunion","Rwanda","Seychelles","Somalia","South Sudan","Uganda","United Republic of Tanzania","Zambia","Zimbabwe")
UN_countries_codes$East_Africa[68] <- c("East Africa") #According to UN, East Africa included: c("Burundi","Chagos Archipelago","Comoros","Dijbouti","Eritrea",""Ethiopia,"Ethiopia PDR","French Southern Territories","Kenya","Madagascar","Malawi","Mauritius","Mayotte","Réunion","Rwanda","Seychelles","Somalia","South Sudan","Uganda","United Republic of Tanzania","Zambia","Zimbabwe")
UN_countries_codes$East_Africa[76] <- c("East Africa") #According to UN, East Africa included: c("Burundi","Chagos Archipelago","Comoros","Dijbouti","Eritrea",""Ethiopia,"Ethiopia PDR","French Southern Territories","Kenya","Madagascar","Malawi","Mauritius","Mayotte","Réunion","Rwanda","Seychelles","Somalia","South Sudan","Uganda","United Republic of Tanzania","Zambia","Zimbabwe")
UN_countries_codes$East_Africa[113] <- c("East Africa") #According to UN, East Africa included: c("Burundi","Chagos Archipelago","Comoros","Dijbouti","Eritrea",""Ethiopia,"Ethiopia PDR","French Southern Territories","Kenya","Madagascar","Malawi","Mauritius","Mayotte","Réunion","Rwanda","Seychelles","Somalia","South Sudan","Uganda","United Republic of Tanzania","Zambia","Zimbabwe")
UN_countries_codes$East_Africa[130] <- c("East Africa") #According to UN, East Africa included: c("Burundi","Chagos Archipelago","Comoros","Dijbouti","Eritrea",""Ethiopia,"Ethiopia PDR","French Southern Territories","Kenya","Madagascar","Malawi","Mauritius","Mayotte","Réunion","Rwanda","Seychelles","Somalia","South Sudan","Uganda","United Republic of Tanzania","Zambia","Zimbabwe")
UN_countries_codes$East_Africa[131] <- c("East Africa") #According to UN, East Africa included: c("Burundi","Chagos Archipelago","Comoros","Dijbouti","Eritrea",""Ethiopia,"Ethiopia PDR","French Southern Territories","Kenya","Madagascar","Malawi","Mauritius","Mayotte","Réunion","Rwanda","Seychelles","Somalia","South Sudan","Uganda","United Republic of Tanzania","Zambia","Zimbabwe")
UN_countries_codes$East_Africa[139] <- c("East Africa") #According to UN, East Africa included: c("Burundi","Chagos Archipelago","Comoros","Dijbouti","Eritrea",""Ethiopia,"Ethiopia PDR","French Southern Territories","Kenya","Madagascar","Malawi","Mauritius","Mayotte","Réunion","Rwanda","Seychelles","Somalia","South Sudan","Uganda","United Republic of Tanzania","Zambia","Zimbabwe")
UN_countries_codes$East_Africa[140] <- c("East Africa") #According to UN, East Africa included: c("Burundi","Chagos Archipelago","Comoros","Dijbouti","Eritrea",""Ethiopia,"Ethiopia PDR","French Southern Territories","Kenya","Madagascar","Malawi","Mauritius","Mayotte","Réunion","Rwanda","Seychelles","Somalia","South Sudan","Uganda","United Republic of Tanzania","Zambia","Zimbabwe")
UN_countries_codes$East_Africa[179] <- c("East Africa") #According to UN, East Africa included: c("Burundi","Chagos Archipelago","Comoros","Dijbouti","Eritrea",""Ethiopia,"Ethiopia PDR","French Southern Territories","Kenya","Madagascar","Malawi","Mauritius","Mayotte","Réunion","Rwanda","Seychelles","Somalia","South Sudan","Uganda","United Republic of Tanzania","Zambia","Zimbabwe")
UN_countries_codes$East_Africa[182] <- c("East Africa") #According to UN, East Africa included: c("Burundi","Chagos Archipelago","Comoros","Dijbouti","Eritrea",""Ethiopia,"Ethiopia PDR","French Southern Territories","Kenya","Madagascar","Malawi","Mauritius","Mayotte","Réunion","Rwanda","Seychelles","Somalia","South Sudan","Uganda","United Republic of Tanzania","Zambia","Zimbabwe")
UN_countries_codes$East_Africa[194] <- c("East Africa") #According to UN, East Africa included: c("Burundi","Chagos Archipelago","Comoros","Dijbouti","Eritrea",""Ethiopia,"Ethiopia PDR","French Southern Territories","Kenya","Madagascar","Malawi","Mauritius","Mayotte","Réunion","Rwanda","Seychelles","Somalia","South Sudan","Uganda","United Republic of Tanzania","Zambia","Zimbabwe")
UN_countries_codes$East_Africa[200] <- c("East Africa") #According to UN, East Africa included: c("Burundi","Chagos Archipelago","Comoros","Dijbouti","Eritrea",""Ethiopia,"Ethiopia PDR","French Southern Territories","Kenya","Madagascar","Malawi","Mauritius","Mayotte","Réunion","Rwanda","Seychelles","Somalia","South Sudan","Uganda","United Republic of Tanzania","Zambia","Zimbabwe")
UN_countries_codes$East_Africa[203] <- c("East Africa") #According to UN, East Africa included: c("Burundi","Chagos Archipelago","Comoros","Dijbouti","Eritrea",""Ethiopia,"Ethiopia PDR","French Southern Territories","Kenya","Madagascar","Malawi","Mauritius","Mayotte","Réunion","Rwanda","Seychelles","Somalia","South Sudan","Uganda","United Republic of Tanzania","Zambia","Zimbabwe")
UN_countries_codes$East_Africa[227] <- c("East Africa") #According to UN, East Africa included: c("Burundi","Chagos Archipelago","Comoros","Dijbouti","Eritrea",""Ethiopia,"Ethiopia PDR","French Southern Territories","Kenya","Madagascar","Malawi","Mauritius","Mayotte","Réunion","Rwanda","Seychelles","Somalia","South Sudan","Uganda","United Republic of Tanzania","Zambia","Zimbabwe")
UN_countries_codes$East_Africa[215] <- c("East Africa") #According to UN, East Africa included: c("Burundi","Chagos Archipelago","Comoros","Dijbouti","Eritrea",""Ethiopia,"Ethiopia PDR","French Southern Territories","Kenya","Madagascar","Malawi","Mauritius","Mayotte","Réunion","Rwanda","Seychelles","Somalia","South Sudan","Uganda","United Republic of Tanzania","Zambia","Zimbabwe")
UN_countries_codes$East_Africa[243] <- c("East Africa") #According to UN, East Africa included: c("Burundi","Chagos Archipelago","Comoros","Dijbouti","Eritrea",""Ethiopia,"Ethiopia PDR","French Southern Territories","Kenya","Madagascar","Malawi","Mauritius","Mayotte","Réunion","Rwanda","Seychelles","Somalia","South Sudan","Uganda","United Republic of Tanzania","Zambia","Zimbabwe")
UN_countries_codes$East_Africa[244] <- c("East Africa") #According to UN, East Africa included: c("Burundi","Chagos Archipelago","Comoros","Dijbouti","Eritrea",""Ethiopia,"Ethiopia PDR","French Southern Territories","Kenya","Madagascar","Malawi","Mauritius","Mayotte","Réunion","Rwanda","Seychelles","Somalia","South Sudan","Uganda","United Republic of Tanzania","Zambia","Zimbabwe")

UN_countries_codes$East_Asia[44] <- c("East Asia") #According to UN, East Asia included:c("China","China, Hong Kong SAR","China, Taiwan Province of","Democratic People's Republic of Korea","Japan","Mongolia","Republic of Korea")
UN_countries_codes$East_Asia[97] <- c("East Asia") #According to UN, East Asia included:c("China","China, Hong Kong SAR","China, Taiwan Province of","Democratic People's Republic of Korea","Japan","Mongolia","Republic of Korea")
UN_countries_codes$East_Asia[213] <- c("East Asia") #According to UN, East Asia included:c("China","China, Hong Kong SAR","China, Taiwan Province of","Democratic People's Republic of Korea","Japan","Mongolia","Republic of Korea")
UN_countries_codes$East_Asia[115] <- c("East Asia") #According to UN, East Asia included:c("China","China, Hong Kong SAR","China, Taiwan Province of","Democratic People's Republic of Korea","Japan","Mongolia","Republic of Korea")
UN_countries_codes$East_Asia[109] <- c("East Asia") #According to UN, East Asia included:c("China","China, Hong Kong SAR","China, Taiwan Province of","Democratic People's Republic of Korea","Japan","Mongolia","Republic of Korea")
UN_countries_codes$East_Asia[145] <- c("East Asia") #According to UN, East Asia included:c("China","China, Hong Kong SAR","China, Taiwan Province of","Democratic People's Republic of Korea","Japan","Mongolia","Republic of Korea")
UN_countries_codes$East_Asia[116] <- c("East Asia") #According to UN, East Asia included:c("China","China, Hong Kong SAR","China, Taiwan Province of","Democratic People's Republic of Korea","Japan","Mongolia","Republic of Korea")

UN_countries_codes$Eastern_Europe[20] <- c("Eastern Europe")#According to UN, Eastern Europe included:c("Belarus","Bulgaria","Czechia","Czechoslovakia","Hungary","Poland","Republic of Moldova","Romania","Russian Federation","Slovakia","Ukraine","USSR")
UN_countries_codes$Eastern_Europe[33] <- c("Eastern Europe")#According to UN, Eastern Europe included:c("Belarus","Bulgaria","Czechia","Czechoslovakia","Hungary","Poland","Republic of Moldova","Romania","Russian Federation","Slovakia","Ukraine","USSR")
UN_countries_codes$Eastern_Europe[57] <- c("Eastern Europe")#According to UN, Eastern Europe included:c("Belarus","Bulgaria","Czechia","Czechoslovakia","Hungary","Poland","Republic of Moldova","Romania","Russian Federation","Slovakia","Ukraine","USSR")
UN_countries_codes$Eastern_Europe[98] <- c("Eastern Europe")#According to UN, Eastern Europe included:c("Belarus","Bulgaria","Czechia","Czechoslovakia","Hungary","Poland","Republic of Moldova","Romania","Russian Federation","Slovakia","Ukraine","USSR")
UN_countries_codes$Eastern_Europe[175] <- c("Eastern Europe")#According to UN, Eastern Europe included:c("Belarus","Bulgaria","Czechia","Czechoslovakia","Hungary","Poland","Republic of Moldova","Romania","Russian Federation","Slovakia","Ukraine","USSR")
UN_countries_codes$Eastern_Europe[143] <- c("Eastern Europe")#According to UN, Eastern Europe included:c("Belarus","Bulgaria","Czechia","Czechoslovakia","Hungary","Poland","Republic of Moldova","Romania","Russian Federation","Slovakia","Ukraine","USSR")
UN_countries_codes$Eastern_Europe[180] <- c("Eastern Europe")#According to UN, Eastern Europe included:c("Belarus","Bulgaria","Czechia","Czechoslovakia","Hungary","Poland","Republic of Moldova","Romania","Russian Federation","Slovakia","Ukraine","USSR")
UN_countries_codes$Eastern_Europe[181] <- c("Eastern Europe")#According to UN, Eastern Europe included:c("Belarus","Bulgaria","Czechia","Czechoslovakia","Hungary","Poland","Republic of Moldova","Romania","Russian Federation","Slovakia","Ukraine","USSR")
UN_countries_codes$Eastern_Europe[197] <- c("Eastern Europe")#According to UN, Eastern Europe included:c("Belarus","Bulgaria","Czechia","Czechoslovakia","Hungary","Poland","Republic of Moldova","Romania","Russian Federation","Slovakia","Ukraine","USSR")
UN_countries_codes$Eastern_Europe[228] <- c("Eastern Europe")#According to UN, Eastern Europe included:c("Belarus","Bulgaria","Czechia","Czechoslovakia","Hungary","Poland","Republic of Moldova","Romania","Russian Federation","Slovakia","Ukraine","USSR")

UN_countries_codes$Former_USSR[11] <- c("Former USSR") # Former USSR is not a region in UN but according to https://www.britannica.com/place/Soviet-Union/Toward-the-second-Revolution-1927-30, Former USSR included: c("Armenia","Azerbaijan","Belorussia","Belarus","Estonia","Georgia","Kazakhstan","Kirgiziya","Kyrgystan","Latvia","Lithuania","Moldovia","Moldova","Russia","Tajikistan","Turkmenistan","Ukraine","Uzbekistan")
UN_countries_codes$Former_USSR[15] <- c("Former USSR") # Former USSR is not a region in UN but according to https://www.britannica.com/place/Soviet-Union/Toward-the-second-Revolution-1927-30, Former USSR included: c("Armenia","Azerbaijan","Belorussia","Belarus","Estonia","Georgia","Kazakhstan","Kirgiziya","Kyrgystan","Latvia","Lithuania","Moldovia","Moldova","Russia","Tajikistan","Turkmenistan","Ukraine","Uzbekistan")
UN_countries_codes$Former_USSR[20] <- c("Former USSR") # Former USSR is not a region in UN but according to https://www.britannica.com/place/Soviet-Union/Toward-the-second-Revolution-1927-30, Former USSR included: c("Armenia","Azerbaijan","Belorussia","Belarus","Estonia","Georgia","Kazakhstan","Kirgiziya","Kyrgystan","Latvia","Lithuania","Moldovia","Moldova","Russia","Tajikistan","Turkmenistan","Ukraine","Uzbekistan")
UN_countries_codes$Former_USSR[67] <- c("Former USSR") # Former USSR is not a region in UN but according to https://www.britannica.com/place/Soviet-Union/Toward-the-second-Revolution-1927-30, Former USSR included: c("Armenia","Azerbaijan","Belorussia","Belarus","Estonia","Georgia","Kazakhstan","Kirgiziya","Kyrgystan","Latvia","Lithuania","Moldovia","Moldova","Russia","Tajikistan","Turkmenistan","Ukraine","Uzbekistan")
UN_countries_codes$Former_USSR[79] <- c("Former USSR") # Former USSR is not a region in UN but according to https://www.britannica.com/place/Soviet-Union/Toward-the-second-Revolution-1927-30, Former USSR included: c("Armenia","Azerbaijan","Belorussia","Belarus","Estonia","Georgia","Kazakhstan","Kirgiziya","Kyrgystan","Latvia","Lithuania","Moldovia","Moldova","Russia","Tajikistan","Turkmenistan","Ukraine","Uzbekistan")
UN_countries_codes$Former_USSR[112] <- c("Former USSR") # Former USSR is not a region in UN but according to https://www.britannica.com/place/Soviet-Union/Toward-the-second-Revolution-1927-30, Former USSR included: c("Armenia","Azerbaijan","Belorussia","Belarus","Estonia","Georgia","Kazakhstan","Kirgiziya","Kyrgystan","Latvia","Lithuania","Moldovia","Moldova","Russia","Tajikistan","Turkmenistan","Ukraine","Uzbekistan")
UN_countries_codes$Former_USSR[118] <- c("Former USSR") # Former USSR is not a region in UN but according to https://www.britannica.com/place/Soviet-Union/Toward-the-second-Revolution-1927-30, Former USSR included: c("Armenia","Azerbaijan","Belorussia","Belarus","Estonia","Georgia","Kazakhstan","Kirgiziya","Kyrgystan","Latvia","Lithuania","Moldovia","Moldova","Russia","Tajikistan","Turkmenistan","Ukraine","Uzbekistan")
UN_countries_codes$Former_USSR[120] <- c("Former USSR") # Former USSR is not a region in UN but according to https://www.britannica.com/place/Soviet-Union/Toward-the-second-Revolution-1927-30, Former USSR included: c("Armenia","Azerbaijan","Belorussia","Belarus","Estonia","Georgia","Kazakhstan","Kirgiziya","Kyrgystan","Latvia","Lithuania","Moldovia","Moldova","Russia","Tajikistan","Turkmenistan","Ukraine","Uzbekistan")
UN_countries_codes$Former_USSR[126] <- c("Former USSR") # Former USSR is not a region in UN but according to https://www.britannica.com/place/Soviet-Union/Toward-the-second-Revolution-1927-30, Former USSR included: c("Armenia","Azerbaijan","Belorussia","Belarus","Estonia","Georgia","Kazakhstan","Kirgiziya","Kyrgystan","Latvia","Lithuania","Moldovia","Moldova","Russia","Tajikistan","Turkmenistan","Ukraine","Uzbekistan")
UN_countries_codes$Former_USSR[143] <- c("Former USSR") # Former USSR is not a region in UN but according to https://www.britannica.com/place/Soviet-Union/Toward-the-second-Revolution-1927-30, Former USSR included: c("Armenia","Azerbaijan","Belorussia","Belarus","Estonia","Georgia","Kazakhstan","Kirgiziya","Kyrgystan","Latvia","Lithuania","Moldovia","Moldova","Russia","Tajikistan","Turkmenistan","Ukraine","Uzbekistan")
UN_countries_codes$Former_USSR[181] <- c("Former USSR") # Former USSR is not a region in UN but according to https://www.britannica.com/place/Soviet-Union/Toward-the-second-Revolution-1927-30, Former USSR included: c("Armenia","Azerbaijan","Belorussia","Belarus","Estonia","Georgia","Kazakhstan","Kirgiziya","Kyrgystan","Latvia","Lithuania","Moldovia","Moldova","Russia","Tajikistan","Turkmenistan","Ukraine","Uzbekistan")
UN_countries_codes$Former_USSR[214] <- c("Former USSR") # Former USSR is not a region in UN but according to https://www.britannica.com/place/Soviet-Union/Toward-the-second-Revolution-1927-30, Former USSR included: c("Armenia","Azerbaijan","Belorussia","Belarus","Estonia","Georgia","Kazakhstan","Kirgiziya","Kyrgystan","Latvia","Lithuania","Moldovia","Moldova","Russia","Tajikistan","Turkmenistan","Ukraine","Uzbekistan")
UN_countries_codes$Former_USSR[224] <- c("Former USSR") # Former USSR is not a region in UN but according to https://www.britannica.com/place/Soviet-Union/Toward-the-second-Revolution-1927-30, Former USSR included: c("Armenia","Azerbaijan","Belorussia","Belarus","Estonia","Georgia","Kazakhstan","Kirgiziya","Kyrgystan","Latvia","Lithuania","Moldovia","Moldova","Russia","Tajikistan","Turkmenistan","Ukraine","Uzbekistan")
UN_countries_codes$Former_USSR[228] <- c("Former USSR") # Former USSR is not a region in UN but according to https://www.britannica.com/place/Soviet-Union/Toward-the-second-Revolution-1927-30, Former USSR included: c("Armenia","Azerbaijan","Belorussia","Belarus","Estonia","Georgia","Kazakhstan","Kirgiziya","Kyrgystan","Latvia","Lithuania","Moldovia","Moldova","Russia","Tajikistan","Turkmenistan","Ukraine","Uzbekistan")
UN_countries_codes$Former_USSR[234] <- c("Former USSR") # Former USSR is not a region in UN but according to https://www.britannica.com/place/Soviet-Union/Toward-the-second-Revolution-1927-30, Former USSR included: c("Armenia","Azerbaijan","Belorussia","Belarus","Estonia","Georgia","Kazakhstan","Kirgiziya","Kyrgystan","Latvia","Lithuania","Moldovia","Moldova","Russia","Tajikistan","Turkmenistan","Ukraine","Uzbekistan")

UN_countries_codes$Japan[109] <- c("Japan")

UN_countries_codes$Middle_East[17] <- c("Middle East") #Middle East region is not a region in UN but according to https://en.wikipedia.org/wiki/Middle_East, Middle East included: c("Akrotiri and Dhekelia","Bahrain","Cyprus","Egypt","Iran","Iraq","Israel","Jordan","Kuwait","Lebanon","Oman","Palestine","Qatar","Saudi Arabia","Syria","Turkey","United Arab Emirates","Yemen")
UN_countries_codes$Middle_East[56] <- c("Middle East") #Middle East region is not a region in UN but according to https://en.wikipedia.org/wiki/Middle_East, Middle East included: c("Akrotiri and Dhekelia","Bahrain","Cyprus","Egypt","Iran","Iraq","Israel","Jordan","Kuwait","Lebanon","Oman","Palestine","Qatar","Saudi Arabia","Syria","Turkey","United Arab Emirates","Yemen")
UN_countries_codes$Middle_East[63] <- c("Middle East") #Middle East region is not a region in UN but according to https://en.wikipedia.org/wiki/Middle_East, Middle East included: c("Akrotiri and Dhekelia","Bahrain","Cyprus","Egypt","Iran","Iraq","Israel","Jordan","Kuwait","Lebanon","Oman","Palestine","Qatar","Saudi Arabia","Syria","Turkey","United Arab Emirates","Yemen")
UN_countries_codes$Middle_East[102] <- c("Middle East") #Middle East region is not a region in UN but according to https://en.wikipedia.org/wiki/Middle_East, Middle East included: c("Akrotiri and Dhekelia","Bahrain","Cyprus","Egypt","Iran","Iraq","Israel","Jordan","Kuwait","Lebanon","Oman","Palestine","Qatar","Saudi Arabia","Syria","Turkey","United Arab Emirates","Yemen")
UN_countries_codes$Middle_East[103] <- c("Middle East") #Middle East region is not a region in UN but according to https://en.wikipedia.org/wiki/Middle_East, Middle East included: c("Akrotiri and Dhekelia","Bahrain","Cyprus","Egypt","Iran","Iraq","Israel","Jordan","Kuwait","Lebanon","Oman","Palestine","Qatar","Saudi Arabia","Syria","Turkey","United Arab Emirates","Yemen")
UN_countries_codes$Middle_East[106] <- c("Middle East") #Middle East region is not a region in UN but according to https://en.wikipedia.org/wiki/Middle_East, Middle East included: c("Akrotiri and Dhekelia","Bahrain","Cyprus","Egypt","Iran","Iraq","Israel","Jordan","Kuwait","Lebanon","Oman","Palestine","Qatar","Saudi Arabia","Syria","Turkey","United Arab Emirates","Yemen")
UN_countries_codes$Middle_East[111] <- c("Middle East") #Middle East region is not a region in UN but according to https://en.wikipedia.org/wiki/Middle_East, Middle East included: c("Akrotiri and Dhekelia","Bahrain","Cyprus","Egypt","Iran","Iraq","Israel","Jordan","Kuwait","Lebanon","Oman","Palestine","Qatar","Saudi Arabia","Syria","Turkey","United Arab Emirates","Yemen")
UN_countries_codes$Middle_East[117] <- c("Middle East") #Middle East region is not a region in UN but according to https://en.wikipedia.org/wiki/Middle_East, Middle East included: c("Akrotiri and Dhekelia","Bahrain","Cyprus","Egypt","Iran","Iraq","Israel","Jordan","Kuwait","Lebanon","Oman","Palestine","Qatar","Saudi Arabia","Syria","Turkey","United Arab Emirates","Yemen")
UN_countries_codes$Middle_East[121] <- c("Middle East") #Middle East region is not a region in UN but according to https://en.wikipedia.org/wiki/Middle_East, Middle East included: c("Akrotiri and Dhekelia","Bahrain","Cyprus","Egypt","Iran","Iraq","Israel","Jordan","Kuwait","Lebanon","Oman","Palestine","Qatar","Saudi Arabia","Syria","Turkey","United Arab Emirates","Yemen")
UN_countries_codes$Middle_East[165] <- c("Middle East") #Middle East region is not a region in UN but according to https://en.wikipedia.org/wiki/Middle_East, Middle East included: c("Akrotiri and Dhekelia","Bahrain","Cyprus","Egypt","Iran","Iraq","Israel","Jordan","Kuwait","Lebanon","Oman","Palestine","Qatar","Saudi Arabia","Syria","Turkey","United Arab Emirates","Yemen")
UN_countries_codes$Middle_East[168] <- c("Middle East") #Middle East region is not a region in UN but according to https://en.wikipedia.org/wiki/Middle_East, Middle East included: c("Akrotiri and Dhekelia","Bahrain","Cyprus","Egypt","Iran","Iraq","Israel","Jordan","Kuwait","Lebanon","Oman","Palestine","Qatar","Saudi Arabia","Syria","Turkey","United Arab Emirates","Yemen")
UN_countries_codes$Middle_East[178] <- c("Middle East") #Middle East region is not a region in UN but according to https://en.wikipedia.org/wiki/Middle_East, Middle East included: c("Akrotiri and Dhekelia","Bahrain","Cyprus","Egypt","Iran","Iraq","Israel","Jordan","Kuwait","Lebanon","Oman","Palestine","Qatar","Saudi Arabia","Syria","Turkey","United Arab Emirates","Yemen")
UN_countries_codes$Middle_East[191] <- c("Middle East") #Middle East region is not a region in UN but according to https://en.wikipedia.org/wiki/Middle_East, Middle East included: c("Akrotiri and Dhekelia","Bahrain","Cyprus","Egypt","Iran","Iraq","Israel","Jordan","Kuwait","Lebanon","Oman","Palestine","Qatar","Saudi Arabia","Syria","Turkey","United Arab Emirates","Yemen")
UN_countries_codes$Middle_East[212] <- c("Middle East") #Middle East region is not a region in UN but according to https://en.wikipedia.org/wiki/Middle_East, Middle East included: c("Akrotiri and Dhekelia","Bahrain","Cyprus","Egypt","Iran","Iraq","Israel","Jordan","Kuwait","Lebanon","Oman","Palestine","Qatar","Saudi Arabia","Syria","Turkey","United Arab Emirates","Yemen")
UN_countries_codes$Middle_East[223] <- c("Middle East") #Middle East region is not a region in UN but according to https://en.wikipedia.org/wiki/Middle_East, Middle East included: c("Akrotiri and Dhekelia","Bahrain","Cyprus","Egypt","Iran","Iraq","Israel","Jordan","Kuwait","Lebanon","Oman","Palestine","Qatar","Saudi Arabia","Syria","Turkey","United Arab Emirates","Yemen")
UN_countries_codes$Middle_East[229] <- c("Middle East") #Middle East region is not a region in UN but according to https://en.wikipedia.org/wiki/Middle_East, Middle East included: c("Akrotiri and Dhekelia","Bahrain","Cyprus","Egypt","Iran","Iraq","Israel","Jordan","Kuwait","Lebanon","Oman","Palestine","Qatar","Saudi Arabia","Syria","Turkey","United Arab Emirates","Yemen")
UN_countries_codes$Middle_East[242] <- c("Middle East") #Middle East region is not a region in UN but according to https://en.wikipedia.org/wiki/Middle_East, Middle East included: c("Akrotiri and Dhekelia","Bahrain","Cyprus","Egypt","Iran","Iraq","Israel","Jordan","Kuwait","Lebanon","Oman","Palestine","Qatar","Saudi Arabia","Syria","Turkey","United Arab Emirates","Yemen")

UN_countries_codes$North_Africa[3] <- c("North Africa") #According to UN, North Africa included: c("Algeria","Egypt","Libya","Morocco","Sudan","Sudan(former)","Tunisia","Western Sahara")
UN_countries_codes$North_Africa[63] <- c("North Africa") #According to UN, North Africa included: c("Algeria","Egypt","Libya","Morocco","Sudan","Sudan(former)","Tunisia","Western Sahara")
UN_countries_codes$North_Africa[124] <- c("North Africa") #According to UN, North Africa included: c("Algeria","Egypt","Libya","Morocco","Sudan","Sudan(former)","Tunisia","Western Sahara")
UN_countries_codes$North_Africa[148] <- c("North Africa") #According to UN, North Africa included: c("Algeria","Egypt","Libya","Morocco","Sudan","Sudan(former)","Tunisia","Western Sahara")
UN_countries_codes$North_Africa[203] <- c("North Africa") #According to UN, North Africa included: c("Algeria","Egypt","Libya","Morocco","Sudan","Sudan(former)","Tunisia","Western Sahara")
UN_countries_codes$North_Africa[206] <- c("North Africa") #According to UN, North Africa included: c("Algeria","Egypt","Libya","Morocco","Sudan","Sudan(former)","Tunisia","Western Sahara")
UN_countries_codes$North_Africa[222] <- c("North Africa") #According to UN, North Africa included: c("Algeria","Egypt","Libya","Morocco","Sudan","Sudan(former)","Tunisia","Western Sahara")
UN_countries_codes$North_Africa[241] <- c("North Africa") #According to UN, North Africa included: c("Algeria","Egypt","Libya","Morocco","Sudan","Sudan(former)","Tunisia","Western Sahara")

UN_countries_codes$Oceania[4] <- c("Oceania") # According to UN, Oceania included: c("American Samoa","Australia","Christmas Island","Cocos (Keeling) Islands","Cook Islands","Fiji","French Polynesia","Guam","Heard and McDonald Islands","Johnston Island","Kiribati","Marshall islands","Micronesia","Midway Island","Nauru","New Caledonia","New Zealand","Niue","Norfolk Island","Northern Mariana Islands","Pacific Islands Trust Territory","Palau","Papua New Guinea","Pitcairn","Polynesia","Samoa","Solomon Islands","Tokelau","Tonga","Tuvalu","United States Minor Outlying Islands","Vanawatu","Wake Island","Wallis and Futuna Islands")
UN_countries_codes$Oceania[13] <- c("Oceania") # According to UN, Oceania included: c("American Samoa","Australia","Christmas Island","Cocos (Keeling) Islands","Cook Islands","Fiji","French Polynesia","Guam","Heard and McDonald Islands","Johnston Island","Kiribati","Marshall islands","Micronesia","Midway Island","Nauru","New Caledonia","New Zealand","Niue","Norfolk Island","Northern Mariana Islands","Pacific Islands Trust Territory","Palau","Papua New Guinea","Pitcairn","Polynesia","Samoa","Solomon Islands","Tokelau","Tonga","Tuvalu","United States Minor Outlying Islands","Vanawatu","Wake Island","Wallis and Futuna Islands")
UN_countries_codes$Oceania[45] <- c("Oceania") # According to UN, Oceania included: c("American Samoa","Australia","Christmas Island","Cocos (Keeling) Islands","Cook Islands","Fiji","French Polynesia","Guam","Heard and McDonald Islands","Johnston Island","Kiribati","Marshall islands","Micronesia","Midway Island","Nauru","New Caledonia","New Zealand","Niue","Norfolk Island","Northern Mariana Islands","Pacific Islands Trust Territory","Palau","Papua New Guinea","Pitcairn","Polynesia","Samoa","Solomon Islands","Tokelau","Tonga","Tuvalu","United States Minor Outlying Islands","Vanawatu","Wake Island","Wallis and Futuna Islands")
UN_countries_codes$Oceania[51] <- c("Oceania") # According to UN, Oceania included: c("American Samoa","Australia","Christmas Island","Cocos (Keeling) Islands","Cook Islands","Fiji","French Polynesia","Guam","Heard and McDonald Islands","Johnston Island","Kiribati","Marshall islands","Micronesia","Midway Island","Nauru","New Caledonia","New Zealand","Niue","Norfolk Island","Northern Mariana Islands","Pacific Islands Trust Territory","Palau","Papua New Guinea","Pitcairn","Polynesia","Samoa","Solomon Islands","Tokelau","Tonga","Tuvalu","United States Minor Outlying Islands","Vanawatu","Wake Island","Wallis and Futuna Islands")
UN_countries_codes$Oceania[71] <- c("Oceania") # According to UN, Oceania included: c("American Samoa","Australia","Christmas Island","Cocos (Keeling) Islands","Cook Islands","Fiji","French Polynesia","Guam","Heard and McDonald Islands","Johnston Island","Kiribati","Marshall islands","Micronesia","Midway Island","Nauru","New Caledonia","New Zealand","Niue","Norfolk Island","Northern Mariana Islands","Pacific Islands Trust Territory","Palau","Papua New Guinea","Pitcairn","Polynesia","Samoa","Solomon Islands","Tokelau","Tonga","Tuvalu","United States Minor Outlying Islands","Vanawatu","Wake Island","Wallis and Futuna Islands")
UN_countries_codes$Oceania[75] <- c("Oceania") # According to UN, Oceania included: c("American Samoa","Australia","Christmas Island","Cocos (Keeling) Islands","Cook Islands","Fiji","French Polynesia","Guam","Heard and McDonald Islands","Johnston Island","Kiribati","Marshall islands","Micronesia","Midway Island","Nauru","New Caledonia","New Zealand","Niue","Norfolk Island","Northern Mariana Islands","Pacific Islands Trust Territory","Palau","Papua New Guinea","Pitcairn","Polynesia","Samoa","Solomon Islands","Tokelau","Tonga","Tuvalu","United States Minor Outlying Islands","Vanawatu","Wake Island","Wallis and Futuna Islands")
UN_countries_codes$Oceania[87] <- c("Oceania") # According to UN, Oceania included: c("American Samoa","Australia","Christmas Island","Cocos (Keeling) Islands","Cook Islands","Fiji","French Polynesia","Guam","Heard and McDonald Islands","Johnston Island","Kiribati","Marshall islands","Micronesia","Midway Island","Nauru","New Caledonia","New Zealand","Niue","Norfolk Island","Northern Mariana Islands","Pacific Islands Trust Territory","Palau","Papua New Guinea","Pitcairn","Polynesia","Samoa","Solomon Islands","Tokelau","Tonga","Tuvalu","United States Minor Outlying Islands","Vanawatu","Wake Island","Wallis and Futuna Islands")
UN_countries_codes$Oceania[94] <- c("Oceania") # According to UN, Oceania included: c("American Samoa","Australia","Christmas Island","Cocos (Keeling) Islands","Cook Islands","Fiji","French Polynesia","Guam","Heard and McDonald Islands","Johnston Island","Kiribati","Marshall islands","Micronesia","Midway Island","Nauru","New Caledonia","New Zealand","Niue","Norfolk Island","Northern Mariana Islands","Pacific Islands Trust Territory","Palau","Papua New Guinea","Pitcairn","Polynesia","Samoa","Solomon Islands","Tokelau","Tonga","Tuvalu","United States Minor Outlying Islands","Vanawatu","Wake Island","Wallis and Futuna Islands")
UN_countries_codes$Oceania[114] <- c("Oceania") # According to UN, Oceania included: c("American Samoa","Australia","Christmas Island","Cocos (Keeling) Islands","Cook Islands","Fiji","French Polynesia","Guam","Heard and McDonald Islands","Johnston Island","Kiribati","Marshall islands","Micronesia","Midway Island","Nauru","New Caledonia","New Zealand","Niue","Norfolk Island","Northern Mariana Islands","Pacific Islands Trust Territory","Palau","Papua New Guinea","Pitcairn","Polynesia","Samoa","Solomon Islands","Tokelau","Tonga","Tuvalu","United States Minor Outlying Islands","Vanawatu","Wake Island","Wallis and Futuna Islands")
UN_countries_codes$Oceania[136] <- c("Oceania") # According to UN, Oceania included: c("American Samoa","Australia","Christmas Island","Cocos (Keeling) Islands","Cook Islands","Fiji","French Polynesia","Guam","Heard and McDonald Islands","Johnston Island","Kiribati","Marshall islands","Micronesia","Midway Island","Nauru","New Caledonia","New Zealand","Niue","Norfolk Island","Northern Mariana Islands","Pacific Islands Trust Territory","Palau","Papua New Guinea","Pitcairn","Polynesia","Samoa","Solomon Islands","Tokelau","Tonga","Tuvalu","United States Minor Outlying Islands","Vanawatu","Wake Island","Wallis and Futuna Islands")
UN_countries_codes$Oceania[142] <- c("Oceania") # According to UN, Oceania included: c("American Samoa","Australia","Christmas Island","Cocos (Keeling) Islands","Cook Islands","Fiji","French Polynesia","Guam","Heard and McDonald Islands","Johnston Island","Kiribati","Marshall islands","Micronesia","Midway Island","Nauru","New Caledonia","New Zealand","Niue","Norfolk Island","Northern Mariana Islands","Pacific Islands Trust Territory","Palau","Papua New Guinea","Pitcairn","Polynesia","Samoa","Solomon Islands","Tokelau","Tonga","Tuvalu","United States Minor Outlying Islands","Vanawatu","Wake Island","Wallis and Futuna Islands")
UN_countries_codes$Oceania[152] <- c("Oceania") # According to UN, Oceania included: c("American Samoa","Australia","Christmas Island","Cocos (Keeling) Islands","Cook Islands","Fiji","French Polynesia","Guam","Heard and McDonald Islands","Johnston Island","Kiribati","Marshall islands","Micronesia","Midway Island","Nauru","New Caledonia","New Zealand","Niue","Norfolk Island","Northern Mariana Islands","Pacific Islands Trust Territory","Palau","Papua New Guinea","Pitcairn","Polynesia","Samoa","Solomon Islands","Tokelau","Tonga","Tuvalu","United States Minor Outlying Islands","Vanawatu","Wake Island","Wallis and Futuna Islands")
UN_countries_codes$Oceania[156] <- c("Oceania") # According to UN, Oceania included: c("American Samoa","Australia","Christmas Island","Cocos (Keeling) Islands","Cook Islands","Fiji","French Polynesia","Guam","Heard and McDonald Islands","Johnston Island","Kiribati","Marshall islands","Micronesia","Midway Island","Nauru","New Caledonia","New Zealand","Niue","Norfolk Island","Northern Mariana Islands","Pacific Islands Trust Territory","Palau","Papua New Guinea","Pitcairn","Polynesia","Samoa","Solomon Islands","Tokelau","Tonga","Tuvalu","United States Minor Outlying Islands","Vanawatu","Wake Island","Wallis and Futuna Islands")
UN_countries_codes$Oceania[157] <- c("Oceania") # According to UN, Oceania included: c("American Samoa","Australia","Christmas Island","Cocos (Keeling) Islands","Cook Islands","Fiji","French Polynesia","Guam","Heard and McDonald Islands","Johnston Island","Kiribati","Marshall islands","Micronesia","Midway Island","Nauru","New Caledonia","New Zealand","Niue","Norfolk Island","Northern Mariana Islands","Pacific Islands Trust Territory","Palau","Papua New Guinea","Pitcairn","Polynesia","Samoa","Solomon Islands","Tokelau","Tonga","Tuvalu","United States Minor Outlying Islands","Vanawatu","Wake Island","Wallis and Futuna Islands")
UN_countries_codes$Oceania[161] <- c("Oceania") # According to UN, Oceania included: c("American Samoa","Australia","Christmas Island","Cocos (Keeling) Islands","Cook Islands","Fiji","French Polynesia","Guam","Heard and McDonald Islands","Johnston Island","Kiribati","Marshall islands","Micronesia","Midway Island","Nauru","New Caledonia","New Zealand","Niue","Norfolk Island","Northern Mariana Islands","Pacific Islands Trust Territory","Palau","Papua New Guinea","Pitcairn","Polynesia","Samoa","Solomon Islands","Tokelau","Tonga","Tuvalu","United States Minor Outlying Islands","Vanawatu","Wake Island","Wallis and Futuna Islands")
UN_countries_codes$Oceania[162] <- c("Oceania") # According to UN, Oceania included: c("American Samoa","Australia","Christmas Island","Cocos (Keeling) Islands","Cook Islands","Fiji","French Polynesia","Guam","Heard and McDonald Islands","Johnston Island","Kiribati","Marshall islands","Micronesia","Midway Island","Nauru","New Caledonia","New Zealand","Niue","Norfolk Island","Northern Mariana Islands","Pacific Islands Trust Territory","Palau","Papua New Guinea","Pitcairn","Polynesia","Samoa","Solomon Islands","Tokelau","Tonga","Tuvalu","United States Minor Outlying Islands","Vanawatu","Wake Island","Wallis and Futuna Islands")
UN_countries_codes$Oceania[163] <- c("Oceania") # According to UN, Oceania included: c("American Samoa","Australia","Christmas Island","Cocos (Keeling) Islands","Cook Islands","Fiji","French Polynesia","Guam","Heard and McDonald Islands","Johnston Island","Kiribati","Marshall islands","Micronesia","Midway Island","Nauru","New Caledonia","New Zealand","Niue","Norfolk Island","Northern Mariana Islands","Pacific Islands Trust Territory","Palau","Papua New Guinea","Pitcairn","Polynesia","Samoa","Solomon Islands","Tokelau","Tonga","Tuvalu","United States Minor Outlying Islands","Vanawatu","Wake Island","Wallis and Futuna Islands")
UN_countries_codes$Oceania[167] <- c("Oceania") # According to UN, Oceania included: c("American Samoa","Australia","Christmas Island","Cocos (Keeling) Islands","Cook Islands","Fiji","French Polynesia","Guam","Heard and McDonald Islands","Johnston Island","Kiribati","Marshall islands","Micronesia","Midway Island","Nauru","New Caledonia","New Zealand","Niue","Norfolk Island","Northern Mariana Islands","Pacific Islands Trust Territory","Palau","Papua New Guinea","Pitcairn","Polynesia","Samoa","Solomon Islands","Tokelau","Tonga","Tuvalu","United States Minor Outlying Islands","Vanawatu","Wake Island","Wallis and Futuna Islands")
UN_countries_codes$Oceania[176] <- c("Oceania") # According to UN, Oceania included: c("American Samoa","Australia","Christmas Island","Cocos (Keeling) Islands","Cook Islands","Fiji","French Polynesia","Guam","Heard and McDonald Islands","Johnston Island","Kiribati","Marshall islands","Micronesia","Midway Island","Nauru","New Caledonia","New Zealand","Niue","Norfolk Island","Northern Mariana Islands","Pacific Islands Trust Territory","Palau","Papua New Guinea","Pitcairn","Polynesia","Samoa","Solomon Islands","Tokelau","Tonga","Tuvalu","United States Minor Outlying Islands","Vanawatu","Wake Island","Wallis and Futuna Islands")
UN_countries_codes$Oceania[174] <- c("Oceania") # According to UN, Oceania included: c("American Samoa","Australia","Christmas Island","Cocos (Keeling) Islands","Cook Islands","Fiji","French Polynesia","Guam","Heard and McDonald Islands","Johnston Island","Kiribati","Marshall islands","Micronesia","Midway Island","Nauru","New Caledonia","New Zealand","Niue","Norfolk Island","Northern Mariana Islands","Pacific Islands Trust Territory","Palau","Papua New Guinea","Pitcairn","Polynesia","Samoa","Solomon Islands","Tokelau","Tonga","Tuvalu","United States Minor Outlying Islands","Vanawatu","Wake Island","Wallis and Futuna Islands")
UN_countries_codes$Oceania[188] <- c("Oceania") # According to UN, Oceania included: c("American Samoa","Australia","Christmas Island","Cocos (Keeling) Islands","Cook Islands","Fiji","French Polynesia","Guam","Heard and McDonald Islands","Johnston Island","Kiribati","Marshall islands","Micronesia","Midway Island","Nauru","New Caledonia","New Zealand","Niue","Norfolk Island","Northern Mariana Islands","Pacific Islands Trust Territory","Palau","Papua New Guinea","Pitcairn","Polynesia","Samoa","Solomon Islands","Tokelau","Tonga","Tuvalu","United States Minor Outlying Islands","Vanawatu","Wake Island","Wallis and Futuna Islands")
UN_countries_codes$Oceania[199] <- c("Oceania") # According to UN, Oceania included: c("American Samoa","Australia","Christmas Island","Cocos (Keeling) Islands","Cook Islands","Fiji","French Polynesia","Guam","Heard and McDonald Islands","Johnston Island","Kiribati","Marshall islands","Micronesia","Midway Island","Nauru","New Caledonia","New Zealand","Niue","Norfolk Island","Northern Mariana Islands","Pacific Islands Trust Territory","Palau","Papua New Guinea","Pitcairn","Polynesia","Samoa","Solomon Islands","Tokelau","Tonga","Tuvalu","United States Minor Outlying Islands","Vanawatu","Wake Island","Wallis and Futuna Islands")
UN_countries_codes$Oceania[219] <- c("Oceania") # According to UN, Oceania included: c("American Samoa","Australia","Christmas Island","Cocos (Keeling) Islands","Cook Islands","Fiji","French Polynesia","Guam","Heard and McDonald Islands","Johnston Island","Kiribati","Marshall islands","Micronesia","Midway Island","Nauru","New Caledonia","New Zealand","Niue","Norfolk Island","Northern Mariana Islands","Pacific Islands Trust Territory","Palau","Papua New Guinea","Pitcairn","Polynesia","Samoa","Solomon Islands","Tokelau","Tonga","Tuvalu","United States Minor Outlying Islands","Vanawatu","Wake Island","Wallis and Futuna Islands")
UN_countries_codes$Oceania[220] <- c("Oceania") # According to UN, Oceania included: c("American Samoa","Australia","Christmas Island","Cocos (Keeling) Islands","Cook Islands","Fiji","French Polynesia","Guam","Heard and McDonald Islands","Johnston Island","Kiribati","Marshall islands","Micronesia","Midway Island","Nauru","New Caledonia","New Zealand","Niue","Norfolk Island","Northern Mariana Islands","Pacific Islands Trust Territory","Palau","Papua New Guinea","Pitcairn","Polynesia","Samoa","Solomon Islands","Tokelau","Tonga","Tuvalu","United States Minor Outlying Islands","Vanawatu","Wake Island","Wallis and Futuna Islands")
UN_countries_codes$Oceania[226] <- c("Oceania") # According to UN, Oceania included: c("American Samoa","Australia","Christmas Island","Cocos (Keeling) Islands","Cook Islands","Fiji","French Polynesia","Guam","Heard and McDonald Islands","Johnston Island","Kiribati","Marshall islands","Micronesia","Midway Island","Nauru","New Caledonia","New Zealand","Niue","Norfolk Island","Northern Mariana Islands","Pacific Islands Trust Territory","Palau","Papua New Guinea","Pitcairn","Polynesia","Samoa","Solomon Islands","Tokelau","Tonga","Tuvalu","United States Minor Outlying Islands","Vanawatu","Wake Island","Wallis and Futuna Islands")
UN_countries_codes$Oceania[232] <- c("Oceania") # According to UN, Oceania included: c("American Samoa","Australia","Christmas Island","Cocos (Keeling) Islands","Cook Islands","Fiji","French Polynesia","Guam","Heard and McDonald Islands","Johnston Island","Kiribati","Marshall islands","Micronesia","Midway Island","Nauru","New Caledonia","New Zealand","Niue","Norfolk Island","Northern Mariana Islands","Pacific Islands Trust Territory","Palau","Papua New Guinea","Pitcairn","Polynesia","Samoa","Solomon Islands","Tokelau","Tonga","Tuvalu","United States Minor Outlying Islands","Vanawatu","Wake Island","Wallis and Futuna Islands")
UN_countries_codes$Oceania[235] <- c("Oceania") # According to UN, Oceania included: c("American Samoa","Australia","Christmas Island","Cocos (Keeling) Islands","Cook Islands","Fiji","French Polynesia","Guam","Heard and McDonald Islands","Johnston Island","Kiribati","Marshall islands","Micronesia","Midway Island","Nauru","New Caledonia","New Zealand","Niue","Norfolk Island","Northern Mariana Islands","Pacific Islands Trust Territory","Palau","Papua New Guinea","Pitcairn","Polynesia","Samoa","Solomon Islands","Tokelau","Tonga","Tuvalu","United States Minor Outlying Islands","Vanawatu","Wake Island","Wallis and Futuna Islands")
UN_countries_codes$Oceania[240] <- c("Oceania") # According to UN, Oceania included: c("American Samoa","Australia","Christmas Island","Cocos (Keeling) Islands","Cook Islands","Fiji","French Polynesia","Guam","Heard and McDonald Islands","Johnston Island","Kiribati","Marshall islands","Micronesia","Midway Island","Nauru","New Caledonia","New Zealand","Niue","Norfolk Island","Northern Mariana Islands","Pacific Islands Trust Territory","Palau","Papua New Guinea","Pitcairn","Polynesia","Samoa","Solomon Islands","Tokelau","Tonga","Tuvalu","United States Minor Outlying Islands","Vanawatu","Wake Island","Wallis and Futuna Islands")

UN_countries_codes$OECD_Europe[14] <- c("OECD Europe") #According to UN, OECD Europe included: c("Austria","Belgium","Belgium-Luxembourg","Czechia","Denmark","Estonia","Finland","France","Germany","Greece","Hungary","Iceland","Ireland","Italy","Latvia","Luxembourg","Netherlands","Norway","Poland","Portugal","Slovakia","Slovenia","Spain","Sweden","Switzerland","United Kingdom")
UN_countries_codes$OECD_Europe[21] <- c("OECD Europe") #According to UN, OECD Europe included: c("Austria","Belgium","Belgium-Luxembourg","Czechia","Denmark","Estonia","Finland","France","Germany","Greece","Hungary","Iceland","Ireland","Italy","Latvia","Luxembourg","Netherlands","Norway","Poland","Portugal","Slovakia","Slovenia","Spain","Sweden","Switzerland","United Kingdom")
UN_countries_codes$OECD_Europe[57] <- c("OECD Europe") #According to UN, OECD Europe included: c("Austria","Belgium","Belgium-Luxembourg","Czechia","Denmark","Estonia","Finland","France","Germany","Greece","Hungary","Iceland","Ireland","Italy","Latvia","Luxembourg","Netherlands","Norway","Poland","Portugal","Slovakia","Slovenia","Spain","Sweden","Switzerland","United Kingdom")
UN_countries_codes$OECD_Europe[58] <- c("OECD Europe") #According to UN, OECD Europe included: c("Austria","Belgium","Belgium-Luxembourg","Czechia","Denmark","Estonia","Finland","France","Germany","Greece","Hungary","Iceland","Ireland","Italy","Latvia","Luxembourg","Netherlands","Norway","Poland","Portugal","Slovakia","Slovenia","Spain","Sweden","Switzerland","United Kingdom")
UN_countries_codes$OECD_Europe[67] <- c("OECD Europe") #According to UN, OECD Europe included: c("Austria","Belgium","Belgium-Luxembourg","Czechia","Denmark","Estonia","Finland","France","Germany","Greece","Hungary","Iceland","Ireland","Italy","Latvia","Luxembourg","Netherlands","Norway","Poland","Portugal","Slovakia","Slovenia","Spain","Sweden","Switzerland","United Kingdom")
UN_countries_codes$OECD_Europe[72] <- c("OECD Europe") #According to UN, OECD Europe included: c("Austria","Belgium","Belgium-Luxembourg","Czechia","Denmark","Estonia","Finland","France","Germany","Greece","Hungary","Iceland","Ireland","Italy","Latvia","Luxembourg","Netherlands","Norway","Poland","Portugal","Slovakia","Slovenia","Spain","Sweden","Switzerland","United Kingdom")
UN_countries_codes$OECD_Europe[73] <- c("OECD Europe") #According to UN, OECD Europe included: c("Austria","Belgium","Belgium-Luxembourg","Czechia","Denmark","Estonia","Finland","France","Germany","Greece","Hungary","Iceland","Ireland","Italy","Latvia","Luxembourg","Netherlands","Norway","Poland","Portugal","Slovakia","Slovenia","Spain","Sweden","Switzerland","United Kingdom")
UN_countries_codes$OECD_Europe[80] <- c("OECD Europe") #According to UN, OECD Europe included: c("Austria","Belgium","Belgium-Luxembourg","Czechia","Denmark","Estonia","Finland","France","Germany","Greece","Hungary","Iceland","Ireland","Italy","Latvia","Luxembourg","Netherlands","Norway","Poland","Portugal","Slovakia","Slovenia","Spain","Sweden","Switzerland","United Kingdom")
UN_countries_codes$OECD_Europe[83] <- c("OECD Europe") #According to UN, OECD Europe included: c("Austria","Belgium","Belgium-Luxembourg","Czechia","Denmark","Estonia","Finland","France","Germany","Greece","Hungary","Iceland","Ireland","Italy","Latvia","Luxembourg","Netherlands","Norway","Poland","Portugal","Slovakia","Slovenia","Spain","Sweden","Switzerland","United Kingdom")
UN_countries_codes$OECD_Europe[98] <- c("OECD Europe") #According to UN, OECD Europe included: c("Austria","Belgium","Belgium-Luxembourg","Czechia","Denmark","Estonia","Finland","France","Germany","Greece","Hungary","Iceland","Ireland","Italy","Latvia","Luxembourg","Netherlands","Norway","Poland","Portugal","Slovakia","Slovenia","Spain","Sweden","Switzerland","United Kingdom")
UN_countries_codes$OECD_Europe[99] <- c("OECD Europe") #According to UN, OECD Europe included: c("Austria","Belgium","Belgium-Luxembourg","Czechia","Denmark","Estonia","Finland","France","Germany","Greece","Hungary","Iceland","Ireland","Italy","Latvia","Luxembourg","Netherlands","Norway","Poland","Portugal","Slovakia","Slovenia","Spain","Sweden","Switzerland","United Kingdom")
UN_countries_codes$OECD_Europe[104] <- c("OECD Europe") #According to UN, OECD Europe included: c("Austria","Belgium","Belgium-Luxembourg","Czechia","Denmark","Estonia","Finland","France","Germany","Greece","Hungary","Iceland","Ireland","Italy","Latvia","Luxembourg","Netherlands","Norway","Poland","Portugal","Slovakia","Slovenia","Spain","Sweden","Switzerland","United Kingdom")
UN_countries_codes$OECD_Europe[107] <- c("OECD Europe") #According to UN, OECD Europe included: c("Austria","Belgium","Belgium-Luxembourg","Czechia","Denmark","Estonia","Finland","France","Germany","Greece","Hungary","Iceland","Ireland","Italy","Latvia","Luxembourg","Netherlands","Norway","Poland","Portugal","Slovakia","Slovenia","Spain","Sweden","Switzerland","United Kingdom")
UN_countries_codes$OECD_Europe[120] <- c("OECD Europe") #According to UN, OECD Europe included: c("Austria","Belgium","Belgium-Luxembourg","Czechia","Denmark","Estonia","Finland","France","Germany","Greece","Hungary","Iceland","Ireland","Italy","Latvia","Luxembourg","Netherlands","Norway","Poland","Portugal","Slovakia","Slovenia","Spain","Sweden","Switzerland","United Kingdom")
UN_countries_codes$OECD_Europe[127] <- c("OECD Europe") #According to UN, OECD Europe included: c("Austria","Belgium","Belgium-Luxembourg","Czechia","Denmark","Estonia","Finland","France","Germany","Greece","Hungary","Iceland","Ireland","Italy","Latvia","Luxembourg","Netherlands","Norway","Poland","Portugal","Slovakia","Slovenia","Spain","Sweden","Switzerland","United Kingdom")
UN_countries_codes$OECD_Europe[154] <- c("OECD Europe") #According to UN, OECD Europe included: c("Austria","Belgium","Belgium-Luxembourg","Czechia","Denmark","Estonia","Finland","France","Germany","Greece","Hungary","Iceland","Ireland","Italy","Latvia","Luxembourg","Netherlands","Norway","Poland","Portugal","Slovakia","Slovenia","Spain","Sweden","Switzerland","United Kingdom")
UN_countries_codes$OECD_Europe[164] <- c("OECD Europe") #According to UN, OECD Europe included: c("Austria","Belgium","Belgium-Luxembourg","Czechia","Denmark","Estonia","Finland","France","Germany","Greece","Hungary","Iceland","Ireland","Italy","Latvia","Luxembourg","Netherlands","Norway","Poland","Portugal","Slovakia","Slovenia","Spain","Sweden","Switzerland","United Kingdom")
UN_countries_codes$OECD_Europe[175] <- c("OECD Europe") #According to UN, OECD Europe included: c("Austria","Belgium","Belgium-Luxembourg","Czechia","Denmark","Estonia","Finland","France","Germany","Greece","Hungary","Iceland","Ireland","Italy","Latvia","Luxembourg","Netherlands","Norway","Poland","Portugal","Slovakia","Slovenia","Spain","Sweden","Switzerland","United Kingdom")
UN_countries_codes$OECD_Europe[176] <- c("OECD Europe") #According to UN, OECD Europe included: c("Austria","Belgium","Belgium-Luxembourg","Czechia","Denmark","Estonia","Finland","France","Germany","Greece","Hungary","Iceland","Ireland","Italy","Latvia","Luxembourg","Netherlands","Norway","Poland","Portugal","Slovakia","Slovenia","Spain","Sweden","Switzerland","United Kingdom")
UN_countries_codes$OECD_Europe[197] <- c("OECD Europe") #According to UN, OECD Europe included: c("Austria","Belgium","Belgium-Luxembourg","Czechia","Denmark","Estonia","Finland","France","Germany","Greece","Hungary","Iceland","Ireland","Italy","Latvia","Luxembourg","Netherlands","Norway","Poland","Portugal","Slovakia","Slovenia","Spain","Sweden","Switzerland","United Kingdom")
UN_countries_codes$OECD_Europe[204] <- c("OECD Europe") #According to UN, OECD Europe included: c("Austria","Belgium","Belgium-Luxembourg","Czechia","Denmark","Estonia","Finland","France","Germany","Greece","Hungary","Iceland","Ireland","Italy","Latvia","Luxembourg","Netherlands","Norway","Poland","Portugal","Slovakia","Slovenia","Spain","Sweden","Switzerland","United Kingdom")
UN_countries_codes$OECD_Europe[210] <- c("OECD Europe") #According to UN, OECD Europe included: c("Austria","Belgium","Belgium-Luxembourg","Czechia","Denmark","Estonia","Finland","France","Germany","Greece","Hungary","Iceland","Ireland","Italy","Latvia","Luxembourg","Netherlands","Norway","Poland","Portugal","Slovakia","Slovenia","Spain","Sweden","Switzerland","United Kingdom")
UN_countries_codes$OECD_Europe[211] <- c("OECD Europe") #According to UN, OECD Europe included: c("Austria","Belgium","Belgium-Luxembourg","Czechia","Denmark","Estonia","Finland","France","Germany","Greece","Hungary","Iceland","Ireland","Italy","Latvia","Luxembourg","Netherlands","Norway","Poland","Portugal","Slovakia","Slovenia","Spain","Sweden","Switzerland","United Kingdom")
UN_countries_codes$OECD_Europe[230] <- c("OECD Europe") #According to UN, OECD Europe included: c("Austria","Belgium","Belgium-Luxembourg","Czechia","Denmark","Estonia","Finland","France","Germany","Greece","Hungary","Iceland","Ireland","Italy","Latvia","Luxembourg","Netherlands","Norway","Poland","Portugal","Slovakia","Slovenia","Spain","Sweden","Switzerland","United Kingdom")

UN_countries_codes$Southern_Africa[28] <- c("Southern Africa") #According to UN, Southern Africa included: c("Botswana,"Eswatini","Swaziland","Lesotho","Namibia","South Africa")
UN_countries_codes$Southern_Africa[209] <- c("Southern Africa") #According to UN, Southern Africa included: c("Botswana,"Eswatini","Swaziland","Lesotho","Namibia","South Africa")
UN_countries_codes$Southern_Africa[122] <- c("Southern Africa") #According to UN, Southern Africa included: c("Botswana,"Eswatini","Swaziland","Lesotho","Namibia","South Africa")
UN_countries_codes$Southern_Africa[151] <- c("Southern Africa") #According to UN, Southern Africa included: c("Botswana,"Eswatini","Swaziland","Lesotho","Namibia","South Africa")
UN_countries_codes$Southern_Africa[201] <- c("Southern Africa") #According to UN, Southern Africa included: c("Botswana,"Eswatini","Swaziland","Lesotho","Namibia","South Africa")

UN_countries_codes$South_America[10] <- c("South America") #According to UN, South America included: c("Argentina","Bolivia (Plurinational State of)","Bouvet Island","Brazil","Chile","Colombia","Ecuador","Falkland Islands (Malvinas)","French Guyana","Paraguay","Peru")
UN_countries_codes$South_America[26] <- c("South America") #According to UN, South America included: c("Argentina","Bolivia (Plurinational State of)","Bouvet Island","Brazil","Chile","Colombia","Ecuador","Falkland Islands (Malvinas)","French Guyana","Paraguay","Peru")
UN_countries_codes$South_America[29] <- c("South America") #According to UN, South America included: c("Argentina","Bolivia (Plurinational State of)","Bouvet Island","Brazil","Chile","Colombia","Ecuador","Falkland Islands (Malvinas)","French Guyana","Paraguay","Peru")
UN_countries_codes$South_America[30] <- c("South America") #According to UN, South America included: c("Argentina","Bolivia (Plurinational State of)","Bouvet Island","Brazil","Chile","Colombia","Ecuador","Falkland Islands (Malvinas)","French Guyana","Paraguay","Peru")
UN_countries_codes$South_America[43] <- c("South America") #According to UN, South America included: c("Argentina","Bolivia (Plurinational State of)","Bouvet Island","Brazil","Chile","Colombia","Ecuador","Falkland Islands (Malvinas)","French Guyana","Paraguay","Peru")
UN_countries_codes$South_America[47] <- c("South America") #According to UN, South America included: c("Argentina","Bolivia (Plurinational State of)","Bouvet Island","Brazil","Chile","Colombia","Ecuador","Falkland Islands (Malvinas)","French Guyana","Paraguay","Peru")
UN_countries_codes$South_America[62] <- c("South America") #According to UN, South America included: c("Argentina","Bolivia (Plurinational State of)","Bouvet Island","Brazil","Chile","Colombia","Ecuador","Falkland Islands (Malvinas)","French Guyana","Paraguay","Peru")
UN_countries_codes$South_America[69] <- c("South America") #According to UN, South America included: c("Argentina","Bolivia (Plurinational State of)","Bouvet Island","Brazil","Chile","Colombia","Ecuador","Falkland Islands (Malvinas)","French Guyana","Paraguay","Peru")
UN_countries_codes$South_America[74] <- c("South America") #According to UN, South America included: c("Argentina","Bolivia (Plurinational State of)","Bouvet Island","Brazil","Chile","Colombia","Ecuador","Falkland Islands (Malvinas)","French Guyana","Paraguay","Peru")
UN_countries_codes$South_America[171] <- c("South America") #According to UN, South America included: c("Argentina","Bolivia (Plurinational State of)","Bouvet Island","Brazil","Chile","Colombia","Ecuador","Falkland Islands (Malvinas)","French Guyana","Paraguay","Peru")
UN_countries_codes$South_America[172] <- c("South America") #According to UN, South America included: c("Argentina","Bolivia (Plurinational State of)","Bouvet Island","Brazil","Chile","Colombia","Ecuador","Falkland Islands (Malvinas)","French Guyana","Paraguay","Peru")

UN_countries_codes$South_Asia[1] <- c("South Asia") #According to UN, South Asia included: c("Afghanistan","Bangladesh","Bhutan","India","Iran Islamic Republic of","Maldives","Nepal","Pakistan","Sri Lanka")
UN_countries_codes$South_Asia[18] <- c("South Asia") #According to UN, South Asia included: c("Afghanistan","Bangladesh","Bhutan","India","Iran Islamic Republic of","Maldives","Nepal","Pakistan","Sri Lanka")
UN_countries_codes$South_Asia[25] <- c("South Asia") #According to UN, South Asia included: c("Afghanistan","Bangladesh","Bhutan","India","Iran Islamic Republic of","Maldives","Nepal","Pakistan","Sri Lanka")
UN_countries_codes$South_Asia[100] <- c("South Asia") #According to UN, South Asia included: c("Afghanistan","Bangladesh","Bhutan","India","Iran Islamic Republic of","Maldives","Nepal","Pakistan","Sri Lanka")
UN_countries_codes$South_Asia[102] <- c("South Asia") #According to UN, South Asia included: c("Afghanistan","Bangladesh","Bhutan","India","Iran Islamic Republic of","Maldives","Nepal","Pakistan","Sri Lanka")
UN_countries_codes$South_Asia[133] <- c("South Asia") #According to UN, South Asia included: c("Afghanistan","Bangladesh","Bhutan","India","Iran Islamic Republic of","Maldives","Nepal","Pakistan","Sri Lanka")
UN_countries_codes$South_Asia[153] <- c("South Asia") #According to UN, South Asia included: c("Afghanistan","Bangladesh","Bhutan","India","Iran Islamic Republic of","Maldives","Nepal","Pakistan","Sri Lanka")
UN_countries_codes$South_Asia[166] <- c("South Asia") #According to UN, South Asia included: c("Afghanistan","Bangladesh","Bhutan","India","Iran Islamic Republic of","Maldives","Nepal","Pakistan","Sri Lanka")
UN_countries_codes$South_Asia[205] <- c("South Asia") #According to UN, South Asia included: c("Afghanistan","Bangladesh","Bhutan","India","Iran Islamic Republic of","Maldives","Nepal","Pakistan","Sri Lanka")

UN_countries_codes$Southeast_Asia[32] <- c("Southeast Asia") #According to UN, Eastern Asia and South_eastern Asia included: c("Brunei Darussalam","Cambodia","China","China, Hong Kong SAR","China, Macao SAR","China, Mainland","China, Taiwan Province of","Democratic People's Republic of Korea","Indonesia","Japan","Lao People's Democratic Republic","Malaysia","Mongolia","Myanmar","Philippines","Republic of Korea","Singapore","Thailand","Timor-Leste","Viet Nam")
UN_countries_codes$Southeast_Asia[36] <- c("Southeast Asia") #According to UN, Eastern Asia and South_eastern Asia included: c("Brunei Darussalam","Cambodia","China","China, Hong Kong SAR","China, Macao SAR","China, Mainland","China, Taiwan Province of","Democratic People's Republic of Korea","Indonesia","Japan","Lao People's Democratic Republic","Malaysia","Mongolia","Myanmar","Philippines","Republic of Korea","Singapore","Thailand","Timor-Leste","Viet Nam")
UN_countries_codes$Southeast_Asia[44] <- c("Southeast Asia") #According to UN, Eastern Asia and South_eastern Asia included: c("Brunei Darussalam","Cambodia","China","China, Hong Kong SAR","China, Macao SAR","China, Mainland","China, Taiwan Province of","Democratic People's Republic of Korea","Indonesia","Japan","Lao People's Democratic Republic","Malaysia","Mongolia","Myanmar","Philippines","Republic of Korea","Singapore","Thailand","Timor-Leste","Viet Nam")
UN_countries_codes$Southeast_Asia[97] <- c("Southeast Asia") #According to UN, Eastern Asia and South_eastern Asia included: c("Brunei Darussalam","Cambodia","China","China, Hong Kong SAR","China, Macao SAR","China, Mainland","China, Taiwan Province of","Democratic People's Republic of Korea","Indonesia","Japan","Lao People's Democratic Republic","Malaysia","Mongolia","Myanmar","Philippines","Republic of Korea","Singapore","Thailand","Timor-Leste","Viet Nam")
UN_countries_codes$Southeast_Asia[128] <- c("Southeast Asia") #According to UN, Eastern Asia and South_eastern Asia included: c("Brunei Darussalam","Cambodia","China","China, Hong Kong SAR","China, Macao SAR","China, Mainland","China, Taiwan Province of","Democratic People's Republic of Korea","Indonesia","Japan","Lao People's Democratic Republic","Malaysia","Mongolia","Myanmar","Philippines","Republic of Korea","Singapore","Thailand","Timor-Leste","Viet Nam")
UN_countries_codes$Southeast_Asia[213] <- c("Southeast Asia") #According to UN, Eastern Asia and South_eastern Asia included: c("Brunei Darussalam","Cambodia","China","China, Hong Kong SAR","China, Macao SAR","China, Mainland","China, Taiwan Province of","Democratic People's Republic of Korea","Indonesia","Japan","Lao People's Democratic Republic","Malaysia","Mongolia","Myanmar","Philippines","Republic of Korea","Singapore","Thailand","Timor-Leste","Viet Nam")
UN_countries_codes$Southeast_Asia[115] <- c("Southeast Asia") #According to UN, Eastern Asia and South_eastern Asia included: c("Brunei Darussalam","Cambodia","China","China, Hong Kong SAR","China, Macao SAR","China, Mainland","China, Taiwan Province of","Democratic People's Republic of Korea","Indonesia","Japan","Lao People's Democratic Republic","Malaysia","Mongolia","Myanmar","Philippines","Republic of Korea","Singapore","Thailand","Timor-Leste","Viet Nam")
UN_countries_codes$Southeast_Asia[116] <- c("Southeast Asia") #According to UN, Eastern Asia and South_eastern Asia included: c("Brunei Darussalam","Cambodia","China","China, Hong Kong SAR","China, Macao SAR","China, Mainland","China, Taiwan Province of","Democratic People's Republic of Korea","Indonesia","Japan","Lao People's Democratic Republic","Malaysia","Mongolia","Myanmar","Philippines","Republic of Korea","Singapore","Thailand","Timor-Leste","Viet Nam")
UN_countries_codes$Southeast_Asia[101] <- c("Southeast Asia") #According to UN, Eastern Asia and South_eastern Asia included: c("Brunei Darussalam","Cambodia","China","China, Hong Kong SAR","China, Macao SAR","China, Mainland","China, Taiwan Province of","Democratic People's Republic of Korea","Indonesia","Japan","Lao People's Democratic Republic","Malaysia","Mongolia","Myanmar","Philippines","Republic of Korea","Singapore","Thailand","Timor-Leste","Viet Nam")
UN_countries_codes$Southeast_Asia[109] <- c("Southeast Asia") #According to UN, Eastern Asia and South_eastern Asia included: c("Brunei Darussalam","Cambodia","China","China, Hong Kong SAR","China, Macao SAR","China, Mainland","China, Taiwan Province of","Democratic People's Republic of Korea","Indonesia","Japan","Lao People's Democratic Republic","Malaysia","Mongolia","Myanmar","Philippines","Republic of Korea","Singapore","Thailand","Timor-Leste","Viet Nam")
UN_countries_codes$Southeast_Asia[119] <- c("Southeast Asia") #According to UN, Eastern Asia and South_eastern Asia included: c("Brunei Darussalam","Cambodia","China","China, Hong Kong SAR","China, Macao SAR","China, Mainland","China, Taiwan Province of","Democratic People's Republic of Korea","Indonesia","Japan","Lao People's Democratic Republic","Malaysia","Mongolia","Myanmar","Philippines","Republic of Korea","Singapore","Thailand","Timor-Leste","Viet Nam")
UN_countries_codes$Southeast_Asia[132] <- c("Southeast Asia") #According to UN, Eastern Asia and South_eastern Asia included: c("Brunei Darussalam","Cambodia","China","China, Hong Kong SAR","China, Macao SAR","China, Mainland","China, Taiwan Province of","Democratic People's Republic of Korea","Indonesia","Japan","Lao People's Democratic Republic","Malaysia","Mongolia","Myanmar","Philippines","Republic of Korea","Singapore","Thailand","Timor-Leste","Viet Nam")
UN_countries_codes$Southeast_Asia[145] <- c("Southeast Asia") #According to UN, Eastern Asia and South_eastern Asia included: c("Brunei Darussalam","Cambodia","China","China, Hong Kong SAR","China, Macao SAR","China, Mainland","China, Taiwan Province of","Democratic People's Republic of Korea","Indonesia","Japan","Lao People's Democratic Republic","Malaysia","Mongolia","Myanmar","Philippines","Republic of Korea","Singapore","Thailand","Timor-Leste","Viet Nam")
UN_countries_codes$Southeast_Asia[150] <- c("Southeast Asia") #According to UN, Eastern Asia and South_eastern Asia included: c("Brunei Darussalam","Cambodia","China","China, Hong Kong SAR","China, Macao SAR","China, Mainland","China, Taiwan Province of","Democratic People's Republic of Korea","Indonesia","Japan","Lao People's Democratic Republic","Malaysia","Mongolia","Myanmar","Philippines","Republic of Korea","Singapore","Thailand","Timor-Leste","Viet Nam")
UN_countries_codes$Southeast_Asia[173] <- c("Southeast Asia") #According to UN, Eastern Asia and South_eastern Asia included: c("Brunei Darussalam","Cambodia","China","China, Hong Kong SAR","China, Macao SAR","China, Mainland","China, Taiwan Province of","Democratic People's Republic of Korea","Indonesia","Japan","Lao People's Democratic Republic","Malaysia","Mongolia","Myanmar","Philippines","Republic of Korea","Singapore","Thailand","Timor-Leste","Viet Nam")
UN_countries_codes$Southeast_Asia[196] <- c("Southeast Asia") #According to UN, Eastern Asia and South_eastern Asia included: c("Brunei Darussalam","Cambodia","China","China, Hong Kong SAR","China, Macao SAR","China, Mainland","China, Taiwan Province of","Democratic People's Republic of Korea","Indonesia","Japan","Lao People's Democratic Republic","Malaysia","Mongolia","Myanmar","Philippines","Republic of Korea","Singapore","Thailand","Timor-Leste","Viet Nam")
UN_countries_codes$Southeast_Asia[216] <- c("Southeast Asia") #According to UN, Eastern Asia and South_eastern Asia included: c("Brunei Darussalam","Cambodia","China","China, Hong Kong SAR","China, Macao SAR","China, Mainland","China, Taiwan Province of","Democratic People's Republic of Korea","Indonesia","Japan","Lao People's Democratic Republic","Malaysia","Mongolia","Myanmar","Philippines","Republic of Korea","Singapore","Thailand","Timor-Leste","Viet Nam")
UN_countries_codes$Southeast_Asia[217] <- c("Southeast Asia") #According to UN, Eastern Asia and South_eastern Asia included: c("Brunei Darussalam","Cambodia","China","China, Hong Kong SAR","China, Macao SAR","China, Mainland","China, Taiwan Province of","Democratic People's Republic of Korea","Indonesia","Japan","Lao People's Democratic Republic","Malaysia","Mongolia","Myanmar","Philippines","Republic of Korea","Singapore","Thailand","Timor-Leste","Viet Nam")
UN_countries_codes$Southeast_Asia[237] <- c("Southeast Asia") #According to UN, Eastern Asia and South_eastern Asia included: c("Brunei Darussalam","Cambodia","China","China, Hong Kong SAR","China, Macao SAR","China, Mainland","China, Taiwan Province of","Democratic People's Republic of Korea","Indonesia","Japan","Lao People's Democratic Republic","Malaysia","Mongolia","Myanmar","Philippines","Republic of Korea","Singapore","Thailand","Timor-Leste","Viet Nam")

UN_countries_codes$USA[231] <- c("USA")
UN_countries_codes$USA[232] <- c("USA")
UN_countries_codes$USA[239] <- c("USA")

UN_countries_codes$West_Africa[23] <- c("West Africa") # According to UN, Western Africa included: c("Benin","Burkino Faso","Cabo Verde","Cote dÍvoire","Gambia","Ghana","Guinea","Guinea-Bissau","Liberia","Mali","Mauritania","Niger","Nigeria","Saint Helena, Ascension and Tristan da Cunha","Senegal","Sierra Leone","Togo")
UN_countries_codes$West_Africa[34] <- c("West Africa") # According to UN, Western Africa included: c("Benin","Burkino Faso","Cabo Verde","Cote dÍvoire","Gambia","Ghana","Guinea","Guinea-Bissau","Liberia","Mali","Mauritania","Niger","Nigeria","Saint Helena, Ascension and Tristan da Cunha","Senegal","Sierra Leone","Togo")
UN_countries_codes$West_Africa[39] <- c("West Africa") # According to UN, Western Africa included: c("Benin","Burkino Faso","Cabo Verde","Cote dÍvoire","Gambia","Ghana","Guinea","Guinea-Bissau","Liberia","Mali","Mauritania","Niger","Nigeria","Saint Helena, Ascension and Tristan da Cunha","Senegal","Sierra Leone","Togo")
UN_countries_codes$West_Africa[53] <- c("West Africa") # According to UN, Western Africa included: c("Benin","Burkino Faso","Cabo Verde","Cote dÍvoire","Gambia","Ghana","Guinea","Guinea-Bissau","Liberia","Mali","Mauritania","Niger","Nigeria","Saint Helena, Ascension and Tristan da Cunha","Senegal","Sierra Leone","Togo")
UN_countries_codes$West_Africa[78] <- c("West Africa") # According to UN, Western Africa included: c("Benin","Burkino Faso","Cabo Verde","Cote dÍvoire","Gambia","Ghana","Guinea","Guinea-Bissau","Liberia","Mali","Mauritania","Niger","Nigeria","Saint Helena, Ascension and Tristan da Cunha","Senegal","Sierra Leone","Togo")
UN_countries_codes$West_Africa[81] <- c("West Africa") # According to UN, Western Africa included: c("Benin","Burkino Faso","Cabo Verde","Cote dÍvoire","Gambia","Ghana","Guinea","Guinea-Bissau","Liberia","Mali","Mauritania","Niger","Nigeria","Saint Helena, Ascension and Tristan da Cunha","Senegal","Sierra Leone","Togo")
UN_countries_codes$West_Africa[90] <- c("West Africa") # According to UN, Western Africa included: c("Benin","Burkino Faso","Cabo Verde","Cote dÍvoire","Gambia","Ghana","Guinea","Guinea-Bissau","Liberia","Mali","Mauritania","Niger","Nigeria","Saint Helena, Ascension and Tristan da Cunha","Senegal","Sierra Leone","Togo")
UN_countries_codes$West_Africa[91] <- c("West Africa") # According to UN, Western Africa included: c("Benin","Burkino Faso","Cabo Verde","Cote dÍvoire","Gambia","Ghana","Guinea","Guinea-Bissau","Liberia","Mali","Mauritania","Niger","Nigeria","Saint Helena, Ascension and Tristan da Cunha","Senegal","Sierra Leone","Togo")
UN_countries_codes$West_Africa[123] <- c("West Africa") # According to UN, Western Africa included: c("Benin","Burkino Faso","Cabo Verde","Cote dÍvoire","Gambia","Ghana","Guinea","Guinea-Bissau","Liberia","Mali","Mauritania","Niger","Nigeria","Saint Helena, Ascension and Tristan da Cunha","Senegal","Sierra Leone","Togo")
UN_countries_codes$West_Africa[134] <- c("West Africa") # According to UN, Western Africa included: c("Benin","Burkino Faso","Cabo Verde","Cote dÍvoire","Gambia","Ghana","Guinea","Guinea-Bissau","Liberia","Mali","Mauritania","Niger","Nigeria","Saint Helena, Ascension and Tristan da Cunha","Senegal","Sierra Leone","Togo")
UN_countries_codes$West_Africa[138] <- c("West Africa") # According to UN, Western Africa included: c("Benin","Burkino Faso","Cabo Verde","Cote dÍvoire","Gambia","Ghana","Guinea","Guinea-Bissau","Liberia","Mali","Mauritania","Niger","Nigeria","Saint Helena, Ascension and Tristan da Cunha","Senegal","Sierra Leone","Togo")
UN_countries_codes$West_Africa[159] <- c("West Africa") # According to UN, Western Africa included: c("Benin","Burkino Faso","Cabo Verde","Cote dÍvoire","Gambia","Ghana","Guinea","Guinea-Bissau","Liberia","Mali","Mauritania","Niger","Nigeria","Saint Helena, Ascension and Tristan da Cunha","Senegal","Sierra Leone","Togo")
UN_countries_codes$West_Africa[160] <- c("West Africa") # According to UN, Western Africa included: c("Benin","Burkino Faso","Cabo Verde","Cote dÍvoire","Gambia","Ghana","Guinea","Guinea-Bissau","Liberia","Mali","Mauritania","Niger","Nigeria","Saint Helena, Ascension and Tristan da Cunha","Senegal","Sierra Leone","Togo")
UN_countries_codes$West_Africa[183] <- c("West Africa") # According to UN, Western Africa included: c("Benin","Burkino Faso","Cabo Verde","Cote dÍvoire","Gambia","Ghana","Guinea","Guinea-Bissau","Liberia","Mali","Mauritania","Niger","Nigeria","Saint Helena, Ascension and Tristan da Cunha","Senegal","Sierra Leone","Togo")
UN_countries_codes$West_Africa[192] <- c("West Africa") # According to UN, Western Africa included: c("Benin","Burkino Faso","Cabo Verde","Cote dÍvoire","Gambia","Ghana","Guinea","Guinea-Bissau","Liberia","Mali","Mauritania","Niger","Nigeria","Saint Helena, Ascension and Tristan da Cunha","Senegal","Sierra Leone","Togo")
UN_countries_codes$West_Africa[195] <- c("West Africa") # According to UN, Western Africa included: c("Benin","Burkino Faso","Cabo Verde","Cote dÍvoire","Gambia","Ghana","Guinea","Guinea-Bissau","Liberia","Mali","Mauritania","Niger","Nigeria","Saint Helena, Ascension and Tristan da Cunha","Senegal","Sierra Leone","Togo")
UN_countries_codes$West_Africa[218] <- c("West Africa") # According to UN, Western Africa included: c("Benin","Burkino Faso","Cabo Verde","Cote dÍvoire","Gambia","Ghana","Guinea","Guinea-Bissau","Liberia","Mali","Mauritania","Niger","Nigeria","Saint Helena, Ascension and Tristan da Cunha","Senegal","Sierra Leone","Togo")

UN_countries_codes$Ghana[81] <- c("Ghana")

UN_countries_codes$Kenya[113] <- c("Kenya")

UN_countries_codes$Mali[134] <- c("Mali")

UN_countries_codes$South_and_South_East_Asia[32] <- c("South and South East Asia") #According to UN, Eastern Asia and South_eastern Asia included: c("Brunei Darussalam","Cambodia","China","China, Hong Kong SAR","China, Macao SAR","China, Mainland","China, Taiwan Province of","Democratic People's Republic of Korea","Indonesia","Japan","Lao People's Democratic Republic","Malaysia","Mongolia","Myanmar","Philippines","Republic of Korea","Singapore","Thailand","Timor-Leste","Viet Nam")
UN_countries_codes$South_and_South_East_Asia[36] <- c("South and South East Asia") #According to UN, Eastern Asia and South_eastern Asia included: c("Brunei Darussalam","Cambodia","China","China, Hong Kong SAR","China, Macao SAR","China, Mainland","China, Taiwan Province of","Democratic People's Republic of Korea","Indonesia","Japan","Lao People's Democratic Republic","Malaysia","Mongolia","Myanmar","Philippines","Republic of Korea","Singapore","Thailand","Timor-Leste","Viet Nam")
UN_countries_codes$South_and_South_East_Asia[44] <- c("South and South East Asia") #According to UN, Eastern Asia and South_eastern Asia included: c("Brunei Darussalam","Cambodia","China","China, Hong Kong SAR","China, Macao SAR","China, Mainland","China, Taiwan Province of","Democratic People's Republic of Korea","Indonesia","Japan","Lao People's Democratic Republic","Malaysia","Mongolia","Myanmar","Philippines","Republic of Korea","Singapore","Thailand","Timor-Leste","Viet Nam")
UN_countries_codes$South_and_South_East_Asia[97] <- c("South and South East Asia") #According to UN, Eastern Asia and South_eastern Asia included: c("Brunei Darussalam","Cambodia","China","China, Hong Kong SAR","China, Macao SAR","China, Mainland","China, Taiwan Province of","Democratic People's Republic of Korea","Indonesia","Japan","Lao People's Democratic Republic","Malaysia","Mongolia","Myanmar","Philippines","Republic of Korea","Singapore","Thailand","Timor-Leste","Viet Nam")
UN_countries_codes$South_and_South_East_Asia[128] <- c("South and South East Asia") #According to UN, Eastern Asia and South_eastern Asia included: c("Brunei Darussalam","Cambodia","China","China, Hong Kong SAR","China, Macao SAR","China, Mainland","China, Taiwan Province of","Democratic People's Republic of Korea","Indonesia","Japan","Lao People's Democratic Republic","Malaysia","Mongolia","Myanmar","Philippines","Republic of Korea","Singapore","Thailand","Timor-Leste","Viet Nam")
UN_countries_codes$South_and_South_East_Asia[213] <- c("South and South East Asia") #According to UN, Eastern Asia and South_eastern Asia included: c("Brunei Darussalam","Cambodia","China","China, Hong Kong SAR","China, Macao SAR","China, Mainland","China, Taiwan Province of","Democratic People's Republic of Korea","Indonesia","Japan","Lao People's Democratic Republic","Malaysia","Mongolia","Myanmar","Philippines","Republic of Korea","Singapore","Thailand","Timor-Leste","Viet Nam")
UN_countries_codes$South_and_South_East_Asia[115] <- c("South and South East Asia") #According to UN, Eastern Asia and South_eastern Asia included: c("Brunei Darussalam","Cambodia","China","China, Hong Kong SAR","China, Macao SAR","China, Mainland","China, Taiwan Province of","Democratic People's Republic of Korea","Indonesia","Japan","Lao People's Democratic Republic","Malaysia","Mongolia","Myanmar","Philippines","Republic of Korea","Singapore","Thailand","Timor-Leste","Viet Nam")
UN_countries_codes$South_and_South_East_Asia[116] <- c("South and South East Asia") #According to UN, Eastern Asia and South_eastern Asia included: c("Brunei Darussalam","Cambodia","China","China, Hong Kong SAR","China, Macao SAR","China, Mainland","China, Taiwan Province of","Democratic People's Republic of Korea","Indonesia","Japan","Lao People's Democratic Republic","Malaysia","Mongolia","Myanmar","Philippines","Republic of Korea","Singapore","Thailand","Timor-Leste","Viet Nam")
UN_countries_codes$South_and_South_East_Asia[101] <- c("South and South East Asia") #According to UN, Eastern Asia and South_eastern Asia included: c("Brunei Darussalam","Cambodia","China","China, Hong Kong SAR","China, Macao SAR","China, Mainland","China, Taiwan Province of","Democratic People's Republic of Korea","Indonesia","Japan","Lao People's Democratic Republic","Malaysia","Mongolia","Myanmar","Philippines","Republic of Korea","Singapore","Thailand","Timor-Leste","Viet Nam")
UN_countries_codes$South_and_South_East_Asia[109] <- c("South and South East Asia") #According to UN, Eastern Asia and South_eastern Asia included: c("Brunei Darussalam","Cambodia","China","China, Hong Kong SAR","China, Macao SAR","China, Mainland","China, Taiwan Province of","Democratic People's Republic of Korea","Indonesia","Japan","Lao People's Democratic Republic","Malaysia","Mongolia","Myanmar","Philippines","Republic of Korea","Singapore","Thailand","Timor-Leste","Viet Nam")
UN_countries_codes$South_and_South_East_Asia[119] <- c("South and South East Asia") #According to UN, Eastern Asia and South_eastern Asia included: c("Brunei Darussalam","Cambodia","China","China, Hong Kong SAR","China, Macao SAR","China, Mainland","China, Taiwan Province of","Democratic People's Republic of Korea","Indonesia","Japan","Lao People's Democratic Republic","Malaysia","Mongolia","Myanmar","Philippines","Republic of Korea","Singapore","Thailand","Timor-Leste","Viet Nam")
UN_countries_codes$South_and_South_East_Asia[132] <- c("South and South East Asia") #According to UN, Eastern Asia and South_eastern Asia included: c("Brunei Darussalam","Cambodia","China","China, Hong Kong SAR","China, Macao SAR","China, Mainland","China, Taiwan Province of","Democratic People's Republic of Korea","Indonesia","Japan","Lao People's Democratic Republic","Malaysia","Mongolia","Myanmar","Philippines","Republic of Korea","Singapore","Thailand","Timor-Leste","Viet Nam")
UN_countries_codes$South_and_South_East_Asia[145] <- c("South and South East Asia") #According to UN, Eastern Asia and South_eastern Asia included: c("Brunei Darussalam","Cambodia","China","China, Hong Kong SAR","China, Macao SAR","China, Mainland","China, Taiwan Province of","Democratic People's Republic of Korea","Indonesia","Japan","Lao People's Democratic Republic","Malaysia","Mongolia","Myanmar","Philippines","Republic of Korea","Singapore","Thailand","Timor-Leste","Viet Nam")
UN_countries_codes$South_and_South_East_Asia[150] <- c("South and South East Asia") #According to UN, Eastern Asia and South_eastern Asia included: c("Brunei Darussalam","Cambodia","China","China, Hong Kong SAR","China, Macao SAR","China, Mainland","China, Taiwan Province of","Democratic People's Republic of Korea","Indonesia","Japan","Lao People's Democratic Republic","Malaysia","Mongolia","Myanmar","Philippines","Republic of Korea","Singapore","Thailand","Timor-Leste","Viet Nam")
UN_countries_codes$South_and_South_East_Asia[173] <- c("South and South East Asia") #According to UN, Eastern Asia and South_eastern Asia included: c("Brunei Darussalam","Cambodia","China","China, Hong Kong SAR","China, Macao SAR","China, Mainland","China, Taiwan Province of","Democratic People's Republic of Korea","Indonesia","Japan","Lao People's Democratic Republic","Malaysia","Mongolia","Myanmar","Philippines","Republic of Korea","Singapore","Thailand","Timor-Leste","Viet Nam")
UN_countries_codes$South_and_South_East_Asia[196] <- c("South and South East Asia") #According to UN, Eastern Asia and South_eastern Asia included: c("Brunei Darussalam","Cambodia","China","China, Hong Kong SAR","China, Macao SAR","China, Mainland","China, Taiwan Province of","Democratic People's Republic of Korea","Indonesia","Japan","Lao People's Democratic Republic","Malaysia","Mongolia","Myanmar","Philippines","Republic of Korea","Singapore","Thailand","Timor-Leste","Viet Nam")
UN_countries_codes$South_and_South_East_Asia[216] <- c("South and South East Asia") #According to UN, Eastern Asia and South_eastern Asia included: c("Brunei Darussalam","Cambodia","China","China, Hong Kong SAR","China, Macao SAR","China, Mainland","China, Taiwan Province of","Democratic People's Republic of Korea","Indonesia","Japan","Lao People's Democratic Republic","Malaysia","Mongolia","Myanmar","Philippines","Republic of Korea","Singapore","Thailand","Timor-Leste","Viet Nam")
UN_countries_codes$South_and_South_East_Asia[217] <- c("South and South East Asia") #According to UN, Eastern Asia and South_eastern Asia included: c("Brunei Darussalam","Cambodia","China","China, Hong Kong SAR","China, Macao SAR","China, Mainland","China, Taiwan Province of","Democratic People's Republic of Korea","Indonesia","Japan","Lao People's Democratic Republic","Malaysia","Mongolia","Myanmar","Philippines","Republic of Korea","Singapore","Thailand","Timor-Leste","Viet Nam")
UN_countries_codes$South_and_South_East_Asia[237] <- c("South and South East Asia") #According to UN, Eastern Asia and South_eastern Asia included: c("Brunei Darussalam","Cambodia","China","China, Hong Kong SAR","China, Macao SAR","China, Mainland","China, Taiwan Province of","Democratic People's Republic of Korea","Indonesia","Japan","Lao People's Democratic Republic","Malaysia","Mongolia","Myanmar","Philippines","Republic of Korea","Singapore","Thailand","Timor-Leste","Viet Nam")

UN_countries_codes$Australia[13] <- c("Australia")

UN_countries_codes$United_States_of_America[231] <- c("United States of America")
UN_countries_codes$United_States_of_America[232] <- c("United States of America")
UN_countries_codes$United_States_of_America[239] <- c("United States of America")

UN_countries_codes$European_Union_and_United_Kingdom[14] <- c("European Union and United Kingdom") #According to UN, European Union (27) and United Kingdom included: c("Austria","Belgium","Belgium-Luxembourg","Bulgaria","Croatia","Cyprus","Czechia","Denmark","Estonia","Finland","France","Greece","Hungary","Ireland","Italy","Latvia","Lithuania","Luxembourg","Malta","Netherlands","Poland","Portugal","Romania","Slovakia","Slovenia","Spain","Sweden","United Kingdom")
UN_countries_codes$European_Union_and_United_Kingdom[21] <- c("European Union and United Kingdom") #According to UN, European Union (27) and United Kingdom included: c("Austria","Belgium","Belgium-Luxembourg","Bulgaria","Croatia","Cyprus","Czechia","Denmark","Estonia","Finland","France","Greece","Hungary","Ireland","Italy","Latvia","Lithuania","Luxembourg","Malta","Netherlands","Poland","Portugal","Romania","Slovakia","Slovenia","Spain","Sweden","United Kingdom")
UN_countries_codes$European_Union_and_United_Kingdom[33] <- c("European Union and United Kingdom") #According to UN, European Union (27) and United Kingdom included: c("Austria","Belgium","Belgium-Luxembourg","Bulgaria","Croatia","Cyprus","Czechia","Denmark","Estonia","Finland","France","Greece","Hungary","Ireland","Italy","Latvia","Lithuania","Luxembourg","Malta","Netherlands","Poland","Portugal","Romania","Slovakia","Slovenia","Spain","Sweden","United Kingdom")
UN_countries_codes$European_Union_and_United_Kingdom[54] <- c("European Union and United Kingdom") #According to UN, European Union (27) and United Kingdom included: c("Austria","Belgium","Belgium-Luxembourg","Bulgaria","Croatia","Cyprus","Czechia","Denmark","Estonia","Finland","France","Greece","Hungary","Ireland","Italy","Latvia","Lithuania","Luxembourg","Malta","Netherlands","Poland","Portugal","Romania","Slovakia","Slovenia","Spain","Sweden","United Kingdom")
UN_countries_codes$European_Union_and_United_Kingdom[56] <- c("European Union and United Kingdom") #According to UN, European Union (27) and United Kingdom included: c("Austria","Belgium","Belgium-Luxembourg","Bulgaria","Croatia","Cyprus","Czechia","Denmark","Estonia","Finland","France","Greece","Hungary","Ireland","Italy","Latvia","Lithuania","Luxembourg","Malta","Netherlands","Poland","Portugal","Romania","Slovakia","Slovenia","Spain","Sweden","United Kingdom")
UN_countries_codes$European_Union_and_United_Kingdom[57] <- c("European Union and United Kingdom") #According to UN, European Union (27) and United Kingdom included: c("Austria","Belgium","Belgium-Luxembourg","Bulgaria","Croatia","Cyprus","Czechia","Denmark","Estonia","Finland","France","Greece","Hungary","Ireland","Italy","Latvia","Lithuania","Luxembourg","Malta","Netherlands","Poland","Portugal","Romania","Slovakia","Slovenia","Spain","Sweden","United Kingdom")
UN_countries_codes$European_Union_and_United_Kingdom[58] <- c("European Union and United Kingdom") #According to UN, European Union (27) and United Kingdom included: c("Austria","Belgium","Belgium-Luxembourg","Bulgaria","Croatia","Cyprus","Czechia","Denmark","Estonia","Finland","France","Greece","Hungary","Ireland","Italy","Latvia","Lithuania","Luxembourg","Malta","Netherlands","Poland","Portugal","Romania","Slovakia","Slovenia","Spain","Sweden","United Kingdom")
UN_countries_codes$European_Union_and_United_Kingdom[67] <- c("European Union and United Kingdom") #According to UN, European Union (27) and United Kingdom included: c("Austria","Belgium","Belgium-Luxembourg","Bulgaria","Croatia","Cyprus","Czechia","Denmark","Estonia","Finland","France","Greece","Hungary","Ireland","Italy","Latvia","Lithuania","Luxembourg","Malta","Netherlands","Poland","Portugal","Romania","Slovakia","Slovenia","Spain","Sweden","United Kingdom")
UN_countries_codes$European_Union_and_United_Kingdom[72] <- c("European Union and United Kingdom") #According to UN, European Union (27) and United Kingdom included: c("Austria","Belgium","Belgium-Luxembourg","Bulgaria","Croatia","Cyprus","Czechia","Denmark","Estonia","Finland","France","Greece","Hungary","Ireland","Italy","Latvia","Lithuania","Luxembourg","Malta","Netherlands","Poland","Portugal","Romania","Slovakia","Slovenia","Spain","Sweden","United Kingdom")
UN_countries_codes$European_Union_and_United_Kingdom[73] <- c("European Union and United Kingdom") #According to UN, European Union (27) and United Kingdom included: c("Austria","Belgium","Belgium-Luxembourg","Bulgaria","Croatia","Cyprus","Czechia","Denmark","Estonia","Finland","France","Greece","Hungary","Ireland","Italy","Latvia","Lithuania","Luxembourg","Malta","Netherlands","Poland","Portugal","Romania","Slovakia","Slovenia","Spain","Sweden","United Kingdom")
UN_countries_codes$European_Union_and_United_Kingdom[83] <- c("European Union and United Kingdom") #According to UN, European Union (27) and United Kingdom included: c("Austria","Belgium","Belgium-Luxembourg","Bulgaria","Croatia","Cyprus","Czechia","Denmark","Estonia","Finland","France","Greece","Hungary","Ireland","Italy","Latvia","Lithuania","Luxembourg","Malta","Netherlands","Poland","Portugal","Romania","Slovakia","Slovenia","Spain","Sweden","United Kingdom")
UN_countries_codes$European_Union_and_United_Kingdom[98] <- c("European Union and United Kingdom") #According to UN, European Union (27) and United Kingdom included: c("Austria","Belgium","Belgium-Luxembourg","Bulgaria","Croatia","Cyprus","Czechia","Denmark","Estonia","Finland","France","Greece","Hungary","Ireland","Italy","Latvia","Lithuania","Luxembourg","Malta","Netherlands","Poland","Portugal","Romania","Slovakia","Slovenia","Spain","Sweden","United Kingdom")
UN_countries_codes$European_Union_and_United_Kingdom[104] <- c("European Union and United Kingdom") #According to UN, European Union (27) and United Kingdom included: c("Austria","Belgium","Belgium-Luxembourg","Bulgaria","Croatia","Cyprus","Czechia","Denmark","Estonia","Finland","France","Greece","Hungary","Ireland","Italy","Latvia","Lithuania","Luxembourg","Malta","Netherlands","Poland","Portugal","Romania","Slovakia","Slovenia","Spain","Sweden","United Kingdom")
UN_countries_codes$European_Union_and_United_Kingdom[107] <- c("European Union and United Kingdom") #According to UN, European Union (27) and United Kingdom included: c("Austria","Belgium","Belgium-Luxembourg","Bulgaria","Croatia","Cyprus","Czechia","Denmark","Estonia","Finland","France","Greece","Hungary","Ireland","Italy","Latvia","Lithuania","Luxembourg","Malta","Netherlands","Poland","Portugal","Romania","Slovakia","Slovenia","Spain","Sweden","United Kingdom")
UN_countries_codes$European_Union_and_United_Kingdom[120] <- c("European Union and United Kingdom") #According to UN, European Union (27) and United Kingdom included: c("Austria","Belgium","Belgium-Luxembourg","Bulgaria","Croatia","Cyprus","Czechia","Denmark","Estonia","Finland","France","Greece","Hungary","Ireland","Italy","Latvia","Lithuania","Luxembourg","Malta","Netherlands","Poland","Portugal","Romania","Slovakia","Slovenia","Spain","Sweden","United Kingdom")
UN_countries_codes$European_Union_and_United_Kingdom[126] <- c("European Union and United Kingdom") #According to UN, European Union (27) and United Kingdom included: c("Austria","Belgium","Belgium-Luxembourg","Bulgaria","Croatia","Cyprus","Czechia","Denmark","Estonia","Finland","France","Greece","Hungary","Ireland","Italy","Latvia","Lithuania","Luxembourg","Malta","Netherlands","Poland","Portugal","Romania","Slovakia","Slovenia","Spain","Sweden","United Kingdom")
UN_countries_codes$European_Union_and_United_Kingdom[127] <- c("European Union and United Kingdom") #According to UN, European Union (27) and United Kingdom included: c("Austria","Belgium","Belgium-Luxembourg","Bulgaria","Croatia","Cyprus","Czechia","Denmark","Estonia","Finland","France","Greece","Hungary","Ireland","Italy","Latvia","Lithuania","Luxembourg","Malta","Netherlands","Poland","Portugal","Romania","Slovakia","Slovenia","Spain","Sweden","United Kingdom")
UN_countries_codes$European_Union_and_United_Kingdom[135] <- c("European Union and United Kingdom") #According to UN, European Union (27) and United Kingdom included: c("Austria","Belgium","Belgium-Luxembourg","Bulgaria","Croatia","Cyprus","Czechia","Denmark","Estonia","Finland","France","Greece","Hungary","Ireland","Italy","Latvia","Lithuania","Luxembourg","Malta","Netherlands","Poland","Portugal","Romania","Slovakia","Slovenia","Spain","Sweden","United Kingdom")
UN_countries_codes$European_Union_and_United_Kingdom[154] <- c("European Union and United Kingdom") #According to UN, European Union (27) and United Kingdom included: c("Austria","Belgium","Belgium-Luxembourg","Bulgaria","Croatia","Cyprus","Czechia","Denmark","Estonia","Finland","France","Greece","Hungary","Ireland","Italy","Latvia","Lithuania","Luxembourg","Malta","Netherlands","Poland","Portugal","Romania","Slovakia","Slovenia","Spain","Sweden","United Kingdom")
UN_countries_codes$European_Union_and_United_Kingdom[175] <- c("European Union and United Kingdom") #According to UN, European Union (27) and United Kingdom included: c("Austria","Belgium","Belgium-Luxembourg","Bulgaria","Croatia","Cyprus","Czechia","Denmark","Estonia","Finland","France","Greece","Hungary","Ireland","Italy","Latvia","Lithuania","Luxembourg","Malta","Netherlands","Poland","Portugal","Romania","Slovakia","Slovenia","Spain","Sweden","United Kingdom")
UN_countries_codes$European_Union_and_United_Kingdom[176] <- c("European Union and United Kingdom") #According to UN, European Union (27) and United Kingdom included: c("Austria","Belgium","Belgium-Luxembourg","Bulgaria","Croatia","Cyprus","Czechia","Denmark","Estonia","Finland","France","Greece","Hungary","Ireland","Italy","Latvia","Lithuania","Luxembourg","Malta","Netherlands","Poland","Portugal","Romania","Slovakia","Slovenia","Spain","Sweden","United Kingdom")
UN_countries_codes$European_Union_and_United_Kingdom[180] <- c("European Union and United Kingdom") #According to UN, European Union (27) and United Kingdom included: c("Austria","Belgium","Belgium-Luxembourg","Bulgaria","Croatia","Cyprus","Czechia","Denmark","Estonia","Finland","France","Greece","Hungary","Ireland","Italy","Latvia","Lithuania","Luxembourg","Malta","Netherlands","Poland","Portugal","Romania","Slovakia","Slovenia","Spain","Sweden","United Kingdom")
UN_countries_codes$European_Union_and_United_Kingdom[197] <- c("European Union and United Kingdom") #According to UN, European Union (27) and United Kingdom included: c("Austria","Belgium","Belgium-Luxembourg","Bulgaria","Croatia","Cyprus","Czechia","Denmark","Estonia","Finland","France","Greece","Hungary","Ireland","Italy","Latvia","Lithuania","Luxembourg","Malta","Netherlands","Poland","Portugal","Romania","Slovakia","Slovenia","Spain","Sweden","United Kingdom")
UN_countries_codes$European_Union_and_United_Kingdom[198] <- c("European Union and United Kingdom") #According to UN, European Union (27) and United Kingdom included: c("Austria","Belgium","Belgium-Luxembourg","Bulgaria","Croatia","Cyprus","Czechia","Denmark","Estonia","Finland","France","Greece","Hungary","Ireland","Italy","Latvia","Lithuania","Luxembourg","Malta","Netherlands","Poland","Portugal","Romania","Slovakia","Slovenia","Spain","Sweden","United Kingdom")
UN_countries_codes$European_Union_and_United_Kingdom[204] <- c("European Union and United Kingdom") #According to UN, European Union (27) and United Kingdom included: c("Austria","Belgium","Belgium-Luxembourg","Bulgaria","Croatia","Cyprus","Czechia","Denmark","Estonia","Finland","France","Greece","Hungary","Ireland","Italy","Latvia","Lithuania","Luxembourg","Malta","Netherlands","Poland","Portugal","Romania","Slovakia","Slovenia","Spain","Sweden","United Kingdom")
UN_countries_codes$European_Union_and_United_Kingdom[210] <- c("European Union and United Kingdom") #According to UN, European Union (27) and United Kingdom included: c("Austria","Belgium","Belgium-Luxembourg","Bulgaria","Croatia","Cyprus","Czechia","Denmark","Estonia","Finland","France","Greece","Hungary","Ireland","Italy","Latvia","Lithuania","Luxembourg","Malta","Netherlands","Poland","Portugal","Romania","Slovakia","Slovenia","Spain","Sweden","United Kingdom")
UN_countries_codes$European_Union_and_United_Kingdom[230] <- c("European Union and United Kingdom") #According to UN, European Union (27) and United Kingdom included: c("Austria","Belgium","Belgium-Luxembourg","Bulgaria","Croatia","Cyprus","Czechia","Denmark","Estonia","Finland","France","Greece","Hungary","Ireland","Italy","Latvia","Lithuania","Luxembourg","Malta","Netherlands","Poland","Portugal","Romania","Slovakia","Slovenia","Spain","Sweden","United Kingdom")

UN_countries_codes$India_Odisha[100] <- c("India_Odisha")

UN_countries_codes$Atlantic_Zone_of_Costa_Rica[52] <- c("Atlantic Zone of Costa Rica")

UN_countries_codes$North_Macedonia[129] <- c("North Macedonia")

UN_countries_codes$North_America[38] <- c("North America")
UN_countries_codes$North_America[231] <- c("North America")
UN_countries_codes$North_America[232] <- c("North America")
UN_countries_codes$North_America[239] <- c("North America")

UN_countries_codes$India[100] <- c("India")

UN_countries_codes$Australia_New_South_Wales_South_East[13] <- c("Australia New South Wales South East")
UN_countries_codes$Australia_New_South_Wales_South_West[13] <- c("Australia New South Wales South West")
UN_countries_codes$Australia_South_Australia_Lower_EP[13] <- c("Australia South Australia Lower EP")
UN_countries_codes$Australia_South_Australia_Mid_North[13] <- c("Australia South Australia Mid North")
UN_countries_codes$Australia_South_Australia_Murray_Mallee[13] <- c("Australia South Australia Murray Mallee")
UN_countries_codes$Australia_South_Australia_South_East[13] <- c("Australia South Australia South East")
UN_countries_codes$Australia_South_Australia_Upper_EP[13] <- c("Australia South Australia Upper EP")
UN_countries_codes$Australia_South_Australia_Yorke_Penn.[13] <- c("Australia South Australia Yorke Penn.")
UN_countries_codes$Australia_Victoria_Mallee[13] <- c("Australia Victoria Mallee")
UN_countries_codes$Australia_Victoria_North_Central[13] <- c("Australia Victoria North Central")
UN_countries_codes$Australia_Victoria_North_East[13] <- c("Australia Victoria North East")
UN_countries_codes$Australia_Victoria_Wimmera[13] <- c("Australia Victoria Wimmera")

UN_countries_codes$Ethiopia[68] <- c("Ethiopia")

UN_countries_codes$Malawi[131] <- c("Malawi")

#Create dataframe which shows countries in each Original_region
#Deselect extraneous columns
UN_country_codes_summary <- select(UN_countries_codes,
                                   -`Alpha-2_code`,
                                   -`Alpha-3_code`,
                                   -Numeric_code,
                                   -Latitude_average,
                                   -Longitude_average)

#Create function to convert Region name to country name if a region name is in cell.
A <- function(x) ifelse(is.na(x),x,UN_country_codes_summary$Country)
ncol_df <- ncol(UN_country_codes_summary)

#Apply function to data frame and delete obsolete Country column
UN_country_codes_summary <- UN_country_codes_summary %>% mutate(across(2:all_of(ncol_df),A)) %>% select(-Country)

#Transpose data frame
Transposed_UN_country_codes_summary <-as.data.frame(t(UN_country_codes_summary))
Transposed_UN_country_codes_summary <- cbind(rownames(Transposed_UN_country_codes_summary), data.frame(Transposed_UN_country_codes_summary, row.names=NULL))

#Create column with only the terms Crop_products and Crop_residues----
#Create dataframe with unique Original_crop-Component names
Unique_original_crop_component <- as.data.frame(unique(Crop_df$Original_crop_component)) %>% rename(Original_crop_component="unique(Crop_df$Original_crop_component)") %>% 
  arrange( Original_crop_component, desc())

#Create column with Crop_component information
Unique_original_crop_component <- Unique_original_crop_component %>% mutate(Crop_component=case_when(
  str_detect(Original_crop_component,regex("bulbs|burley|cane|capsules|cob|crop product|curd|crop_products|fruit|grain|head|nut|pea|pulse|root|seed|tuber|vegetable", ignore_case=TRUE))~"Crop_products",
  str_detect(Original_crop_component,regex("bagasse|burs|crop residue|crop_residues|dry fibre|empty bunches|floral parts|hay|husk|lint|shell|stalk|stem|stover|straw|sweet, vine|tops|trash|vine|white, vine", ignore_case=TRUE))~"Crop_residues",
  TRUE~Original_crop_component))

#Merge with main data frame
Crop_df <- merge(Crop_df, Unique_original_crop_component)

#Note there is ambiguity over whether Leaves or Fibre (as Original_crop_component) were Crop_products of Crop_residues, and this depended on crop type.
#For 'fibre' Crop_products should be for: Jute olitorious, Jute capsularis.
#For 'fibre' or 'Fibre' Crop_residues should be for: Coffee arabica, Oil palm
#For 'leaves' Crop_products should be for: Tobacco, flue cured, Tobacco, Tea, Guinea grass, Berseem, and Kenaf
#For 'leaves' Crop_residues should be for: Onions, Jute, and Sugarcane. 
#In addition, 'All Crops' from Conant et al relate to proportion of crop residues removed from harvested area so this can be given 'Crop_residues' in the Crop_component column.

#These were changed in the data frame accordingly
#Create column with Original_crop and Original_crop_component together.
Crop_df$Original_crop_Original_crop_component <- paste(Crop_df$Original_crop, Crop_df$Crop_component)

Crop_df <- Crop_df %>% mutate(Crop_component=case_when(
  str_detect(Original_crop_Original_crop_component,regex("Jute olitorious fibre|Jute capsularis fibre|Tobacco, flue-cured leaves|Tea leaves|Guinea grass leaves|Berseem leaves|Kenaf leaves|Tobacco Leaves", ignore_case=TRUE))~"Crop_products",
  str_detect(Original_crop_Original_crop_component,regex("Coffee arabica fibre|Oil palm fibre|Onions leaves|Jute leaves|Sugarcane leaves|All_crops NA", ignore_case=TRUE))~"Crop_residues",
  TRUE~Crop_component))

#Deselect Original_crop_Original_crop_component column
Crop_df <- select(Crop_df, 
                  -Original_crop_Original_crop_component)

#Create standardised UN country code columns----
#Create a function to convert country names into UN standardised names and regions. 
Crop_df$ISO3_CODE <- countrycode(Crop_df$Original_region, origin = 'country.name', destination = 'iso3c')
Crop_df$Country_iso3c <- countrycode(Crop_df$ISO3_CODE, origin = 'iso3c', destination = 'country.name')
Crop_df$UN_region_name <- countrycode(Crop_df$ISO3_CODE, origin = 'iso3c', destination = 'un.region.name')
Crop_df$UN_sub_region_name <- countrycode(Crop_df$ISO3_CODE, origin = 'iso3c', destination = 'un.regionsub.name')
Crop_df$UN_intermediate_region_name <- countrycode(Crop_df$ISO3_CODE, origin = 'iso3c', destination = 'un.regionintermediate.name')

#Create column with UN equivalent of the 'Item Code' category. 
#Create dataframe with unique Original_crop names to test that the function works ok and doesn't leave any NAs
Unique_original_crops <- as.data.frame(unique(Crop_df$Original_crop)) %>% rename(Original_crop="unique(Crop_df$Original_crop)") %>% 
  arrange( Original_crop, desc())

write.csv(Unique_original_crops,"data/Unique_original_crops.csv",row.names=FALSE)

#Add another column to show UN equivalent 'Item_Code' category.----
#Create function to create new column with UN Item_Codes
Original_crop_to_item_code_converter <- function (df,Original_crop){
  df <- df %>%mutate(item_code = case_when(
  str_detect(Original_crop,regex("Agave fibres nes", ignore_case=TRUE))~"800",
  str_detect(Original_crop,regex("^Alfalfa$", ignore_case=TRUE))~"643",
  str_detect(Original_crop,regex("^Alfalfa  \\(DM\\)$", ignore_case=TRUE))~"643",
  str_detect(Original_crop,regex("^Alfalfa for forage$", ignore_case=TRUE))~"643",
  str_detect(Original_crop,regex("^Alfalfa for forage and silage$", ignore_case=TRUE))~"643",  
  str_detect(Original_crop,regex("^Alfalfa haylage \\(50% dm\\)$", ignore_case=TRUE))~"643",  
  str_detect(Original_crop,regex("^Alfalfa Meal and Pellets$", ignore_case=TRUE))~"1892",
  str_detect(Original_crop,regex("^All_crops$", ignore_case=TRUE))~"",  
  str_detect(Original_crop,regex("^Almonds Shelled$", ignore_case=TRUE))~"231",  
  str_detect(Original_crop,regex("^Almonds, with shell$", ignore_case=TRUE))~"221",
  str_detect(Original_crop,regex("^Almonds; with shell$", ignore_case=TRUE))~"221",
  str_detect(Original_crop,regex("^Alsike Clover \\(DM\\)$", ignore_case=TRUE))~"640",  
  str_detect(Original_crop,regex("^Anise, badian, fennel, corian.$", ignore_case=TRUE))~"711",
  str_detect(Original_crop,regex("^Anise, badian, fennel, coriander$", ignore_case=TRUE))~"711",
  str_detect(Original_crop,regex("^Anise; badian; fennel; corian.$", ignore_case=TRUE))~"711",  
  str_detect(Original_crop,regex("^Apple$", ignore_case=TRUE))~"515",
  str_detect(Original_crop,regex("^Apple juice, concentrated$", ignore_case=TRUE))~"519",
  str_detect(Original_crop,regex("^Apple juice, single strength$", ignore_case=TRUE))~"518",  
  str_detect(Original_crop,regex("^Apples$", ignore_case=TRUE))~"515",
  str_detect(Original_crop,regex("^Apricots$", ignore_case=TRUE))~"526",
  str_detect(Original_crop,regex("^Arecanuts$", ignore_case=TRUE))~"226",  
  str_detect(Original_crop,regex("^Arrowweed$", ignore_case=TRUE))~"",
  str_detect(Original_crop,regex("^Artichokes$", ignore_case=TRUE))~"366",
  str_detect(Original_crop,regex("^Asparagus$", ignore_case=TRUE))~"367",  
  str_detect(Original_crop,regex("^Avocados$", ignore_case=TRUE))~"572",
  str_detect(Original_crop,regex("^Azolla$", ignore_case=TRUE))~"",
  str_detect(Original_crop,regex("^Bacon and Ham$", ignore_case=TRUE))~"1039",  
  str_detect(Original_crop,regex("^Bagasse$", ignore_case=TRUE))~"156",
  str_detect(Original_crop,regex("^Bahiagrass$", ignore_case=TRUE))~"639",
  str_detect(Original_crop,regex("^Bajra grass$", ignore_case=TRUE))~"639",  
  str_detect(Original_crop,regex("^Bajra residue$", ignore_case=TRUE))~"639",
  str_detect(Original_crop,regex("^Bambara beans$", ignore_case=TRUE))~"203",
  str_detect(Original_crop,regex("^Banana$", ignore_case=TRUE))~"486",  
  str_detect(Original_crop,regex("^Bananas$", ignore_case=TRUE))~"486",
  str_detect(Original_crop,regex("^barley$", ignore_case=TRUE))~"44",
  str_detect(Original_crop,regex("^Barley$", ignore_case=TRUE))~"44",  
  str_detect(Original_crop,regex("^Barley Flour and Grits$", ignore_case=TRUE))~"48",
  str_detect(Original_crop,regex("^Barley grain$", ignore_case=TRUE))~"44",
  str_detect(Original_crop,regex("^Barley Pearled$", ignore_case=TRUE))~"46",  
  str_detect(Original_crop,regex("^Barley straw$", ignore_case=TRUE))~"44",  
  str_detect(Original_crop,regex("^Barley straw per t of grain$", ignore_case=TRUE))~"44",
  str_detect(Original_crop,regex("^Bastfibres, other$", ignore_case=TRUE))~"782",  
  str_detect(Original_crop,regex("^Bean, dry$", ignore_case=TRUE))~"176",
  str_detect(Original_crop,regex("^Bean, lima$", ignore_case=TRUE))~"176",
  str_detect(Original_crop,regex("^Bean, mung$", ignore_case=TRUE))~"176",  
  str_detect(Original_crop,regex("^Beans \\(dry\\)$", ignore_case=TRUE))~"176",
  str_detect(Original_crop,regex("^Beans and Pulses$", ignore_case=TRUE))~"1954",
  str_detect(Original_crop,regex("^Beans, dry$", ignore_case=TRUE))~"176",  
  str_detect(Original_crop,regex("^Beans, green$", ignore_case=TRUE))~"414",
  str_detect(Original_crop,regex("^Beans; dry$", ignore_case=TRUE))~"176",
  str_detect(Original_crop,regex("^Beans; green$", ignore_case=TRUE))~"414",  
  str_detect(Original_crop,regex("^Beer of Barley$", ignore_case=TRUE))~"51",
  str_detect(Original_crop,regex("^Beer of Sorghum$", ignore_case=TRUE))~"86",
  str_detect(Original_crop,regex("^Beeswax$", ignore_case=TRUE))~"1183",  
  str_detect(Original_crop,regex("^Beet$", ignore_case=TRUE))~"157",
  str_detect(Original_crop,regex("^Beet Pulp$", ignore_case=TRUE))~"169",
  str_detect(Original_crop,regex("^Beet, for forage$", ignore_case=TRUE))~"641",  
  str_detect(Original_crop,regex("^Beets for Fodder$", ignore_case=TRUE))~"1892",
  str_detect(Original_crop,regex("^Bell peppers$", ignore_case=TRUE))~"401",
  str_detect(Original_crop,regex("^Bentgrass$", ignore_case=TRUE))~"639",  
  str_detect(Original_crop,regex("^Bermuda grass$", ignore_case=TRUE))~"639",
  str_detect(Original_crop,regex("^Bermudagrass$", ignore_case=TRUE))~"639",
  str_detect(Original_crop,regex("^Berries Nes$", ignore_case=TRUE))~"558",  
  str_detect(Original_crop,regex("^Berseem$", ignore_case=TRUE))~"640",
  str_detect(Original_crop,regex("^Bever. Dist.Alc$", ignore_case=TRUE))~"634",
  str_detect(Original_crop,regex("^Beverage Non-Alc$", ignore_case=TRUE))~"633",  
  str_detect(Original_crop,regex("^Big bluestem$", ignore_case=TRUE))~"639",
  str_detect(Original_crop,regex("^Bird meat, nes$", ignore_case=TRUE))~"1166",
  str_detect(Original_crop,regex("^Birdsfoot trefoil$", ignore_case=TRUE))~"643",  
  str_detect(Original_crop,regex("^Birdsfoot trefoil \\(DM\\)$", ignore_case=TRUE))~"643",
  str_detect(Original_crop,regex("^Black gram$", ignore_case=TRUE))~"191",
  str_detect(Original_crop,regex("^Black gram bran$", ignore_case=TRUE))~"213",  
  str_detect(Original_crop,regex("^Black gram residue$", ignore_case=TRUE))~"",  
  str_detect(Original_crop,regex("^Blueberries$", ignore_case=TRUE))~"552",
  str_detect(Original_crop,regex("^Bluegrass$", ignore_case=TRUE))~"639",  
  str_detect(Original_crop,regex("^Bluegrass-pastd.$", ignore_case=TRUE))~"639",
  str_detect(Original_crop,regex("^Bluegrass \\(DM\\)$", ignore_case=TRUE))~"639",
  str_detect(Original_crop,regex("^Brachiaria mutica$", ignore_case=TRUE))~"639",  
  str_detect(Original_crop,regex("^Bran Buckwheat$", ignore_case=TRUE))~"91",
  str_detect(Original_crop,regex("^Bran of Barley$", ignore_case=TRUE))~"91",
  str_detect(Original_crop,regex("^Bran of Cereals$", ignore_case=TRUE))~"112",  
  str_detect(Original_crop,regex("^Bran of Fonio$", ignore_case=TRUE))~"96",
  str_detect(Original_crop,regex("^Bran of Maize$", ignore_case=TRUE))~"59",
  str_detect(Original_crop,regex("^Bran of Millet$", ignore_case=TRUE))~"81",  
  str_detect(Original_crop,regex("^Bran of Mixed Grains$", ignore_case=TRUE))~"105",
  str_detect(Original_crop,regex("^Bran of Oats$", ignore_case=TRUE))~"77",
  str_detect(Original_crop,regex("^Bran of Pulses$", ignore_case=TRUE))~"213",  
  str_detect(Original_crop,regex("^Bran of Rice$", ignore_case=TRUE))~"35",
  str_detect(Original_crop,regex("^Bran of Rye$", ignore_case=TRUE))~"73",
  str_detect(Original_crop,regex("^Bran of Sorghum$", ignore_case=TRUE))~"85",  
  str_detect(Original_crop,regex("^Bran of Wheat$", ignore_case=TRUE))~"17",
  str_detect(Original_crop,regex("^Brazil Nuts Shelled$", ignore_case=TRUE))~"229",
  str_detect(Original_crop,regex("^Brazil nuts, with shell$", ignore_case=TRUE))~"216",  
  str_detect(Original_crop,regex("^Brazil nuts; with shell$", ignore_case=TRUE))~"216",
  str_detect(Original_crop,regex("^Bread$", ignore_case=TRUE))~"20",
  str_detect(Original_crop,regex("^Breakfast Cereals$", ignore_case=TRUE))~"41",
  str_detect(Original_crop,regex("^Broad beans$", ignore_case=TRUE))~"181",
  str_detect(Original_crop,regex("^Broad beans, horse beans, dry$", ignore_case=TRUE))~"181",
  str_detect(Original_crop,regex("^Broad beans; horse beans; dry$", ignore_case=TRUE))~"181",
  str_detect(Original_crop,regex("^Broken rice$", ignore_case=TRUE))~"32",
  str_detect(Original_crop,regex("^Bromegrass$", ignore_case=TRUE))~"639",
  str_detect(Original_crop,regex("^Bromegrass  \\(DM\\)$", ignore_case=TRUE))~"639",
  str_detect(Original_crop,regex("^Brown mustard$", ignore_case=TRUE))~"292",
  str_detect(Original_crop,regex("^Brown rice$", ignore_case=TRUE))~"27",
  str_detect(Original_crop,regex("^Buckwheat$", ignore_case=TRUE))~"89",
  str_detect(Original_crop,regex("^Buffalo meat$", ignore_case=TRUE))~"1806",
  str_detect(Original_crop,regex("^Bulgur$", ignore_case=TRUE))~"21",
  str_detect(Original_crop,regex("^Butter Cow Milk$", ignore_case=TRUE))~"886",
  str_detect(Original_crop,regex("^Butter of Karite Nuts$", ignore_case=TRUE))~"264",
  str_detect(Original_crop,regex("^Butter,Ghee of Sheep Milk$", ignore_case=TRUE))~"983",
  str_detect(Original_crop,regex("^Butterm.,Curdl,Acid.Milk$", ignore_case=TRUE))~"893",
  str_detect(Original_crop,regex("^Cabbage$", ignore_case=TRUE))~"358",
  str_detect(Original_crop,regex("^Cabbage, for forage$", ignore_case=TRUE))~"358",
  str_detect(Original_crop,regex("^Cabbages and other brassicas$", ignore_case=TRUE))~"358",
  str_detect(Original_crop,regex("^Cake of Copra$", ignore_case=TRUE))~"253",
  str_detect(Original_crop,regex("^Cake of Cottonseed$", ignore_case=TRUE))~"332",
  str_detect(Original_crop,regex("^Cake of Groundnuts$", ignore_case=TRUE))~"245",
  str_detect(Original_crop,regex("^Cake of Hempseed$", ignore_case=TRUE))~"338",
  str_detect(Original_crop,regex("^Cake of Kapok$", ignore_case=TRUE))~"314",
  str_detect(Original_crop,regex("^Cake of Linseed$", ignore_case=TRUE))~"335",
  str_detect(Original_crop,regex("^Cake of Maize$", ignore_case=TRUE))~"61",
  str_detect(Original_crop,regex("^Cake of Mustard$", ignore_case=TRUE))~"294",
  str_detect(Original_crop,regex("^Cake of Oilseeds, Nes$", ignore_case=TRUE))~"",
  str_detect(Original_crop,regex("^Cake of Palm Kernel$", ignore_case=TRUE))~"259",
  str_detect(Original_crop,regex("^Cake of Rapeseed$", ignore_case=TRUE))~"272",
  str_detect(Original_crop,regex("^Cake of Sesame Seed$", ignore_case=TRUE))~"291",
  str_detect(Original_crop,regex("^Cake of Soybeans$", ignore_case=TRUE))~"238",
  str_detect(Original_crop,regex("^Cake Rice Bran$", ignore_case=TRUE))~"37",
  str_detect(Original_crop,regex("^Cake Safflower$", ignore_case=TRUE))~"282",
  str_detect(Original_crop,regex("^Camel meat$", ignore_case=TRUE))~"1164",
  str_detect(Original_crop,regex("^Canary seed$", ignore_case=TRUE))~"101",
  str_detect(Original_crop,regex("^Cane Tops$", ignore_case=TRUE))~"630",
  str_detect(Original_crop,regex("^Canned Mushrooms$", ignore_case=TRUE))~"491",
  str_detect(Original_crop,regex("^Canola$", ignore_case=TRUE))~"270",
  str_detect(Original_crop,regex("^Canola grain$", ignore_case=TRUE))~"271",
  str_detect(Original_crop,regex("^Cantaloupe$", ignore_case=TRUE))~"568",
  str_detect(Original_crop,regex("^Carobs$", ignore_case=TRUE))~"461",
  str_detect(Original_crop,regex("^Carrot$", ignore_case=TRUE))~"462",
  str_detect(Original_crop,regex("^Carrots$", ignore_case=TRUE))~"426",
  str_detect(Original_crop,regex("^Carrots and turnips$", ignore_case=TRUE))~"426",
  str_detect(Original_crop,regex("^Carrots, for forage$", ignore_case=TRUE))~"426",
  str_detect(Original_crop,regex("^Cashew Nuts Shelled$", ignore_case=TRUE))~"230",
  str_detect(Original_crop,regex("^Cashew nuts, with shell$", ignore_case=TRUE))~"217",
  str_detect(Original_crop,regex("^Cashew nuts; with shell$", ignore_case=TRUE))~"217",
  str_detect(Original_crop,regex("^Cashewapple$", ignore_case=TRUE))~"591",
  str_detect(Original_crop,regex("^Cassava$", ignore_case=TRUE))~"125",
  str_detect(Original_crop,regex("^Cassava Dried$", ignore_case=TRUE))~"125",
  str_detect(Original_crop,regex("^Cassava leaves$", ignore_case=TRUE))~"378",
  str_detect(Original_crop,regex("^Cassava Starch$", ignore_case=TRUE))~"129",
  str_detect(Original_crop,regex("^Castor$", ignore_case=TRUE))~"265",
  str_detect(Original_crop,regex("^Castor oil seed$", ignore_case=TRUE))~"265",
  str_detect(Original_crop,regex("^Cattails$", ignore_case=TRUE))~"",
  str_detect(Original_crop,regex("^Cattle Butch.Fat$", ignore_case=TRUE))~"869",
  str_detect(Original_crop,regex("^Cattle meat$", ignore_case=TRUE))~"867",
  str_detect(Original_crop,regex("^Cauliflower$", ignore_case=TRUE))~"393",
  str_detect(Original_crop,regex("^Cauliflowers and broccoli$", ignore_case=TRUE))~"393",
  str_detect(Original_crop,regex("^Celery$", ignore_case=TRUE))~"436",
  str_detect(Original_crop,regex("^Cereal Preparations, Nes$", ignore_case=TRUE))~"1941",
  str_detect(Original_crop,regex("^Cereals other$", ignore_case=TRUE))~"108",
  str_detect(Original_crop,regex("^Cereals, nes$", ignore_case=TRUE))~"108",
  str_detect(Original_crop,regex("^Cheese of Goat Mlk$", ignore_case=TRUE))~"1021",
  str_detect(Original_crop,regex("^Cheese of Sheep Milk$", ignore_case=TRUE))~"984",
  str_detect(Original_crop,regex("^Cheese of Skimmed Cow Milk$", ignore_case=TRUE))~"904",
  str_detect(Original_crop,regex("^Cheese of Whole Cow Milk$", ignore_case=TRUE))~"901",
  str_detect(Original_crop,regex("^Cherries$", ignore_case=TRUE))~"531",
  str_detect(Original_crop,regex("^Chestnuts$", ignore_case=TRUE))~"220",
  str_detect(Original_crop,regex("^Chick pea$", ignore_case=TRUE))~"191",
  str_detect(Original_crop,regex("^Chick peas$", ignore_case=TRUE))~"191",
  str_detect(Original_crop,regex("^Chicken meat$", ignore_case=TRUE))~"1058",
  str_detect(Original_crop,regex("^Chickpea$", ignore_case=TRUE))~"191",
  str_detect(Original_crop,regex("^Chickpea bran$", ignore_case=TRUE))~"213",
  str_detect(Original_crop,regex("^Chickpea dry residue$", ignore_case=TRUE))~"213",
  str_detect(Original_crop,regex("^Chickpea leaf$", ignore_case=TRUE))~"213",
  str_detect(Original_crop,regex("^Chickpea powder$", ignore_case=TRUE))~"213",
  str_detect(Original_crop,regex("^Chicory roots$", ignore_case=TRUE))~"459",
  str_detect(Original_crop,regex("^Chillies and peppers, dry$", ignore_case=TRUE))~"689",
  str_detect(Original_crop,regex("^Chillies and peppers, green$", ignore_case=TRUE))~"401",
  str_detect(Original_crop,regex("^Chillies and peppers; dry$", ignore_case=TRUE))~"689",
  str_detect(Original_crop,regex("^Chillies and peppers; green$", ignore_case=TRUE))~"401",
  str_detect(Original_crop,regex("^Chocolate Prsnes$", ignore_case=TRUE))~"2911",
  str_detect(Original_crop,regex("^Cider Etc$", ignore_case=TRUE))~"517",
  str_detect(Original_crop,regex("^Cigarettes$", ignore_case=TRUE))~"828",
  str_detect(Original_crop,regex("^Cigars Cheroots$", ignore_case=TRUE))~"517",
  str_detect(Original_crop,regex("^Cinnamon \\(canella\\)$", ignore_case=TRUE))~"693",
  str_detect(Original_crop,regex("^Citrus$", ignore_case=TRUE))~"512",
  str_detect(Original_crop,regex("^Citrus fruit, nes$", ignore_case=TRUE))~"512",
  str_detect(Original_crop,regex("^Citrus fruit; nes$", ignore_case=TRUE))~"512",
  str_detect(Original_crop,regex("^Citrus fruits$", ignore_case=TRUE))~"512",
  str_detect(Original_crop,regex("^Citrus juice, concentrated$", ignore_case=TRUE))~"514",
  str_detect(Original_crop,regex("^Citrus juice, single strength$", ignore_case=TRUE))~"513",
  str_detect(Original_crop,regex("^Clover-grass$", ignore_case=TRUE))~"641",
  str_detect(Original_crop,regex("^Clover for forage and silage$", ignore_case=TRUE))~"640",
  str_detect(Original_crop,regex("^Clover, for forage$", ignore_case=TRUE))~"640",
  str_detect(Original_crop,regex("^Cloves$", ignore_case=TRUE))~"698",
  str_detect(Original_crop,regex("^Cmpd Feed,Oth Or Nes$", ignore_case=TRUE))~"845",
  str_detect(Original_crop,regex("^Cmpd Feed,Pigs$", ignore_case=TRUE))~"845",
  str_detect(Original_crop,regex("^Cmpd Feed,Poultry$", ignore_case=TRUE))~"845",
  str_detect(Original_crop,regex("^Coarse Goat Hair$", ignore_case=TRUE))~"1031",
  str_detect(Original_crop,regex("^Coastal Bermuda$", ignore_case=TRUE))~"639",
  str_detect(Original_crop,regex("^Cocoa$", ignore_case=TRUE))~"665",
  str_detect(Original_crop,regex("^Cocoa beans$", ignore_case=TRUE))~"661",
  str_detect(Original_crop,regex("^Cocoa Butter$", ignore_case=TRUE))~"664",
  str_detect(Original_crop,regex("^Cocoa Paste$", ignore_case=TRUE))~"662",
  str_detect(Original_crop,regex("^Cocoa, beans$", ignore_case=TRUE))~"661",
  str_detect(Original_crop,regex("^Cocoapowder&Cake$", ignore_case=TRUE))~"665",
  str_detect(Original_crop,regex("^Coconut$", ignore_case=TRUE))~"249",
  str_detect(Original_crop,regex("^Coconut \\(copra\\) oil$", ignore_case=TRUE))~"252",
  str_detect(Original_crop,regex("^Coconuts$", ignore_case=TRUE))~"249",
  str_detect(Original_crop,regex("^Coconuts Desiccated$", ignore_case=TRUE))~"250",
  str_detect(Original_crop,regex("^Cocoon Unr.&Waste$", ignore_case=TRUE))~"249",
  str_detect(Original_crop,regex("^Coffee$", ignore_case=TRUE))~"656",
  str_detect(Original_crop,regex("^Coffee arabica$", ignore_case=TRUE))~"656",
  str_detect(Original_crop,regex("^Coffee Extracts$", ignore_case=TRUE))~"659",
  str_detect(Original_crop,regex("^Coffee Husks and Skins$", ignore_case=TRUE))~"660",
  str_detect(Original_crop,regex("^Coffee Roasted$", ignore_case=TRUE))~"657",
  str_detect(Original_crop,regex("^Coffee Subst. Cont.Coffee$", ignore_case=TRUE))~"658",
  str_detect(Original_crop,regex("^Coffee, green$", ignore_case=TRUE))~"656",
  str_detect(Original_crop,regex("^Coir$", ignore_case=TRUE))~"813",
  str_detect(Original_crop,regex("^Commercial concentrate feed$", ignore_case=TRUE))~"845",
  str_detect(Original_crop,regex("^Compound Feed, Cattle$", ignore_case=TRUE))~"845",
  str_detect(Original_crop,regex("^Congo signal$", ignore_case=TRUE))~"639",
  str_detect(Original_crop,regex("^Copra$", ignore_case=TRUE))~"251",
  str_detect(Original_crop,regex("^Corn$", ignore_case=TRUE))~"56",
  str_detect(Original_crop,regex("^Corn grain$", ignore_case=TRUE))~"56",
  str_detect(Original_crop,regex("^Corn silage \\(35% dm\\)$", ignore_case=TRUE))~"446",
  str_detect(Original_crop,regex("^Corn silage \\(67% water\\)$", ignore_case=TRUE))~"446",
  str_detect(Original_crop,regex("^Corn silage \\(67% water\\) per t of grain$", ignore_case=TRUE))~"446",
  str_detect(Original_crop,regex("^Corn stover$", ignore_case=TRUE))~"56",
  str_detect(Original_crop,regex("^Corn stover per t of grain$", ignore_case=TRUE))~"56",
  str_detect(Original_crop,regex("^Cotton$", ignore_case=TRUE))~"767",
  str_detect(Original_crop,regex("^Cotton \\(lint\\)$", ignore_case=TRUE))~"767",
  str_detect(Original_crop,regex("^Cotton burs 30% DW$", ignore_case=TRUE))~"769",
  str_detect(Original_crop,regex("^Cotton Carded,Combed$", ignore_case=TRUE))~"768",
  str_detect(Original_crop,regex("^Cotton floral parts$", ignore_case=TRUE))~"769",
  str_detect(Original_crop,regex("^Cotton lint$", ignore_case=TRUE))~"767",
  str_detect(Original_crop,regex("^Cotton lint 28% DW$", ignore_case=TRUE))~"767",
  str_detect(Original_crop,regex("^Cotton Linter$", ignore_case=TRUE))~"767",
  str_detect(Original_crop,regex("^Cotton seeds 42% DW$", ignore_case=TRUE))~"329",
  str_detect(Original_crop,regex("^Cotton stover$", ignore_case=TRUE))~"329",
  str_detect(Original_crop,regex("^Cotton straw$", ignore_case=TRUE))~"329",
  str_detect(Original_crop,regex("^Cotton Waste$", ignore_case=TRUE))~"329",
  str_detect(Original_crop,regex("^Cottonseed$", ignore_case=TRUE))~"329",
  str_detect(Original_crop,regex("^Cottonseed oil$", ignore_case=TRUE))~"331",
  str_detect(Original_crop,regex("^Cow milk, whole, fresh$", ignore_case=TRUE))~"882",
  str_detect(Original_crop,regex("^Cow pea$", ignore_case=TRUE))~"195",
  str_detect(Original_crop,regex("^Cow peas, dry$", ignore_case=TRUE))~"195",
  str_detect(Original_crop,regex("^Cow peas; dry$", ignore_case=TRUE))~"195",
  str_detect(Original_crop,regex("^Cowpea$", ignore_case=TRUE))~"195",
  str_detect(Original_crop,regex("^Cranberries$", ignore_case=TRUE))~"554",
  str_detect(Original_crop,regex("^Cream Fresh$", ignore_case=TRUE))~"885",
  str_detect(Original_crop,regex("^Crude Materials$", ignore_case=TRUE))~"1293",
  str_detect(Original_crop,regex("^Cucumbers$", ignore_case=TRUE))~"397",
  str_detect(Original_crop,regex("^Cucumbers and gherkins$", ignore_case=TRUE))~"397",
  str_detect(Original_crop,regex("^Currants$", ignore_case=TRUE))~"550",
  str_detect(Original_crop,regex("^Dallisgrass$", ignore_case=TRUE))~"639",
  str_detect(Original_crop,regex("^Dates$", ignore_case=TRUE))~"577",
  str_detect(Original_crop,regex("^Degras$", ignore_case=TRUE))~"1222",
  str_detect(Original_crop,regex("^Douglas fir$", ignore_case=TRUE))~"1866",
  str_detect(Original_crop,regex("^Dried Mushrooms$", ignore_case=TRUE))~"449",
  str_detect(Original_crop,regex("^Dry Apricots$", ignore_case=TRUE))~"526",
  str_detect(Original_crop,regex("^Duck meat$", ignore_case=TRUE))~"2074",
  str_detect(Original_crop,regex("^Duckweed$", ignore_case=TRUE))~"",
  str_detect(Original_crop,regex("^Durum wheat$", ignore_case=TRUE))~"15",
  str_detect(Original_crop,regex("^Eggplants \\(aubergines\\)$", ignore_case=TRUE))~"399",
  str_detect(Original_crop,regex("^Eggs Dried$", ignore_case=TRUE))~"1064",
  str_detect(Original_crop,regex("^Eggs Liquid$", ignore_case=TRUE))~"1062",
  str_detect(Original_crop,regex("^Extensive$", ignore_case=TRUE))~"",
  str_detect(Original_crop,regex("^Extracts Tea, Mate, Prep$", ignore_case=TRUE))~"667",
  str_detect(Original_crop,regex("^Faba bean$", ignore_case=TRUE))~"420",
  str_detect(Original_crop,regex("^Fat Liver Prep \\(Foie Gras\\)$", ignore_case=TRUE))~"1060",
  str_detect(Original_crop,regex("^Fat of Camels$", ignore_case=TRUE))~"1129",
  str_detect(Original_crop,regex("^Fat of Cattle$", ignore_case=TRUE))~"869",
  str_detect(Original_crop,regex("^Fat of Pigs$", ignore_case=TRUE))~"1037",
  str_detect(Original_crop,regex("^Fat of Poultry$", ignore_case=TRUE))~"1065",
  str_detect(Original_crop,regex("^Fat of Ptry Rend$", ignore_case=TRUE))~"1066",
  str_detect(Original_crop,regex("^Fat of Sheep$", ignore_case=TRUE))~"979",
  str_detect(Original_crop,regex("^Fat Prep Nes$", ignore_case=TRUE))~"1243",
  str_detect(Original_crop,regex("^Fatty Acids$", ignore_case=TRUE))~"1276",
  str_detect(Original_crop,regex("^Feed Additives$", ignore_case=TRUE))~"",
  str_detect(Original_crop,regex("^Feed Minerals$", ignore_case=TRUE))~"",
  str_detect(Original_crop,regex("^Feed Supplements$", ignore_case=TRUE))~"845",
  str_detect(Original_crop,regex("^Fescue$", ignore_case=TRUE))~"639",
  str_detect(Original_crop,regex("^Fescue \\(DM\\)$", ignore_case=TRUE))~"639",
  str_detect(Original_crop,regex("^Fiber crops$", ignore_case=TRUE))~"821",
  str_detect(Original_crop,regex("^Fibre crops nes$", ignore_case=TRUE))~"821",
  str_detect(Original_crop,regex("^Fibre Crops Nes$", ignore_case=TRUE))~"821",
  str_detect(Original_crop,regex("^Fibres$", ignore_case=TRUE))~"1901",
  str_detect(Original_crop,regex("^Field pea$", ignore_case=TRUE))~"195",
  str_detect(Original_crop,regex("^Figs$", ignore_case=TRUE))~"569",
  str_detect(Original_crop,regex("^Figs Dried$", ignore_case=TRUE))~"569",
  str_detect(Original_crop,regex("^Fine Goat Hair$", ignore_case=TRUE))~"1218",
  str_detect(Original_crop,regex("^Finger millet$", ignore_case=TRUE))~"79",
  str_detect(Original_crop,regex("^Finger millet bran$", ignore_case=TRUE))~"81",
  str_detect(Original_crop,regex("^Finger millet residue$", ignore_case=TRUE))~"79",
  str_detect(Original_crop,regex("^Fingermillet$", ignore_case=TRUE))~"79",
  str_detect(Original_crop,regex("^Flattened rice bran$", ignore_case=TRUE))~"37",
  str_detect(Original_crop,regex("^Flax$", ignore_case=TRUE))~"773",
  str_detect(Original_crop,regex("^Flax and hemp$", ignore_case=TRUE))~"773",
  str_detect(Original_crop,regex("^Flax fibre and tow$", ignore_case=TRUE))~"773",
  str_detect(Original_crop,regex("^Flax Fibre Raw$", ignore_case=TRUE))~"773",
  str_detect(Original_crop,regex("^Flax grain$", ignore_case=TRUE))~"771",
  str_detect(Original_crop,regex("^Flax straw$", ignore_case=TRUE))~"773",
  str_detect(Original_crop,regex("^Flax Tow Waste$", ignore_case=TRUE))~"773",
  str_detect(Original_crop,regex("^Flour of Buckwheat$", ignore_case=TRUE))~"104",
  str_detect(Original_crop,regex("^Flour of Cassava$", ignore_case=TRUE))~"126",
  str_detect(Original_crop,regex("^Flour of Cereals$", ignore_case=TRUE))~"111",
  str_detect(Original_crop,regex("^Flour of Fonio$", ignore_case=TRUE))~"95",
  str_detect(Original_crop,regex("^Flour of Fruits$", ignore_case=TRUE))~"",
  str_detect(Original_crop,regex("^Flour of Maize$", ignore_case=TRUE))~"58",
  str_detect(Original_crop,regex("^Flour of Millet$", ignore_case=TRUE))~"111",
  str_detect(Original_crop,regex("^Flour of Mixed Grain$", ignore_case=TRUE))~"104",
  str_detect(Original_crop,regex("^Flour of Mustard$", ignore_case=TRUE))~"295",
  str_detect(Original_crop,regex("^Flour of Oilseeds$", ignore_case=TRUE))~"",
  str_detect(Original_crop,regex("^Flour of Pulses$", ignore_case=TRUE))~"212",
  str_detect(Original_crop,regex("^Flour of Roots and Tubers$", ignore_case=TRUE))~"150",
  str_detect(Original_crop,regex("^Flour of Rye$", ignore_case=TRUE))~"111",
  str_detect(Original_crop,regex("^Flour of Sorghum$", ignore_case=TRUE))~"111",
  str_detect(Original_crop,regex("^Flour of Wheat$", ignore_case=TRUE))~"16",
  str_detect(Original_crop,regex("^Flowers$", ignore_case=TRUE))~"1972",
  str_detect(Original_crop,regex("^Fodder maize$", ignore_case=TRUE))~"446",
  str_detect(Original_crop,regex("^Fodder other on arable land$", ignore_case=TRUE))~"1892",
  str_detect(Original_crop,regex("^Fodder root crops$", ignore_case=TRUE))~"1892",
  str_detect(Original_crop,regex("^Fonio$", ignore_case=TRUE))~"94",
  str_detect(Original_crop,regex("^Food Prep Nes$", ignore_case=TRUE))~"1232",
  str_detect(Original_crop,regex("^Food Prep,Flour,Malt Extract$", ignore_case=TRUE))~"115",
  str_detect(Original_crop,regex("^Food Waste,Prep. for Feed$", ignore_case=TRUE))~"",
  str_detect(Original_crop,regex("^Food Wastes$", ignore_case=TRUE))~"653",
  str_detect(Original_crop,regex("^forage Products$", ignore_case=TRUE))~"641",
  str_detect(Original_crop,regex("^Forage sorghum \\(30% dm\\)$", ignore_case=TRUE))~"83",
  str_detect(Original_crop,regex("^Frozen Potatoes$", ignore_case=TRUE))~"116",
  str_detect(Original_crop,regex("^Fruit crops$", ignore_case=TRUE))~"619",
  str_detect(Original_crop,regex("^Fruit Dried Nes$", ignore_case=TRUE))~"620",
  str_detect(Original_crop,regex("^Fruit Fresh Nes$", ignore_case=TRUE))~"619",
  str_detect(Original_crop,regex("^Fruit Juice Nes$", ignore_case=TRUE))~"622",
  str_detect(Original_crop,regex("^Fruit Prp Nes$", ignore_case=TRUE))~"662",
  str_detect(Original_crop,regex("^Fruit Tropical Dried Nes$", ignore_case=TRUE))~"603",
  str_detect(Original_crop,regex("^Fruit, citrus nes$", ignore_case=TRUE))~"512",
  str_detect(Original_crop,regex("^Fruit, fresh nes$", ignore_case=TRUE))~"619",
  str_detect(Original_crop,regex("^Fruit, pome nes$", ignore_case=TRUE))~"542",
  str_detect(Original_crop,regex("^Fruit, stone nes$", ignore_case=TRUE))~"541",
  str_detect(Original_crop,regex("^Fruit, tropical fresh nes$", ignore_case=TRUE))~"603",
  str_detect(Original_crop,regex("^Fruit,Nut,Peel, Sugar Prs$", ignore_case=TRUE))~"",
  str_detect(Original_crop,regex("^Fruit; tropical fresh nes$", ignore_case=TRUE))~"603",
  str_detect(Original_crop,regex("^Fruits other$", ignore_case=TRUE))~"619",
  str_detect(Original_crop,regex("^Game meat$", ignore_case=TRUE))~"1163",
  str_detect(Original_crop,regex("^Garlic$", ignore_case=TRUE))~"406",
  str_detect(Original_crop,regex("^Generic grains$", ignore_case=TRUE))~"108",
  str_detect(Original_crop,regex("^Germ of Maize$", ignore_case=TRUE))~"57",
  str_detect(Original_crop,regex("^Germ of Wheat$", ignore_case=TRUE))~"19",
  str_detect(Original_crop,regex("^Ghee,Butteroil of Cow Milk$", ignore_case=TRUE))~"887",
  str_detect(Original_crop,regex("^Ginger$", ignore_case=TRUE))~"720",
  str_detect(Original_crop,regex("^Glucose and Dextrose$", ignore_case=TRUE))~"172",
  str_detect(Original_crop,regex("^Gluten Feed&Meal$", ignore_case=TRUE))~"846",
  str_detect(Original_crop,regex("^Goat meat$", ignore_case=TRUE))~"1017",
  str_detect(Original_crop,regex("^Goose and guinea fowl meat$", ignore_case=TRUE))~"1073",
  str_detect(Original_crop,regex("^Gooseberries$", ignore_case=TRUE))~"549",
  str_detect(Original_crop,regex("^Grain maize$", ignore_case=TRUE))~"56",
  str_detect(Original_crop,regex("^Grain, mixed$", ignore_case=TRUE))~"108",
  str_detect(Original_crop,regex("^Gram flour$", ignore_case=TRUE))~"212",
  str_detect(Original_crop,regex("^Grape Juice$", ignore_case=TRUE))~"562",
  str_detect(Original_crop,regex("^Grapefruit \\(inc. pomelos\\)$", ignore_case=TRUE))~"507",
  str_detect(Original_crop,regex("^Grapefruit juice, concentrated$", ignore_case=TRUE))~"510",
  str_detect(Original_crop,regex("^Grapes$", ignore_case=TRUE))~"560",
  str_detect(Original_crop,regex("^Grass pea residue$", ignore_case=TRUE))~"643",
  str_detect(Original_crop,regex("^Grease incl. Lanolin Wool$", ignore_case=TRUE))~"994",
  str_detect(Original_crop,regex("^Green gram$", ignore_case=TRUE))~"191",
  str_detect(Original_crop,regex("^Green gram bran$", ignore_case=TRUE))~"213",
  str_detect(Original_crop,regex("^Green gram residue$", ignore_case=TRUE))~"213",
  str_detect(Original_crop,regex("^Green groundnut plant$", ignore_case=TRUE))~"242",
  str_detect(Original_crop,regex("^Green maize tree$", ignore_case=TRUE))~"",
  str_detect(Original_crop,regex("^Groundnut$", ignore_case=TRUE))~"242",
  str_detect(Original_crop,regex("^Groundnut haulm$", ignore_case=TRUE))~"242",
  str_detect(Original_crop,regex("^Groundnut oil$", ignore_case=TRUE))~"244",
  str_detect(Original_crop,regex("^Groundnuts$", ignore_case=TRUE))~"242",
  str_detect(Original_crop,regex("^Groundnuts Shelled$", ignore_case=TRUE))~"243",
  str_detect(Original_crop,regex("^Groundnuts, with shell$", ignore_case=TRUE))~"242",
  str_detect(Original_crop,regex("^Groundnuts; with shell$", ignore_case=TRUE))~"242",
  str_detect(Original_crop,regex("^Guava$", ignore_case=TRUE))~"571",
  str_detect(Original_crop,regex("^Guinea grass$", ignore_case=TRUE))~"639",
  str_detect(Original_crop,regex("^Guineagrass$", ignore_case=TRUE))~"639",
  str_detect(Original_crop,regex("^Gums Natural$", ignore_case=TRUE))~"839",
  str_detect(Original_crop,regex("^Hair Carded/ Combed$", ignore_case=TRUE))~"",
  str_detect(Original_crop,regex("^Hair Coarse Nes$", ignore_case=TRUE))~"1031",
  str_detect(Original_crop,regex("^Hair Fine$", ignore_case=TRUE))~"1218",
  str_detect(Original_crop,regex("^Hair of Horses$", ignore_case=TRUE))~"1031",
  str_detect(Original_crop,regex("^Hay \\(Clover, Lucerne,Etc\\)$", ignore_case=TRUE))~"858",
  str_detect(Original_crop,regex("^Hay \\(Unspecified\\)$", ignore_case=TRUE))~"859",
  str_detect(Original_crop,regex("^Hay for forage, from grasses$", ignore_case=TRUE))~"859",
  str_detect(Original_crop,regex("^Hay for forage, from legumes$", ignore_case=TRUE))~"858",
  str_detect(Original_crop,regex("^Hay Non Legum$", ignore_case=TRUE))~"859",
  str_detect(Original_crop,regex("^Hazelnuts Shelled$", ignore_case=TRUE))~"233",
  str_detect(Original_crop,regex("^Hazelnuts, with shell$", ignore_case=TRUE))~"225",
  str_detect(Original_crop,regex("^Hazelnuts; with shell$", ignore_case=TRUE))~"225",
  str_detect(Original_crop,regex("^Hedge lucerne$", ignore_case=TRUE))~"",
  str_detect(Original_crop,regex("^Hemp$", ignore_case=TRUE))~"336",
  str_detect(Original_crop,regex("^Hemp tow waste$", ignore_case=TRUE))~"777",
  str_detect(Original_crop,regex("^Hemp Tow Waste$", ignore_case=TRUE))~"777",
  str_detect(Original_crop,regex("^Hempseed$", ignore_case=TRUE))~"336",
  str_detect(Original_crop,regex("^Hen eggs, in shell$", ignore_case=TRUE))~"1062",
  str_detect(Original_crop,regex("^Hides Dry Slt Horses$", ignore_case=TRUE))~"1777",
  str_detect(Original_crop,regex("^Hides Dry Slt Nes$", ignore_case=TRUE))~"1777",
  str_detect(Original_crop,regex("^Hides Drysalt Buf$", ignore_case=TRUE))~"957",
  str_detect(Original_crop,regex("^Hides Nes$", ignore_case=TRUE))~"1777",
  str_detect(Original_crop,regex("^Hides Nes Cattle$", ignore_case=TRUE))~"919",
  str_detect(Original_crop,regex("^Hides Unsp Camels$", ignore_case=TRUE))~"1777",
  str_detect(Original_crop,regex("^Hides Unsp Horse$", ignore_case=TRUE))~"1777",
  str_detect(Original_crop,regex("^Hides Wet Salted Buffaloes$", ignore_case=TRUE))~"957",
  str_detect(Original_crop,regex("^Hides Wet Salted Camels$", ignore_case=TRUE))~"1777",
  str_detect(Original_crop,regex("^Hides Wet Salted Cattle$", ignore_case=TRUE))~"919",
  str_detect(Original_crop,regex("^Hides Wet Salted Horses$", ignore_case=TRUE))~"1777",
  str_detect(Original_crop,regex("^Hides Wet Salted Nes$", ignore_case=TRUE))~"1777",
  str_detect(Original_crop,regex("^Hidesdry S.Cattle$", ignore_case=TRUE))~"1777",
  str_detect(Original_crop,regex("^Homogen. Cooked Fruit Prp$", ignore_case=TRUE))~"626",
  str_detect(Original_crop,regex("^Homogen.Meat Prp.$", ignore_case=TRUE))~"877",
  str_detect(Original_crop,regex("^Homogen.Veget.Prep$", ignore_case=TRUE))~"476",
  str_detect(Original_crop,regex("^Honey, natural$", ignore_case=TRUE))~"1182",
  str_detect(Original_crop,regex("^Hops$", ignore_case=TRUE))~"677",
  str_detect(Original_crop,regex("^Horse gram bran$", ignore_case=TRUE))~"2549",
  str_detect(Original_crop,regex("^Horse gram residue$", ignore_case=TRUE))~"2549",
  str_detect(Original_crop,regex("^Horse meat$", ignore_case=TRUE))~"1097",
  str_detect(Original_crop,regex("^Hybrid Napier$", ignore_case=TRUE))~"639",
  str_detect(Original_crop,regex("^Ice Cream and Edible Ice$", ignore_case=TRUE))~"910",
  str_detect(Original_crop,regex("^Indiangrass$", ignore_case=TRUE))~"639",
  str_detect(Original_crop,regex("^Infant Food$", ignore_case=TRUE))~"109",
  str_detect(Original_crop,regex("^Intensive$", ignore_case=TRUE))~"",
  str_detect(Original_crop,regex("^Isoglucose$", ignore_case=TRUE))~"175",
  str_detect(Original_crop,regex("^Jojoba seed$", ignore_case=TRUE))~"277",
  str_detect(Original_crop,regex("^Jojoba Seeds$", ignore_case=TRUE))~"277",
  str_detect(Original_crop,regex("^Juice of Grapefruit$", ignore_case=TRUE))~"509",
  str_detect(Original_crop,regex("^Juice of Pineapples$", ignore_case=TRUE))~"576",
  str_detect(Original_crop,regex("^Juice of Tomatoes$", ignore_case=TRUE))~"390",
  str_detect(Original_crop,regex("^Juice of Vegetables Nes$", ignore_case=TRUE))~"466",
  str_detect(Original_crop,regex("^Jute$", ignore_case=TRUE))~"780",
  str_detect(Original_crop,regex("^Jute capsularis$", ignore_case=TRUE))~"780",
  str_detect(Original_crop,regex("^Jute olitorious$", ignore_case=TRUE))~"780",
  str_detect(Original_crop,regex("^Kapok fibre$", ignore_case=TRUE))~"778",
  str_detect(Original_crop,regex("^Kapok Fibre$", ignore_case=TRUE))~"778",
  str_detect(Original_crop,regex("^Kapokseed in Shell$", ignore_case=TRUE))~"311",
  str_detect(Original_crop,regex("^Kapokseed Shelled$", ignore_case=TRUE))~"312",
  str_detect(Original_crop,regex("^Karakul Skins$", ignore_case=TRUE))~"995",
  str_detect(Original_crop,regex("^Karite Nuts \\(Sheanuts\\)$", ignore_case=TRUE))~"263",
  str_detect(Original_crop,regex("^Kenaf$", ignore_case=TRUE))~"430",
  str_detect(Original_crop,regex("^Kiwi fruit$", ignore_case=TRUE))~"592",
  str_detect(Original_crop,regex("^Kolanuts$", ignore_case=TRUE))~"224",
  str_detect(Original_crop,regex("^Lactose$", ignore_case=TRUE))~"173",
  str_detect(Original_crop,regex("^Lard$", ignore_case=TRUE))~"1043",
  str_detect(Original_crop,regex("^Lard Stearine Oil$", ignore_case=TRUE))~"1221",
  str_detect(Original_crop,regex("^Leather Use&Waste$", ignore_case=TRUE))~"",
  str_detect(Original_crop,regex("^Leaves$", ignore_case=TRUE))~"",
  str_detect(Original_crop,regex("^Leeks, other alliaceous veg$", ignore_case=TRUE))~"407",
  str_detect(Original_crop,regex("^Leeks, other alliaceous vegetables$", ignore_case=TRUE))~"407",
  str_detect(Original_crop,regex("^Leeks; other alliaceous veg$", ignore_case=TRUE))~"407",
  str_detect(Original_crop,regex("^Leguminous for Silage$", ignore_case=TRUE))~"640",
  str_detect(Original_crop,regex("^Leguminous vegetables, nes$", ignore_case=TRUE))~"420",
  str_detect(Original_crop,regex("^Leguminous vegetables; nes$", ignore_case=TRUE))~"420",
  str_detect(Original_crop,regex("^Lemon juice, concentrated$", ignore_case=TRUE))~"499",
  str_detect(Original_crop,regex("^Lemon juice, single strength$", ignore_case=TRUE))~"498",
  str_detect(Original_crop,regex("^Lemons and limes$", ignore_case=TRUE))~"497",
  str_detect(Original_crop,regex("^Lentil$", ignore_case=TRUE))~"201",
  str_detect(Original_crop,regex("^Lentils$", ignore_case=TRUE))~"201",
  str_detect(Original_crop,regex("^Lespedeza$", ignore_case=TRUE))~"187",
  str_detect(Original_crop,regex("^Lettuce \\(heads\\)$", ignore_case=TRUE))~"372",
  str_detect(Original_crop,regex("^Lettuce and chicory$", ignore_case=TRUE))~"372",
  str_detect(Original_crop,regex("^Linseed$", ignore_case=TRUE))~"333",
  str_detect(Original_crop,regex("^Linseed oil$", ignore_case=TRUE))~"334",
  str_detect(Original_crop,regex("^Liquid Margarine$", ignore_case=TRUE))~"1241",
  str_detect(Original_crop,regex("^Little bluestem$", ignore_case=TRUE))~"639",
  str_detect(Original_crop,regex("^Liver Prep.$", ignore_case=TRUE))~"878",
  str_detect(Original_crop,regex("^Local concentrate feed$", ignore_case=TRUE))~"845",
  str_detect(Original_crop,regex("^Local grass$", ignore_case=TRUE))~"639",
  str_detect(Original_crop,regex("^Long bean residue$", ignore_case=TRUE))~"211",
  str_detect(Original_crop,regex("^Lupin$", ignore_case=TRUE))~"210",
  str_detect(Original_crop,regex("^Lupins$", ignore_case=TRUE))~"210",
  str_detect(Original_crop,regex("^Macaroni$", ignore_case=TRUE))~"18",
  str_detect(Original_crop,regex("^maize$", ignore_case=TRUE))~"56",
  str_detect(Original_crop,regex("^Maize$", ignore_case=TRUE))~"56",
  str_detect(Original_crop,regex("^Maize flour$", ignore_case=TRUE))~"58",
  str_detect(Original_crop,regex("^Maize for forage$", ignore_case=TRUE))~"446",
  str_detect(Original_crop,regex("^Maize for forage and silage$", ignore_case=TRUE))~"446",
  str_detect(Original_crop,regex("^Maize grain$", ignore_case=TRUE))~"56",
  str_detect(Original_crop,regex("^Maize leaf$", ignore_case=TRUE))~"56",
  str_detect(Original_crop,regex("^Maize oil$", ignore_case=TRUE))~"60",
  str_detect(Original_crop,regex("^Maize stover$", ignore_case=TRUE))~"56",
  str_detect(Original_crop,regex("^Maize, green$", ignore_case=TRUE))~"446",
  str_detect(Original_crop,regex("^Maize; green$", ignore_case=TRUE))~"446",
  str_detect(Original_crop,regex("^Malt$", ignore_case=TRUE))~"49",
  str_detect(Original_crop,regex("^Malt Extract$", ignore_case=TRUE))~"115",
  str_detect(Original_crop,regex("^Mango$", ignore_case=TRUE))~"5715",
  str_detect(Original_crop,regex("^Mango Juice$", ignore_case=TRUE))~"583",
  str_detect(Original_crop,regex("^Mango Pulp$", ignore_case=TRUE))~"584",
  str_detect(Original_crop,regex("^Mangoes, mangosteens, guavas$", ignore_case=TRUE))~"571",
  str_detect(Original_crop,regex("^Mangoes; mangosteens; guavas$", ignore_case=TRUE))~"571",
  str_detect(Original_crop,regex("^Manila fibre \\(abaca\\)$", ignore_case=TRUE))~"809",
  str_detect(Original_crop,regex("^Manila Fibre \\(Abaca\\)$", ignore_case=TRUE))~"809",
  str_detect(Original_crop,regex("^Maple Sugar and Syrups$", ignore_case=TRUE))~"160",
  str_detect(Original_crop,regex("^Marc of Grapes$", ignore_case=TRUE))~"",
  str_detect(Original_crop,regex("^Margrine Short$", ignore_case=TRUE))~"1242",
  str_detect(Original_crop,regex("^Mate$", ignore_case=TRUE))~"672", 
  str_detect(Original_crop,regex("^Mat<e9>$", ignore_case=TRUE))~"672", 
  str_detect(Original_crop,regex("^Mat<U+383C><U+3E32>$", ignore_case=TRUE))~"672",
  str_detect(Original_crop,regex("^Meal Meat$", ignore_case=TRUE))~"1173",
  str_detect(Original_crop,regex("^Mean for C3 cereals$", ignore_case=TRUE))~"108",
  str_detect(Original_crop,regex("^Mean for C4 cereals$", ignore_case=TRUE))~"108",
  str_detect(Original_crop,regex("^Mean for legumes$", ignore_case=TRUE))~"211",
  str_detect(Original_crop,regex("^Meat-CattleBoneless\\(Beef&Veal\\)$", ignore_case=TRUE))~"1806",
  str_detect(Original_crop,regex("^Meat Dried Nes$", ignore_case=TRUE))~"1166",
  str_detect(Original_crop,regex("^Meat Extracts$", ignore_case=TRUE))~"",
  str_detect(Original_crop,regex("^Meat nes$", ignore_case=TRUE))~"1166",
  str_detect(Original_crop,regex("^Meat of Asses$", ignore_case=TRUE))~"1166",
  str_detect(Original_crop,regex("^Meat of Beef,Drd, Sltd,Smkd$", ignore_case=TRUE))~"",
  str_detect(Original_crop,regex("^Meat of Chicken Canned$", ignore_case=TRUE))~"1061",
  str_detect(Original_crop,regex("^Melons, other \\(inc.cantaloupes\\)$", ignore_case=TRUE))~"568",
  str_detect(Original_crop,regex("^Melonseed$", ignore_case=TRUE))~"299",
  str_detect(Original_crop,regex("^Meslin$", ignore_case=TRUE))~"15",
  str_detect(Original_crop,regex("^Milk Skimmed Cond$", ignore_case=TRUE))~"896",
  str_detect(Original_crop,regex("^Milk Skimmed Dry$", ignore_case=TRUE))~"898",
  str_detect(Original_crop,regex("^Milk Skimmed Evp$", ignore_case=TRUE))~"895",
  str_detect(Original_crop,regex("^Milk Skm of Cows$", ignore_case=TRUE))~"888",
  str_detect(Original_crop,regex("^Milk Whole Cond$", ignore_case=TRUE))~"889",
  str_detect(Original_crop,regex("^Milk Whole Dried$", ignore_case=TRUE))~"897",
  str_detect(Original_crop,regex("^Milk Whole Evp$", ignore_case=TRUE))~"894",
  str_detect(Original_crop,regex("^Milkdry Buttrmilk$", ignore_case=TRUE))~"899",
  str_detect(Original_crop,regex("^Milled/Husked Rice$", ignore_case=TRUE))~"31", #DOUBLE CHECK THI SWORKS
  str_detect(Original_crop,regex("^Millet$", ignore_case=TRUE))~"79",
  str_detect(Original_crop,regex("^Millet grain$", ignore_case=TRUE))~"79",
  str_detect(Original_crop,regex("^Millet straw$", ignore_case=TRUE))~"79",
  str_detect(Original_crop,regex("^Mint oil$", ignore_case=TRUE))~"753",
  str_detect(Original_crop,regex("^Mixed grain$", ignore_case=TRUE))~"108",
  str_detect(Original_crop,regex("^Mixed grass and legumes, for forage$", ignore_case=TRUE))~"641",
  str_detect(Original_crop,regex("^Mixes and Doughs$", ignore_case=TRUE))~"114",
  str_detect(Original_crop,regex("^Molasses$", ignore_case=TRUE))~"165",
  str_detect(Original_crop,regex("^Mung bean$", ignore_case=TRUE))~"176",
  str_detect(Original_crop,regex("^Mungbeans$", ignore_case=TRUE))~"176",
  str_detect(Original_crop,regex("^Mushrooms and truffles$", ignore_case=TRUE))~"449",
  str_detect(Original_crop,regex("^Must of Grapes$", ignore_case=TRUE))~"2620",
  str_detect(Original_crop,regex("^Mustard$", ignore_case=TRUE))~"292",
  str_detect(Original_crop,regex("^Mustard oil$", ignore_case=TRUE))~"2574",
  str_detect(Original_crop,regex("^Mustard seed$", ignore_case=TRUE))~"292",
  str_detect(Original_crop,regex("^New energy crops \\(ligneous\\)$", ignore_case=TRUE))~"",
  str_detect(Original_crop,regex("^Northern hardwoods$", ignore_case=TRUE))~"1867",
  str_detect(Original_crop,regex("^Nurseries$", ignore_case=TRUE))~"",
  str_detect(Original_crop,regex("^Nutmeg, mace and cardamoms$", ignore_case=TRUE))~"702",
  str_detect(Original_crop,regex("^Nutmeg; mace and cardamoms$", ignore_case=TRUE))~"702",
  str_detect(Original_crop,regex("^Nuts, nes$", ignore_case=TRUE))~"234",
  str_detect(Original_crop,regex("^Nuts; nes$", ignore_case=TRUE))~"234",
  str_detect(Original_crop,regex("^Oat$", ignore_case=TRUE))~"75",
  str_detect(Original_crop,regex("^Oat grain$", ignore_case=TRUE))~"75",
  str_detect(Original_crop,regex("^Oat haylage \\(40% dm\\)$", ignore_case=TRUE))~"75",
  str_detect(Original_crop,regex("^Oat straw$", ignore_case=TRUE))~"75",
  str_detect(Original_crop,regex("^Oat straw per t of grain$", ignore_case=TRUE))~"75",
  str_detect(Original_crop,regex("^Oats$", ignore_case=TRUE))~"75",
  str_detect(Original_crop,regex("^Oats Rolled$", ignore_case=TRUE))~"75",
  str_detect(Original_crop,regex("^Offals Liver Chicken$", ignore_case=TRUE))~"1059",
  str_detect(Original_crop,regex("^Offals Liver Duck$", ignore_case=TRUE))~"1075",
  str_detect(Original_crop,regex("^Offals Liver Geese$", ignore_case=TRUE))~"1074",
  str_detect(Original_crop,regex("^Offals Nes$", ignore_case=TRUE))~"",
  str_detect(Original_crop,regex("^Offals of Cattle, Edible$", ignore_case=TRUE))~"869",
  str_detect(Original_crop,regex("^Offals of Goats, Edible$", ignore_case=TRUE))~"1018",
  str_detect(Original_crop,regex("^Offals of Horses$", ignore_case=TRUE))~"1098",
  str_detect(Original_crop,regex("^Offals of Pigs, Edible$", ignore_case=TRUE))~"1036",
  str_detect(Original_crop,regex("^Offals of Sheep,Edible$", ignore_case=TRUE))~"978",
  str_detect(Original_crop,regex("^Oil-palm$", ignore_case=TRUE))~"254",
  str_detect(Original_crop,regex("^Oil Boiled Etc$", ignore_case=TRUE))~"1274",
  str_detect(Original_crop,regex("^Oil cake$", ignore_case=TRUE))~"",
  str_detect(Original_crop,regex("^Oil Citronella$", ignore_case=TRUE))~"737",
  str_detect(Original_crop,regex("^Oil crops$", ignore_case=TRUE))~"340",
  str_detect(Original_crop,regex("^Oil crops other$", ignore_case=TRUE))~"340",
  str_detect(Original_crop,regex("^Oil Essential Nes$", ignore_case=TRUE))~"753",
  str_detect(Original_crop,regex("^Oil Hydrogenated$", ignore_case=TRUE))~"1275",
  str_detect(Original_crop,regex("^Oil of Castor Beans$", ignore_case=TRUE))~"265",
  str_detect(Original_crop,regex("^Oil of Jojoba$", ignore_case=TRUE))~"339",
  str_detect(Original_crop,regex("^Oil of Kapok$", ignore_case=TRUE))~"339",
  str_detect(Original_crop,regex("^Oil of Olive Residues$", ignore_case=TRUE))~"261",
  str_detect(Original_crop,regex("^Oil of Tung Nuts$", ignore_case=TRUE))~"339",
  str_detect(Original_crop,regex("^Oil of vegetable origin, nes$", ignore_case=TRUE))~"339",
  str_detect(Original_crop,regex("^Oil palm$", ignore_case=TRUE))~"257",
  str_detect(Original_crop,regex("^Oil palm fruit$", ignore_case=TRUE))~"257",
  str_detect(Original_crop,regex("^Oil, palm fruit$", ignore_case=TRUE))~"257",
  str_detect(Original_crop,regex("^Oils,Fats of Animal Nes$", ignore_case=TRUE))~"1168",
  str_detect(Original_crop,regex("^Oilseeds nes$", ignore_case=TRUE))~"339",
  str_detect(Original_crop,regex("^Oilseeds, Nes$", ignore_case=TRUE))~"339",
  str_detect(Original_crop,regex("^Oilseeds; Nes$", ignore_case=TRUE))~"339",
  str_detect(Original_crop,regex("^Okra$", ignore_case=TRUE))~"430",
  str_detect(Original_crop,regex("^Olive oil, virgin$", ignore_case=TRUE))~"261",
  str_detect(Original_crop,regex("^Olive Residues$", ignore_case=TRUE))~"260",
  str_detect(Original_crop,regex("^Olives$", ignore_case=TRUE))~"260",
  str_detect(Original_crop,regex("^Olives Preserved$", ignore_case=TRUE))~"262",
  str_detect(Original_crop,regex("^OMFED concentrate feed$", ignore_case=TRUE))~"845",
  str_detect(Original_crop,regex("^Onion$", ignore_case=TRUE))~"402",
  str_detect(Original_crop,regex("^Onions$", ignore_case=TRUE))~"402",
  str_detect(Original_crop,regex("^Onions \\(inc. shallots\\), green$", ignore_case=TRUE))~"402",
  str_detect(Original_crop,regex("^Onions \\(inc. shallots\\); green$", ignore_case=TRUE))~"402",
  str_detect(Original_crop,regex("^Onions, dry$", ignore_case=TRUE))~"403",
  str_detect(Original_crop,regex("^Onions, shallots, green$", ignore_case=TRUE))~"402",
  str_detect(Original_crop,regex("^Onions; dry$", ignore_case=TRUE))~"403",
  str_detect(Original_crop,regex("^Orange juice, concentrated$", ignore_case=TRUE))~"492",
  str_detect(Original_crop,regex("^Orange juice, single strength$", ignore_case=TRUE))~"491",
  str_detect(Original_crop,regex("^Oranges$", ignore_case=TRUE))~"490",
  str_detect(Original_crop,regex("^Orchard grass$", ignore_case=TRUE))~"639",
  str_detect(Original_crop,regex("^Orchardgrass$", ignore_case=TRUE))~"639",
  str_detect(Original_crop,regex("^Orchardgrass  \\(DM\\)$", ignore_case=TRUE))~"639",
  str_detect(Original_crop,regex("^Other$", ignore_case=TRUE))~"",
  str_detect(Original_crop,regex("^Other Bastfibres$", ignore_case=TRUE))~"782",
  str_detect(Original_crop,regex("^Other bird eggs,in shell$", ignore_case=TRUE))~"1091",
  str_detect(Original_crop,regex("^Other cereals$", ignore_case=TRUE))~"108",
  str_detect(Original_crop,regex("^Other Conc, Nes$", ignore_case=TRUE))~"845",
  str_detect(Original_crop,regex("^Other crops$", ignore_case=TRUE))~"",
  str_detect(Original_crop,regex("^Other Fructose and Syrup$", ignore_case=TRUE))~"166",
  str_detect(Original_crop,regex("^Other fruits$", ignore_case=TRUE))~"619",
  str_detect(Original_crop,regex("^Other grasses, for forage$", ignore_case=TRUE))~"639",
  str_detect(Original_crop,regex("^Other industrial crops$", ignore_case=TRUE))~"",
  str_detect(Original_crop,regex("^Other legumes, for forage$", ignore_case=TRUE))~"643",
  str_detect(Original_crop,regex("^Other melons \\(inc.cantaloupes\\)$", ignore_case=TRUE))~"658",
  str_detect(Original_crop,regex("^Other oils$", ignore_case=TRUE))~"339",
  str_detect(Original_crop,regex("^Other oilseeds, for forage$", ignore_case=TRUE))~"339",
  str_detect(Original_crop,regex("^Other root crops$", ignore_case=TRUE))~"149",
  str_detect(Original_crop,regex("^Other vegetables$", ignore_case=TRUE))~"463",
  str_detect(Original_crop,regex("^Paddy husk$", ignore_case=TRUE))~"27",
  str_detect(Original_crop,regex("^Paddy rice$", ignore_case=TRUE))~"27",
  str_detect(Original_crop,regex("^Palm kernel oil$", ignore_case=TRUE))~"254",
  str_detect(Original_crop,regex("^Palm kernels$", ignore_case=TRUE))~"254",
  str_detect(Original_crop,regex("^Palm oil$", ignore_case=TRUE))~"254",
  str_detect(Original_crop,regex("^Pangolagrass$", ignore_case=TRUE))~"639",
  str_detect(Original_crop,regex("^Papaya$", ignore_case=TRUE))~"600",
  str_detect(Original_crop,regex("^Papayas$", ignore_case=TRUE))~"600",
  str_detect(Original_crop,regex("^Paragrass$", ignore_case=TRUE))~"639",
  str_detect(Original_crop,regex("^Paste of Tomatoes$", ignore_case=TRUE))~"391",
  str_detect(Original_crop,regex("^Pastry$", ignore_case=TRUE))~"22",
  str_detect(Original_crop,regex("^Pea$", ignore_case=TRUE))~"417",
  str_detect(Original_crop,regex("^Peaches$", ignore_case=TRUE))~"534",
  str_detect(Original_crop,regex("^Peaches and nectarines$", ignore_case=TRUE))~"534",
  str_detect(Original_crop,regex("^Peanut$", ignore_case=TRUE))~"242",
  str_detect(Original_crop,regex("^Peanut Butter$", ignore_case=TRUE))~"247",
  str_detect(Original_crop,regex("^Peanut nuts$", ignore_case=TRUE))~"242",
  str_detect(Original_crop,regex("^Peanut stover$", ignore_case=TRUE))~"242",
  str_detect(Original_crop,regex("^Peanuts$", ignore_case=TRUE))~"242",
  str_detect(Original_crop,regex("^Pearl millet$", ignore_case=TRUE))~"79",
  str_detect(Original_crop,regex("^Pearl Millet$", ignore_case=TRUE))~"79",
  str_detect(Original_crop,regex("^Pearl millet residue$", ignore_case=TRUE))~"79",
  str_detect(Original_crop,regex("^Pearlmillet$", ignore_case=TRUE))~"79",
  str_detect(Original_crop,regex("^Pears$", ignore_case=TRUE))~"521",
  str_detect(Original_crop,regex("^Peas$", ignore_case=TRUE))~"417",
  str_detect(Original_crop,regex("^Peas, dry$", ignore_case=TRUE))~"187",
  str_detect(Original_crop,regex("^Peas, green$", ignore_case=TRUE))~"417",
  str_detect(Original_crop,regex("^Peas; dry$", ignore_case=TRUE))~"187",
  str_detect(Original_crop,regex("^Peas; green$", ignore_case=TRUE))~"417",
  str_detect(Original_crop,regex("^Pepper \\(piper spp.\\)$", ignore_case=TRUE))~"687",
  str_detect(Original_crop,regex("^Pepper \\(Piper spp.\\)$", ignore_case=TRUE))~"687",
  str_detect(Original_crop,regex("^Peppermint$", ignore_case=TRUE))~"748",
  str_detect(Original_crop,regex("^Persimmons$", ignore_case=TRUE))~"587",
  str_detect(Original_crop,regex("^Pet Food$", ignore_case=TRUE))~"843",
  str_detect(Original_crop,regex("^Phragmites$", ignore_case=TRUE))~"",
  str_detect(Original_crop,regex("^Pig Butcher Fat$", ignore_case=TRUE))~"1037",
  str_detect(Original_crop,regex("^Pigeon pea$", ignore_case=TRUE))~"197",
  str_detect(Original_crop,regex("^Pigeon pea leaf$", ignore_case=TRUE))~"417",
  str_detect(Original_crop,regex("^Pigeon pea residue$", ignore_case=TRUE))~"417",
  str_detect(Original_crop,regex("^Pigeon peas$", ignore_case=TRUE))~"417",
  str_detect(Original_crop,regex("^Pineapple$", ignore_case=TRUE))~"574",
  str_detect(Original_crop,regex("^Pineapple Juice Conc$", ignore_case=TRUE))~"580",
  str_detect(Original_crop,regex("^Pineapples$", ignore_case=TRUE))~"574",
  str_detect(Original_crop,regex("^Pineapples Cand$", ignore_case=TRUE))~"575",
  str_detect(Original_crop,regex("^Pistachios$", ignore_case=TRUE))~"223",
  str_detect(Original_crop,regex("^Plantain$", ignore_case=TRUE))~"489",
  str_detect(Original_crop,regex("^Plantains$", ignore_case=TRUE))~"489",
  str_detect(Original_crop,regex("^Plantains and others$", ignore_case=TRUE))~"489",
  str_detect(Original_crop,regex("^Platen rice bran$", ignore_case=TRUE))~"35",
  str_detect(Original_crop,regex("^Plum juice, concentrated$", ignore_case=TRUE))~"539",
  str_detect(Original_crop,regex("^Plum juice, single strength$", ignore_case=TRUE))~"538",
  str_detect(Original_crop,regex("^Plums and sloes$", ignore_case=TRUE))~"536",
  str_detect(Original_crop,regex("^Plums Dried \\(Prunes\\)$", ignore_case=TRUE))~"536",
  str_detect(Original_crop,regex("^Polished rice$", ignore_case=TRUE))~"",
  str_detect(Original_crop,regex("^Pome fruit; nes$", ignore_case=TRUE))~"542",
  str_detect(Original_crop,regex("^Popcorn$", ignore_case=TRUE))~"56",
  str_detect(Original_crop,regex("^Poppy Oil$", ignore_case=TRUE))~"297",
  str_detect(Original_crop,regex("^Poppy seed$", ignore_case=TRUE))~"296",
  str_detect(Original_crop,regex("^Pork$", ignore_case=TRUE))~"1035",
  str_detect(Original_crop,regex("^Pot Barley$", ignore_case=TRUE))~"",
  str_detect(Original_crop,regex("^Potato$", ignore_case=TRUE))~"116",
  str_detect(Original_crop,regex("^Potato above-ground stems & leaves$", ignore_case=TRUE))~"116",
  str_detect(Original_crop,regex("^Potato Offals$", ignore_case=TRUE))~"116",
  str_detect(Original_crop,regex("^Potato tuber$", ignore_case=TRUE))~"116",
  str_detect(Original_crop,regex("^Potatoes$", ignore_case=TRUE))~"116",
  str_detect(Original_crop,regex("^Potatoes and tubers$", ignore_case=TRUE))~"116",
  str_detect(Original_crop,regex("^Potatoes Flour$", ignore_case=TRUE))~"150",
  str_detect(Original_crop,regex("^Prep of Pig Meat$", ignore_case=TRUE))~"1042",
  str_detect(Original_crop,regex("^Preparations of Beef Meat$", ignore_case=TRUE))~"",
  str_detect(Original_crop,regex("^Prepared Groundnuts$", ignore_case=TRUE))~"246",
  str_detect(Original_crop,regex("^Prepared Meat Nes$", ignore_case=TRUE))~"",
  str_detect(Original_crop,regex("^Prepared Nuts \\(Exc.Groundnuts\\)$", ignore_case=TRUE))~"246",
  str_detect(Original_crop,regex("^Processed Cheese$", ignore_case=TRUE))~"1745",
  str_detect(Original_crop,regex("^Prod.of Nat.Milk Constit$", ignore_case=TRUE))~"",
  str_detect(Original_crop,regex("^Pulp of Fruit for Feed$", ignore_case=TRUE))~"628",
  str_detect(Original_crop,regex("^Pulpwood$", ignore_case=TRUE))~"1685",
  str_detect(Original_crop,regex("^Pulses$", ignore_case=TRUE))~"211",
  str_detect(Original_crop,regex("^Pulses, nes$", ignore_case=TRUE))~"211",
  str_detect(Original_crop,regex("^Pulses; nes$", ignore_case=TRUE))~"211",
  str_detect(Original_crop,regex("^Pumpkins, squash and gourds$", ignore_case=TRUE))~"394",
  str_detect(Original_crop,regex("^Pumpkins; squash and gourds$", ignore_case=TRUE))~"394",
  str_detect(Original_crop,regex("^Pyrethrum Extr$", ignore_case=TRUE))~"755",
  str_detect(Original_crop,regex("^Pyrethrum, dried$", ignore_case=TRUE))~"754",
  str_detect(Original_crop,regex("^Pyrethrum,Dried$", ignore_case=TRUE))~"754",
  str_detect(Original_crop,regex("^Pyrethrum;Dried$", ignore_case=TRUE))~"754",
  str_detect(Original_crop,regex("^Quinces$", ignore_case=TRUE))~"523",
  str_detect(Original_crop,regex("^Quinoa$", ignore_case=TRUE))~"92",
  str_detect(Original_crop,regex("^Rabbit meat$", ignore_case=TRUE))~"1141",
  str_detect(Original_crop,regex("^Raisins$", ignore_case=TRUE))~"561",
  str_detect(Original_crop,regex("^Ramie$", ignore_case=TRUE))~"788",
  str_detect(Original_crop,regex("^Rape$", ignore_case=TRUE))~"270",
  str_detect(Original_crop,regex("^Rapeseed$", ignore_case=TRUE))~"270",
  str_detect(Original_crop,regex("^Rapeseed oil$", ignore_case=TRUE))~"271",
  str_detect(Original_crop,regex("^Raspberries$", ignore_case=TRUE))~"547",
  str_detect(Original_crop,regex("^Reconsti.Ted Milk$", ignore_case=TRUE))~"908",
  str_detect(Original_crop,regex("^Red clover$", ignore_case=TRUE))~"640",
  str_detect(Original_crop,regex("^Red clover \\(DM\\)$", ignore_case=TRUE))~"640",
  str_detect(Original_crop,regex("^Red gram residue$", ignore_case=TRUE))~"",
  str_detect(Original_crop,regex("^Reed canarygrass$", ignore_case=TRUE))~"639",
  str_detect(Original_crop,regex("^Reed canarygrass \\(DM\\)$", ignore_case=TRUE))~"639",
  str_detect(Original_crop,regex("^Res.Fatty Subs$", ignore_case=TRUE))~"1277",
  str_detect(Original_crop,regex("^Rice$", ignore_case=TRUE))~"27",
  str_detect(Original_crop,regex("^Rice  total  \\(Rice milled equivalent\\)$", ignore_case=TRUE))~"27",
  str_detect(Original_crop,regex("^Rice bran$", ignore_case=TRUE))~"",
  str_detect(Original_crop,regex("^Rice bran oil$", ignore_case=TRUE))~"36",
  str_detect(Original_crop,regex("^Rice Fermented Beverages$", ignore_case=TRUE))~"39",
  str_detect(Original_crop,regex("^Rice Flour$", ignore_case=TRUE))~"38",
  str_detect(Original_crop,regex("^Rice grain$", ignore_case=TRUE))~"27",
  str_detect(Original_crop,regex("^Rice husk$", ignore_case=TRUE))~"27",
  str_detect(Original_crop,regex("^Rice paddy straw$", ignore_case=TRUE))~"27",
  str_detect(Original_crop,regex("^Rice straw$", ignore_case=TRUE))~"27",
  str_detect(Original_crop,regex("^Rice, paddy$", ignore_case=TRUE))~"27",
  str_detect(Original_crop,regex("^Rocket salad$", ignore_case=TRUE))~"372",
  str_detect(Original_crop,regex("^Roots and tuber crops$", ignore_case=TRUE))~"149",
  str_detect(Original_crop,regex("^Roots and Tubers Dried$", ignore_case=TRUE))~"149",
  str_detect(Original_crop,regex("^Roots and Tubers, nes$", ignore_case=TRUE))~"149",
  str_detect(Original_crop,regex("^Roots and Tubers; nes$", ignore_case=TRUE))~"149",
  str_detect(Original_crop,regex("^Roots other$", ignore_case=TRUE))~"149",
  str_detect(Original_crop,regex("^Rubber$", ignore_case=TRUE))~"836",
  str_detect(Original_crop,regex("^Rubber Nat Dry$", ignore_case=TRUE))~"836",
  str_detect(Original_crop,regex("^Rubber, natural$", ignore_case=TRUE))~"836",
  str_detect(Original_crop,regex("^Rushes$", ignore_case=TRUE))~"641",
  str_detect(Original_crop,regex("^Rye$", ignore_case=TRUE))~"71",
  str_detect(Original_crop,regex("^Rye grain$", ignore_case=TRUE))~"71",
  str_detect(Original_crop,regex("^Rye grass, for forage$", ignore_case=TRUE))~"639",
  str_detect(Original_crop,regex("^Rye straw$", ignore_case=TRUE))~"1892",
  str_detect(Original_crop,regex("^Rye straw per t of grain$", ignore_case=TRUE))~"1892",
  str_detect(Original_crop,regex("^Ryegrass$", ignore_case=TRUE))~"639",
  str_detect(Original_crop,regex("^Ryegrass \\(DM\\)$", ignore_case=TRUE))~"639",
  str_detect(Original_crop,regex("^Safflower$", ignore_case=TRUE))~"280",
  str_detect(Original_crop,regex("^Safflower oil$", ignore_case=TRUE))~"281",
  str_detect(Original_crop,regex("^Safflower seed$", ignore_case=TRUE))~"280",
  str_detect(Original_crop,regex("^Saltgrass$", ignore_case=TRUE))~"639",
  str_detect(Original_crop,regex("^Sapota$", ignore_case=TRUE))~"619",
  str_detect(Original_crop,regex("^Sausage Beef&Veal$", ignore_case=TRUE))~"874",
  str_detect(Original_crop,regex("^Sausages of Pig Meat$", ignore_case=TRUE))~"1041",
  str_detect(Original_crop,regex("^Sedges$", ignore_case=TRUE))~"641",
  str_detect(Original_crop,regex("^Seed cotton$", ignore_case=TRUE))~"328",
  str_detect(Original_crop,regex("^Sesame$", ignore_case=TRUE))~"289",
  str_detect(Original_crop,regex("^Sesame oil$", ignore_case=TRUE))~"290",
  str_detect(Original_crop,regex("^Sesame seed$", ignore_case=TRUE))~"289",
  str_detect(Original_crop,regex("^Sesameseed$", ignore_case=TRUE))~"289",
  str_detect(Original_crop,regex("^Sesamum$", ignore_case=TRUE))~"420",
  str_detect(Original_crop,regex("^Sesbania grandifolia$", ignore_case=TRUE))~"463",
  str_detect(Original_crop,regex("^Sheep meat$", ignore_case=TRUE))~"977",
  str_detect(Original_crop,regex("^Sheep milk, whole, fresh$", ignore_case=TRUE))~"982",
  str_detect(Original_crop,regex("^Sheepskins$", ignore_case=TRUE))~"1777",
  str_detect(Original_crop,regex("^Silk-worm cocoons, reelable$", ignore_case=TRUE))~"1185",
  str_detect(Original_crop,regex("^Silk Raw$", ignore_case=TRUE))~"1186",
  str_detect(Original_crop,regex("^Sisal$", ignore_case=TRUE))~"789",
  str_detect(Original_crop,regex("^Skin Furs$", ignore_case=TRUE))~"1777",
  str_detect(Original_crop,regex("^Skins Nes Calves$", ignore_case=TRUE))~"919",
  str_detect(Original_crop,regex("^Skins Nes Goats$", ignore_case=TRUE))~"1777",
  str_detect(Original_crop,regex("^Skins Nes Pigs$", ignore_case=TRUE))~"1777",
  str_detect(Original_crop,regex("^Skins Nes Sheep$", ignore_case=TRUE))~"1777",
  str_detect(Original_crop,regex("^Skins of Rabbits$", ignore_case=TRUE))~"1777",
  str_detect(Original_crop,regex("^Skins Wet Salted Calves$", ignore_case=TRUE))~"919",
  str_detect(Original_crop,regex("^Skins Wet Salted Goats$", ignore_case=TRUE))~"1777",
  str_detect(Original_crop,regex("^Skins With Wool Sheep$", ignore_case=TRUE))~"1777",
  str_detect(Original_crop,regex("^Skinsdry S.Calves$", ignore_case=TRUE))~"1777",
  str_detect(Original_crop,regex("^Skinsdry Slt Goat$", ignore_case=TRUE))~"1777",
  str_detect(Original_crop,regex("^Skinsdry Sltdpigs$", ignore_case=TRUE))~"1777",
  str_detect(Original_crop,regex("^Skinsdry Sltsheep$", ignore_case=TRUE))~"1777",
  str_detect(Original_crop,regex("^Skinswet Salted$", ignore_case=TRUE))~"1777",
  str_detect(Original_crop,regex("^Skinswet Sltdpigs$", ignore_case=TRUE))~"1777",
  str_detect(Original_crop,regex("^Small millet residue$", ignore_case=TRUE))~"79",
  str_detect(Original_crop,regex("^Snap beans$", ignore_case=TRUE))~"414",
  str_detect(Original_crop,regex("^Soft wheat$", ignore_case=TRUE))~"15",
  str_detect(Original_crop,regex("^Sorghum$", ignore_case=TRUE))~"83",
  str_detect(Original_crop,regex("^Sorghum-29$", ignore_case=TRUE))~"83",
  str_detect(Original_crop,regex("^Sorghum-sudan  \\(DM\\)$", ignore_case=TRUE))~"83",
  str_detect(Original_crop,regex("^Sorghum-sudan \\(50% dm\\)$", ignore_case=TRUE))~"83",
  str_detect(Original_crop,regex("^Sorghum grain$", ignore_case=TRUE))~"83",
  str_detect(Original_crop,regex("^Sorghum stover$", ignore_case=TRUE))~"83",
  str_detect(Original_crop,regex("^Sorghum stover per t of grain$", ignore_case=TRUE))~"83",
  str_detect(Original_crop,regex("^Sorghum sudan$", ignore_case=TRUE))~"83",
  str_detect(Original_crop,regex("^Sorghum Sudan$", ignore_case=TRUE))~"83",
  str_detect(Original_crop,regex("^Sorghum, for forage$", ignore_case=TRUE))~"639",
  str_detect(Original_crop,regex("^Sour cherries$", ignore_case=TRUE))~"530",
  str_detect(Original_crop,regex("^Soya Paste$", ignore_case=TRUE))~"240",
  str_detect(Original_crop,regex("^Soya Sauce$", ignore_case=TRUE))~"239",
  str_detect(Original_crop,regex("^Soybean$", ignore_case=TRUE))~"236",
  str_detect(Original_crop,regex("^Soybean grain$", ignore_case=TRUE))~"236",
  str_detect(Original_crop,regex("^Soybean hay \\(DM\\)$", ignore_case=TRUE))~"859",
  str_detect(Original_crop,regex("^Soybean oil$", ignore_case=TRUE))~"237",
  str_detect(Original_crop,regex("^Soybean stover$", ignore_case=TRUE))~"236",
  str_detect(Original_crop,regex("^Soybean stover per t of grain$", ignore_case=TRUE))~"236",
  str_detect(Original_crop,regex("^Soybeans$", ignore_case=TRUE))~"236",
  str_detect(Original_crop,regex("^Spermaceti$", ignore_case=TRUE))~"1223",
  str_detect(Original_crop,regex("^Spices, nes$", ignore_case=TRUE))~"723",
  str_detect(Original_crop,regex("^Spinach$", ignore_case=TRUE))~"373",
  str_detect(Original_crop,regex("^Spring wheat$", ignore_case=TRUE))~"15",
  str_detect(Original_crop,regex("^Stone fruit, nes$", ignore_case=TRUE))~"541",
  str_detect(Original_crop,regex("^Stone fruit; nes$", ignore_case=TRUE))~"541",
  str_detect(Original_crop,regex("^Straw Husks$", ignore_case=TRUE))~"",
  str_detect(Original_crop,regex("^Strawberries$", ignore_case=TRUE))~"544",
  str_detect(Original_crop,regex("^String beans$", ignore_case=TRUE))~"423",
  str_detect(Original_crop,regex("^Stylo grass$", ignore_case=TRUE))~"639",
  str_detect(Original_crop,regex("^Subabool$", ignore_case=TRUE))~"641",
  str_detect(Original_crop,regex("^Sugar$", ignore_case=TRUE))~"167",
  str_detect(Original_crop,regex("^Sugar-cane$", ignore_case=TRUE))~"156",
  str_detect(Original_crop,regex("^Sugar beet$", ignore_case=TRUE))~"157",
  str_detect(Original_crop,regex("^Sugar beets$", ignore_case=TRUE))~"157",
  str_detect(Original_crop,regex("^Sugar beets tops$", ignore_case=TRUE))~"157",
  str_detect(Original_crop,regex("^Sugar cane$", ignore_case=TRUE))~"156",
  str_detect(Original_crop,regex("^Sugar cane bagasse$", ignore_case=TRUE))~"156",
  str_detect(Original_crop,regex("^Sugar Confectionery$", ignore_case=TRUE))~"168",
  str_detect(Original_crop,regex("^Sugar crops, nes$", ignore_case=TRUE))~"161",
  str_detect(Original_crop,regex("^Sugar crops; nes$", ignore_case=TRUE))~"161",
  str_detect(Original_crop,regex("^Sugar Non- Centrifugal$", ignore_case=TRUE))~"163",
  str_detect(Original_crop,regex("^Sugar Raw Centrifugal$", ignore_case=TRUE))~"162",
  str_detect(Original_crop,regex("^Sugar Refined$", ignore_case=TRUE))~"164",
  str_detect(Original_crop,regex("^Sugar, nes$", ignore_case=TRUE))~"161",
  str_detect(Original_crop,regex("^Sugarbeet root$", ignore_case=TRUE))~"157",
  str_detect(Original_crop,regex("^Sugarbeet top$", ignore_case=TRUE))~"157",
  str_detect(Original_crop,regex("^Sugarcane$", ignore_case=TRUE))~"156",
  str_detect(Original_crop,regex("^Sugarcane leaf$", ignore_case=TRUE))~"156",
  str_detect(Original_crop,regex("^Sunflower$", ignore_case=TRUE))~"267",
  str_detect(Original_crop,regex("^Sunflower Cake$", ignore_case=TRUE))~"269",
  str_detect(Original_crop,regex("^Sunflower grain$", ignore_case=TRUE))~"267",
  str_detect(Original_crop,regex("^Sunflower oil$", ignore_case=TRUE))~"268",
  str_detect(Original_crop,regex("^Sunflower seed$", ignore_case=TRUE))~"267",
  str_detect(Original_crop,regex("^Sunflower stover$", ignore_case=TRUE))~"267",
  str_detect(Original_crop,regex("^Sunflower stover per t of grain$", ignore_case=TRUE))~"267",
  str_detect(Original_crop,regex("^Swedes for Fodder$", ignore_case=TRUE))~"1892",
  str_detect(Original_crop,regex("^Sweet corn$", ignore_case=TRUE))~"56",
  str_detect(Original_crop,regex("^Sweet Corn Frozen$", ignore_case=TRUE))~"56",
  str_detect(Original_crop,regex("^Sweet Corn Prep or Preserved$", ignore_case=TRUE))~"",
  str_detect(Original_crop,regex("^Sweet potato$", ignore_case=TRUE))~"122",
  str_detect(Original_crop,regex("^Sweet Potato$", ignore_case=TRUE))~"122",
  str_detect(Original_crop,regex("^Sweet potatoes$", ignore_case=TRUE))~"122",
  str_detect(Original_crop,regex("^Switchgrass$", ignore_case=TRUE))~"639",
  str_detect(Original_crop,regex("^Switchgrass \\(DM\\)$", ignore_case=TRUE))~"639",
  str_detect(Original_crop,regex("^Table beets$", ignore_case=TRUE))~"157",
  str_detect(Original_crop,regex("^Table grapes$", ignore_case=TRUE))~"560",
  str_detect(Original_crop,regex("^Table olives$", ignore_case=TRUE))~"260",
  str_detect(Original_crop,regex("^Tall fescue$", ignore_case=TRUE))~"639",
  str_detect(Original_crop,regex("^Tallow$", ignore_case=TRUE))~"1225",
  str_detect(Original_crop,regex("^Tallowtree Seeds$", ignore_case=TRUE))~"305",
  str_detect(Original_crop,regex("^Tangerine Juice$", ignore_case=TRUE))~"496",
  str_detect(Original_crop,regex("^Tangerines, mandarins, clem.$", ignore_case=TRUE))~"495",
  str_detect(Original_crop,regex("^Tangerines, mandarins, clementines, satsumas$", ignore_case=TRUE))~"495",
  str_detect(Original_crop,regex("^Tangerines; mandarins; clem.$", ignore_case=TRUE))~"495",
  str_detect(Original_crop,regex("^Tapioca of Cassava$", ignore_case=TRUE))~"127",
  str_detect(Original_crop,regex("^Tapioca of Potatoes$", ignore_case=TRUE))~"121",
  str_detect(Original_crop,regex("^Taro \\(cocoyam\\)$", ignore_case=TRUE))~"136",
  str_detect(Original_crop,regex("^Tea$", ignore_case=TRUE))~"667",
  str_detect(Original_crop,regex("^Teff$", ignore_case=TRUE))~"108",
  str_detect(Original_crop,regex("^Temperate cereals$", ignore_case=TRUE))~"108",
  str_detect(Original_crop,regex("^Timothy$", ignore_case=TRUE))~"639",
  str_detect(Original_crop,regex("^Timothy  \\(DM\\)$", ignore_case=TRUE))~"639",
  str_detect(Original_crop,regex("^Tobacco$", ignore_case=TRUE))~"826",
  str_detect(Original_crop,regex("^Tobacco leaves$", ignore_case=TRUE))~"826",
  str_detect(Original_crop,regex("^Tobacco Products Nes$", ignore_case=TRUE))~"831",
  str_detect(Original_crop,regex("^Tobacco, flue-cured$", ignore_case=TRUE))~"826",
  str_detect(Original_crop,regex("^Tobacco, unmanufactured$", ignore_case=TRUE))~"826",
  str_detect(Original_crop,regex("^Tomato$", ignore_case=TRUE))~"388",
  str_detect(Original_crop,regex("^Tomato Peeled$", ignore_case=TRUE))~"392",
  str_detect(Original_crop,regex("^Tomatoes$", ignore_case=TRUE))~"388",
  str_detect(Original_crop,regex("^Tomatojuice Concentrated$", ignore_case=TRUE))~"390",
  str_detect(Original_crop,regex("^Tree leaf$", ignore_case=TRUE))~"",
  str_detect(Original_crop,regex("^Triticale$", ignore_case=TRUE))~"97",
  str_detect(Original_crop,regex("^Tropical cereals$", ignore_case=TRUE))~"108",
  str_detect(Original_crop,regex("^Tung Nuts$", ignore_case=TRUE))~"275",
  str_detect(Original_crop,regex("^Turkey meat$", ignore_case=TRUE))~"1089",
  str_detect(Original_crop,regex("^Turnips$", ignore_case=TRUE))~"426",
  str_detect(Original_crop,regex("^Turnips for Fodder$", ignore_case=TRUE))~"426",
  str_detect(Original_crop,regex("^Vanilla$", ignore_case=TRUE))~"692",
  str_detect(Original_crop,regex("^Veg Prod for Feed$", ignore_case=TRUE))~"652",
  str_detect(Original_crop,regex("^Veg.in Tem. Preservatives$", ignore_case=TRUE))~"475",
  str_detect(Original_crop,regex("^Veg.Prep. Or Pres.Frozen$", ignore_case=TRUE))~"473",
  str_detect(Original_crop,regex("^Veg.Prod.Fresh Or Dried$", ignore_case=TRUE))~"460",
  str_detect(Original_crop,regex("^Vegetable Frozen$", ignore_case=TRUE))~"473",
  str_detect(Original_crop,regex("^Vegetable leaf$", ignore_case=TRUE))~"652",
  str_detect(Original_crop,regex("^Vegetable Tallow$", ignore_case=TRUE))~"306",
  str_detect(Original_crop,regex("^Vegetables$", ignore_case=TRUE))~"463",
  str_detect(Original_crop,regex("^Vegetables Dehydrated$", ignore_case=TRUE))~"469",
  str_detect(Original_crop,regex("^Vegetables fresh nes$", ignore_case=TRUE))~"463",
  str_detect(Original_crop,regex("^Vegetables in Vinegar$", ignore_case=TRUE))~"471",
  str_detect(Original_crop,regex("^Vegetables Preserved Nes$", ignore_case=TRUE))~"472",
  str_detect(Original_crop,regex("^Vegetables Roots Fodder$", ignore_case=TRUE))~"149",
  str_detect(Original_crop,regex("^Vegetables, dried nes$", ignore_case=TRUE))~"469",
  str_detect(Original_crop,regex("^Vegetables, fresh nes$", ignore_case=TRUE))~"1735",
  str_detect(Original_crop,regex("^Vegetables, leguminous nes$", ignore_case=TRUE))~"420",
  str_detect(Original_crop,regex("^Vermouths&Similar$", ignore_case=TRUE))~"565",
  str_detect(Original_crop,regex("^Vetch$", ignore_case=TRUE))~"205",
  str_detect(Original_crop,regex("^Vetch  \\(DM\\)$", ignore_case=TRUE))~"205",
  str_detect(Original_crop,regex("^Vetches$", ignore_case=TRUE))~"205",
  str_detect(Original_crop,regex("^Vitamins$", ignore_case=TRUE))~"853",
  str_detect(Original_crop,regex("^Wafers$", ignore_case=TRUE))~"110",
  str_detect(Original_crop,regex("^Walnuts Shelled$", ignore_case=TRUE))~"232",
  str_detect(Original_crop,regex("^Walnuts, with shell$", ignore_case=TRUE))~"222",
  str_detect(Original_crop,regex("^Walnuts; with shell$", ignore_case=TRUE))~"222",
  str_detect(Original_crop,regex("^Water hyacinth$", ignore_case=TRUE))~"",
  str_detect(Original_crop,regex("^Watermelons$", ignore_case=TRUE))~"567",
  str_detect(Original_crop,regex("^Waxes Vegetable$", ignore_case=TRUE))~"1296",
  str_detect(Original_crop,regex("^Wheat$", ignore_case=TRUE))~"15",
  str_detect(Original_crop,regex("^Wheat \\(spring\\) grain$", ignore_case=TRUE))~"15",
  str_detect(Original_crop,regex("^Wheat \\(winter\\) grain$", ignore_case=TRUE))~"15",
  str_detect(Original_crop,regex("^Wheat bran$", ignore_case=TRUE))~"17",
  str_detect(Original_crop,regex("^Wheat flour$", ignore_case=TRUE))~"111",
  str_detect(Original_crop,regex("^Wheat straw$", ignore_case=TRUE))~"15",
  str_detect(Original_crop,regex("^Wheat straw per t of grain$", ignore_case=TRUE))~"15",
  str_detect(Original_crop,regex("^Wheatgrass$", ignore_case=TRUE))~"",
  str_detect(Original_crop,regex("^Whey Cheese$", ignore_case=TRUE))~"905",
  str_detect(Original_crop,regex("^Whey Condensed$", ignore_case=TRUE))~"890",
  str_detect(Original_crop,regex("^Whey Dry$", ignore_case=TRUE))~"900",
  str_detect(Original_crop,regex("^Whey Fresh$", ignore_case=TRUE))~"890",
  str_detect(Original_crop,regex("^Wine$", ignore_case=TRUE))~"564",
  str_detect(Original_crop,regex("^Winter wheat$", ignore_case=TRUE))~"15",
  str_detect(Original_crop,regex("^Wool Degreased$", ignore_case=TRUE))~"988",
  str_detect(Original_crop,regex("^Wool Shoddy$", ignore_case=TRUE))~"987",
  str_detect(Original_crop,regex("^Wool, greasy$", ignore_case=TRUE))~"987",
  str_detect(Original_crop,regex("^Yams$", ignore_case=TRUE))~"137",
  str_detect(Original_crop,regex("^Yautia \\(cocoyam\\)$", ignore_case=TRUE))~"135",
  str_detect(Original_crop,regex("^Yogh Conc.Or Not$", ignore_case=TRUE))~"892",
  str_detect(Original_crop,regex("^Yoghurt$", ignore_case=TRUE))~"891",
  str_detect(Original_crop,regex("^Zyziphus$", ignore_case=TRUE))~"",
  TRUE ~ Original_crop))
return(df)      
}

#Use function to create item_code column
Unique_original_crops <- Original_crop_to_item_code_converter(Unique_original_crops, Unique_original_crops$Original_Crop)

#Filter FAO_2022_item_codes to Item_Group_Code 1714 to avoid double ups in Item code values (across Domains)-and Item_Group_Code 1714 is for Crops Primary Item Group that are applicable to our analysis.
FAO_2022_item_codes$`Item Group Code` <-as.numeric(FAO_2022_item_codes$`Item Group Code`) #Set Item Group Code to numeric

FAO_2022_item_codes <- filter(FAO_2022_item_codes,`Item Group Code`==1714)

#Add other codes to Unique_original_crops data frame based on Item Code
Unique_original_crops <- merge(Unique_original_crops, FAO_2022_item_codes, by.x=c("item_code"), by.y=c("Item Code"))

#Save as csv file
write.csv(Unique_original_crops,"data/Unique_original_crops_with_UN_codes.csv",row.names=FALSE)

#Merge Unique_original_crops and Crop_df data frames together so every row has UN item codes if they happen to be applicable to Item Group Code 1714.
Crop_df <- merge(Crop_df, Unique_original_crops, by.x=c("Original_crop"), by.y=c("Original_crop"), all.x=T)

#Rename column header names so spaces are replaced by "_"'s and all column header letters are lower case.
names(Crop_df)  %<>% stringr::str_replace_all("\\s","_") %>% tolower

#Add Item Codes using function. I do not add item names to these because the names vary depending on what UN domain they are used in.   
Crop_df <- Original_crop_to_item_code_converter(Crop_df , Crop_df$original_crop)

#Reorder columns of Crop_df
Crop_df <- select(Crop_df, 
                  original_crop,
                  item,
                  item_code,
                  item_group_code,
                  item_group,
                  factor,
                  hs_code,
                  hs07_code,
                  hs12_code,
                  cpc_code,
                  original_crop_component,
                  crop_component,
                  original_region,
                  iso3_code,
                  country_iso3c,
                  un_region_name,
                  un_sub_region_name,
                  un_intermediate_region_name,
                  latitude,
                  longitude,
                  reference_where_data_were_collated,
                  website_of_source_of_collated_data,
                  primary_reference_of_dataset,
                  variable,
                  value)

#Replace referrals to \nNitrogen_to_protein_factor with Nitrogen_to_protein_factor.
Crop_df$variable<- gsub("\nNitrogen_to_protein_factor","Nitrogen_to_protein_factor",Crop_df$variable)#Exclude "_"'s from original_region column

#Create meta-data file for Crop_df
Meta_data_Crop_df <- as.data.frame(unique(Crop_df$variable)) 

#Rename variable column
Meta_data_Crop_df <- rename(Meta_data_Crop_df,Variable ="unique(Crop_df$variable)")

#Extract nutrients
Meta_data_Crop_df$Nutrient <- str_extract(Meta_data_Crop_df$Variable, "[^_]+")  

#Create unit column for some of the fresh weight variables (this makes is easier to differentiate with the other pc variables that are on a dry matter basis)
Meta_data_Crop_df<-Meta_data_Crop_df %>% mutate(Description=case_when(
  str_detect(Variable,regex("HI$", ignore_case=TRUE))~"Mean harvest index as a proportion of crop products as above ground biomass",
  str_detect(Variable,regex("HI_sd$", ignore_case=TRUE))~"Standard deviation of harvest index as a proportion of crop products as above ground biomass",
  str_detect(Variable,regex("DM_pc$", ignore_case=TRUE))~"Mean dry matter percentage of fresh weight",
  str_detect(Variable,regex("DM_pc_sd$", ignore_case=TRUE))~"Standard deviation of dry matter percentage of fresh weight",
  str_detect(Variable,regex("CR_removed_pc", ignore_case=TRUE))~"Mean percentage of crop residue removed from harvested area",
  str_detect(Variable,regex("Water_pc", ignore_case=TRUE))~"Mean water (moisture) percentage of fresh weight",
  str_detect(Variable,regex("Fixed_N_kg_N_metric_t_fresh_crop_product", ignore_case=TRUE))~"Mean quantity of nitrogen fixed in kilograms of nitrogen per tonne of fresh weight crop product",
  str_detect(Variable,regex("Recycled_N_kg_N_t_fresh_crop_product", ignore_case=TRUE))~"Mean quantity of nitrogen recycled in kilograms of nitrogen per tonne of fresh weight crop product",
  str_detect(Variable,regex("Recycled_P_kg_P_t_fresh_crop_product", ignore_case=TRUE))~"Mean quantity of elemental phosphorus recycled in kilograms of phosphorus per tonne of fresh weight crop product",
  str_detect(Variable,regex("Recycled_K_kg_K_t_fresh_crop_product", ignore_case=TRUE))~"Mean quantity of elemental potassium recycled in kilograms of potassium per tonne of fresh weight crop product",
  str_detect(Variable,regex("Biomass_productivity_g_per_g", ignore_case=TRUE))~"Biomass productivity in grams per gram",
  str_detect(Variable,regex("Nitrogen_requirement_of_crop_mg_per_g", ignore_case=TRUE))~"Nitrogen requirement of crop in milligrams per gram",
  str_detect(Variable,regex("Mean_Above_ground_biomass_kg_DM_ha", ignore_case=TRUE))~"Mean above ground biomass in kilograms of dry matter per hectare",
  str_detect(Variable,regex("n_Above_ground_biomass_kg_DM_ha", ignore_case=TRUE))~"Number of samples used for estimating mean above ground biomass",
  str_detect(Variable,regex("Minimum_Above_ground_biomass_kg_DM_ha", ignore_case=TRUE))~"Minimum value for above ground biomass in kilograms of dry matter per hectare",
  str_detect(Variable,regex("Maximum_Above_ground_biomass_kg_DM_ha", ignore_case=TRUE))~"Maximum value for above ground biomass in kilograms of dry matter per hectare",
  str_detect(Variable,regex("Mean_Grain_yield_kg_DM_ha", ignore_case=TRUE))~"Mean grain yield in kilograms of dry matter per hectare",
  str_detect(Variable,regex("n_Grain_yield_kg_DM_ha", ignore_case=TRUE))~"Number of samples used for estimating mean grain yield",
  str_detect(Variable,regex("Minimum_Grain_yield_kg_DM_ha", ignore_case=TRUE))~"Minimum value for grain yield in kilograms of dry matter per hectare",
  str_detect(Variable,regex("Maximum_Grain_yield_kg_DM_ha", ignore_case=TRUE))~"Maximum value for grain yield in kilograms of dry matter per hectare",
  str_detect(Variable,regex("n_Harvest_index", ignore_case=TRUE))~"Number of samples used for estimating mean harvest index",
  str_detect(Variable,regex("Minimum_Harvest_index", ignore_case=TRUE))~"Minimum value for harvest index where harvest index is a proportion",
  str_detect(Variable,regex("Maximum_Harvest_index", ignore_case=TRUE))~"Maximum value for harvest index where harvest index is a proportion",
  str_detect(Variable,regex("P_pc_no_samples", ignore_case=TRUE))~"Number of samples used for estimating mean elemental phosphorus concentration",
  str_detect(Variable,regex("Ratio_residues_removed_from_field", ignore_case=TRUE))~"Ratio of crop residues removed from the field",
  str_detect(Variable,regex("No_samples", ignore_case=TRUE))~"Number of samples",
  str_detect(Variable,regex("DM_pc_max", ignore_case=TRUE))~"Maximum value for dry matter percentage",
  str_detect(Variable,regex("DM_pc_min", ignore_case=TRUE))~"Minimum value for dry matter percentage",
  str_detect(Variable,regex("Protein_pc_max", ignore_case=TRUE))~"Maximum value for protein percentage in percentage of dry matter as protein",
  str_detect(Variable,regex("Protein_pc_min", ignore_case=TRUE))~"Minimum value for protein percentage in percentage of dry matter as protein",
  str_detect(Variable,regex("Yield_kg_DM_ha", ignore_case=TRUE))~"Mean yield in kilograms of dry matter per hectare",
  str_detect(Variable,regex("Yield_kg_DM_ha_sd", ignore_case=TRUE))~"Standard deviation of yield in kilograms of dry matter per hectare",
  str_detect(Variable,regex("HI_sd", ignore_case=TRUE))~"Standard deviation of harvest index where harvest index is a proportion",
  str_detect(Variable,regex("N_pc_min_90pc_CI", ignore_case=TRUE))~"Percentage of dry matter as nitrogen, minimum value for nitrogen concentration of crop component. Note (as per pg 1-2 of Nijhof (1987)) 'the established ranges cover 90% [confidence interval, CI] of the values found, i.e. the upper and lower extremes were excluded'.",
  str_detect(Variable,regex("N_pc_max_90pc_CI", ignore_case=TRUE))~"Percentage of dry matter as nitrogen, maximum value for nitrogen concentration of crop component. Note (as per pg 1-2 of Nijhof (1987)) 'the established ranges cover 90% [confidence interval, CI] of the values found, i.e. the upper and lower extremes were excluded'.",
  str_detect(Variable,regex("P_pc_min_90pc_CI", ignore_case=TRUE))~"Percentage of dry matter as elemental phosphorus, minimum value for elemental phosphorus concentration of crop component. Note (as per pg 1-2 of Nijhof (1987)) 'the established ranges cover 90% [confidence interval, CI] of the values found, i.e. the upper and lower extremes were excluded'.",
  str_detect(Variable,regex("P_pc_max_90pc_CI", ignore_case=TRUE))~"Percentage of dry matter as elemental phosphorus, maximum value for elemental phosphorus concentration of crop component. Note (as per pg 1-2 of Nijhof (1987)) 'the established ranges cover 90% [confidence interval, CI] of the values found, i.e. the upper and lower extremes were excluded'.",
  str_detect(Variable,regex("K_pc_min_90pc_CI", ignore_case=TRUE))~"Percentage of dry matter as elemental potassium, minimum value for elemental potassium concentration of crop component. Note (as per pg 1-2 of Nijhof (1987)) 'the established ranges cover 90% [confidence interval, CI] of the values found, i.e. the upper and lower extremes were excluded'.",
  str_detect(Variable,regex("K_pc_max_90pc_CI", ignore_case=TRUE))~"Percentage of dry matter as elemental potassium, maximum value for elemental potassium concentration of crop component. Note (as per pg 1-2 of Nijhof (1987)) 'the established ranges cover 90% [confidence interval, CI] of the values found, i.e. the upper and lower extremes were excluded'.",
  str_detect(Variable,regex("Nitrogen_to_protein_factor", ignore_case=TRUE))~"Factor used to convert nitrogen concentration to protein concentration",
  str_detect(Variable,regex("N_pc_max", ignore_case=TRUE))~"Maximum value for nitrogen concentration as a percentage of dry matter",
  str_detect(Variable,regex("N_pc_min", ignore_case=TRUE))~"Minimum value for nitrogen concentration as a percentage of dry matter",
  str_detect(Variable,regex("P_pc_min", ignore_case=TRUE))~"Minimum value for elemental phosphorus concentration as a percentage of dry matter",
  str_detect(Variable,regex("K_pc_min", ignore_case=TRUE))~"Minimum value for elemental potassium concentration as a percentage of dry matter",
  TRUE~"NA")) 

#Turn NAs to true missing values for coalesce.  
Meta_data_Crop_df$Description <- ifelse(grepl("NA",Meta_data_Crop_df$Description),NA,Meta_data_Crop_df$Description)

#Create unit column for some of the fresh weight variables (this makes is easier to differentiate with the other pc variables that are on a dry matter basis)
Meta_data_Crop_df<-Meta_data_Crop_df %>% mutate(Description1=case_when(
  str_detect(Variable,regex("_pc$", ignore_case=TRUE))~"Mean percentage of dry matter as ",
  str_detect(Variable,regex("_pc_sd$", ignore_case=TRUE))~"Standard deviation of percentage of dry matter as ",
  str_detect(Variable,regex("_pc_fresh$", ignore_case=TRUE))~"Mean percentage of fresh weight as ",
  TRUE~"NA")) 

#Turn NAs to true missing values for coalesce.  
Meta_data_Crop_df$Description1 <- ifelse(grepl("NA",Meta_data_Crop_df$Description1),NA,Meta_data_Crop_df$Description1)

#Paste columns of info together to link units with elements
Meta_data_Crop_df$Description1 <- paste(Meta_data_Crop_df$Description1,Meta_data_Crop_df$Nutrient)

#Coalesce columns of information together
Meta_data_Crop_df$Description <- coalesce(Meta_data_Crop_df$Description,Meta_data_Crop_df$Description1)

#De-select unnecessary columns
Meta_data_Crop_df <- select(Meta_data_Crop_df, -Description1, -Nutrient)

#Create Units column
Meta_data_Crop_df<-Meta_data_Crop_df %>% mutate(Units=case_when(
  str_detect(Description,regex("percentage", ignore_case=TRUE))~"Percentage",
  str_detect(Description,regex("kilograms of nitrogen per tonne of fresh weight crop product", ignore_case=TRUE))~"kg N per tonne fresh weight of crop products",  
  str_detect(Description,regex("kilograms of phosphorus per tonne of fresh weight crop product", ignore_case=TRUE))~"kg elemental P per tonne fresh weight of crop products",   
  str_detect(Description,regex("milligrams per gram", ignore_case=TRUE))~"Mg per gram",  
  str_detect(Description,regex("kilograms of dry matter per hectare", ignore_case=TRUE))~"kg DM per hectare",  
  str_detect(Description,regex("Mean quantity of elemental potassium recycled in kilograms of potassium per tonne of fresh weight crop product", ignore_case=TRUE))~"kg elemental K per tonne fresh weight crop products",    
  str_detect(Description,regex("Mean harvest index as a proportion of crop products as above ground biomass", ignore_case=TRUE))~"Proportion",  
  str_detect(Description,regex("Factor used to convert nitrogen concentration to protein concentration", ignore_case=TRUE))~"Numeric value",  
  str_detect(Description,regex("Number of samples used for estimating mean above ground biomass", ignore_case=TRUE))~"Numeric value",  
  str_detect(Description,regex("Number of samples used for estimating mean grain yield", ignore_case=TRUE))~"Numeric value",  
  str_detect(Description,regex("Number of samples used for estimating mean harvest index", ignore_case=TRUE))~"Numeric value",  
  str_detect(Description,regex("Minimum value for harvest index where harvest index is a proportion", ignore_case=TRUE))~"Proportion",  
  str_detect(Description,regex("Maximum value for harvest index where harvest index is a proportion", ignore_case=TRUE))~"Proportion",  
  str_detect(Description,regex("Number of samples used for estimating mean elemental phosphorus concentration", ignore_case=TRUE))~"Numeric value",  
  str_detect(Description,regex("Ratio of crop residues removed from the field", ignore_case=TRUE))~"Ratio",  
  str_detect(Description,regex("Number of samples", ignore_case=TRUE))~"Numeric value", 
  str_detect(Description,regex("Standard deviation of harvest index as a proportion of crop products as above ground biomass", ignore_case=TRUE))~"Proportion", 
  str_detect(Description,regex("grams per gram", ignore_case=TRUE))~"grams per gram", 
  TRUE~"NA")) %>% arrange(Variable)

#Create a data frame with country names for each Original_region
#List unique Original_region names
Meta_data_Original_regions <- as.data.frame(unique(Crop_df_references$Original_region))

#Rename variable column
colnames(Meta_data_Original_regions) <- c("Original_region")

#Create meta data file for Crop_df---
#Create dataframe with header names in separate rows. 
Meta_data_Crop_df_a <- as.data.frame(colnames(Crop_df))

#Add meta-data information to data frame
Meta_data_Crop_df_a<- rename(Meta_data_Crop_df_a, "Parameter"="colnames(Crop_df)")
Meta_data_Crop_df_a$Description <- c( "Name of original crop used in reference where data were collated",
                                      "Item name following UN FAO statistical conventions (note only item names were given to crops in the Item Group Code 1714 it was too difficult to assign Item names to other Item codes because the names vary depending on Item Group Code or Domain)",
                                      "Item code following UN FAO statistical conventions",
                                      "Item code group following UN FAO statistical conventions",
                                      "Item group following UN FAO statistical conventions",
                                      "Factor value based on UN FAO statistical conventions",
                                      "Harmonized System Code",
                                      "Harmonized System Code 2007",
                                      "Harmonized System Code 2012",
                                      "Central Product Classification for goods and services promulgated by the United Nations Statistical Commission",
                                      "Name of original crop category used in source of data",
                                      "Standardised crop component category that the values are applicable to, either as Crop_products or Crop_residues. These were standardised based on the original_crop_component information",
                                      "Name of original region used in source document or dataset",
                                      "The 3-letter alpha-3 country code following the ISO 3166 international standards, see https://www.iso.org/publication/PUB500001.html",
                                      "The country name following the English ISO 3166 international standards, see https://www.iso.org/publication/PUB500001.html",
                                      "UN region name based on ISO 3166 country name",
                                      "UN sub-region name based on ISO 3166 country name",
                                      "UN intermediate region name based on ISO 3166 country name",
                                      "Latitude of origin of value if applicable. Many data do not have latitude as they represent whole regions or countries",
                                      "Longitude of origin of value if applicable. Many data do not have longitude as they represent whole regions or countries",
                                      "Details of reference where there data were collated",
                                      "Details of the website of the reference where data were collated",
                                      "Details of the primary reference if the reference data were collated from came from multiple sources",
                                      "Name of variable. Note in some cases these variables had been standardised from the original values so that nutrient concentrations were available on a percentage of dry matter basis",
                                      "Numeric value of variable")


Meta_data_Crop_df_a$Format <- c("Character","Character","Character","Numeric","Character","Numeric","Character","Character",
                                "Character","Character","Character","Character","Character","Character","Character","Character",
                                "Character","Character","Numeric","Numeric","Character","Character","Character","Character","Numeric")

Meta_data_Crop_df_a$Units <- c("Character","Character","Character","Numeric","Character","Numeric","Character","Character",
                               "Character","Character","Character","Character","Character","Character","Character","Character",
                               "Character","Character","Numeric","Numeric","Character","Character","Character","Character with a description of each variable available in the accompanying Meta_data_Combined_crop_data file","Numeric")

#Save csv files
write.csv(Crop_df,"data/standardised/Combined_crop_data.csv", row.names = FALSE)
write.csv(Crop_df_references,"data/standardised/Combined_crop_data_references.csv", row.names = FALSE)
write.csv(UN_countries_codes,"data/standardised/UN_countries_codes.csv", row.names = FALSE)
write.csv(UN_country_codes_summary,"data/standardised/UN_country_codes_summary.csv",row.names=FALSE)
write.csv(Transposed_UN_country_codes_summary,"data/standardised/Transposed_UN_country_codes_summary.csv",row.names=FALSE)
write.csv(Meta_data_Original_regions, "data/standardised/Original_regions_listed")
write.csv(Meta_data_Crop_df_a,"data/standardised/Meta_data_Combined_crop_data_1.csv",row.names= FALSE)
write.csv(Meta_data_Crop_df,"data/standardised/Meta_data_Combined_crop_data_2.csv", row.names = FALSE)

#Tidy Transposed_UN_country_codes_summary csv file. 
headers = read.csv("data/standardised/Transposed_UN_country_codes_summary.csv", skip = 1, header = F, nrows = 1, as.is = T)
Transposed_UN_country_codes_summary = read.csv("data/standardised/Transposed_UN_country_codes_summary.csv", skip = 2, header = F)
colnames(Transposed_UN_country_codes_summary)= headers
write.csv(Transposed_UN_country_codes_summary,"data/standardised/Transposed_UN_country_codes_summary.csv",row.names=FALSE)

#Create melted dataframe of all original_regions and their listed countries and iso3c codes. List in alphabetic order of country name.For appendix.
df_countries <- Transposed_UN_country_codes_summary
df_countries <- melt(df_countries,id="World")
df_countries <- na.omit(df_countries[]) %>% select(-variable) %>% rename(original_region=World, country=value) #Exclude NAs and rename columns
df_countries <- df_countries %>%
  group_by(country) %>%
  summarise(original_region = paste(original_region, collapse = "| "))
df_countries$original_region <- gsub("_"," ",df_countries$original_region)#Exclude "_"'s from original_region column
df_countries$iso3_code <- countrycode(df_countries$country, origin = 'country.name', destination = 'iso3c')
df_countries <- select(df_countries,country,iso3_code,original_region )#Select order of columns.
write.csv(df_countries,"data/standardised/Original_region_names_and_assigned_countries_for_appendix.csv",row.names=FALSE)

#Create melted dataframe of all original_regions and their listed countries and iso3c codes. List in alphabetic order of country name.For easier machine readable format.
df_countries <- Transposed_UN_country_codes_summary
df_countries <- melt(df_countries,id="World")
df_countries <- na.omit(df_countries[]) %>% select(-variable) %>% rename(original_region=World, country=value) #Exclude NAs and rename columns
#df_countries <- df_countries %>%
 # group_by(country) %>%
#  summarise(original_region = paste(original_region, collapse = "| "))
df_countries$original_region <- gsub("_"," ",df_countries$original_region)#Exclude "_"'s from original_region column
df_countries$iso3_code <- countrycode(df_countries$country, origin = 'country.name', destination = 'iso3c')
df_countries <- select(df_countries,country,iso3_code,original_region )#Select order of columns.
write.csv(df_countries,"data/standardised/Original_region_names_and_assigned_countries.csv",row.names=FALSE)

#Create data frame with original_crop on one column and UN item name and code in other column. 
Crop_df_1 <- mutate(Crop_df) %>% 
  select(item, item_code,item_group_code,original_crop) %>% 
  filter(item_group_code ==1714) 
Crop_df_1$item_original_crop <- paste(Crop_df_1$item,"_",Crop_df_1$original_crop)
Crop_df_1 <- unique(select(Crop_df_1, item, item_code, original_crop))


write.csv(Crop_df_1,"data/standardised/Original_crop_names_in_each_item_category.csv",row.names=FALSE)
Crop_df_1 <- Crop_df_1 %>%
  group_by(item, item_code) %>%
  summarise(original_crop = paste(original_crop, collapse = "| "))
write.csv(Crop_df_1,"data/standardised/Original_crop_names_in_each_item_category_appendix_format.csv",row.names=FALSE)