#Description of script: ---
##This script uses the standardised data created from script_1 and creates plots showing variation in values between sources of data.

#This script was written in R version 4.1.0 with a 64-bit computer.

#Libraries----
library(readr)
library(tidyverse)
library(ggplot2)

#Settings----
#Ensure all values are decimals rather than scientific notation. See: https://stackoverflow.com/questions/44725001/convert-scientific-notation-to-numeric-preserving-decimals/44725320
options(scipen = 999)

#Read files----
df <- as.data.frame(read_csv("data/standardised/Combined_crop_data.csv"))

#Filter to HI variables
df_HI <- filter(df, variable=="HI")

#Filter df to only 'Crop_products' that are included in the 1714 Item Group Code ("Crops Primary").----
df <- filter(df, item_group_code==1714, 
             crop_component=="Crop_products")

#Row bind HI data with Crop_products data.
df <- rbind(df,df_HI)

#Filter to variables of interest
var_interest <- c("HI","DM_pc","N_pc","P_pc","K_pc", "N_pc_fresh","P_pc_fresh","K_pc_fresh")
df <- filter(df, variable %in% var_interest)

#Create dataframe with mean dry matter percentages (note that there are not enough dry matter percentages for every FAO crop if we filter to only 'World' as the original_region, so mean values are across all data)----
#This will help us convert nutrient concentration (on a DM basis) to a fresh weight basis for comparison with the original FAO coefficients. 
df_DM <- filter(df, 
                variable =="DM_pc")

df_DM_means <- df_DM %>% 
  group_by(item, item_code, item_group_code) %>% 
  select(-factor,-cpc_code,-latitude,-longitude)%>% 
  summarise(across(where(is.numeric),mean,na.rm=TRUE)) %>% 
  rename("DM_pc"=value)

#Where we only have nutrient concentrations on a dry matter basis convert to a fresh weight basis so we maximise use of data and then add to the df with only fresh weight measures..----
#Create dataframe with only the nutrient concentrations on a dry matter basis so they can be converted to a fresh basis.
DM_var_interest <- c("HI","N_pc","P_pc","K_pc")
dfa <- filter(df, variable %in% DM_var_interest)

dfa <- merge(dfa, df_DM_means,by.x=c("item","item_code","item_group_code"), by.y=c("item","item_code","item_group_code"),)

dfa <- mutate(dfa, 
              "value1"=value*(DM_pc/100),
              "variable1"=case_when(
                str_detect(variable,regex("N_pc", ignore_case=TRUE))~"N_pc_fresh",
                str_detect(variable,regex("P_pc", ignore_case=TRUE))~"P_pc_fresh",                
                str_detect(variable,regex("K_pc", ignore_case=TRUE))~"K_pc_fresh",
                TRUE~variable)) %>% select(-variable, -value, -DM_pc) %>% 
  rename("variable"="variable1",
         "value"="value1")

#Create data frame with only the nutrient concentrations on a fresh basis
Fresh_var_interest <- c("HI","N_pc_fresh","P_pc_fresh","P2O5_pc_fresh","K_pc_fresh","K2O_pc_fresh", "DM_pc")
dfb <- filter(df, variable %in% Fresh_var_interest)
df <- rbind(dfa,dfb) #Create combined dataframe with only variables of interest, including all nutrient concentrations on a fresh weight basis.

df <- unique(df) #Removed double ups in fresh weight nutrient concentrations.

#Summarise variables for each item for each reference_where_data_were_collated.
df <- df %>% select(-factor,-cpc_code,-latitude,-longitude) %>% 
  group_by(item, item_code, variable,original_region, reference_where_data_were_collated) %>% #Note if you want to show variation in values per source between original_regions then add original_region in this 'group_by' list ,
  summarise(across(where(is.numeric),mean,na.rm=TRUE))

#Filter to data that purport to represent the world and for main crops and variables of interest.
df <- filter(df, #original_region=="World", 
             variable %in% c("HI","DM_pc","N_pc_fresh","P_pc_fresh","K_pc_fresh"),
             item %in% c("Barley","Rapeseed","Maize","Potatoes","Rice, paddy","Soybeans","Wheat"))

#Pivot wider the variables so they are each in separate columns
df <- pivot_wider(df, names_from = variable, values_from = value)

#Create columnn with abbreviated source of data information so it fits better on a plot legend.---
df <- df %>% mutate(reference=case_when(
  str_detect(reference_where_data_were_collated, regex("Unkovich et al", ignore_case = TRUE))~"Unkovich et al (2010)",
  str_detect(reference_where_data_were_collated, regex("Panagos", ignore_case = TRUE))~"Panagos et al (2022)",
  str_detect(reference_where_data_were_collated, regex("Bouwman", ignore_case = TRUE))~"Bouwman et al (2017)",
  str_detect(reference_where_data_were_collated, regex("IPCC \\( 2019 \\)", ignore_case = TRUE))~"IPCC (2019)",
  str_detect(reference_where_data_were_collated, regex("Koopmans", ignore_case = TRUE))~"Koopmans & Koppejan (2008)",
  str_detect(reference_where_data_were_collated, regex("Kumssa", ignore_case = TRUE))~"Kumssa et al (2022)",
  str_detect(reference_where_data_were_collated, regex("Roy \\(2016\\)", ignore_case = TRUE))~"Roy (2016)",
  str_detect(reference_where_data_were_collated, regex("USDA \\(2009\\)", ignore_case = TRUE))~"USDA (2009)",
  str_detect(reference_where_data_were_collated, regex("FAO\\(2004\\)", ignore_case = TRUE))~"FAO (2004)",
  str_detect(reference_where_data_were_collated, regex("FAO\\(2020\\)", ignore_case = TRUE))~"FAO (2020)",
  str_detect(reference_where_data_were_collated, regex("International Fertilizer", ignore_case = TRUE))~"IFA (2020)",
  str_detect(reference_where_data_were_collated, regex("IPNI \\(2014\\)", ignore_case = TRUE))~"IPNI (2014)",
  str_detect(reference_where_data_were_collated, regex("Lassaletta et al \\(2014\\)", ignore_case = TRUE))~"Lassaletta et al (2014)",
  str_detect(reference_where_data_were_collated, regex("Zhang et al \\(2021\\)", ignore_case = TRUE))~"Zhang et al (2021)",
  str_detect(reference_where_data_were_collated, regex("Swain et al \\(2022\\)", ignore_case = TRUE))~"Swain et al (2022)",
  str_detect(reference_where_data_were_collated, regex("Bessembinder, J \\(1995\\)", ignore_case = TRUE))~"Bessembinder (1995)",
  str_detect(reference_where_data_were_collated, regex("Norton \\(2011\\)", ignore_case = TRUE))~"Norton (2011)",
  TRUE~reference_where_data_were_collated))

#Create function for plotting various nutrient concentrations----
fun_plot <- function(df, x,y,shape,size,ylab){
  
  ggplot(df)+
    geom_point(aes(x = {{ x }}, y = {{ y }}, shape= {{ shape }}, size=size)) +
    theme_classic() +
    guides(size = "none")+ 
    labs(shape = "Source of data")+
    ylab({{ ylab}}) + theme(text = element_text(size = 10))+
    scale_shape_manual(values=c(0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17))
}

#Apply ggplot function to data.----
plots1 <- list()
plots1$HI_plot <- fun_plot(df=df,
                           x=item,
                           y=HI,
                           shape=reference, 
                           size=14,ylab="HI")
plots1$DM_pc_fresh_plot <- fun_plot(df=df,
                                    x=item,
                                    y=DM_pc,
                                    shape=reference, 
                                    size=14,ylab="Dry matter (% fresh weight as dry matter)")
plots1$N_pc_fresh_plot <- fun_plot(df=df,
                                   x=item,
                                   y=N_pc_fresh,
                                   shape=reference, 
                                   size=14,ylab="Nitrogen (% fresh weight as nutrient)")
plots1$P_pc_fresh_plot <- fun_plot(df=df,
                                   x=item,
                                   y=P_pc_fresh,
                                   shape=reference, 
                                   size=14,ylab="Elemental phosphorus (% fresh weight as nutrient)")
plots1$K_pc_fresh_plot <- fun_plot(df=df,
                                   x=item,
                                   y=K_pc_fresh,
                                   shape=reference, 
                                   size=14,ylab="Elemental potassium (% fresh weight as nutrient)")

#Save the summary plots with unique names
lapply(names(plots1), 
       function(x)ggsave(path = "./results/", 
                         filename=paste(x,"_selected_crops.jpg",sep=""), 
                         plot=plots1[[x]],width=15,height=5))
#Note, some sources had multiple values because they had multiple estimates per region. 
