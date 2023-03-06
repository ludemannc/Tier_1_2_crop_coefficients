This README_LUDEMANN_CROP_DATA_2022.txt file was generated on 2023-03-01 by Cameron Ludemann

GENERAL INFORMATION

1. Titles of Datasets: 

README_LUDEMANN_CROP_DATA_2022.txt
Provides details of all the datafiles in this DRYAD submission.

Data_diagram_25102022.pdf
Illustrates the general process for standardisation of data into the final formats.

Combined dataset (Combined_crop_data.csv)
This dataset includes the combined crop coefficient data from all sources before it was summarised by Tier category. Two meta data files accompany the Combined_crop_data.csv file. The Meta_data_Combined_crop_data_1.csv file describes the header parameters, while the Meta_data_Combined_crop_data_2.csv file describes units for each variable used in the file.

Data summarised at Tier 1 and 2 level (Tier_1_and_2_crop_coefficients.csv) 
This dataset includes a summary of the data from the Combined_crop_data.csv file. Data were summarised into mean values across sources that purported to represent the World (indicated as being Tier 1) and across sources that purported to represent different regions of the world (Tier 2). The Tier 2 crop coefficients excluded duplicated data from the same primary source (e.g. secondary sources that used data from IPNI (2004)). Tier 2 data were summarised for each UN recognised country based on mean values from regions that were applicable for each country where data were available. In the absence of any better country or sub-country specific survey data, it is envisioned that preference be given to using the Tier 2 country estimates of crop coefficients in any global analyses. If any countries do not have Tier 2 crop coefficients then Tier 1 values can be used.
A meta data file accompanies this file and is named Meta_data_Tier_1_and_2_crop_coefficients.csv. This describes the header parameters.

Data summarised for UN FAO Cropland Nutrient Budget (World_crop_coefficients_for_UN_FAO.csv)
This file contains a summary of Tier 1 data from the Tier_1_and_2_crop_coefficients.csv file, and it was used in the 2022 update of the UN FAO Cropland Nutrient Budget.  A meta data file accompanies this file and is named Meta_data_World_crop_coefficients_for_UN_FAO.csv. This describes the header parameters.

Crop category data file (Original_crop_names_in_each_item_category.csv)
This file includes information on the UN FAO crop item categories, crop item codes and original crop names assigned to each item in this dataset. The parameter named item indicates the United Nations crop type ‘item’ name, item_code indicates the United Nations item code number, and original_crop  indicates the name of the crop category originally used in the source of data.

Region data file (Original_region_names_and_assigned_countries.csv)
This file includes information on countries and alpha-3 country codes assigned to each original region category (used in the sources of data). The parameter named country indicates the official United Nations English name of the country, iso3_code indicates the 3-letter ISO 3166 United Nations code (https://www.iso.org/publication/PUB500001.html) to signify country or region.

2. Author Information
	A. Principal Investigator Contact Information
		Name: Cameron Ludemann
		Institution: Wageningen University and Research
		Address: Plant Production Systems, Wageningen, The Netherlands. 
		Email: cameron.ludemann@wur.nl/cameronludemann@gmail.com

	B. Associate or Co-investigator Contact Information
		Name: Achim Dobermann
		Institution: International Fertilizer Association
		Address: Paris, France.
		Email: adobermann@fertilizer.org


3. Date of data collection (single date, range, approximate date)   2020-2022: 

4. Geographic location of data collection: World wide

5. Information about funding sources that supported the collection of the data: The International Fertilizer Association funded collation of these data.


SHARING/ACCESS INFORMATION

1. Licenses/restrictions placed on the data: CC0 1.0

2. Links to publications that cite or use the data:
Ludemann, C; Hijbeek, R; van Loon, M; Murrell, S; van Ittersum, M and Dobermann, A (2022), Global data on crop nutrient concentration and harvest indices, Dryad, Dataset, https://doi.org/10.5061/dryad.n2z34tn0x
UN FAO & IFA (2022) Crop Nutrient Budget https://www.fao.org/faostat/en/

3. Links to other publicly accessible locations of the data: https://doi.org/10.5061/dryad.n2z34tn0x and https://www.fao.org/faostat/en/

4. Links/relationships to ancillary data sets: NA

5. Was data derived from another source? yes
	A. If yes, list source(s): 
Secondary and primary sources of data used are listed in the Combined_crop_data.csv file. 


6. Recommended citation for this dataset: 
Ludemann, C; Hijbeek, R; van Loon, M; Murrell, S; van Ittersum, M and Dobermann, A (2022), Global data on crop nutrient concentration and harvest indices, Dryad, Dataset, https://doi.org/10.5061/dryad.n2z34tn0x

DATA & FILE OVERVIEW

1. Additional related data collected that was not included in the current data package: NA

2. Are there multiple versions of the dataset? Yes
	A. If yes, name of file(s) that was updated: All files were updated.
		i. Why was the file updated? It was found that data from Roy et al (2004) for India (Table 29) referred to total uptake per unit crop product rather than nutrient concentration of crop products. Therefore data from India were excluded from estimates. 
		ii. When was the file updated? 1 March 2023

