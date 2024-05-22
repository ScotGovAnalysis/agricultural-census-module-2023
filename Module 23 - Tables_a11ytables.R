# https://co-analysis.github.io/a11ytables/reference/create_a11ytable.html

#https://teams.microsoft.com/l/message/19:67052c1c-3dbf-4a6c-99e1-bccedcb6b3ea_a5980e49-2bde-4c5f-bbb1-b4e87cae8b60@unq.gbl.spaces/1711028132040?context=%7B%22contextType%22%3A%22chat%22%7D
rm(list=ls())

# Load packages
library(tidyverse)
library(tibble)
library(tidyr)
library(janitor)
library(data.table)
library(haven)
library(RtoSQLServer)
library(a11ytables)
library(stargazer)
library(pillar)
library(gt)
library(rapid.spreadsheets)
library(openxlsx)

# View the list of vignettes for the package
##vignette(package = "pillar")

setwd("//s0177a/datashare/seerad/ags/census/branch1/NewStructure/Surveys/June/Codeconversion_2023/2023")
# Prepare some  tables of information

Code_directory <- ("//s0177a/datashare/seerad/ags/census/branch1/NewStructure/Surveys/June/Codeconversion_2023/2023")

temp1 = list.files(pattern="summary\\.csv")
temp1

myfiles = lapply(temp1, read.delim)
myfiles


# Import required library 
library(parallel) 


# Create a list of file names 

file_list1 = list.files(pattern="df\\.txt")
file_list1


extract_data_from_file <- function(file) { 
  # code to extract data from file 
  data <- readLines(file) 
  # perform data extraction operations here 
  # e.g. strsplit(data, " ") 
  return(data) 
} 

#files <- list.files(pattern="df\\.txt") 
cl <- makeCluster(detectCores()) 
results <- parLapply(cl, file_list1, 
                     extract_data_from_file) 
stopCluster(cl) 
# print the results 
print(results) 

##### Table 1 Winter soil cover#####
SoilSumTble<-read.csv("soilsummary.csv")
Table_1_df<-as_tibble(SoilSumTble)
class(Table_1_df)
#[1]"tbl_df"     "tbl"        "data.frame"

head(Table_1_df)
# Prepare tables of information

#Negative Selection
Table_1_df<-Table_1_df %>%
  select(-1)
Table_1_df <- Table_1_df %>% mutate(across(c(2:6), round, 0))
colnames(Table_1_df) = c('Soil Cover Type','Area (Hectares)', 'proportion of soil cover type (%)', 'Holdings (Number)','Average hectares covered per holding (Hectares)', 'Average holding size (Hectares)')
head(Table_1_df)
# Save data frame
save(data, file = "Table_1_df")


######Table 2 Tillage######
tillage_Tble<-read.csv("tillagesummary.csv")
Table_2_df<-as_tibble(tillage_Tble)
class(Table_2_df)
#[1]"tbl_df"     "tbl"        "data.frame"
head(Table_2_df)
Table_2_df<-Table_2_df %>%
  select(-1)
Table_2_df <- Table_2_df %>% mutate(across(c(2:6), round, 0))
colnames(Table_2_df) = c('Tillage Type','Area (Hectares)', 'Tillage type (%)','Holdings (Number)','Average area per holding (Hectares)', 'Average holding area (Hectares)')
Table_2_df

##### Table 3 Irrigation by nuts 2####
Irrig_NUts2_Tble<-read.csv("irrigationsummary.csv")
Table_3_df<-as_tibble(Irrig_NUts2_Tble)
class(Table_3_df)
#[1]"tbl_df"     "tbl"        "data.frame"
head(Table_3_df)
Table_3_df<-Table_3_df %>%  select(-1)
Table_3_df <- Table_3_df %>% mutate(across(c(2:5), round, 0))
colnames(Table_3_df) <- c("Region","Area (Hectares)", "Holdings (Number)","Average area per holding (Hectares)", 'Average holding area (Hectares)')
Table_3_df


#### Table 4 Nutrient summary soil ph etc ####
NuteSumTble<-read.csv("nutrientsummary.csv")
Table_4_df<-as_tibble(NuteSumTble)
class(Table_4_df)
#[1]"tbl_df"     "tbl"        "data.frame"
Table_4_df
Table_4_df<-Table_4_df %>%  select(-1)
Table_4_df <- Table_4_df %>% mutate(across(c(2:4), round, 0))
colnames(Table_4_df) <- c('Soil nutrient management','Holdings (Number)', '% of holdings','Average holding area (Hectares)')
Table_4_df
#Table_4_df <- Table_4_df[, c(1,4,2,3)]


#####Table 5 =ph and mngt plan####
Table_5_df<-read.csv("nutrientareassummary.csv")
Table_5_df<-as_tibble(Table_5_df)
class(Table_5_df)
#[1]"tbl_df"     "tbl"        "data.frame"
Table_5_df
Table_5_df<-Table_5_df %>%  select(-1)
Table_5_df <- Table_5_df %>% mutate(across(c(2:5), round, 0))
colnames(Table_5_df) <- c('Soil nutrient management','Area (Hectares)','Holdings (Number)', '% of holdings','Average holding area (Hectares)')
Table_5_df

# Table 6. mixedswardsummary.csv
Table_6_df<-read.csv("mixedswardsummary.csv")
Table_6_df<-as_tibble(Table_6_df)
class(Table_6_df)
#[1]"tbl_df"     "tbl"        "data.frame"
Table_6_df
Table_6_df<-Table_6_df %>%  select(-1)
Table_6_df <- Table_6_df %>% mutate(across(c(2:6), round, 0))
colnames(Table_6_df) <- c('Region','Area of mixed sward (Hectares)','Holdings (Number)', 'Average mixed sward area per holding (Hectares)','Average grassland area per holding (Hectares)',' Area of grassland (Hectares)', 'Grassland as mixed sward (%)')
Table_6_df


# Table 7. Nitrogen 250 summary.#
Table_7_df<-read.csv("nitrogen250_summary.csv")
Table_7_df<-as_tibble(Table_7_df)
class(Table_7_df)
#[1]"tbl_df"     "tbl"        "data.frame"
Table_7_df
Table_7_df<-Table_7_df %>%  select(-1)
Table_7_df <- Table_7_df %>% mutate(across(c(2:5), round, 0))
colnames(Table_7_df) <- c('Region',' Total nitrogen (Kg)','Holdings (Number)', 'Application rate (Kg/Hectare)','Average mixed sward area per holding (Hectares)','Average grassland area per holding (Hectares)')
Table_7_df

# Table 8. Nitrogen 400 summary.###
Table_8_df<-read.csv("nitrogen400_summary.csv")
Table_8_df<-as_tibble(Table_8_df)
class(Table_8_df)
#[1]"tbl_df"     "tbl"        "data.frame"
Table_8_df
Table_8_df<-Table_8_df %>%  select(-1)
Table_8_df <- Table_8_df %>% mutate(across(c(2:6), round, 0))
colnames(Table_8_df) <- c('Region',' Total nitrogen (Kg)','Holdings (Number)', 'Application rate (Kg/Hectare)','Average mixed sward area per holding (Hectares)','Average grassland area per holding (Hectares)')
Table_8_df


##### Table 9 Nitrogen and Manure####
#[1] "data.table" "data.frame"
Table_9_df<-read.csv("manuresummary.csv")
Table_9_df<-as_tibble(Table_9_df)
class(Table_9_df)
head(Table_9_df)
Table_9_df<-Table_9_df %>%  select(-1)
Table_9_df <- Table_9_df %>% mutate(across(c(2:6), round, 0))
colnames(Table_9_df) <- c('Region', 'Manure (Tonnes)','Holdings (Number)', 'Application rate (Tonnes/Hectare)','Average mixed sward area per holding (Hectares)','Average grassland area per holding (Hectares)')
Table_9_df


# Table 10. solidsummary.###
Table_10_df<-read.csv("solidsummary.csv")
Table_10_df<-as_tibble(Table_10_df)
class(Table_10_df)
#[1]"tbl_df"     "tbl"        "data.frame"
Table_10_df
Table_10_df<-Table_10_df %>%  select(-1)
Table_10_df <- Table_10_df %>% mutate(across(c(2), round, 0))
colnames(Table_10_df) <- c('Manure use and movement', 'Tonnes','Holdings (Number)')
Table_10_df


# Table 11. slurrysummary###
Table_11_df<-read.csv("slurrysummary.csv")
Table_11_df<-as_tibble(Table_11_df)
class(Table_11_df)
#[1]"tbl_df"     "tbl"        "data.frame"
Table_11_df
Table_11_df<-Table_11_df %>%  select(-1)
Table_11_df <- Table_11_df %>% mutate(across(c(2), round, 0))
colnames(Table_11_df) <- c('Slurry/liquid maure use and movement','Cubic metres','Holdings (Number)')
Table_11_df

# Table 12. sfertilisersummary.###
Table_12_df<-read.csv("fertilisersummary.csv")
Table_12_df<-as_tibble(Table_12_df)
class(Table_12_df)
#[1]"tbl_df"     "tbl"        "data.frame"
Table_12_df
Table_12_df<-Table_12_df %>%  select(-1)
Table_12_df <- Table_12_df %>% mutate(across(c(2), round, 0))
colnames(Table_12_df) <- c('Mineral/organic fertilisers', 'Tonnes','Holdings (Number)')
Table_12_df


# Table 13. spreadsummary.###
Table_13_df<-read.csv("spreadsummary.csv")
Table_13_df<-as_tibble(Table_13_df)
class(Table_13_df)
#[1]"tbl_df"     "tbl"        "data.frame"
Table_13_df
Table_13_df<-Table_13_df %>%  select(-1)
Table_13_df <- Table_13_df %>% mutate(across(c(2), round, 0))
colnames(Table_13_df) <- c('Fertiliser application technique', 'Average (%)','Holdings (Number)')
Table_13_df

# Table 14. storagesummary.###
Table_14_df<-read.csv("storagesummary.csv")
Table_14_df<-as_tibble(Table_14_df)
class(Table_14_df)
#[1]"tbl_df"     "tbl"        "data.frame"
Table_14_df
Table_14_df<-Table_14_df %>%  select(-1)
Table_14_df <- Table_14_df %>% mutate(across(c(2), round, 0))
colnames(Table_14_df) <- c('Storage system', 'Average (%)','Holdings (Number)')
Table_14_df

# Table 15. storage time summary.###
Table_15_df<-read.csv("monthsummary.csv")
Table_15_df<-as_tibble(Table_15_df)
class(Table_15_df)
#[1]"tbl_df"     "tbl"        "data.frame"
Table_15_df
Table_15_df<-Table_15_df %>%  select(-1)
Table_15_df <- Table_15_df %>% mutate(across(c(2), round, 0))
colnames(Table_15_df) <- c('Storage system', 'Average months (Number)','Holdings (Number)')
Table_15_df

# Table 16. manure testing summary.###
Table_16_df<-read.csv("manuretestingsummary.csv")
Table_16_df<-as_tibble(Table_16_df)
class(Table_16_df)
#[1]"tbl_df"     "tbl"        "data.frame"
Table_16_df
Table_16_df<-Table_16_df %>%  select(-1)
Table_16_df <- Table_16_df %>% mutate(across(c(2:3), round, 0))
colnames(Table_16_df) <- c('Nutrient testing and sepration', 'Holdings (Number)','Average holding area (Hectares)')
Table_16_df


cover_list <- list(
  "Information" = c("Data in this workbook relates to the 'June Agricultural Census June 2023 module' statistical release.", 
                    "This workbook contains data from the  Scottish Agricultural Census: June 2023 - Agricultural production methods and nutrient application module.  This module includes data on soil cover on cropping land, tillage types and areas, irrigation facilities, soil testing,  nutrient and fertiliser management and application, in addition to manure/slurry testing, use and storage."),
  "Useful links" = c("[Results from the Scottish Agricultural Census: June 2023](https://www.gov.scot/publications/results-scottish-agricultural-census-june-2023/)","[Agricultural land use in England at 1 June 2023 GOV.UK](https://www.gov.uk/government/statistics/agricultural-land-use-in-england/agricultural-land-use-in-england-at-1-june-2023)",
    "[Survey of agriculture and horticulture Llywodreath Cymru Welsh Government](https://www.bing.com/search?q=Survey+of+agriculture+and+horticulture+Llywodreath+Cymru+Welsh+Government&cvid=0955fcedd7e142308d2a686bf141e1c8&aqs=edge..69i57j69i11004.1663j0j4&FORM=ANAB01&PC=U531)"),
  "Contact" = c( "[argic.stats@gov.scot](mailto:agric.stats@gov.scot)"),
  "Date of Publication" = ("21st May 2024"),
  "Glossary" =c(
                "Band spreader - A boom is mounted behind the tractor or tanker where there is a number of evenly spaced flexible pipes. the fertiliser travels down these pipes to be deposited on the soil or crop surface.",
                "Broadcast spreading - uses a farm implement spreading fertiliser or seeds where no row planting is required and is an alternative to drop spreaders (splash plate or nozzles) – the slurry is forced under pressure through a nozzle, often onto an inclined plate to increase the sideways spread.", 
                "Closed-slot deep injection spreader - slurry is injected under the soil surface, deep injection is over 150mm of depth (alternatively the is open slot shallow injection of up to 50mm depth).",
                "Cover crop - any non-cash crop, planted to cover the soil rather than for the purpose of being harvested. These crops have the potential to increase soil organic matter and fertility, improve soil structure, promote water infiltration, reduce erosion,  and limit disease and pest outbreaks.",
                "Cropping land - area of land available for cropping, typically this consists of cereals, oilseeds, potatoes,  other arable crops, horticultural crops, uncropped arable land, and temporary grassland.",
                "Deep litter systems - a type of animal housing system where repeated layers of litter (such as straw or sawdust ) are used for bedding and animals to defecate in. New layers of litter are continuously added. Also known as the 'build-up method'.",
                "Inversion tillage - often practiced by ploughing, this method flips over topsoil, burying the surface residues and sowing seeds within it.",
                "Mixed sward - mixed sward or multispecies sward (also known as herbal leys), is a grass mixture that is made up of two or more species including grasses, brassicas, legumes, and herbs.",
                "Mineral fertilisers - the inorganic substances consisting of essential micronutrients which are applied to the soil to enhance the phyto-availability of micronutrient content in soil, and so improve the quality of crop.",
                "Nutrient management plan - a document that outlines how to manage nutrients effectively to minimise environmental impacts while maximising economic benefits. It involves balancing crop nutrient needs with nutrients applied and typically includes soil test results, manure/ biosolids analyses (if applicable), yield goals and plans for the timing, amount, form, placement, and application of nutrients.",
                "Organic waste fertilisers - fertilisers that are naturally produced i.e. peat and animal wastes including manure and slurry, treated sewage sludge, plant wastes from agriculture such as compost and biosolids, and inorganic fertilisers including minerals and ash.",
                "Ploughing - using machinery or tools to turn over soil, the soil is cut through and lifted creating furrows. This differs to tilling.",
                "Precision farming technology - uses advanced technology to optimize crop production, such as crop/soil sensors, GPS, and data analysis. Precision farming technology integrates crop management software to tailor management strategies to specific areas within fields, which may reduce waste, increase yields, and lower the environmental impacts of  farming practices.",
                "Protected urea - are urea fertiliser products with an added inhibitor to decrease nitrogen losses, designed to deliver a more efficient use of nitrogen and increasing urea effectiveness.",
                
                "Reduced/conservation tillage - reduced or no-till farming minimises any disturbance to the soil and organic matter, which promotes long-term health and sustainability of soil. These methods have environmental and economic benefits.",
                "Slurry - excreta produced by livestock (other than poultry),  while in a building or yard (including any bedding, rainwater and washings mixed with it), that has a consistency which allows it to be discharged by gravity or pumped. The liquid part of separated slurry is also defined as slurry.",
                "Tillage - preparation of soil for growing crops. Typically, this involves clearing the land, followed by the use of machinery such as a tiller or cultivator to loosen and aerate soil creating a smooth even surface for planting seeds, cuttings, or seedlings. Note tilling differs to ploughing.",
"Trailing hose spreader– the boom of the spreader has multiple hoses connected to it, distributing the slurry close to the ground in bands or strips.", 
"Trailing shoe spreader – similar to a trailing hose spreader but with a 'shoe' attached to each hose allowing the slurry to be deposited under the crop canopy onto the soil.",
                "Waste base fertilisers - fertilisers derived from various waste materials.",
                "Zero tillage - an agricultural technique that does not disturb the soil through tillage. Also known as zero-till, no till farming or direct drilling. This means no cultivation machinery is used to prepare the land for crops reducing soil disturbance. Direct drill method is used to plant crops."
               
  )
   
)

contents_df <- data.frame(
  "Sheet name" = c("Notes", "Table_1", "Table_2", "Table_3","Table_4", "Table_5","Table_6","Table_7",
                   "Table_8", "Table_9", "Table_10","Table_11", "Table_12","Table_13","Table_14",
                   "Table_15", "Table_16"),
  "Sheet title" = c(
    "Notes used in this workbook.",
    "Soil cover on cropping land.",
    "Tillage types and areas.",
    "Areas with irrigation facilities by region.",
    "Soil nutrient management.",
    "Grassland and cropland nutrient management.",
    "Area of grassland managed as a mixed grass-legume sward by region.",
    "Quantity of nitrogen applied to mixed grass-legume sward (in the past 12 months) by region - capped at 250 kg/Ha.",
    "Quantity of nitrogen applied to mixed grass-legume sward (in the past 12 months) by region - capped at 400 kg/Ha.",
    "Quantity of manure applied to mixed grass-legume sward (in the past 12 months) by region.",
    "Manure used, exported, and imported.",
    "Slurry/liquid manure used, exported, and imported.",
    "Mineral and organic fertilisers used in the previous 12 months.",
    "Fertiliser spreading techniques.",
    "Total manure and slurry storage system.",
    "Number of months that manure and slurry produced on site can be stored.",
    "Manure and slurry nutrient tested and application breaks." 
 ),
  check.names = FALSE
)


notes_df <- data.frame(
  "Note number" = paste0("[note ", 1:11, "]"),
  "Note text" = c("Data is based on the number of respondents to this question in the Agricultural and Horticultural Census Module of June 2023 only.", 
                  "The data reflects that there can be more than one type of soil cover type on any single holding.", 
                  "The data reflects that there can be more than one type of tillage method used on any holding.",
                  "Protected urea includes products like BASF’s Limus® or YaraVera® AMIPLUS® please see glossary for more details.",
                  "Following expert advice, responses for nitrogen levels were capped at 250 kilogrammes per hectare (kg/Ha) in table 7 and 400 kg/ha in Table 8.",
                  "Organic fertiliser waste other than manure used.", 
                  "Recorded as percentage  - respondents were asked to supply a percentage for each type, equalling 100% the answers have been averaged from this.",
                  "Average percentage relates to the manure/slurry that is stored using this this technique.", 
                  "Note holding number any holding recorded as using this technique.",
                  "Number of months that manure and slurry produced on site can be stored without risk of runoff, and without occasional emptying.",
                  "Manure and slurry nutrient tested and application breaks (over the past 12 months)."

),
  check.names = FALSE
)
#checks
notes_df
contents_df
notes_df
cover_list

# Create 'a11ytables' object
                          

  my_a11ytable <- 
  a11ytables::create_a11ytable(
    tab_titles = c("Cover", "Contents", "Notes", "Table 1", "Table 2", "Table 3", "Table 4",
                   "Table_5","Table_6","Table_7",
                   "Table_8", "Table_9", "Table_10","Table_11", "Table_12","Table_13","Table_14",
                   "Table_15", "Table_16"),
    sheet_types = c("cover", "contents", "notes", "tables", "tables", "tables", "tables",
                  "tables", "tables", "tables",
                  "tables", "tables", "tables", "tables",
                  "tables", "tables", "tables","tables", "tables"),
    sheet_titles = c(
      "Scottish Agricultural Census: June 2023 - Agricultural production methods and nutrient application module data tables.",
      "Table of contents.",
      "Notes.",
      "Table 1: Soil cover on cropping land in Scotland [Note 1 & 2].",
      "Table 2: Tillage types and areas [Note 1 & 3].",
      "Table 3: Areas with irrigation facilities by region [Note 1].",
      "Table 4: Soil nutrient management [Note 1 & 4].",
      "Table 5: Grassland and cropland nutrient management [Note 1].",
      "Table 6: Area of grassland managed as a mixed grass-legume sward by region [Note 1].",
      "Table 7: Quantity of nitrogen applied to mixed grass-legume sward (in the past 12 months) by region - capped at 250kg/Ha [Note 1 & 5].",
      "Table 8: Quantity of nitrogen applied to mixed grass-legume sward (in the past 12 months) by region - capped at 400kg/Ha [Note 1 & 5].",
      "Table 9: Quantity of manure applied to mixed grass-legume sward (in the past 12 months) by region [Note 1].",
      "Table 10: Manure used, exported, and imported [Note 1].",
      "Table 11: Slurry/liquid manure used, exported, and imported [Note 1].",
      "Table 12: Mineral and organic fertilisers used in the past 12 months [Note 1 & 6].",
      "Table 13: Fertiliser spreading techniques [Note 1 & 7].",
      "Table 14: Total manure and slurry storage system [Note 1, 8 & 9].",
      "Table 15: Number of months that manure and slurry produced on site can be stored [Note 1 & 10].",
      "Table 16: Manure and slurry nutrient tested and application breaks [Note 1 & 11]."
    ), 
    blank_cells = c(
      rep(NA_character_, 3),
      rep("Some cells refer to notes which can be found on the Notes Worksheet.", 16)),

    custom_rows = list(
      NA_character_,
      NA_character_,
      NA_character_,
      " ",#4 consecutive tabs
      " ",
      " ",
      " ",
      " ",
      " ",
      " ",
      " ",
      " ",
      " ",
      " ",
      " ",
      " ",
      " ",
      " ",
     
      ""#7th table has a custom row
    ),
sources = c(NA_character_,
            NA_character_,
            NA_character_,
            rep("[JAC., 2023]((https://www.gov.scot/publications/results-scottish-agricultural-census-june-2023/))",16)
#"The Source Material, 2024."
),


tables = list(cover_list, contents_df, notes_df, Table_1_df, Table_2_df, Table_3_df, Table_4_df,
              Table_5_df, Table_6_df, Table_7_df,
              Table_8_df, Table_9_df, Table_10_df, Table_11_df,
              Table_12_df, Table_13_df, Table_14_df,Table_15_df, Table_16_df)


  )

my_wb1<- a11ytables::generate_workbook(my_a11ytable)

##### Add Notes ####

for(i in 4:19){

openxlsx::writeFormula(my_wb1, 
                       sheet = i,
                       x = '=HYPERLINK("#Notes!A1", "Link to Notes table")',
                       startCol = 1,
                       startRow= 4)}


openxlsx::saveWorkbook(my_wb1, "JAC Module Tables 003.xlsx")
