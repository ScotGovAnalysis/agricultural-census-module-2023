# Initial analysis of the module 2023 data and data validations and corrections
# Code created by Lucy Nevard 17.01.24


rm(list=ls())

# Load packages


library(tidyverse)
library(janitor)
library(data.table)
library(haven)
library(RtoSQLServer)
library(a11ytables)
library(stargazer)
library(VIM)

SAS_agscens_directory <- ("//s0177a/sasdata1/ags/census/agscens")

SAS_agstemp_directory <- ("//s0177a/sasdata1/ags/census/agstemp")


Code_directory <- ("//s0177a/datashare/seerad/ags/census/branch1/NewStructure/Surveys/June/Codeconversion_2023/2023")



# ADM schema for export

server <- "s0196a\\ADM"
database <- "RuralAndEnvironmentalScienceFarmingStatistics"
schema <- "juneagriculturalsurvey2023alpha"

#run item numbers
source("item_numbers.R")

jac2023<-read_table_from_db(server=server, 
                            database=database, 
                            schema=schema, 
                            table_name="JAC23_module_only_280324")

# jac2023full<-read_table_from_db(server=server, 
#                                 database=database, 
#                                 schema=schema, 
#                                 table_name="JAC23_cattle_revision_1112")

agscens_format <-read_table_from_db(server = server,
                                    database = database,
                                    schema = schema,
                                    table_name = "agscens_SAS_format")


agsresponse<-jac2023


# find percentage of grassland and crop holdings for item3028 and item3029. grass and crops combined for the others
agsresponse<-ungroup(agsresponse)

agsresponse<-agsresponse %>% 
  dplyr::rowwise() %>% 
  mutate(allgrassrg = sum(item2321, item2322, item47, na.rm = TRUE)) %>%
  mutate(allcrop=sum(item40-item39, na.rm=TRUE))


grasslandholdings<-agsresponse %>% 
  select(parish, holding, allgrassrg) %>% 
  filter(allgrassrg>0) %>% 
  select(parish, holding)

grassland<-as.numeric(nrow(grasslandholdings))

cropholdings<-agsresponse %>% 
  select(parish, holding, allcrop) %>% 
  filter(allcrop>0) %>% 
  select(parish, holding)

crop<-as.numeric(nrow(cropholdings))


cropandgrass<-merge(grasslandholdings, cropholdings, all="TRUE")
cropgrassval<-as.numeric(nrow(cropandgrass))

jacmodule<-agsresponse %>% 
  select(1:21, item40, item50, section_12, section_13, allgrassrg)

soilcovervars<-c("item2358", "item2359", "item3027", "item2360", "item2365", "item2361", "item2362")
tillagevars<-c("item2655","item2364","item2369")


jacmodule<-jacmodule %>% 
  dplyr::rowwise() %>% 
  mutate(allsoilcover=sum(item2358, item2359, item3027, item2360, item2365, item2361, item2362, na.rm=TRUE),
         alltillage=sum(item2655, item2364, item2369, na.rm=TRUE),
         diffsoil=item12-allsoilcover,
         difftill=item12-alltillage)

# Add regions (4) for tables

jacmodule<-jacmodule %>% 
  mutate(regn=ifelse(agricreg>=1&agricreg<=4,1,
                     ifelse(agricreg==5,2,
                            ifelse(agricreg>=6&agricreg<=9,3,
                                   ifelse(agricreg>=10&agricreg<=14,4,NA)))))

jacmoduleorig<-jacmodule


# Table for soil cover
# Keep only if they're using one or more of the methods


soilcover<-jacmodule %>% 
  mutate(anysoilcover=ifelse(allsoilcover>0,1,0)) %>% 
  filter(anysoilcover==1)

soilcover<-ungroup(soilcover)

soilcover_summary<-soilcover %>% 
  select(soilcovervars, allsoilcover) %>% 
  summarise(across(c(soilcovervars, allsoilcover), list(sum=sum, n=~ sum(.x != 0)))) %>% 
  gather(key="Statistic", value="Value") %>% 
  separate(Statistic, into=c("Variable", "Statistic"), sep="_") %>% 
  spread(key="Statistic", value="Value") 

allsoilcoverval<-soilcover_summary %>% 
  filter(Variable=="allsoilcover") %>% 
  pull(sum)

soilcover_summary<-soilcover_summary %>% 
  mutate(percent=sum/allsoilcoverval*100,
         haperholding=sum/n) %>% 
  select(Variable, sum, percent, n,  haperholding) %>% 
  rename(Hectares=sum,
         "Number of holdings"=n,
         "Proportion of all soil cover (%)" = percent,
         "Average hectares covered per holding"=haperholding) 


soilcoveranswer<-c("Autumn/winter crops", "Field vegetables and potatoes (not harvested)","Multi-annual plants", "Cover or intermediate crop which is legume", "Cover or intermediate crop which is not legume", "Plant residues", "Bare soil")

means<-jacmodule %>% 
  ungroup() %>% 
  summarise(across(soilcovervars, ~mean(item50[.>0], na.rm=TRUE)))

means<- means %>% pivot_longer(everything(), names_to = "Variable", values_to = "mean")

soilcover_summary<-soilcover_summary %>% 
  left_join(means)

soilcoverlookup<-data.frame(soilcoveranswer, soilcovervars)

soilcoverlookup<-soilcoverlookup %>% 
  rename(Variable=soilcovervars)

soilcoverlookup<-soilcoverlookup %>% 
  add_row(Variable = "allsoilcover", soilcoveranswer = "All soil cover types")

meantotal<-mean(soilcover$item50)


soilcover_summary<-merge(soilcover_summary, soilcoverlookup)

soilcover_final<-soilcover_summary %>% 
  mutate(mean=ifelse(Variable=="allsoilcover", meantotal, mean)) %>% 
  select(7,2,3,4,5,6) %>% 
  slice(2,4,7,3,8,5,6,1) %>% 
  rename("Average holding size (Hectares)"=mean)

write.csv(soilcover_final, paste0(Code_directory, "/soilsummary.csv"))


# Table for tillage
# Keep only if they're using one or more of the methods


tillage<-jacmodule %>% 
  mutate(anytillage=ifelse(alltillage>0,1,0)) %>% 
  filter(anytillage==1)

tillage<-ungroup(tillage)

tillage_summary<-tillage %>% 
  select(alltillage, tillagevars) %>% 
  summarise(across(c(tillagevars, alltillage), list(sum=sum, n=~ sum(.x != 0)))) %>% 
  gather(key="Statistic", value="Value") %>% 
  separate(Statistic, into=c("Variable", "Statistic"), sep="_") %>% 
  spread(key="Statistic", value="Value") 

alltillageval<-tillage_summary %>% 
  filter(Variable=="alltillage") %>% 
  pull(sum)

tillage_summary<-tillage_summary %>% 
  mutate(percent=sum/alltillageval*100,
         haperholding=sum/n) %>% 
  select(Variable, sum, percent, n, haperholding) %>% 
  rename(Hectares=sum,
         "Number of holdings"=n,
         "Proportion of all tillage (%)" = percent,
         "Hectares per holding"=haperholding) 

tillageanswer<-c("Zero tillage", "Reduced tillage","Inversion tillage")

means<-jacmodule %>% 
  ungroup() %>% 
  summarise(across(tillagevars, ~mean(item50[.>0], na.rm=TRUE)))

means<- means %>% pivot_longer(everything(), names_to = "Variable", values_to = "mean")

tillage_summary<-tillage_summary %>% 
  left_join(means)

tillagelookup<-data.frame(tillageanswer, tillagevars)

tillagelookup<-tillagelookup %>% 
  rename(Variable=tillagevars)

tillagelookup<-tillagelookup %>% 
  add_row(Variable = "alltillage", tillageanswer = "All tillage types")

meantotal<-mean(tillage$item50)

tillage_summary<-merge(tillage_summary, tillagelookup)

tillage_final<-tillage_summary %>% 
  mutate(mean=ifelse(Variable=="alltillage", meantotal, mean)) %>% 
  select(7,2,3,4,5,6) %>% 
  slice(3,2,4,1) %>% 
  rename("Average holding size (Hectares)"=mean)

write.csv(tillage_final, paste0(Code_directory, "/tillagesummary.csv"))


# Irrigation ----


irrigation<-jacmodule %>% 
  filter(item2638>0)

check<-irrigation %>% 
  select(item12, item50, item2638)

irrigation<-ungroup(irrigation)


irrigation_grouped<-irrigation %>% 
  select(regn, item2638, item50) %>% 
  group_by(regn) %>% 
  summarise(sum=sum(item2638), n=length(item2638), mean=mean(item50)) %>% 
  mutate("Hectares per holding"=sum/n) %>% 
rename(Hectares=sum, "Number of holdings" =n)


irrigation_summary<-irrigation %>% 
  select(regn, item2638, item50) %>%
  summarise(sum=sum(item2638), n=length(item2638), mean=mean(item50)) %>% 
  mutate("Hectares per holding"=sum/n) %>% 
  rename(Hectares=sum, "Number of holdings" =n) %>% 
  mutate(regn=0) %>% 
  bind_rows(irrigation_grouped) %>% 
  slice(2:5, 1) %>% 
  mutate(Region=ifelse(regn==1, 'North West',
                       ifelse(regn==2, 'North East',
                        ifelse(regn==3, 'South East',
                               ifelse(regn==4, "South West", "Scotland"))))) %>% 
  select(Region, Hectares, 'Number of holdings', 'Hectares per holding', mean) %>% 
  rename("Average holding size (Hectares)"=mean)


write.csv(irrigation_summary, paste0(Code_directory, "/irrigationsummary.csv"))


# Nutrient management  ----

nutrmanvars<-c("item3028", "item3029", "item3032", "item3514", "item2684", "item2685")

## Nutrient management summary ----


# of those who've tested soil, how many have change their crop nutrient application

soilcroptesting<-nrow(soilcroptesting<-jacmodule %>% 
                        filter(item3029==1))

soilcropchange<-nrow(soilcropchange<-jacmodule %>% 
                        filter((item3028==1|item3029==1) & item3032==1))

print(changes<-(soilcropchange/soilcroptesting)*100)


nutrientmanagement_summary<-jacmodule %>% 
  select(nutrmanvars) %>% 
  ungroup() %>% 
  summarise(across(everything(), (n=~ sum(.x ==1,na.rm=TRUE))))

nutrientmanagement_summary<- nutrientmanagement_summary %>% pivot_longer(everything(), names_to = "question", values_to = "number")

means<-jacmodule %>% 
  ungroup() %>% 
  summarise(across(nutrmanvars, ~mean(item50[.==1], na.rm=TRUE)))

means<- means %>% pivot_longer(everything(), names_to = "question", values_to = "mean")

nutrientmanagement_summary<-nutrientmanagement_summary %>% 
  left_join(means)


nutrientmanagement_summary<-nutrientmanagement_summary %>% 
  mutate(percentage=ifelse(question=="item3028", number/grassland*100, 
                           ifelse(question =="item3029"| question=="item3032", number/crop*100,
                                  number/cropgrassval*100))) %>% 
  rename(Variable=question,
         "Number of holdings" = number,
         "Percentage of holdings" = percentage,
         "Average holding size (ha)" = mean) %>% 
  select(1,2,4,3)


nutrmananswer<-c("Soil testing on grassland in last five years", "Soil testing on cropland in last five years","Soil testing resulted in change of crop nutrient application", "Wrote or updated nutrient management plan in last year", "Uses protected urea", "Uses precision farming technology")

nutrmanlookup<-data.frame(nutrmananswer, nutrmanvars)

nutrmanlookup<-nutrmanlookup %>% 
  rename(Variable=nutrmanvars)


nutrman_summary<-merge(nutrientmanagement_summary, nutrmanlookup)

nutrman_summary<-nutrman_summary %>% 
  select(5,2:4) %>% 
  slice(3,4,5,6,1,2) %>% 
  rename(" " = nutrmananswer)

write.csv(nutrman_summary, paste0(Code_directory, "/nutrientsummary.csv"))


# Nutrient management - area questions 

# agsresponse<-agsresponse %>% 
#   mutate(ph_sum = sum(get(area_ph_crop), get(area_ph_grass)))
# 
# agsresponse<-agsresponse %>% 
#   mutate(nutrient_sum = sum(get(nutrient_plan_grass), get(nutrient_plan_crop)))
# 
# agsresponse<-agsresponse %>% 
#   mutate(alln=sum(item2682, item2683))


# Summary for areas

nutrmanvarsareas<-c("item3512", "item3513", "item2656", "item2657")

nutrientmanareas_summary<-jacmodule %>% 
  select(nutrmanvarsareas) %>% 
  ungroup() %>% 
  mutate(across(everything(),~replace_na(., 0))) %>% 
  summarise(across(c(nutrmanvarsareas), list(sum=sum, n=~ sum(.x != 0)))) %>% 
  gather(key="Statistic", value="Value") %>% 
  separate(Statistic, into=c("Variable", "Statistic"), sep="_") %>% 
  spread(key="Statistic", value="Value")

means<-jacmodule %>% 
  ungroup() %>% 
  summarise(across(nutrmanvarsareas, ~mean(item50[.!=0], na.rm=TRUE)))

means<- means %>% pivot_longer(everything(), names_to = "Variable", values_to = "mean")

nutrientmanareas_summary<-nutrientmanareas_summary %>% 
  left_join(means, by="Variable")


nutrientmanareas_summary<-nutrientmanareas_summary %>% 
  mutate(percentage=ifelse(Variable=="item2656" | Variable=="item3512" | Variable=="item2681", n/grassland*100, 
                           n/crop*100),
         haperholding=sum/n) %>% 
  rename( "Number of holdings" = n,
         "Hectares" = sum,
         "Percentage of holdings" = percentage,
         "Average holding size (ha)" = mean) %>% 
  select(1,2,4,5,3)


nutrmanareaanswer<-c("Regular pH testing on grassland", "Regular pH testing on cropland","Nutrient management plan used on grassland", "Nutrient management plan used on cropland")

nutrmanarealookup<-data.frame(nutrmanareaanswer, nutrmanvarsareas)

nutrmanarealookup<-nutrmanarealookup %>% 
  rename(Variable=nutrmanvarsareas)

nutrientmanareas_summary<-merge(nutrientmanareas_summary, nutrmanarealookup)

nutrientmanareas_summary<-nutrientmanareas_summary %>% 
  select(6, 5, 2, 4, 3) %>% 
  slice(3,4,1,2) %>% 
  rename(" " = nutrmanareaanswer)

write.csv(nutrientmanareas_summary, paste0(Code_directory, "/nutrientareassummary.csv"))


# Imputation of missing mixed sward areas ---------------------------------

jacmodule<-jacmodule %>% 
  mutate(item2681_imp = "FALSE")

nitrogen250<-jacmodule %>% 
  filter(item2681>0 & item2682>0) %>% 
  select(parish, holding, item2681, item2682, item12, allgrassrg, agricreg) %>% 
  mutate(item2682kg=item2682*1000,
         nperha=item2682kg/item2681,
         tonnesperha=item2682/item2681,
         npergrass=item2682kg/allgrassrg) %>% 
  filter(nperha<=250)

modelnitrogen<-lm(nitrogen250$item2681~nitrogen250$item2682kg)

summary(modelnitrogen)

manure<-jacmoduleorig %>% 
  filter(item2681>0 & item2683>0) %>% 
  select(parish, holding, item2681, item2683, item12, allgrassrg, agricreg) %>% 
  mutate(manureperha=item2683/item2681) %>% 
  filter(manureperha<=100)

modelmanure<-lm(manure$item2681~manure$item2683)

summary(modelmanure)

# modelnitrogen has a higher coefficient - will use this to impute where nitrogen data are present. 

missing<-jacmodule %>% 
  select(item2681, item2682, item2683) %>% 
  filter((item2681==0 | is.na(item2681)) & (item2682>0 | item2683>0))

missing2<-jacmodule %>% 
  select(item2681, item2682, item2683) %>% 
  filter((item2681==0 | is.na(item2681)) & (item2682>0 & item2683>0)) # 286 have area misisng and both nitrogen and manure


missingnitrogen<-jacmodule %>% 
  select(parish, holding, item2681, item2682) %>% 
  filter((item2681==0 | is.na(item2681)) & item2682>0)


nitrogenimp<-nitrogen250 %>% 
  select(parish, holding, item2681, item2682) %>% 
  bind_rows(missingnitrogen) %>% 
  mutate(item2681=ifelse(item2681==0, NA, item2681))

imp<-regressionImp(nitrogenimp,formula=item2681~item2682)

modelnitrogenafterimp<-lm(imp$item2681~imp$item2682)
summary(modelnitrogenafterimp)

imp<-imp %>% 
  select(-item2682) %>% 
  mutate(item2681_imp=as.character(item2681_imp))

jacmodule<-rows_update(jacmodule, imp, by=c("parish", "holding"))

# impute areas when manure data present but no nitrogen data

missingmanure<-jacmodule %>% 
  select(parish, holding, item2681, item2683) %>% 
  filter((item2681==0 | is.na(item2681)) & item2683>0)

# remove those already imputed using nitrogen data

missingmanure<-anti_join(missingmanure, imp, by=c("parish","holding"))

manureimp<-manure %>% 
  select(parish, holding, item2681, item2683) %>% 
  bind_rows(missingmanure) %>% 
  mutate(item2681=ifelse(item2681==0, NA, item2681))

impmanure<-regressionImp(manureimp,formula=item2681~item2683)

modelmanureafterimp<-lm(impmanure$item2681~impmanure$item2683)
summary(modelmanureafterimp)

impmanure<-impmanure %>% 
  select(-item2683) %>% 
  mutate(item2681_imp=as.character(item2681_imp))

jacmodule<-rows_update(jacmodule, impmanure, by=c("parish", "holding"))


stillmissing<-jacmodule %>% 
  select(item2681, item2682, item2683) %>% 
  filter((item2681==0 | is.na(item2681)) & (item2682>0 | item2683>0)) # none missing, imputation successfull



# Correct imputed areas when more than allgrassrg

sward_error <- jacmodule %>%
  filter(item2681 > allgrassrg) %>%
  select(item12, item50, allgrassrg, item2681, parish, holding, item2681_imp)

sward_error <- sward_error %>%
  mutate(item2681 = allgrassrg)

jacmodule <-
  rows_update(jacmodule, sward_error, by = c("parish", "holding"))


# Mixed sward area summary -------------------------------------------------------


mixedsward<-jacmodule %>% 
  filter(item2681>0) %>% 
  ungroup()

mixedsward_grouped<-mixedsward %>% 
  select(regn, item2681, allgrassrg) %>% 
  group_by(regn) %>% 
  summarise(sum=sum(item2681), n=length(item2681), mean=mean(allgrassrg), sumgrass=sum(allgrassrg)) %>% 
  mutate("Average mixed sward area per holding (Hectares)"=sum/n,
         "Grassland as mixed sward (%)" = sum/sumgrass*100) %>% 
  rename("Area of mixed sward (Hectares)"=sum, "Number of holdings" =n, "Area of grassland" =sumgrass) 


mixedsward_summary<-mixedsward %>% 
  select(regn, item2681, allgrassrg) %>%
  summarise(sum=sum(item2681), n=length(item2681), mean=mean(allgrassrg), sumgrass=sum(allgrassrg)) %>% 
  mutate("Average mixed sward area per holding (Hectares)"=sum/n,
         "Grassland as mixed sward (%)" = sum/sumgrass*100) %>% 
  rename("Area of mixed sward (Hectares)"=sum, "Number of holdings" =n, "Area of grassland" =sumgrass) %>% 
  mutate(regn=0) %>% 
  bind_rows(mixedsward_grouped) %>% 
  slice(2:5, 1) %>% 
  mutate(Region=ifelse(regn==1, 'North West',
                       ifelse(regn==2, 'North East',
                              ifelse(regn==3, 'South East',
                                     ifelse(regn==4, "South West", "Scotland"))))) %>% 
  select(Region,  'Area of mixed sward (Hectares)', 'Number of holdings', 
         'Average mixed sward area per holding (Hectares)', mean,
         'Area of grassland', 'Grassland as mixed sward (%)') %>% 
  rename("Average grassland area per holding (Hectares)"=mean)


write.csv(mixedsward_summary, paste0(Code_directory, "/mixedswardsummary.csv"))


# Nitrogen on mixed sward summary -----------------------------------------

# Only those with mixed sward areas


# Summary 1 - cut-off at 400 kg/ha
nitrogen<-jacmodule %>% 
  filter(item2681>0 & item2682>0) %>% 
  select(item2681, item2682, allgrassrg, regn) %>% 
  mutate(item2682kg=item2682*1000,
         nperha=item2682kg/item2681,
         tonnesperha=item2682/item2681,
         npergrass=item2682kg/allgrassrg)


nitrogen400grouped<-nitrogen %>% 
  ungroup() %>% 
  filter(nperha<=400) %>%
  select(regn, item2682kg, allgrassrg, nperha, item2681) %>% 
  group_by(regn) %>% 
  summarise(sum=sum(item2682kg), n=length(item2682kg), nperha = mean(nperha), mean=mean(item2681), meangrass=mean(allgrassrg)) %>% 
  rename("Total nitrogen (kg)"=sum, "Number of holdings" =n) 


nitrogen400_summary<-nitrogen %>% 
  ungroup() %>% 
  filter(nperha<=400) %>%
  select(regn, item2682kg, allgrassrg, nperha, item2681) %>%
  summarise(sum=sum(item2682kg), n=length(item2682kg), nperha = mean(nperha), mean=mean(item2681), meangrass=mean(allgrassrg)) %>% 
  rename("Total nitrogen (kg)"=sum, "Number of holdings" =n) %>% 
  mutate(regn=0) %>% 
  bind_rows(nitrogen400grouped) %>% 
  slice(2:5, 1) %>% 
  mutate(Region=ifelse(regn==1, 'North West',
                       ifelse(regn==2, 'North East',
                              ifelse(regn==3, 'South East',
                                     ifelse(regn==4, "South West", "Scotland"))))) %>% 
  rename("Average mixed sward area per holding (Hectares)"=mean,
    "Average grassland area per holding (Hectares)" = meangrass,
         "Application rate (kg/ha)" = nperha) %>% 
  select(7, 1:5)

write.csv(nitrogen400_summary, paste0(Code_directory, "/nitrogen400_summary.csv"))

# Summary 2 - cut-off at 250 kg/ha


nitrogen250grouped<-nitrogen %>% 
  ungroup() %>% 
  filter(nperha<=250) %>%
  select(regn, item2682kg, allgrassrg, nperha, item2681) %>% 
  group_by(regn) %>% 
  summarise(sum=sum(item2682kg), n=length(item2682kg), nperha = mean(nperha), mean=mean(item2681), meangrass=mean(allgrassrg)) %>% 
  rename("Total nitrogen (kg)"=sum, "Number of holdings" =n) 



nitrogen250_summary<-nitrogen %>% 
  ungroup() %>% 
  filter(nperha<=250) %>%
  select(regn, item2682kg, allgrassrg, nperha, item2681) %>%
  summarise(sum=sum(item2682kg), n=length(item2682kg), nperha = mean(nperha), mean=mean(item2681), meangrass=mean(allgrassrg)) %>% 
  rename("Total nitrogen (kg)"=sum, "Number of holdings" =n) %>% 
  mutate(regn=0) %>% 
  bind_rows(nitrogen250grouped) %>% 
  slice(2:5, 1) %>% 
  mutate(Region=ifelse(regn==1, 'North West',
                       ifelse(regn==2, 'North East',
                              ifelse(regn==3, 'South East',
                                     ifelse(regn==4, "South West", "Scotland"))))) %>% 
  rename("Average mixed sward area per holding (Hectares)"=mean,
         "Average grassland area per holding (Hectares)" = meangrass,
         "Application rate (kg/ha)" = nperha) %>% 
  select(7, 1:5)

write.csv(nitrogen250_summary, paste0(Code_directory, "/nitrogen250_summary.csv"))



# Manure summary ----------------------------------------------------------

manure<-jacmodule %>% 
  filter(item2681>0 & item2683>0) %>% 
  select(item2681, item2683, allgrassrg, regn) %>% 
  mutate(manureperha=item2683/item2681)


manuregrouped<-manure %>% 
  ungroup() %>% 
  filter(manureperha<=100) %>% 
  select(regn, item2683, item2681, manureperha, allgrassrg) %>% 
  group_by(regn) %>% 
  summarise(sum=sum(item2683), n=length(item2683), manureperha = mean(manureperha), mean=mean(item2681), meangrass=mean(allgrassrg)) %>% 
  rename("Total manure (tonnes)"=sum, "Number of holdings" =n) 



manure_summary<-manure %>% 
  ungroup() %>% 
  filter(manureperha<=100) %>% 
  select(regn, item2683, item2681, manureperha, allgrassrg) %>%
  summarise(sum=sum(item2683), n=length(item2683), manureperha = mean(manureperha), mean=mean(item2681), meangrass=mean(allgrassrg)) %>% 
  rename("Total manure (tonnes)"=sum, "Number of holdings" =n) %>% 
  mutate(regn=0) %>% 
  bind_rows(manuregrouped) %>% 
  slice(2:5, 1) %>% 
  mutate(Region=ifelse(regn==1, 'North West',
                       ifelse(regn==2, 'North East',
                              ifelse(regn==3, 'South East',
                                     ifelse(regn==4, "South West", "Scotland"))))) %>% 
  rename("Average mixed sward area per holding (Hectares)"=mean,
         "Average grassland area per holding (Hectares)" = meangrass,
         "Application rate (tonnes/ha)" = manureperha) %>% 
  select(7, 1:5)

write.csv(manure_summary, paste0(Code_directory, "/manuresummary.csv"))



# Solid manure ------------------------------------------------------------

solidvars<-c("item5129", "item5104", "item5105")


solid<-jacmodule %>% 
  select(solidvars) %>% 
  mutate(item5104=ifelse(is.na(item5104), 0, item5104),
         item5105=ifelse(is.na(item5105), 0, item5105))

solid<-ungroup(solid)

solid_summary<-solid %>% 
  summarise(across(everything(), list(sum=sum, n=~ sum(.x != 0)))) %>% 
  gather(key="Statistic", value="Value") %>% 
  separate(Statistic, into=c("question", "Statistic"), sep="_") %>% 
  spread(key="Statistic", value="Value") 


solid_summary<-solid_summary %>% 
  rename( "Number of holdings" = n,
          "Tonnes" = sum)

solidanswer<-c("Solid manure used on this holding", "Solid manure exported from holding", "Solid manure imported to holding")

solidlookup<-data.frame(solidanswer, solidvars)

solidlookup<-solidlookup %>% 
  rename(question=solidvars)

solid_summary<-merge(solid_summary, solidlookup)

solid_summary_final<-solid_summary %>% 
  select(4,3,2) %>% 
  slice(3,1,2) %>% 
  rename(" " = solidanswer)

write.csv(solid_summary_final, paste0(Code_directory, "/solidsummary.csv"))



# Slurry ------------------------------------------------------------------


slurryvars<-c("item5102", "item5106", "item5107")


slurry<-jacmodule %>% 
  select(slurryvars) %>% 
  mutate(item5102=ifelse(is.na(item5102), 0, item5102),
         item5106=ifelse(is.na(item5106), 0, item5106),
         item5107=ifelse(is.na(item5107), 0, item5107))

slurry<-ungroup(slurry)

slurry_summary<-slurry %>% 
  summarise(across(everything(), list(sum=sum, n=~ sum(.x != 0)))) %>% 
  gather(key="Statistic", value="Value") %>% 
  separate(Statistic, into=c("question", "Statistic"), sep="_") %>% 
  spread(key="Statistic", value="Value") 


slurry_summary<-slurry_summary %>% 
  rename( "Number of holdings" = n,
          "Cubic metres" = sum)

slurryanswer<-c("Slurry/liquid manure used on this holding", "Slurry/liquid manure exported from holding", "Slurry/liquid manure imported to holding")

slurrylookup<-data.frame(slurryanswer, slurryvars)

slurrylookup<-slurrylookup %>% 
  rename(question=slurryvars)

slurry_summary<-merge(slurry_summary, slurrylookup)

slurry_summary_final<-slurry_summary %>% 
  select(4,3,2) %>% 
  rename(" " = slurryanswer)

write.csv(slurry_summary_final, paste0(Code_directory, "/slurrysummary.csv"))


#check solid and slurry

both<-jacmodule %>% 
  select(parish, holding, item5129, item5102)

# Fertiliser spread -------------------------------------------------------


fertiliservars<-c("item5130", "item5103")


fertiliser<-jacmodule %>% 
  select(fertiliservars) %>% 
  mutate(item5103=ifelse(is.na(item5103), 0, item5103))

checkmineral<-jacmodule %>% 
  filter(item5130>0)

checkorganic<-jacmodule %>% 
  filter(item5103>0)


fertiliser<-ungroup(fertiliser)

fertiliser_summary<-fertiliser %>% 
  summarise(across(everything(), list(sum=sum, n=~ sum(.x != 0)))) %>% 
  gather(key="Statistic", value="Value") %>% 
  separate(Statistic, into=c("question", "Statistic"), sep="_") %>% 
  spread(key="Statistic", value="Value") 


fertiliser_summary<-fertiliser_summary %>% 
  rename( "Number of holdings" = n,
          "Tonnes" = sum)

fertiliseranswer<-c("Mineral fertilisers spread", "Organic waste and waste base fertilisers (e.g. compost, anaerobic digesters) spread")

fertiliserlookup<-data.frame(fertiliseranswer, fertiliservars)

fertiliserlookup<-fertiliserlookup %>% 
  rename(question=fertiliservars)

fertiliser_summary<-merge(fertiliser_summary, fertiliserlookup)

fertiliser_summary_final<-fertiliser_summary %>% 
  select(4,3,2) %>% 
  slice(2,1)

write.csv(fertiliser_summary_final, paste0(Code_directory, "/fertilisersummary.csv"))


# Spread techniques -------------------------------------------------------

spreadvars<-c("item5108", "item5109", "item5110", "item5111", "item5112", "item5113", "item5114", "item5131")

# Calculate overall percentages


spread<-jacmodule %>% 
  select(spreadvars) %>% 
  dplyr::rowwise() %>% 
  mutate(allspread = sum(
    get(broadcast_spreader_less_four_hours),
    get(broadcast_spreader_more_four_hours),
    get(broadcast_spreader_not_ploughed),
    get(band_spreader_hose),
    get(band_spreader_shoe),
    get(open_slot_shallow_injection),
    get(closed_slot_deep_injection),
    get(other_spread)
  )) %>% 
  filter(allspread==100)


spread_summary<-spread %>% 
  ungroup() %>% 
  summarise(across(everything(), list(mean=mean, n=~ sum(.x != 0)))) %>% 
  gather(key="Statistic", value="Value") %>% 
  separate(Statistic, into=c("question", "Statistic"), sep="_") %>% 
  spread(key="Statistic", value="Value")
  

spreadanswer<-c("Broadcast spreader with manure ploughed in within 4 hours", "Broadcast spreader with manure ploughed in after 4 or more hours",
                "Broadcast spreader with manure not ploughed in", "Band spreader with a trailing hose", "Band spreading with a trailing shoe",
                "Open-slot shallow injection spreader", "Closed-slot deep injection spreader", "Other")

spreadlookup<-data.frame(spreadanswer, spreadvars)

spreadlookup<-spreadlookup %>% 
  rename(question=spreadvars) %>% 
  add_row(question = "allspread", spreadanswer = "All techniques")


spread_summary<-merge(spread_summary, spreadlookup)

spread_summary_final<-spread_summary %>% 
  select(4,2,3) %>% 
  slice(2:9, 1) %>% 
  rename("Technique" = spreadanswer,
         "Average percentage" = mean,
         "Number of holdings" = n)

write.csv(spread_summary_final, paste0(Code_directory, "/spreadsummary.csv"))


# Storage -----------------------------------------------------------------


storagevars<-c("item5115", "item5116", "item5117", "item5118", "item5119", "item5120", "item5121", "item5122", "item5123", "item5132")


storage<-jacmodule %>%  
  select(storagevars) %>% 
  dplyr::rowwise() %>% 
  mutate(allstorage = sum(
    get(storage_heaps),
    get(storage_compost),
    get(storage_pits_below_animals),
    get(storage_deep_litter),
    get(storage_without_cover),
    get(storage_permeable_cover),
    get(storage_impermeable_cover),
    get(storage_other_facilities),
    get(daily_spread),
    get(other_systems)
  )) %>% 
  filter(allstorage==100)


storage_summary<-storage %>% 
  ungroup() %>% 
  summarise(across(everything(), list(mean=mean, n=~ sum(.x != 0)))) %>% 
  gather(key="Statistic", value="Value") %>% 
  separate(Statistic, into=c("question", "Statistic"), sep="_") %>% 
  spread(key="Statistic", value="Value")


storageanswer<-c("Manure solid storage in heaps", "Manure stored in compost piles", "Manure stored in pits below animal confinement",
                 "Manure stored in deep litter systems", "Liquid manure/slurry storage without cover", "Liquid manure/slurry storage with permeable cover",
                 "Liquid manure/slurry storage with impermeable cover", "Manure stored in other facilities (not elsewhere classified)",
                 "Daily spread", "Other")

storagelookup<-data.frame(storageanswer, storagevars)

storagelookup<-storagelookup %>% 
  rename(question=storagevars) %>% 
  add_row(question = "allstorage", storageanswer = "All techniques")

storage_summary<-merge(storage_summary, storagelookup)

storage_summary_final<-storage_summary %>% 
  select(4,2,3) %>% 
  slice(2:11, 1) %>% 
  rename("Storage system" = storageanswer,
         "Average percentage" = mean,
         "Number of holdings" = n)

write.csv(storage_summary_final, paste0(Code_directory, "/storagesummary.csv"))


# Months  -----------------------------------------------------------------

monthvars<-c("item5124", "item5125", "item5126", "item5127", "item5128")


months<-jacmodule %>%  
  select(monthvars) %>% 
  mutate_all(~replace(., is.na(.), 0))

month_summary<-months %>% 
  ungroup() %>% 
  summarise(across(everything(), list(mean=~mean(.x[.x != 0]), n=~ sum(.x != 0)))) %>% 
  gather(key="Statistic", value="Value") %>% 
  separate(Statistic, into=c("question", "Statistic"), sep="_") %>% 
  spread(key="Statistic", value="Value")


monthanswer<-c("Manure stored in compost piles", "Manure stored in pits below animal confinement","Manure stored in deep litter systems",
                "Liquid manure/slurry storage", "Manure stored in other facilities")

monthlookup<-data.frame(monthanswer, monthvars)

monthlookup<-monthlookup %>% 
  rename(question=monthvars)

month_summary<-merge(month_summary, monthlookup)

month_summary_final<-month_summary %>% 
  select(4,2,3) %>% 
  rename("Storage system" = monthanswer,
         "Average number of months" = mean,
         "Number of holdings" = n)

write.csv(month_summary_final, paste0(Code_directory, "/monthsummary.csv"))




# Testing - final two questions -------------------------------------------

testingvars<-c("item2662", "item2663")

manuretesting_summary<-jacmodule %>% 
  select(testingvars) %>% 
  ungroup() %>% 
  summarise(across(everything(), (n=~ sum(.x ==1,na.rm=TRUE))))

manuretesting_summary<- manuretesting_summary %>% pivot_longer(everything(), names_to = "question", values_to = "number")


means<-jacmodule %>% 
  ungroup() %>% 
  summarise(across(testingvars, ~mean(item12[.==1], na.rm=TRUE)))

means<- means %>% pivot_longer(everything(), names_to = "question", values_to = "mean")


manuretesting_summary<-manuretesting_summary %>% 
  left_join(means)


manuretesting_summary<-manuretesting_summary %>% 
  rename(Variable=question,
         "Number of holdings" = number,
         "Average holding size (ha)" = mean)


testinganswer<-c("Tested the nutrient value of manure and slurry used", "Always separated applications of slurry and/or mineral fertiliser by at least five days")

testinglookup<-data.frame(testinganswer, testingvars)

testinglookup<-testinglookup %>% 
  rename(Variable=testingvars)


manuretesting_summary<-merge(manuretesting_summary, testinglookup)

manuretesting_summary<-manuretesting_summary %>% 
  select(4,2,3) %>% 
  rename(" " = testinganswer)

write.csv(manuretesting_summary, paste0(Code_directory, "/manuretestingsummary.csv"))


# Save imputed dataset?
#
write_dataframe_to_db(
  server = server,
  database = database,
  schema = schema,
  table_name = "JAC23_module_only_280324_imputed",
  dataframe = jacmodule,
  append_to_existing = FALSE,
  versioned_table = FALSE,
  batch_size = 10000
)
