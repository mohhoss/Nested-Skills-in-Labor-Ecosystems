# General Description: Studying Interconnections of Occupation's Distribution of Skill Importance and Skill Categories' Importance Distributions
## Link to the latext file (?): https://www.overleaf.com/8811855357wspqcczxkpbs

# SECTIONS:
# 1. INITIATION
# 2. LOADING DATA
# 3. DEMOGRAPHIC DISTRIBUTION OF SKILLS (FIGURE 7)

# 1. INITIATION     ===========================================================================
#==============================================================================================
library(dplyr)
library(tidyr)
library(ggplot2)
library(crayon)
library(tm)
library(readxl)
library(ggrepel)
library(stargazer)
library(data.table)
library(stringr)
library(data.tree)
library(parallel)
library(mclust)
library(moments)
library(lsa) ## For cosine function
library(NbClust) ## For NbClust function
library(DescTools) ## Needed for weighted sd: SD
library(patchwork)
library(ggthemes)
library(png)
library(igraph)
library(ggpattern) ## Needed for patterned bar plots
library(markovchain)



rm(list = ls())
basepath <- "path"
setwd(paste0(basepath,""))
dir()


# 2. LOADING DATA     =========================================================================
#==============================================================================================
## 2.1. Skill data - 120 in Skills/Abilities/Knowledge in Total
skill_occ <- read_excel(paste0(basepath,"Skills.xlsx"))
skill_occ[skill_occ$'Element Name' == "Mathematics",'Element Name'] <- "Mathematics Skills"

ability_occ <- read_excel(paste0(basepath,"Abilities.xlsx"))

know_occ <- read_excel(paste0(basepath,"Knowledge.xlsx"))
know_occ[know_occ$'Element Name' == "Mathematics",'Element Name'] <- "Mathematics Knowledge"

skills_occ_raw <- skill_occ %>% mutate(type = "skill") %>% #filter(`Recommend Suppress` == "N") %>% 
  rename(occ_8_dig = `O*NET-SOC Code`, element_ID = `Element ID`) %>% 
  select(occ_8_dig, element_ID, `Scale Name`, `Data Value`, N, type) %>%
  pivot_wider(id_cols = c("occ_8_dig","element_ID","type"), names_from = `Scale Name`, values_from = "Data Value") %>%
  rbind(ability_occ %>% mutate(type = "ability") %>% #filter(`Recommend Suppress` == "N") %>% 
          rename(occ_8_dig = `O*NET-SOC Code`, element_ID = `Element ID`) %>% 
          select(occ_8_dig, element_ID, `Scale Name`, `Data Value`, N, type) %>%
          pivot_wider(id_cols = c("occ_8_dig","element_ID","type"), names_from = `Scale Name`, values_from = "Data Value")) %>%
  rbind(know_occ %>% mutate(type = "knowledge") %>% #filter(`Recommend Suppress` == "N") %>% 
          rename(occ_8_dig = `O*NET-SOC Code`, element_ID = `Element ID`) %>% 
          select(occ_8_dig, element_ID, `Scale Name`, `Data Value`, N, type) %>%
          pivot_wider(id_cols = c("occ_8_dig","element_ID","type"), names_from = `Scale Name`, values_from = "Data Value")) %>%
  mutate(occ_code = substr(occ_8_dig, 1,7), year = 2019) %>%
  distinct() %>% as.data.frame()

## 2.2. Recording Skill and Occupation Names
skills_names <- skill_occ %>% mutate(type = "skill") %>% rename(element_ID = `Element ID`, element_title = `Element Name`) %>% distinct(element_ID, element_title, type) %>%
  rbind(ability_occ %>% mutate(type = "ability") %>% rename(element_ID = `Element ID`, element_title = `Element Name`) %>% distinct(element_ID, element_title, type)) %>%
  rbind(know_occ %>% mutate(type = "knowledge") %>% rename(element_ID = `Element ID`, element_title = `Element Name`) %>% distinct(element_ID, element_title, type))

occ_names <- skill_occ %>% rename(occ_8_dig = `O*NET-SOC Code`, occ_title = Title) %>% distinct(occ_8_dig, occ_title) %>%
  rbind(ability_occ %>% rename(occ_8_dig = `O*NET-SOC Code`, occ_title = Title) %>% distinct(occ_8_dig, occ_title)) %>%
  rbind(know_occ %>% rename(occ_8_dig = `O*NET-SOC Code`, occ_title = Title) %>% distinct(occ_8_dig, occ_title)) %>% distinct()


## 2.3. Preferred Clustering Data
skill_subtype <- read.csv("preferred Skill Clustering and subtypes.csv") %>% 
  select(element_ID, gen_related)
skill_subtype$gen_related <- factor(skill_subtype$gen_related,
                                    levels = c("General", "Nested Intermediate", "Nested Specific", "Un-nested Intermediate", "Un-nested Specific" ))
skill_clusters <- read.csv("preferred Skill Clustering and subtypes.csv") %>% 
  select(element_ID, skill_Cluster)
skill_clusters$skill_Cluster <- factor(skill_clusters$skill_Cluster, levels = rev(c("General", "Intermediate", "Specific")))


## 2.4. The CPI inflation calculator - between 1980-2023 taken from https://www.bls.gov/data/inflation_calculator.htm
CPI_df <- read_xlsx(paste0(basepath, "Annual CPI Series - 1980-2023.xlsx")) %>%
  select(Year, Annual) %>% rename(year = Year) %>% as.data.frame()
CPI_df <- CPI_df %>% mutate(Current = CPI_df[CPI_df$year == 2022,]$Annual)


# 3. DEMOGRAPHIC DISTRIBUTION OF SKILLS      ==================================================
#==============================================================================================
### 3.1.1 IPUMS data ------------------------------------------------------------------------
#### https://usa.ipums.org
ipums_df <- read.csv(paste0(basepath, "cps.csv"), header = T) %>%
  mutate(gender = ifelse(SEX == 1, "Male", ifelse(SEX == 2,"Female", NA)))

### 3.1.2. CPS Crosswalks ------------------------------------------------------------------------
#### Category Description for Race, and Hispanic - https://usa.ipums.org
ipums_race_ref <- read_excel(paste0(basepath, "IPUMS Key.xlsx"), sheet = "Race")
ipums_hispan_ref <- read_excel(paste0(basepath, "IPUMS Key.xlsx"), sheet = "HISPAN")

#### Crosswalk between Ipums CPS occ 2010 to soc 2010 - find it at https://www.bls.gov/cps/cpsoccind.htm
ipums_occ2010_to_soc_2010 <- read_excel(paste0(basepath, "2010-occ-codes-with-crosswalk-from-2002-2011.xls"), 
                                        sheet = "2010OccCodeList", skip = 4) %>%
  rename(cps_2010 = "2010 Census Code", soc_2010 = "2010 SOC Code", "Occupation" = `Occupation 2010 Description`) %>%
  select(Occupation, soc_2010, cps_2010) %>% distinct() %>% mutate(cps_2010 = as.numeric(cps_2010)) ## generated NAs belong to genral groups

## there are Xs on the SOC_2010 side: let's handle them assuming X denote any combination between 0-9 I fill them with observed values in our skill data
xx_contaminated_rows = ipums_occ2010_to_soc_2010[grepl("XX", ipums_occ2010_to_soc_2010$soc_2010),] %>%
  mutate(sub_soc = substr(soc_2010,1,5)) %>% 
  left_join(distinct(skills_occ_raw, occ_code) %>% mutate(sub_soc = substr(occ_code,1,5))) %>%
  ungroup() %>% select(cps_2010, occ_code, Occupation)

x_contaminated_rows = ipums_occ2010_to_soc_2010[grepl("[0-9][A-Za-z]$", ipums_occ2010_to_soc_2010$soc_2010),] %>%
  mutate(sub_soc = substr(soc_2010,1,6)) %>% 
  left_join(distinct(skills_occ_raw, occ_code) %>% mutate(sub_soc = substr(occ_code,1,6))) %>%
  ungroup() %>% select(cps_2010, occ_code, Occupation)

completed_soc_cps_crosswalk = ipums_occ2010_to_soc_2010[!grepl("X", ipums_occ2010_to_soc_2010$soc_2010),] %>% 
  rename(occ_code = soc_2010) %>% bind_rows(xx_contaminated_rows) %>% bind_rows(x_contaminated_rows) %>%
  distinct(cps_2010, occ_code, Occupation) %>% drop_na()


## matching individual level data with race, ethnicity, and soc occupation info.
### We lose 2,407,754 rows due to occupational mismatches or because they are not in the workforce.
age_dem_occ_indiv <- ipums_df %>% 
  select(YEAR, AGE, gender, RACE, HISPAN, OCC2010, INCWAGE, EDUC, LABFORCE, WKSTAT,UHRSWORKT, UHRSWORK1) %>%
  left_join(ipums_race_ref %>% rename(RACE = Value, race = Label)) %>%
  left_join(ipums_hispan_ref %>% rename(HISPAN = Value) %>% select(-Label) %>%
              mutate(Hispanic = ifelse(HISPAN == 0, FALSE, ifelse(HISPAN %in% c(901, 902), NA, TRUE))), by = "HISPAN") %>%
  left_join(completed_soc_cps_crosswalk, by = c("OCC2010"="cps_2010")) %>%
  rename(year = YEAR, age = AGE) %>% 
  left_join(CPI_df, by = c("year")) %>% mutate(INCWAGE = Current * INCWAGE/(Annual)) %>%
  select(year, age, race, gender, Hispanic, occ_code, OCC2010, EDUC, INCWAGE, LABFORCE, WKSTAT, UHRSWORK1) %>%
  tibble::rowid_to_column("ID")

## keeping those in the workforce; that leaves us with 2,175,890 observations with matched occupations.
age_dem_occ_indiv <- age_dem_occ_indiv %>% filter(LABFORCE == 2, WKSTAT %in% c(10, 11))
age_dem_occ_indiv <- age_dem_occ_indiv %>% select(-OCC2010, -LABFORCE) %>% drop_na(occ_code)
age_dem_occ_indiv <- age_dem_occ_indiv %>% mutate(Race_Ethnicity = ifelse(is.na(Hispanic), race, ifelse(Hispanic, "Hispanic or Latino", race)))
age_dem_occ_indiv <- age_dem_occ_indiv %>% mutate(UHRSWORK1 = ifelse(is.na(UHRSWORK1), NA,
                                                                     ifelse(UHRSWORK1 %in% c(997,999), NA, UHRSWORK1)))
age_dem_occ_indiv <- age_dem_occ_indiv %>% mutate(weekly_wage = ifelse(is.na(UHRSWORK1), NA, 
                                                                       ifelse(UHRSWORK1==0, NA, INCWAGE/UHRSWORK1)))

### 3.1.3. Standardizing Individual Data -----------------------------------------------------------
#### Excluding individuals below 18 and above 55 to avoid attrition issues
#### Excluding individuals with adjusted wage of below $10,000 to produce more reliable estimates
cohort_ind_df <- age_dem_occ_indiv %>% select(-WKSTAT, -UHRSWORK1) %>% 
  filter(year >= 1980, age > 17, age <56, INCWAGE >= 10000) %>% 
  inner_join(data.frame(Race_Ethnicity = c("White","Hispanic or Latino", "Black","Asian only", "Asian or Pacific Islander"),
                        race_bracket = c("White","Hispanic/Latino", "Black","Asian", "Asian"))) %>% # racet brackets
  select(-race, -Hispanic, -Race_Ethnicity) %>% as.data.frame()
## ********** LOOK AT THE RESTRICTIONS SET ABOVE, YEAR, AGE, ADJUSTED ANNUAL SALARY, WORK STATUS


## This is the cohort data we obtain wherein individuals born in the same year form a synthetic cohort followed over time.
head(cohort_ind_df)

