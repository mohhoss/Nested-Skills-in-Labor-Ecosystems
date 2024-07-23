# General Description: Studying Interconnections of Occupation's Distribution of Skill Importance and Skill Categories' Importance Distributions
## Link to the latext file (?): https://www.overleaf.com/8811855357wspqcczxkpbs

# SECTIONS:
# 1. INITIATION
# 2. LOADING DATA
# 3. SKILL GROUPS (FIGURE 1)
# 4. SKILL INTERDEPENDENCE FIGURE (FIGURE 2)
# 5. EXPANSION ON NESTEDNESS (FIGURE 3)
# 6. SKILL ACQUISITION AND TIME (FIGURE 4) - LOOK AT BG VIZ FILE
# 7. INPUT AND OUTPUT OF SKILLS (FIGURE 5)
# 8. COMPARISON OF OLD AND NEW SKILLS (FIGURE 7)
# 9. DEMOGRAPHIC DISTRIBUTION OF SKILLS (FIGURE 6)

# 1. INITIATION     ===========================================================================
#==============================================================================================
##---------------------------------------------------------------------------------------------
## 1.1. Packages
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
library(parallel)
library(mclust)
library(moments)
library(NbClust) 
library(DescTools) 
library(patchwork)
library(ggthemes)
library(igraph)
library(markovchain)

##---------------------------------------------------------------------------------------------
## 1.2. Memory and Path
rm(list = ls())
basepath <- "UPDATE WITH YOUR PATH"
wdpath <- "UPDATE WITH YOUR WORKING DIRECTORY PATH"

setwd(paste0(basepath,wdpath))
dir()

##---------------------------------------------------------------------------------------------
## 1.3. Functions

### 1.3.1. Summary Function based on dplyr
summarySE <- function(data=NULL, measurevar, groupvars=NULL, na.rm=T,
                      conf.interval=.95) {
  # New version of length which can handle NA's: if na.rm==T, don't count them
  length2 <- function (x, na.rm=FALSE) {
    if (na.rm) sum(!is.na(x))
    else       length(x)
  }
  
  # This does the summary. For each group's data frame, return a vector with
  # N, mean, and sd
  data <- data %>% dplyr::rename("measurevar" = paste0(measurevar)) %>%
    as.data.table() 
  
  datac <- data[, .(N=length2(measurevar, na.rm=na.rm),
                    mean= mean(measurevar, na.rm=na.rm),
                    median= median(measurevar, na.rm=na.rm),
                    q25= quantile(measurevar, probs = 0.25, na.rm=na.rm),
                    q75= quantile(measurevar, probs = 0.75, na.rm=na.rm),
                    sd= sd(measurevar, na.rm=na.rm),
                    max = max(measurevar, na.rm=na.rm),
                    min = min(measurevar, na.rm=na.rm)),
                by=groupvars]
  
  datac <- datac %>% arrange_at(groupvars)
  
  colnames(datac)[which(colnames(datac) == "mean")] <- paste0(measurevar)
  
  datac$se <- datac$sd / sqrt(datac$N)  # Calculate standard error of the mean
  
  # Confidence interval multiplier for standard error
  # Calculate t-statistic for confidence interval: 
  # e.g., if conf.interval is .95, use .975 (above/below), and use df=N-1
  ciMult <- qt(conf.interval/2 + .5, datac$N-1)
  datac$ci <- datac$se * ciMult
  
  return(datac)
}

### 1.3.2. Function to Calculate the overlap nestedness metric (Nc) of Wright and Reeves 1992
Nc <- function(X) {
  ## Sites are expected in rows and Species in Columns
  X <- as.matrix(X)
  marg_totals = colSums(X)
  paired_column_fill_combos = marg_totals * sapply(marg_totals - 1, function(x) max(x,0))/2
  Nc = sum(paired_column_fill_combos)
  # C = (Nc - mean(paired_column_fill_combos))/(max(paired_column_fill_combos) - mean(paired_column_fill_combos))
  C = NA
  return(data.frame(Nc = Nc, C = C))
}


# 2. LOADING DATA     =========================================================================
#==============================================================================================
##---------------------------------------------------------------------------------------------
## 2.1. Skill data - 120 in Skills/Abilities/Knowledge in Total from O*NET 24.4 (2019)
### Find the data in the following url: https://www.onetcenter.org/db_releases.html
### 2.1.1. Read the Skill data in
skill_occ <- read_excel(paste0(basepath,"Skills.xlsx"))
skill_occ[skill_occ$'Element Name' == "Mathematics",'Element Name'] <- "Mathematics Skills"

ability_occ <- read_excel(paste0(basepath,"Abilities.xlsx"))

know_occ <- read_excel(paste0(basepath,"Knowledge.xlsx"))
know_occ[know_occ$'Element Name' == "Mathematics",'Element Name'] <- "Mathematics Knowledge"

### 2.1.2. Combine the Skill Data
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

#### Keep the skill titles
skills_names <- skill_occ %>% mutate(type = "skill") %>% rename(element_ID = `Element ID`, element_title = `Element Name`) %>% distinct(element_ID, element_title, type) %>%
  rbind(ability_occ %>% mutate(type = "ability") %>% rename(element_ID = `Element ID`, element_title = `Element Name`) %>% distinct(element_ID, element_title, type)) %>%
  rbind(know_occ %>% mutate(type = "knowledge") %>% rename(element_ID = `Element ID`, element_title = `Element Name`) %>% distinct(element_ID, element_title, type))

#### Keep the occupation titles
occ_names <- skill_occ %>% rename(occ_8_dig = `O*NET-SOC Code`, occ_title = Title) %>% distinct(occ_8_dig, occ_title) %>%
  rbind(ability_occ %>% rename(occ_8_dig = `O*NET-SOC Code`, occ_title = Title) %>% distinct(occ_8_dig, occ_title)) %>%
  rbind(know_occ %>% rename(occ_8_dig = `O*NET-SOC Code`, occ_title = Title) %>% distinct(occ_8_dig, occ_title)) %>% distinct()

##---------------------------------------------------------------------------------------------
## 2.2. Adding Wage Data from OWES 2019
### Find the data in the following url: https://www.bls.gov/oes/special-requests/oesm19all.zip
### The following code puts bounded values on extremes as reported by OWES
occ_wage <- read_xlsx(paste0(basepath,"national_M2019_dl.xlsx"), col_names = T) %>%
  mutate_at(vars(tot_emp:a_pct90), function(x){return(gsub(",","",x))}) %>%
  mutate_at(vars(starts_with("a_")), function(x){return(ifelse(x=="#",208001,x))}) %>%
  mutate_at(vars(starts_with("h_")), function(x){return(ifelse(x=="#",100.1,x))}) %>%
  mutate_at(vars(tot_emp:a_pct90), function(x){return(ifelse(x=="**",NA,x))}) %>%
  mutate_at(vars(tot_emp:a_pct90), function(x){return(ifelse(x=="*", NA,x))}) %>% 
  mutate(year = 2019) %>% mutate_at(vars(tot_emp:a_pct90),as.numeric) %>%
  ungroup() %>% group_by(year, occ_code, occ_title) %>%
  summarise_at(vars(tot_emp, h_median, a_median, h_mean, a_mean, a_pct10, a_pct25, a_pct75, a_pct90), mean, na.rm = TRUE) %>%
  as.data.frame()

##---------------------------------------------------------------------------------------------
## 2.3. Education Data from O*NET (find in the zip file you downloaded)
occ_educ_df <- read_excel(paste0(basepath,"Education, Training, and Experience.xlsx")) %>%
  group_by(`O*NET-SOC`,`Element Name`) %>% summarise(val = sum(Category * `Data Value`)/ sum(`Data Value`)) %>%
  pivot_wider(id_cols = `O*NET-SOC`, names_from = `Element Name`, values_from = val) %>%
  rename(occ_8_dig = `O*NET-SOC`, "Education.Avg" = "Required Level of Education", "Experience.Avg" = "Related Work Experience", 
         "OnSiteTraining.Avg" = "On-Site or In-Plant Training", "InJobTraining.Avg" = "On-the-Job Training")


##---------------------------------------------------------------------------------------------
## 2.4. CPI Series - 1980-2023 from https://www.bls.gov/data/inflation_calculator.htm
CPI_df <- read_xlsx(paste0(basepath, "Dropbox/Research/Data/BLS Occupational Employment Statistics/Annual CPI Series - 1980-2023.xlsx"), skip = 11) %>%
  select(Year, Annual) %>% rename(year = Year) %>% as.data.frame()
CPI_df <- CPI_df %>% mutate(Current = CPI_df[CPI_df$year == 2022,]$Annual)


# ##---------------------------------------------------------------------------------------------
## 2.5. Preferred Clustering Data
### It is Provided in the Github Rep for Simplicity (we obtain the clusters from the following commented code; categories are based on nestedness---section EXPANSION ON NESTEDNESS)
#### Code for obtaining
# k <- 3
# set.seed(123)
# bins <- 70
# skill_importance_dist_binned <- skills_occ_raw %>% rename(val = Level) %>%
#   distinct(occ_8_dig, element_ID, val, occ_code, year) %>% drop_na() %>%
#   dplyr::mutate(Level_binned = cut_interval(val, n = bins, right = F)) %>%
#   drop_na() %>%
#   group_by(year, element_ID, Level_binned) %>% dplyr::summarise(Freq = n_distinct(occ_8_dig)) %>% distinct()
# 
# skill_importance_binned_wide <- skill_importance_dist_binned %>% arrange(Level_binned, element_ID) %>%
#   pivot_wider(id_cols = "element_ID", names_from = "Level_binned", values_from = "Freq", values_fill = 0) %>%
#   tibble::column_to_rownames("element_ID")
# 
# skill_dist_kmean <- data.frame(cos_membership = amap::Kmeans(skill_importance_binned_wide, k, method = metric, iter.max = 1000)$cluster,
#                                element_ID = rownames(skill_importance_binned_wide)) %>%
#   inner_join(skills_names, by = "element_ID") %>%
#   as.data.frame()
# 
# modif_skill_occ = skills_occ_raw %>%
#   group_by(element_ID) %>% mutate_at(vars(Level, Importance), function(x){return((x - mean(x))/(sd(x)))}) %>%
#   rename(val = Level)
# 
# skill_dist_kmean <- skill_dist_kmean %>% select(-type) %>%
#   inner_join(skills_occ_raw %>% rename(val = Level) %>% left_join(skill_dist_kmean, by= "element_ID") %>%
#                group_by(cos_membership) %>% dplyr::summarise(cos_skew = skewness(val), cos_avg = mean(val)) %>%
#                ungroup() %>% mutate(cos_rank = rank((cos_avg), ties.method = "min"))) %>%
#   inner_join(modif_skill_occ %>% left_join(skill_dist_kmean, by= "element_ID") %>%
#                ungroup() %>% group_by(element_ID, cos_membership) %>% summarise(skew = skewness(val)) %>%
#                distinct(element_ID, cos_membership, skew)) %>%
#   as.data.frame()

#### Read in the Data Instead
skill_categories <- read.csv("preferred Skill Clustering and subtypes - Aug 5 2023.csv") %>%
  select(element_ID, gen_related) %>% rename("categories" = "gen_related")
skill_categories$categories <- factor(skill_categories$categories,
                                        levels = c("General", "Nested Intermediate", "Nested Specific", "Un-nested Intermediate", "Un-nested Specific" ))
skill_clusters <- read.csv("preferred Skill Clustering and subtypes - Aug 5 2023.csv") %>%
  select(element_ID, skill_Cluster)
skill_clusters$skill_Cluster <- factor(skill_clusters$skill_Cluster, levels = rev(c("General", "Intermediate", "Specific")))

uniq_skills <- unique(skills_occ_raw$element_ID)


##---------------------------------------------------------------------------------------------
## 2.6. ONET SOC Crosswalk
### Find at the following url: https://www.onetcenter.org/crosswalks.html
SOC_2000_2010_crosswalk <- read_xlsx(paste0(basepath,
                                            "Dropbox/Research/Data/ONET SOC Crosswalks/ONET SOC 2000 to 2010 Crosswalk - May 2022.xlsx")) %>%
  distinct(`O*NET-SOC 2000 Code`, `O*NET-SOC 2010 Code`) %>% 
  rename(onet_soc_2000 = `O*NET-SOC 2000 Code`, onet_soc_2010 = `O*NET-SOC 2010 Code`)


##---------------------------------------------------------------------------------------------
## 2.7. Binary Version of the Occupational Skill Data
### It is Provided in the Github Rep for Simplicity (we obtain this from the Python Code Provided by Serrano et al. 2009)
serrano_binary <- read.csv(paste0(baepath,"Binary Skill-Occupation Bipartite - a la Serrano et al 2009 - alpha_in = 0.4, alpha_out = 0.275 - Nov 2022.csv")) %>%
  select(-weight)


##---------------------------------------------------------------------------------------------
## 2.8. Frey and Osborne Raw data
### Read the data from the project rep.
frey_osborne <- read_excel(paste0(basepath, "Frey and Osborn 2017 - Automation Risk.xlsx")) %>%
  rename(occ_code = `SOC code`) %>% mutate_at(vars(Probability), as.numeric)


##---------------------------------------------------------------------------------------------
## 2.9. Skill Dependency Network
### Download from Project Rep.
skill_dependency <- read.csv(paste0(basepath, "Joe_et_al_2020_Serrano_alpha_in = 0.4, alpha_out = 0.275_filtered_z-thres_4.75_a-thres_0.05 edgelist.csv"))

##---------------------------------------------------------------------------------------------
## 2.10. LRC metric for Skills based on the dependency network
### Download from Project Rep.
LRC_df <- read.csv(paste0(basepath, "Skill LCR - Parsimonious_Joe_et_al_2020_Serrano_alpha_in = 0.4, alpha_out = 0.275_filtered_z-thres_4.75_a-thres_0.05_our_skill_clusters - Jul 12 2023.csv"))


##---------------------------------------------------------------------------------------------
## 2.11. Our Choice of Colors (red, gray, blue)
my3_colors <- c(rgb(0.901960784313726, 0.027450980392157,  0.164705882352941), #red
                    rgb(89/255, 89/255, 89/255), # gray
                    rgb(0.258823529411765,   0.309803921568627,   0.643137254901961)) # blue



# 3. SKILL CLUSTERING FIGURE (FIGURE 1)     ===================================================
#==============================================================================================
## Empirical Boxplots
temp <- skills_occ_raw %>% inner_join(skill_clusters) %>% inner_join(skills_names) %>%
  bind_rows(data.frame(occ_8_dig = NA, element_ID = NA, Level = 7)) %>%
  ungroup() %>% mutate(Level_intv = cut_width(Level, 0.5, center = 0, ordered_result = T)) %>%
  group_by(Level_intv, year, skill_Cluster, element_ID, element_title, .drop = F) %>% dplyr::summarise(n_occ = n_distinct(occ_8_dig)) %>%
  ungroup() %>% distinct() %>% drop_na() %>%
  inner_join(data.frame(COLOR = rev(my3_colors), skill_Cluster = levels(skill_clusters$skill_Cluster))) %>%
  left_join(data.frame(example1 = c("English Language", "Mathematics Skills", "Programming"),
                       example2 = c("Oral Expression", "Negotiation", "Dynamic Flexibility"), 
                       skill_Cluster = rev(levels(skill_clusters$skill_Cluster)))) %>%
  mutate(COLOR = as.character(COLOR)) %>% 
  left_join(data_frame(skill_Cluster = c("General","Intermediate","Specific"), Labels = c("General","Intermediate","Specific"))) %>%
  as.data.frame() 

temp$n_occ[is.na(temp$n_occ)] <- 0
temp$skill_Cluster <- factor(temp$skill_Cluster, levels = unique((skill_clusters %>% arrange((skill_Cluster)))$skill_Cluster))


p <- temp %>%
  split(.$skill_Cluster) %>%
  lapply(function(temp_grp) {
    
    c <- ggplot(temp_grp, aes(x = Level_intv, y = n_occ)) +
      geom_boxplot(color = unique(temp_grp$COLOR), fill = unique(temp_grp$COLOR), alpha = 0.2) +
      theme_stata() + theme(axis.title = element_text(size = 16), title = element_text(size = 12),
                            axis.text.y = element_text(angle = 0, size = 14),
                            # axis.ticks.x = element_blank(), axis.ticks.y = element_blank(),
                            axis.text.x = element_text(size = 14, angle = 0), 
                            strip.text = element_text(size = 14), plot.background = element_blank(),
                            panel.border = element_rect(color = 'black', fill = NA),
                            panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                            strip.text.x = element_text(size = 14), strip.text.y = element_text(size = 14), 
                            strip.background = element_rect(fill = NA, size = 1), plot.margin = margin(0,0,0,2, unit = "pt")) +
      scale_x_discrete(breaks = levels(temp$Level_intv), labels = c(rbind(seq(0,7,1),rep("", 7)))[-16]) +
      scale_y_continuous(limits = c(0,650), breaks = seq(0,500, 100)) + guides(color = "none") +
      labs(x = ifelse(temp_grp$skill_Cluster == "Intermediate","Skill Level",""),
           y = "Number of Occupations\n", 
           title = paste(unique(temp_grp$Labels), "Skill Group"))
    
    if(unique(temp_grp$skill_Cluster) != "Specific") {
      c <- c + theme(axis.title.y = element_blank())
    }
    
    d <- ggplot(temp_grp %>% filter(element_title %in% example2 | element_title %in% example1) %>%
                  complete(Level_intv, year, skill_Cluster, element_ID, element_title),
                aes(x = factor(Level_intv), y = n_occ)) +
      facet_wrap(vars(element_title), ncol = 2, labeller = label_wrap_gen(15), scales = "free_y") +
      # scale_y_continuous(limits = c(0,550)) +
      geom_bar(alpha = 0.5, stat = "identity", fill = unique(temp_grp$COLOR), color = unique(temp_grp$COLOR)) +
      guides(color = "none", fill = "none") +
      theme_stata() + theme(plot.background = element_rect(fill = "white"),
                            panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                            axis.text.x = element_blank() , axis.text.y = element_blank(), 
                            axis.ticks.x = element_blank(), axis.ticks.y = element_blank(),
                            axis.title.x = element_blank(), axis.title.y = element_blank(),
                            strip.text = element_text(size = 12), strip.background = element_rect(fill = NA, size = 1),
                            plot.margin = margin(t = -7, r = 0, b = 0, l = 0, unit = "pt"))
    
    c + inset_element(d, 0.001,0.74, 0.99, 0.99)
  })

(p %>% wrap_plots(ncol = 3)) 


# 4. SKILL INTERDEPENDENCE FIGURE (FIGURE 2)     ==============================================
#==============================================================================================
##---------------------------------------------------------------------------------------------
## 4.0. Note:
### Producing Fig 2 operates in Two main steps
### 1. Make Occupational Skill data binary (based on Serrano et al 2009); the code file is in the rep; the output is serrano_binary also available in the rep.
### 2. Use the Binary Occupational Skill data to Produce a Skill Hiearchy (based on Jo et al 2020); the code file is in the rep; the output is skill_dependency also available in the rep.
### The network is then visualized in Gephi
### The following code calculates the arrival/hitting probabilities that are used in parts c-e of fig 2:

##---------------------------------------------------------------------------------------------
## 4.1. Parts c-d: Hitting Probability
prod_space_df <- skill_dependency %>% distinct(source,  target,  alpha)
# prod_space_df <- skill_parsi_dependency %>% distinct(source,  target,  alpha)


prod_space_df <- prod_space_df %>%
  full_join(tidyr::crossing(data.frame(source = union(unique(skill_dependency$source), unique(skill_dependency$target))),
                            data.frame(target = union(unique(skill_dependency$source), unique(skill_dependency$target))), name_repair = "universal"),
            by = c("source", "target")) %>% select(-name_repair)
prod_space_df[is.na(prod_space_df$alpha),]$alpha <- 0


## Forming the matrix
prod_space_mat <- prod_space_df %>% arrange(source, target) %>%
  pivot_wider(id_cols = "source", names_from = "target", values_from = "alpha") %>%
  tibble::column_to_rownames("source") %>% as.matrix()


## Normalizing to obtain a transition matrix - markov transition matrix
rowsum_0_id <- which(rowSums(prod_space_mat)==0)
diag(prod_space_mat[rowsum_0_id,rowsum_0_id])=1
prod_space_mat = prod_space_mat/rowSums(prod_space_mat)


## Calculating the hitting probabilities
mc <- new("markovchain", transitionMatrix = prod_space_mat)
hitting_mat <- hittingProbabilities(mc)


# 5. EXPANSION ON NESTEDNESS (FIGURE 3)      ==================================================
#==============================================================================================
##---------------------------------------------------------------------------------------------
## 5.1. Calculate Nestedness Contributions
### 5.1.1. Observed
obs_skill_occ_binary <- serrano_binary %>% 
  # inner_join(data.frame(element_ID = unique(rca_binary_df$element_ID), element_ID_num = c(1:length(unique(rca_binary_df$element_ID))))) %>%
  select(element_ID, occ_8_dig) %>% mutate(value = 1)

obs_mat <- obs_skill_occ_binary %>% arrange(element_ID,occ_8_dig) %>% 
  pivot_wider(id_cols = "element_ID", names_from = "occ_8_dig", values_from = "value", values_fill = 0) %>% tibble::column_to_rownames("element_ID")
obs_NODF <- vegan::nestednodf(obs_mat)
obs_skill_occ_C <- vegan::nestedchecker(obs_mat)$C.score
obs_skill_occ_Binm <- as.numeric(vegan::nestedbetasor(obs_mat)[2])
obs_skill_occ_div = as.numeric(nesteddisc(obs_mat, niter = 10)$statistic)
obs_skill_occ_Nc = as.numeric(Nc(obs_mat)$Nc)
obs_occ_skill_C <- vegan::nestedchecker(t(obs_mat))$C.score
obs_occ_skill_Binm <- as.numeric(vegan::nestedbetasor(t(obs_mat))[2])
obs_occ_skill_div = as.numeric(nesteddisc(t(obs_mat), niter = 10)$statistic)
obs_occ_skill_Nc = as.numeric(Nc(t(obs_mat))$Nc)

### 5.1.2. Randomized
set.seed(123)
ITER <- 5*10^3
skill_deg <- obs_skill_occ_binary %>% group_by(element_ID) %>% summarise(N = n_distinct(occ_8_dig))
uniq_skills <- unique(obs_skill_occ_binary$element_ID)
uniq_occs <- unique(obs_skill_occ_binary$occ_8_dig)
nested_simul <- do.call(rbind, lapply(1:length(uniq_skills), function(i){
  print(paste0(round(i/length(uniq_skills) * 100, 1), "%"))
  new_mat <- obs_skill_occ_binary %>% filter(element_ID != uniq_skills[i])
  return(do.call(rbind, mclapply(c(1:ITER), function(iter){
    simul_mat = new_mat %>%
      bind_rows(data.frame(element_ID = uniq_skills[i], value = 1,
                           occ_8_dig = sample(x = uniq_occs, size = as.numeric(skill_deg[skill_deg$element_ID == uniq_skills[i], 'N'])))) %>%
      pivot_wider(id_cols = "element_ID", names_from = "occ_8_dig", values_from = "value", values_fill = 0) %>% tibble::column_to_rownames("element_ID")

    ## For other Measures
    simul_NODF <- vegan::nestednodf(simul_mat)
    simul_skill_occ_C <- vegan::nestedchecker(simul_mat)$C.score
    simul_skill_occ_Binm <- as.numeric(vegan::nestedbetasor(simul_mat)[2])
    simul_skill_occ_div = as.numeric(nesteddisc(simul_mat, niter = 10)$statistic)
    simul_skill_occ_Nc = as.numeric(Nc(simul_mat)$Nc)
    simul_occ_skill_C <- vegan::nestedchecker(t(simul_mat))$C.score
    simul_occ_skill_Binm <- as.numeric(vegan::nestedbetasor(t(simul_mat))[2])
    simul_occ_skill_div = as.numeric(nesteddisc(t(simul_mat), niter = 10)$statistic)
    simul_occ_skill_Nc = as.numeric(Nc(t(simul_mat))$Nc)
    return(data.frame(element_ID = uniq_skills[i], iter = iter, 
                      C = simul_skill_occ_C, C_t = simul_occ_skill_C,
                      Binm = simul_skill_occ_Binm, Binm_t = simul_occ_skill_Binm,
                      div = simul_skill_occ_div, div_t = simul_occ_skill_div,
                      Nc = simul_skill_occ_Nc, Nc_t = simul_occ_skill_Nc, row.names = NULL))
    
  }, mc.cores = (detectCores(all.tests = FALSE, logical = TRUE)) )))
  
}))

### 5.1.3. Calculate nestedness Score
skill_nest_contrib <- nested_simul %>% group_by(element_ID) %>% 
  summarise(c_i_C = (as.numeric(obs_skill_occ_C) - mean(C))/sd(C), c_i_Binm = (as.numeric(obs_skill_occ_Binm) - mean(Binm))/sd(Binm),
            c_i_Nc = (as.numeric(obs_skill_occ_Nc) - mean(Nc))/sd(Nc), c_i_div = (as.numeric(obs_skill_occ_div) - mean(div))/sd(div),
            c_i_NODF = (as.numeric(obs_NODF$statistic[2]) - mean(row_NODF))/sd(row_NODF)) %>%
  left_join(skills_names %>% select(element_ID, element_title)) %>% 
  left_join(occ_educ_df %>% inner_join(skills_occ_raw[,c('occ_8_dig','element_ID','Level')]) %>% 
              group_by(element_ID) %>% summarise(level_educ = sum(Education.Avg*Level)/sum(Level)), by = "element_ID") %>%
  left_join(skill_clusters) %>% as.data.frame()

##---------------------------------------------------------------------------------------------
## 5.2. The Figure
y_skill_value <- skills_occ_raw %>% 
  left_join(occ_wage %>% distinct(occ_code, a_mean)) %>% #mutate_at(vars(a_mean), log10)) %>% 
  drop_na() %>%  group_by(element_ID) %>% 
  summarise(Importance = sum(Importance * a_mean)/sum(Importance), Level = sum(Level * a_mean)/sum(Level)) %>%
  left_join(skills_occ_raw %>% left_join(occ_educ_df %>% distinct(occ_8_dig,  Education.Avg)) %>% drop_na() %>%
              group_by(element_ID) %>% summarise(edu = sum(Education.Avg * Level)/sum(Level)) %>% distinct()) %>%
  left_join(LRC_df) %>%
  left_join(frey_osborne %>% distinct(occ_code, Probability, Occupation) %>% mutate_at(vars(Probability), as.numeric) %>% left_join(skills_occ_raw) %>%
              group_by(element_ID) %>% summarise(Automation = sum(Level * Probability, na.rm = T)/sum(Level, na.rm = T)), by = "element_ID") %>%
  rename("Generality (LRC)" = LRC, "Level-weighted Education" = edu , "Importance-weighted Wages" = "Importance", 
         "Level-weighted Wages" = "Level", "Automation Risk Index" = "Automation") %>%
  left_join(skills_names) %>% left_join(skill_clusters) %>% left_join(skill_categories) %>%
  left_join(skill_nest_contrib %>% distinct(element_ID, c_i_NODF,c_i_Nc)) %>%
  pivot_longer(cols = c("Importance-weighted Wages", "Generality (LRC)", "Level-weighted Wages", "Automation Risk Index"), names_to = "Value_type", values_to = "Value_val") %>%
  as.data.frame()
y_skill_value$Value_type = factor(y_skill_value$Value_type,
                                  levels = c("Importance-weighted Wages", "Generality (LRC)", "Level-weighted Wages", "Automation Risk Index"))


## Shape by Nestedness Categories - Alt 1
gg <- ggplot(y_skill_value %>% filter(! Value_type %in% c("Importance-weighted Wages", "Level-weighted Wages"))
       , aes(x = c_i_Nc, y = Value_val, color = categories, fill = categories, shape = categories, linetype = categories)) +
  facet_wrap(vars(Value_type), scales = "free", switch = "y", ncol = 1) +
  geom_point(stroke = 2, size = 5, alpha = 0.5) +
  geom_vline(xintercept = 0, linetype = "dotted", color = "black", size = 1, alpha = .5) +
  geom_text_repel(data = y_skill_value %>% filter(Value_type == "Automation Risk Index", element_title %in% c("Programming", "Fine Arts")),
                  mapping = aes(label = element_title), size = 5, force = 250, segment.size = .5, show.legend = FALSE, nudge_x = -15, nudge_y = .01) +
  geom_text_repel(data = y_skill_value %>% filter(Value_type == "Automation Risk Index", element_title == "Repairing"),
                  mapping = aes(label = element_title), size = 5, force = 250, segment.size = .5, show.legend = FALSE, nudge_x = 20) +
  geom_text(data = data.frame(c_i_Nc = -2, Value_val = 0.75, Value_type = factor("Generality (LRC)", levels = c("Importance-weighted Wages", "Generality (LRC)", "Level-weighted Wages", "Automation Risk Index")), categories = "Nested Intermediate", label = "Nestedness Threshold"),
            mapping = aes(label = label), size = 5, color = "black", alpha = .5, angle = 90) +
  scale_color_manual(values = alpha((c(my3_colors,my3_colors[2:3])),1), breaks = levels(skill_clusters$categories)) +
  scale_fill_manual(values = alpha((c(my3_colors,my3_colors[2:3])),.4), breaks = levels(skill_clusters$categories)) +
  scale_shape_manual(breaks = levels(y_skill_value$categories), values = c(19,19,19, 1,1)) +
  scale_linetype_manual(breaks = levels(y_skill_value$categories), values = c("solid","solid","solid", "dashed","dashed")) +
  theme_stata() + theme(axis.title.y = element_blank(), axis.title.x = element_text(size = 14), axis.text = element_text(size = 14),
                        axis.text.y = element_text(angle = 0), title = element_text(size = 16), 
                        strip.text.y = element_text(size = 14), plot.background = element_blank(),
                        legend.box = "vertical", legend.direction = "vertical", legend.position = c(0,1), legend.background = element_blank(),
                        legend.title = element_blank(), legend.justification = c("left","top"),
                        panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
                        legend.text = element_text(size = 12), panel.border = element_rect(color = 'black', fill = NA, linewidth = 1),
                        strip.text.x = element_text(size = 14), strip.background = element_rect(fill = NA),
                        strip.placement = "outside", legend.text.align = 1, plot.margin = margin(t = 10, r = 0, b = 0, l = 5, unit = "pt")) +
  guides(size = "none", linetype = "none", shape = "none") +
  labs(x = "Skills' Nestedness Score") |
  ggplot(y_skill_value %>% filter(Value_type %in% c("Importance-weighted Wages", "Level-weighted Wages"))
         , aes(x = c_i_Nc, y = Value_val, color = categories, fill = categories, shape = categories, linetype = categories)) +
  facet_wrap(vars(Value_type), scales = "free", switch = "y", ncol = 1) +
  geom_point(stroke = 2, size = 5, alpha = 0.5) +
  geom_vline(xintercept = 0, linetype = "dotted", color = "black", size = 1, alpha = .5) +
  geom_text_repel(data = y_skill_value %>% filter(Value_type %in% c("Importance-weighted Wages", "Level-weighted Wages"), 
                                                  element_title %in% c("Programming", "Fine Arts")),
                  mapping = aes(label = element_title), size = 5, force = 250, segment.size = .5, show.legend = FALSE, nudge_x = -15, nudge_y = .01) +
  geom_text_repel(data = y_skill_value %>% filter(Value_type %in% c("Importance-weighted Wages", "Level-weighted Wages"), element_title == "Repairing"),
                  mapping = aes(label = element_title), size = 5, force = 250, segment.size = .5, show.legend = FALSE, nudge_x = 20, nudge_y = .015) +
  scale_y_continuous(trans = "log10", labels = scales::label_number(scale_cut = scales::cut_si('$'))) +
  scale_color_manual(values = alpha((c(my3_colors,my3_colors[2:3])),1), breaks = levels(skill_clusters$categories)) +
  scale_fill_manual(values = alpha((c(my3_colors,my3_colors[2:3])),.4), breaks = levels(skill_clusters$categories)) +
  scale_shape_manual(breaks = levels(y_skill_value$categories), values = c(19,19,19, 1,1)) +
  scale_linetype_manual(breaks = levels(y_skill_value$categories), values = c("solid","solid","solid", "dashed","dashed")) +
  theme_stata() + theme(axis.title.y = element_blank(), axis.title.x = element_text(size = 14), axis.text = element_text(size = 14),
                        axis.text.y = element_text(angle = 0), title = element_text(size = 16), 
                        strip.text.y = element_text(size = 14), plot.background = element_blank(),
                        legend.box = "vertical", legend.direction = "vertical", legend.position = c(1,0), legend.background = element_blank(),
                        legend.title = element_blank(), legend.justification = c("right","bottom"),
                        panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
                        legend.text = element_text(size = 12), panel.border = element_rect(color = 'black', fill = NA, linewidth = 1),
                        strip.text.x = element_text(size = 14), strip.background = element_rect(fill = NA),
                        strip.placement = "outside", legend.text.align = 1, plot.margin = margin(t = 10, r = 0, b = 0, l = 5, unit = "pt")) +
  guides(size = guide_legend(byrow = TRUE, title.position = "top", title.hjust = 0), linetype = "none",
         shape = guide_legend(override.aes=list(size = 4, color = c(my3_colors,my3_colors[2:3]), alpha = 0.8), label.position = "left")) +
  labs(x = "Skills' Nestedness Score")


# 6. SKILL ACQUISITION AND TIME (FIGURE 4)      ===============================================
#==============================================================================================
## Burning Glass and Age Cohort Analysis
## Uses a different file: Burning Glass Visualizations - Feb 2023.R


# 7. INPUT AND OUTPUT OF SKILLS (FIGURE 5)      ===============================================
#==============================================================================================
## 7.1. Connecting Occupational Measures
edu_df_level <- skills_occ_raw %>% distinct(occ_8_dig, element_ID, Level, year) %>%
  left_join(skill_categories) %>% left_join(skill_clusters) %>% filter(skill_Cluster != "Intermediate") %>%
  ungroup() %>% group_by(year, occ_8_dig, categories) %>% summarise(skill_endow = mean(Level)) %>%
  left_join(occ_educ_df %>% distinct(occ_8_dig, Education.Avg, Experience.Avg, OnSiteTraining.Avg, InJobTraining.Avg)) %>%
  ungroup() %>% pivot_wider(id_cols = c("year", "occ_8_dig", "Education.Avg", "Experience.Avg"), names_from = "categories", values_from = "skill_endow") %>%
  as.data.frame()


edu_df_long <- edu_df_level %>% 
  pivot_longer(cols = -c(year, occ_8_dig, Education.Avg, Experience.Avg, General), values_to = "skill_endow", names_to = "categories") %>%
  mutate(categories = factor(categories, levels = c("Nested Specific", "Un-nested Specific")))


wage_df_level <- skills_occ_raw %>% distinct(occ_code, element_ID, Level, year) %>%
  left_join(skill_clusters) %>% left_join(skill_categories)  %>% filter(skill_Cluster != "Intermediate") %>% 
  ungroup() %>% group_by(year, occ_code, categories) %>% summarise(skill_endow = mean(Level)) %>%
  left_join(occ_wage) %>%
  ungroup() %>% pivot_wider(id_cols = c("year", "occ_code", "a_mean"), names_from = "categories", values_from = "skill_endow") %>%
  as.data.frame()


wage_df_long <- wage_df_level %>% 
  pivot_longer(cols = -c(year, occ_code, a_mean, General), values_to = "skill_endow", names_to = "categories") %>%
  mutate(categories = factor(categories, levels = c("Nested Specific", "Un-nested Specific")))


##---------------------------------------------------------------------------------------------
## 7.2. Input (Education) ------------------------------------------------------
## Education Trends
educ_fig_a <- ggplot(edu_df_long, aes(x = skill_endow, y = Education.Avg, shape = categories)) +
  # facet_wrap(vars(categories_res), scales = "free_x", labeller = label_wrap_gen(45)) +
  stat_summary_bin(aes(linetype = categories), position = position_dodge(0.9), bins = 5, fun.y = "mean", size = 0.5, geom = "line", color = my3_colors[3]) +
  stat_summary_bin(bins = 5, position = position_dodge(0.9), color = my3_colors[3], geom = "point", size = 3) +
  stat_summary_bin(bins = 5, position = position_dodge(0.9), color = my3_colors[3], fun.data = "mean_cl_normal", geom = "errorbar") +
  # scale_size(breaks = c (2,6,10), labels = c("Low","","High")) +
  scale_shape_manual(breaks = levels(edu_df_long$categories), values = c(19, 1), labels = c("Nested", "Un-nested")) +
  theme_stata() + theme(axis.title = element_text(size = 14), axis.text = element_text(size = 14),
                        axis.text.y = element_text(angle = 0), title = element_text(size = 16),
                        strip.text = element_text(size = 12), plot.background = element_blank(),
                        panel.grid.major = element_blank(), panel.grid.minor = element_blank(), legend.title = element_blank(),
                        legend.text = element_text(size = 12), panel.border = element_rect(color = 'black', fill = NA, size = 1),
                        strip.text.x = element_text(size = 14), strip.background = element_rect(fill = NA),
                        strip.text.y = element_text(size = 14), plot.margin = margin(2,0,10,1, unit = "pt")) +
  guides(size = 'none', linetype = "none", shape = guide_legend(override.aes=list(size = 4))) +
  labs(y = "Occupation Education", x = "Occupation Skill Level")

educ_inset <- ggplot(edu_df_long %>% distinct(year, occ_8_dig, General, Education.Avg), aes(x = General, y = Education.Avg)) +
  stat_summary_bin(bins = 5, fun.y = "mean", size = 0.5, geom = "line", color = my3_colors[1]) +
  stat_summary_bin(bins = 5, fun.data = "mean_cl_normal", color = my3_colors[1], size = 0.5) +
  scale_x_continuous(breaks = c(2:5), limits = c(1.5,5)) +
  theme_stata() + theme(plot.background = element_rect(fill = "white"), axis.line=element_line(size=0.5),
                        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                        axis.text.x = element_blank(), axis.text.y = element_blank(),
                        axis.ticks.x = element_blank(), axis.ticks.y = element_blank(),
                        axis.title.x = element_blank(), axis.title.y = element_blank(),
                        strip.background = element_rect(fill = NA), title = element_text(size = 9),
                        plot.margin = margin(t = 0, r = 0, b = 0, l = 0, unit = "pt"),
                        legend.title = element_blank(), legend.text = element_text(size = 8)) +
  guides(linetype = "none", color = "none", shape = "none") + labs(title = "General Skills")

educ_fig_a <- educ_fig_a + inset_element(educ_inset, left = 0.05, bottom = 0.65 , right = 0.45, top = 0.995)


## Education Coefficients ------------------------------------------------------
## Preparing Data
reg_sum = summary(lm(data = edu_df_level, formula = Education.Avg ~ `Nested Specific`))
educ_reg_df <- data.frame(categories = "Nested Specific", control = "Unconditional" ,Education.Avg = reg_sum$coefficients[2,1], std_error = reg_sum$coefficients[2,2])
reg_sum = summary(lm(data = edu_df_level, formula = Education.Avg ~ `Un-nested Specific`))
educ_reg_df <- educ_reg_df %>% 
  bind_rows(data.frame(categories = "Un-nested Specific", control = "Unconditional" ,Education.Avg = reg_sum$coefficients[2,1], std_error = reg_sum$coefficients[2,2]))
reg_sum = summary(lm(data = edu_df_level, formula = Education.Avg ~ `Un-nested Specific` + General))
educ_reg_df <- educ_reg_df %>% 
  bind_rows(data.frame(categories = "Un-nested Specific", control = "Conditional" ,Education.Avg = reg_sum$coefficients[2,1], std_error = reg_sum$coefficients[2,2]))
reg_sum = summary(lm(data = edu_df_level, formula = Education.Avg ~ `Nested Specific` + General))
educ_reg_df <- educ_reg_df %>% 
  bind_rows(data.frame(categories = "Nested Specific", control = "Conditional" ,Education.Avg = reg_sum$coefficients[2,1], std_error = reg_sum$coefficients[2,2]))

educ_reg_df$categories <- factor(educ_reg_df$categories, levels = c("Nested Specific", "Un-nested Specific"))
educ_reg_df$control <- factor(educ_reg_df$control, levels = c("Unconditional", "Conditional"))

## Visualizing
educ_fig_b <- ggplot(educ_reg_df, aes(x = categories, y = Education.Avg, pattern = control)) +
  geom_bar_pattern(stat = "identity", position = position_dodge(0.9), pattern_angle = 45, pattern_fill = "white", color = my3_colors[3], width = 0.85,
                   pattern_density = 0.95, pattern_spacing = 0.025, pattern_key_scale_factor = 0.6, fill = my3_colors[3], alpha = 0.8) +
  geom_errorbar(aes(ymin=Education.Avg-std_error, ymax=Education.Avg+std_error), width=.5, color = "black", size = 0.7, position = position_dodge(0.9)) +
  scale_pattern_manual(values = c("Conditional" = "stripe", "Unconditional" = "none"), labels = rev(c("Conditional", "Unconditional"))) +
  scale_x_discrete(breaks = c("Nested Specific", "Un-nested Specific"), labels = c("Nested", "Un-nested")) +
  geom_hline(yintercept = 0, size = 0.3, color = "black") +
  theme_stata() + theme(axis.text = element_text(size = 14), title = element_text(size = 16),
                        axis.title = element_text(size = 14), axis.text.y = element_text(angle = 0),
                        axis.ticks.x = element_blank(), axis.title.x = element_blank(),
                        strip.text = element_text(size = 12), plot.background = element_blank(),
                        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                        legend.text = element_text(size = 12), panel.border = element_rect(color = 'black', fill = NA, size = 1),
                        strip.text.x = element_text(size = 14), strip.background = element_rect(fill = NA),
                        strip.text.y = element_text(size = 14), plot.margin = margin(2,0,0,2, unit = "pt"),
                        legend.box = "vertical", legend.direction = "vertical", legend.title = element_blank(),
                        legend.position = c(0.99, 1), legend.justification = c("right", "top"), 
                        legend.key.spacing.y = unit(.1,"cm"), legend.background = element_rect(color = NA, fill = NA)) +
  # guides(pattern = "none") +
  guides(pattern = guide_legend(byrow = TRUE, size = 4)) + labs(y = "Education Slope")


##---------------------------------------------------------------------------------------------
## 7.3. Outcome (Wage)
## Returns to Wage
wage_fig_a <- ggplot(wage_df_long, aes(x = skill_endow, y = a_mean, shape = categories)) +
  stat_summary_bin(aes(linetype = categories), position = position_dodge(0.9), bins = 5, fun.y = "mean", size = 0.5, geom = "line", color = my3_colors[3]) +
  scale_y_continuous(trans = "log10", labels = scales::label_number(scale_cut = scales::cut_si('$'))) +
  stat_summary_bin(bins = 5, position = position_dodge(0.9), color = my3_colors[3], geom = "point", size = 3) +
  stat_summary_bin(bins = 5, position = position_dodge(0.9), color = my3_colors[3], fun.data = "mean_cl_normal", geom = "errorbar") +
  scale_shape_manual(breaks = levels(edu_df_long$categories), values = c(19, 1), labels = c("Nested", "Un-nested")) +
  theme_stata() + theme(axis.title = element_text(size = 14), axis.text = element_text(size = 14),
                        axis.text.y = element_text(angle = 0), title = element_text(size = 16),
                        strip.text = element_text(size = 12), plot.background = element_blank(),
                        panel.grid.major = element_blank(), panel.grid.minor = element_blank(), legend.title = element_blank(),
                        legend.text = element_text(size = 12), panel.border = element_rect(color = 'black', fill = NA, size = 1),
                        strip.text.x = element_text(size = 14), strip.background = element_rect(fill = NA),
                        strip.text.y = element_text(size = 14), plot.margin = margin(2,0,10,2, unit = "pt")) +
  guides(size = 'none', linetype = "none", shape = guide_legend(override.aes=list(size = 4))) +
  labs(y = "Occupation Wages", x = "Occupation Skill Level")

wage_inset <- ggplot(wage_df_long %>% distinct(year, occ_code, General, a_mean), aes(x = General, y = a_mean)) +
  # facet_wrap(vars(categories)) +
  scale_y_continuous(trans = "log10", labels = scales::label_number(scale_cut = scales::cut_si('$'))) +
  scale_x_continuous(breaks = c(2:5), limits = c(1.5,5)) +
  stat_summary_bin(bins = 5, fun.y = "mean", size = 0.5, geom = "line", color = my3_colors[1]) +
  # stat_summary_bin(binwidth = .5, fun.data = "mean_se", fill = my3_colors[1], color = my3_colors[1], geom = "ribbon", alpha = 0.2, size = 0.1) +
  stat_summary_bin(bins = 5, fun.data = "mean_cl_normal", color = my3_colors[1], size = 0.5) +
  theme_stata() + theme(plot.background = element_rect(fill = "white", size = 1, color = "black"), axis.line=element_line(size=0.5),
                        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                        axis.text.x = element_blank(), axis.text.y = element_blank(),
                        axis.ticks.x = element_blank(), axis.ticks.y = element_blank(),
                        axis.title.x = element_blank(), axis.title.y = element_blank(),
                        strip.background = element_rect(fill = NA), title = element_text(size = 9),
                        plot.margin = margin(t = 0, r = 0, b = 0, l = 1, unit = "pt"),
                        legend.title = element_blank(), legend.text = element_text(size = 8)) +
  guides(linetype = "none", color = "none", shape = "none") + labs(title = "General Skills")

wage_fig_a <- wage_fig_a + inset_element(wage_inset, left = 0.05, bottom = 0.65 , right = 0.45, top = 0.995)


## Wage Coefficients -----------------------------------------------------------
## Preparing Data
reg_sum = summary(lm(data = wage_df_level, formula = log10(a_mean) ~ `Nested Specific`))
wage_reg_df <- data.frame(categories = "Nested Specific", control = "Unconditional" ,a_mean = reg_sum$coefficients[2,1], std_error = reg_sum$coefficients[2,2])
reg_sum = summary(lm(data = wage_df_level, formula = log10(a_mean) ~ `Un-nested Specific`))
wage_reg_df <- wage_reg_df %>% 
  bind_rows(data.frame(categories = "Un-nested Specific", control = "Unconditional" ,a_mean = reg_sum$coefficients[2,1], std_error = reg_sum$coefficients[2,2]))
reg_sum = summary(lm(data = wage_df_level, formula = log10(a_mean) ~ `Un-nested Specific` + General))
wage_reg_df <- wage_reg_df %>% 
  bind_rows(data.frame(categories = "Un-nested Specific", control = "Conditional" , a_mean = reg_sum$coefficients[2,1], std_error = reg_sum$coefficients[2,2]))
reg_sum = summary(lm(data = wage_df_level, formula = log10(a_mean) ~ `Nested Specific` + General))
wage_reg_df <- wage_reg_df %>% 
  bind_rows(data.frame(categories = "Nested Specific", control = "Conditional" , a_mean = reg_sum$coefficients[2,1], std_error = reg_sum$coefficients[2,2]))

wage_reg_df$categories <- factor(wage_reg_df$categories, levels = c("Nested Specific", "Un-nested Specific"))
wage_reg_df$control <- factor(wage_reg_df$control, levels = c("Unconditional", "Conditional"))

## Visualizing
wage_fig_b <- ggplot(wage_reg_df, aes(x = categories, y = a_mean, pattern = control)) +
  geom_bar_pattern(stat = "identity", position = position_dodge(0.9), pattern_angle = 45, pattern_fill = "white", color = my3_colors[3], width = 0.85,
                   pattern_density = 0.95, pattern_spacing = 0.025, pattern_key_scale_factor = 0.6, fill = my3_colors[3], alpha = 0.8) +
  geom_errorbar(aes(ymin=a_mean-std_error, ymax=a_mean+std_error), width=.5, color = "black", size = 0.7, position = position_dodge(0.9)) +
  scale_pattern_manual(values = c("Conditional" = "stripe", "Unconditional" = "none"), labels = rev(c("Conditional", "Unconditional"))) +
  scale_x_discrete(breaks = c("Nested Specific", "Un-nested Specific"), labels = c("Nested", "Un-nested")) +
  geom_hline(yintercept = 0, size = 0.3, color = "black") +
  theme_stata() + theme(axis.text = element_text(size = 14), title = element_text(size = 16),
                        axis.title = element_text(size = 14), axis.text.y = element_text(angle = 0),
                        axis.ticks.x = element_blank(), axis.title.x = element_blank(),
                        strip.text = element_text(size = 12), plot.background = element_blank(),
                        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                        legend.text = element_text(size = 12), panel.border = element_rect(color = 'black', fill = NA, size = 1),
                        strip.text.x = element_text(size = 14), strip.background = element_rect(fill = NA),
                        strip.text.y = element_text(size = 14), plot.margin = margin(2,0,0,2, unit = "pt"),
                        legend.box = "vertical", legend.direction = "vertical", legend.title = element_blank(),
                        legend.position = c(0.99, 1), legend.justification = c("right", "top"), 
                        legend.key.spacing.y = unit(.1,"cm"), legend.background = element_rect(color = NA, fill = NA)) +
  guides(pattern = guide_legend(byrow = TRUE, size = 4)) + labs(y = "Log(wage) Slope")


## Combining Figures -----------------------------------------------------------
((((wage_fig_a | educ_fig_a) + 
    plot_layout(guides = "collect") &
    # theme(legend.position = "bottom"))) &
      theme(legend.position = "bottom")))
  ) /
  ((((wage_fig_b | educ_fig_b) #+ 
     # plot_layout(guides = "collect") &
     # theme(legend.position = "bottom")
     ))) +
  plot_layout(heights = c(3, 2)) & 
  plot_annotation(tag_levels = list(c('a', '', 'b', '', 'c', 'd'))) &
  theme(plot.tag = element_text(size = 20, face = "bold"))



# 8. COMPARISON OF OLD AND NEW SKILLS (FIGURE 7)       ========================================
#==============================================================================================
## 8.2. Comparing Skill Clusters over Time
skills_occ_2005 <-
  read.table(paste0(basepath,"Dropbox/Research/Data/O*NET 9.0/","Skills.txt"),
             header = T, colClasses = rep("character",7), sep = "\t") %>%
  mutate(type = "skill") %>% #filter(`Recommend Suppress` == "N") %>% 
  rename(occ_8_dig = `O.NET.SOC.Code`, element_ID = `Element.ID`) %>% 
  select(occ_8_dig, element_ID, `Element.Name`, `Data.Value`, Scale.ID, N, type) %>%
  pivot_wider(id_cols = c("occ_8_dig","element_ID","type"), names_from = `Scale.ID`, values_from = "Data.Value") %>%
  rbind(read.table(paste0(basepath,"Dropbox/Research/Data/O*NET 9.0/","Abilities.txt"),
                   header = T, colClasses = rep("character",7), sep = "\t") %>% mutate(type = "ability") %>%
          rename(occ_8_dig = `O.NET.SOC.Code`, element_ID = `Element.ID`) %>% 
          select(occ_8_dig, element_ID, `Element.Name`, `Data.Value`, Scale.ID, N, type) %>%
          pivot_wider(id_cols = c("occ_8_dig","element_ID","type"), names_from = `Scale.ID`, values_from = "Data.Value")) %>%
  rbind(read.table(paste0(basepath,"Dropbox/Research/Data/O*NET 9.0/","Knowledge.txt"),
                   header = T, colClasses = rep("character",7), sep = "\t") %>% mutate(type = "knowledge") %>%
          rename(occ_8_dig = `O.NET.SOC.Code`, element_ID = `Element.ID`) %>% 
          select(occ_8_dig, element_ID, `Element.Name`, `Data.Value`, Scale.ID, N, type) %>%
          pivot_wider(id_cols = c("occ_8_dig","element_ID","type"), names_from = `Scale.ID`, values_from = "Data.Value")) %>%
  mutate(occ_code = substr(occ_8_dig, 1,7), year = 2005) %>% 
  mutate_at(vars(IM, LV), as.numeric) %>% rename(Importance = IM, Level = LV) %>%
  distinct() %>% as.data.frame()


### For panel b
## For multi-facets
temp <- skills_occ_raw %>% 
  bind_rows(skills_occ_2005) %>% select(-type) %>%
  inner_join(skill_clusters) %>% inner_join(skills_names %>% select(-type)) %>%
  ungroup() %>% distinct() %>%
  inner_join(data.frame(skill_Cluster = rep(levels(skill_clusters$skill_Cluster),2),
                        year = c(rep(2019,3),rep(2005,3))) %>% 
               group_by(skill_Cluster, year) %>% mutate(Cluster_year = paste0(skill_Cluster,year, collapse = "")),
             by = c("year", "skill_Cluster")) %>%
  left_join(data_frame(skill_Cluster = c("General","Intermediate","Specific"), 
                       Labels = c("A\t\tGeneral","B\t\tIntermediate","C\t\tSpecific"))) %>%
  filter(skill_Cluster != "Intermediate") %>%
  as.data.frame() 

temp$skill_Cluster <- factor(temp$skill_Cluster, levels = rev(levels(skill_clusters$skill_Cluster )))
temp$Cluster_year <- factor(temp$Cluster_year, levels = c("General2019", "Intermediate2019", "Specific2019", "General2005", "Intermediate2005", "Specific2005"))

temp_agg <- temp %>% group_by(skill_Cluster, year) %>% summarise(avg = mean(Level)) %>% ungroup() %>%
  pivot_wider(id_cols = "skill_Cluster", names_from = "year", values_from = "avg", names_prefix = "avg_") %>%
  group_by(skill_Cluster) %>% mutate(XMIN = min(avg_2005, avg_2019), XMAX = max(avg_2005, avg_2019))

b4 <- ggplot() +
  facet_wrap(vars(skill_Cluster), ncol = 2) +
  geom_errorbar(aes(y = 0.4, xmin = XMIN, xmax = XMAX), width = 0.1, size = 0.5, data = temp_agg) +
  geom_segment(aes(x=avg_2005, y = 0.4, xend=avg_2019, yend=0.4), arrow = arrow(length=unit(0.15, 'cm')), data = temp_agg, size = 0.5) +
  stat_density(aes(x = Level, y = ..density.., linetype = factor(year), alpha = factor(year), color = Cluster_year, fill = Cluster_year),
               data = temp, bw = 0.33, trim = T, position = "identity") +
  theme_stata() + theme(axis.title = element_text(size = 16), title = element_text(size = 18),
                        axis.text.y = element_text(angle = 0, size = 16), axis.text.x = element_text(size = 16), 
                        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                        strip.text = element_text(size = 16), plot.background = element_blank(),
                        panel.border = element_rect(color = 'black', fill = NA, size = 1), plot.margin = margin(0,0,0,0.5, unit = "pt"),
                        strip.text.x = element_text(size = 16), strip.background = element_rect(fill = NA),
                        plot.title = element_text(hjust = 0), legend.title = element_text(size = 12)) +
  scale_x_continuous(limits = c(0,7)) +
  scale_alpha_manual(breaks = c(2005, 2019), values = c(0.25, 0.3)) +
  scale_linetype_manual(breaks = c(2005, 2019), values = c("dashed","solid")) +
  scale_color_manual(breaks = levels(temp$Cluster_year), values = c(my3_colors,"red3", "gray70", "royalblue4")) +
  scale_fill_manual(breaks = levels(temp$Cluster_year), values = c(my3_colors,"red2", "gray70", "royalblue4")) +
  guides(color = "none", fill = "none", linetype=guide_legend(override.aes = list(colour = "black"))) +
  labs(x = "Skill Level", y = "Occupations Density", linetype = "Year", alpha = "Year")


### For panel c
## For multi-facets
temp <- skills_occ_raw %>% 
  bind_rows(skills_occ_2005) %>% select(-type) %>% 
  inner_join(skill_categories) %>%
  inner_join(skill_clusters) %>% filter(skill_Cluster == "Specific") %>%
  ungroup() %>% distinct() %>%
  inner_join(data.frame(skill_Cluster = rep(rev(levels(skill_clusters$skill_Cluster)),2),
                        year = c(rep(2019,3),rep(2005,3))) %>% 
               group_by(skill_Cluster, year) %>% mutate(Cluster_year = paste0(skill_Cluster,year, collapse = "")),
             by = c("year", "skill_Cluster")) %>%
  as.data.frame() 

temp$skill_Cluster <- factor(temp$skill_Cluster, levels = levels(skill_clusters$skill_Cluster ))
temp$Cluster_year <- factor(temp$Cluster_year, levels = c("Specific2019", "Specific2005"))

temp_agg <- temp %>% group_by(categories, year) %>% summarise(avg = mean(Level)) %>% ungroup() %>%
  pivot_wider(id_cols = "categories", names_from = "year", values_from = "avg", names_prefix = "avg_") %>%
  group_by(categories) %>% mutate(XMIN = min(avg_2005, avg_2019), XMAX = max(avg_2005, avg_2019))



c4 <- ggplot() +
  facet_wrap(vars(categories), ncol = 2, labeller = as_labeller(c("Nested Specific" = "Nested", "Un-nested Specific" = "Un-nested"))) +
  geom_errorbar(aes(y = 0.5, xmin = XMIN, xmax = XMAX), width = 0.1, size = 0.5, data = temp_agg) +
  geom_segment(aes(x=avg_2005, y = 0.5, xend=avg_2019, yend=0.5), arrow = arrow(length=unit(0.15, 'cm')), data = temp_agg, size = 0.5) +
  stat_density(aes(x = Level, y = ..density.., linetype = factor(year), alpha = factor(year), color = Cluster_year, fill = Cluster_year),
               data = temp, bw = 0.33, trim = T, position = "identity") +
  
  theme_stata() + theme(axis.title = element_text(size = 16), title = element_text(size = 18),
                        axis.text.y = element_text(angle = 0, size = 16), axis.text.x = element_text(size = 16), 
                        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                        strip.text = element_text(size = 16), plot.background = element_blank(),
                        panel.border = element_rect(color = 'black', fill = NA, size = 1), plot.margin = margin(0,0,0,0.5, unit = "pt"),
                        strip.text.x = element_text(size = 16), strip.background = element_rect(fill = NA),
                        plot.title = element_text(hjust = 0), legend.title = element_text(size = 12)) +
  scale_x_continuous(limits = c(0,7)) +
  scale_alpha_manual(breaks = c(2005, 2019), values = c(0.25, 0.3)) +
  scale_linetype_manual(breaks = c(2005, 2019), values = c("dashed","solid")) +
  scale_color_manual(breaks = levels(temp$Cluster_year), values = c(my3_colors[3], "royalblue4")) +
  scale_fill_manual(breaks = levels(temp$Cluster_year), values = c(my3_colors[3], "royalblue4")) +
  guides(color = "none", fill = "none", linetype=guide_legend(override.aes = list(colour = "black"))) +
  labs(x = "Skill Level", y = "Occupations Density", linetype = "Year", alpha = "Year")



## 7.3. Comparing 2005 and 2019 Nestedness
occ_skill_mat <- serrano_binary %>% select(element_ID, occ_8_dig) %>% mutate(value = 1) %>%
  pivot_wider(id_cols = "element_ID", names_from = "occ_8_dig", values_from = "value", values_fill = 0) %>% tibble::column_to_rownames("element_ID")
paste0("2019 C-score: ",as.numeric(vegan::nestedchecker(occ_skill_mat)$C.score))
# obs_skill_occ_N0 <- as.numeric(vegan::nestedn0(occ_skill_mat)$statistic)
paste0("2019 row NODF: ",as.numeric(vegan::nestednodf(occ_skill_mat)$statistic[2])) ## given that the simulation used bipartite::nested function, I use that instead of this
paste0("2019 Total NODF: ",as.numeric(vegan::nestednodf(occ_skill_mat)$statistic[3])) ## given that the simulation used bipartite::nested function, I use that instead of this
# obs_skill_occ_NODF <- as.numeric(bipartite::nested(occ_skill_mat, method = "NODF"))
paste0("2019 Temp: ",as.numeric(bipartite::nestedness(occ_skill_mat, null.models = F)$temperature))
paste0("2019 Nc: ",as.numeric(Nc(occ_skill_mat)$Nc))

serrano_binary_2005 <- read.csv("Binary Skill-Occupation Bipartite/Binary 2005 Skill-Occupation Bipartite - a la Serrano et al 2009 - alpha_in = 0.4, alpha_out = 0.275 - Dec 2022.csv")
occ_skill_mat_2005 <- serrano_binary_2005 %>% select(element_ID, occ_8_dig) %>% mutate(value = 1) %>%
  pivot_wider(id_cols = "element_ID", names_from = "occ_8_dig", values_from = "value", values_fill = 0) %>% tibble::column_to_rownames("element_ID")
paste0("2005 C-score: ",as.numeric(vegan::nestedchecker(occ_skill_mat_2005)$C.score))
# obs_skill_occ_N0 <- as.numeric(vegan::nestedn0(occ_skill_mat)$statistic)
paste0("2005 row NODF: ",as.numeric(vegan::nestednodf(occ_skill_mat_2005)$statistic[2])) ## given that the simulation used bipartite::nested function, I use that instead of this
paste0("2005 Total NODF: ",as.numeric(vegan::nestednodf(occ_skill_mat_2005)$statistic[3])) ## given that the simulation used bipartite::nested function, I use that instead of this
# obs_skill_occ_NODF <- as.numeric(bipartite::nested(occ_skill_mat, method = "NODF"))
paste0("2005 Temp: ",as.numeric(bipartite::nestedness(occ_skill_mat_2005, null.models = F)$temperature))
paste0("2005 Nc: ",as.numeric(Nc(occ_skill_mat_2005)$Nc))



# 9. DEMOGRAPHIC DISTRIBUTION OF SKILLS (FIGURE 6)      =======================================
#==============================================================================================
## 9.1. You'll need to load IPUMS CPS data
### 9.1.1 IPUMS data ------------------------------------------------------------------------
ipums_df <- read.csv(paste0(basepath, "cps.csv"), header = T) %>%
  mutate(gender = ifelse(SEX == 1, "Male", ifelse(SEX == 2,"Female", NA)))

### 9.1.2. CPS Crosswalks ------------------------------------------------------------------------
ipums_race_ref <- read_excel(paste0(basepath, "IPUMS Key.xlsx"), sheet = "Race")
ipums_hispan_ref <- read_excel(paste0(basepath, "IPUMS Key.xlsx"), sheet = "HISPAN")

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
age_dem_occ_indiv <- ipums_df %>% #filter(LABFORCE == 2) %>% 
  select(YEAR, AGE, gender, RACE, HISPAN, OCC2010, INCWAGE, EDUC, LABFORCE, WKSTAT,UHRSWORKT, UHRSWORK1) %>%
  left_join(ipums_race_ref %>% rename(RACE = Value, race = Label)) %>%
  # left_join(ipums_hispan_ref %>% rename(HISPAN = Value, Hispan = Label)) %>%
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

### 9.1.3. Standardizing Individual Data -----------------------------------------------------------
cohort_ind_df <- age_dem_occ_indiv %>% select(-WKSTAT, -UHRSWORK1) %>% 
  filter(year >= 1980, age > 17, age <56, INCWAGE >= 10000) %>%
  inner_join(data.frame(Race_Ethnicity = c("White","Hispanic or Latino", "Black","Asian only", "Asian or Pacific Islander"),
                        race_bracket = c("White","Hispanic/Latino", "Black","Asian", "Asian"))) %>% # racet brackets
  select(-race, -Hispanic, -Race_Ethnicity) %>% as.data.frame()
## ********** LOOK AT THE RESTRICTIONS SET ABOVE, YEAR, AGE, ADJUSTED ANNUAL SALARY, WORK STATUS


## 9.1.4. Combining Individual's Occ and skills - matched (bottleneck is occupation) individuals are 1,473,411, yielding 7,367,055 individual-skill subtypes
skill_occ_ind = skills_occ_raw %>% distinct(occ_code, occ_8_dig, element_ID, Level) %>%
  left_join(skill_categories) %>% left_join(skill_clusters) %>% 
  group_by(occ_code, skill_Cluster, categories) %>% summarise(avg.Level = mean(Level)) %>%
  left_join(skills_occ_raw %>% distinct(occ_code, occ_8_dig, element_ID, Level) %>%
              left_join(skill_categories) %>% left_join(skill_clusters) %>%
              group_by(occ_code, skill_Cluster, categories, element_ID) %>% summarise(Level = mean(Level)) %>% ungroup() %>% ## aggregating over occ_8_dig
              group_by(occ_code, skill_Cluster, categories) %>% slice_max(Level, n = 5, with_ties = F) %>% summarise(avg.top5.Level = mean(Level))) %>%
  inner_join(cohort_ind_df, by = "occ_code") %>%
  as.data.frame()


skill_occ_cohort = skill_occ_ind %>% ungroup() %>% mutate(birth_yr = year - age) %>%
  as.data.frame()

temp_gender_race_ind = skill_occ_cohort %>% #filter(skill_Cluster %in% c("General", "Specific")) %>% 
  rename(measure = categories) %>% mutate(measure = paste0(as.character(measure), " Skills")) %>%
  distinct(year, measure, avg.Level, gender, race_bracket, ID) %>% rename(value = avg.Level) %>%
  bind_rows(cohort_ind_df %>% select(year, gender, race_bracket, weekly_wage, EDUC, INCWAGE, ID) %>%
              rename("Education" = EDUC, "Weekly Wage" = weekly_wage, "Annual Wage" = INCWAGE) %>%
              pivot_longer(cols = c("Education", "Weekly Wage", "Annual Wage"), names_to = "measure", values_to = "value")) %>%
  mutate(race_bracket = gsub("Hispanic/Latino", "Hispanic or Latinx", race_bracket)) %>%
  ungroup() %>% mutate(year = as.numeric(year)) %>% drop_na(value) %>% filter(value > 0)
temp_gender_race_ind$measure <- factor(temp_gender_race_ind$measure,
                                       levels = c("General Skills", "Nested Specific Skills", "Un-nested Specific Skills", "Education", "Weekly Wage", "Annual Wage"))


## 9.2. Race Story ---------------------------------------------------------------------------------
race_df = temp_gender_race_ind %>%
  ungroup() %>% summarySE(groupvars = c("race_bracket", "measure"), measurevar = "value") %>%
  mutate(N = paste0("n=", round(N/10^6, 2), "M")) %>%
  as.data.frame()

## Bootstrapped data is generated in Demographics and Skill Analysis - Jun 2023.R
gg_race_bootstrap_df <- readRDS("Simulation Output Data/Racial Ratios Bootstrap - Skills, Education, and Wages - 1980-2022 - 10k iter 0.1 perc sample - Jul 6 2023.Rds") %>%
  group_by(race_bracket, measure) %>% summarise(y_min = quantile(ratio, 0.025), y_max = quantile(ratio, 0.975)) %>%
  as.data.frame()

gg_race_df <- race_df %>% filter(race_bracket != "White") %>% select(race_bracket, measure, value, N) %>%
  left_join(race_df %>% filter(race_bracket == "White") %>% select(measure, value) %>% 
              rename(baseline = value), by = c("measure")) %>%
  mutate(ratio = value / baseline) %>%
  left_join(gg_race_bootstrap_df) %>%
  filter(!measure %in% c("Nested Intermediate", "Nested Intermediate Skills", "Un-nested Intermediate", "Un-nested Intermediate Skills"))


gg_race_df <- gg_race_df %>% filter(measure != "Annual Wage") %>% 
  mutate(race_bracket = gsub("Hispanic or Latinx", "Hisp/Latinx", race_bracket)) %>%#,
         # breaks = sapply(measure, custom_rg_breaks))
  left_join(data.frame(measure = c("General Skills", "Nested Specific Skills", "Un-nested Specific Skills", "Education", "Weekly Wage","Annual Wage"),
                       y_lim_min = c(.8,.8,.5, 0.7, .6, .6), y_lim_max = c(1.05,1.1,1.2, 1.2,1.2, 1.2),
                       COLOR = c(my3_colors[1], my3_colors[3], my3_colors[3], "gray", "gray35", "gray25")))

gg_race_df$measure <- factor(gg_race_df$measure,
                             levels = c("General Skills", "Nested Specific Skills", "Un-nested Specific Skills", "Education", "Weekly Wage"))


d_temp <- gg_race_df %>% 
  split(.$measure) %>%
  lapply(function(temp_grp) {
    d_out <- ggplot(temp_grp, aes(x = race_bracket, y = ratio, fill = measure, alpha = race_bracket)) +
      facet_wrap_paginate(vars(measure), ncol = 5, scales = "free_y",
                          labeller = as_labeller(c("General Skills" = "General", "Nested Intermediate Skills" = "Nested", "Nested Specific Skills" = "Nested",
                                                   "Un-nested Intermediate Skills" = "Un-nested", "Un-nested Specific Skills" = "Un-nested",
                                                   "Education" = "Education", "Weekly Wage" = "Weekly Wage"))) +
      geom_bar(stat = "identity", position = position_dodge(0.9)) +
      geom_hline(yintercept = 1, size = 0.3, color = "black") +
      scale_alpha_manual(values = c(1, .8, .6), breaks = c("Asian", "Black", "Hisp/Latinx")) +
      geom_errorbar(aes(ymin = y_min, ymax = y_max), width = 0.5, color = "black", size = 0.7, position = position_dodge(0.9), alpha = 1) +
      scale_fill_manual(breaks = as.character(unique(temp_grp$measure)), values = as.character(unique(temp_grp$COLOR))) +
      scale_color_manual(breaks = as.character(unique(temp_grp$measure)), values = as.character(unique(temp_grp$COLOR))) +
      scale_x_discrete(labels = scales::label_wrap(15)) +
      coord_cartesian(ylim = c(unique(temp_grp$y_lim_min), unique(temp_grp$y_lim_max))) + ## Setting limit in the scale_y... will change the underlying data
      scale_y_continuous(trans = "log10", breaks = seq(unique(temp_grp$y_lim_min), unique(temp_grp$y_lim_max), 0.1)) +
      theme_stata() + theme(axis.text = element_text(size = 14), title = element_text(size = 14),
                            axis.title = element_text(size = 14), axis.text.y = element_text(angle = 0),
                            axis.text.x = element_text(angle = 45, hjust = 1, size = 12), axis.ticks.x = element_blank(),
                            strip.text = element_text(size = 12), plot.background = element_blank(),
                            panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                            legend.title = element_blank(), axis.title.x = element_blank(),  
                            legend.text = element_text(size = 12), panel.border = element_rect(color = NA, fill = NA),
                            strip.text.x = element_text(size = 14), strip.background = element_rect(fill = NA),
                            strip.text.y = element_text(size = 14), plot.margin = margin(0,0,0,5, unit = "pt")) +
      guides(fill = "none", color = "none", pattern = "none", alpha = "none")
      
    if(unique(temp_grp$measure) != "General Skills") {
      d_out <- d_out + theme(axis.title.y = element_blank())
    }
    d_out <- d_out + labs(y = "Race/Ethnicity Gaps")
  })

race_fig <- (d_temp %>% wrap_plots(ncol = 5)) #& #+ plot_layout(guides = "collect") &

  

## 9.3. Gender Story -------------------------------------------------------------------------------
gender_df = temp_gender_race_ind %>%
  ungroup() %>% summarySE(groupvars = c("race_bracket", "gender", "measure"), measurevar = "value") %>%
  mutate(N = paste0("n=", round(N/10^6, 2), "M")) %>%
  as.data.frame()

## Bootstrapped data is generated in Demographics and Skill Analysis - Jun 2023.R
gg_gender_bootstrap_df <- readRDS("Simulation Output Data/Gender Ratios Bootstrap - Skills, Education, and Wages - 1980-2022 - 10k iter 0.1 perc sample - Jul 6 2023.Rds") %>%
  group_by(race_bracket, measure) %>% summarise(y_min = quantile(ratio, 0.025), y_max = quantile(ratio, 0.975))

gg_gender_df <- gender_df %>% filter(gender != "Male") %>% select(race_bracket, measure, value, N) %>%
  left_join(gender_df %>% filter(gender == "Male") %>% select(race_bracket, measure, value) %>% 
              rename(baseline = value), by = c("measure", "race_bracket")) %>%
  mutate(ratio = value / baseline) %>%
  left_join(gg_gender_bootstrap_df) %>%
  filter(!measure %in% c("Nested Intermediate", "Nested Intermediate Skills", "Un-nested Intermediate", "Un-nested Intermediate Skills"))

gg_gender_df <- gg_gender_df %>% filter(measure != "Annual Wage") %>% 
  mutate(race_bracket = gsub("Hispanic or Latinx", "Hisp/Latinx", race_bracket)) %>%#,
  # breaks = sapply(measure, custom_rg_breaks))
  left_join(data.frame(measure = c("General Skills", "Nested Specific Skills", "Un-nested Specific Skills", "Education", "Weekly Wage","Annual Wage"),
                       y_lim_min = c(.8,.8,.5, 0.7, .6, .6), y_lim_max = c(1.05,1.1,1.2, 1.2,1.2, 1.2),
                       COLOR = c(my3_colors[1], my3_colors[3], my3_colors[3], "gray", "gray35", "gray25")))

gg_gender_df$measure <- factor(gg_gender_df$measure,
                             levels = c("General Skills", "Nested Specific Skills", "Un-nested Specific Skills", "Education", "Weekly Wage"))


d_temp <- gg_gender_df %>% 
  split(.$measure) %>%
  lapply(function(temp_grp) {
    d_out <- ggplot(temp_grp, aes(x = race_bracket, y = ratio, fill = measure, alpha = race_bracket)) +
      facet_wrap_paginate(vars(measure), ncol = 5, scales = "free_y",
                          labeller = as_labeller(c("General Skills" = "General", "Nested Intermediate Skills" = "Nested", "Nested Specific Skills" = "Nested",
                                                   "Un-nested Intermediate Skills" = "Un-nested", "Un-nested Specific Skills" = "Un-nested",
                                                   "Education" = "Education", "Weekly Wage" = "Weekly Wage"))) +
      geom_bar(stat = "identity", position = position_dodge(0.9)) +
      geom_hline(yintercept = 1, size = 0.3, color = "black") +
      scale_alpha_manual(values = c(1, .8, .6, .4), breaks = c("Asian", "Black", "Hisp/Latinx", "White")) +
      geom_errorbar(aes(ymin = y_min, ymax = y_max), width = 0.5, color = "black", size = 0.7, position = position_dodge(0.9), alpha = 1) +
      scale_fill_manual(breaks = as.character(unique(temp_grp$measure)), values = as.character(unique(temp_grp$COLOR))) +
      scale_color_manual(breaks = as.character(unique(temp_grp$measure)), values = as.character(unique(temp_grp$COLOR))) +
      scale_x_discrete(labels = scales::label_wrap(15)) +
      coord_cartesian(ylim = c(unique(temp_grp$y_lim_min), unique(temp_grp$y_lim_max))) + ## Setting limit in the scale_y... will change the underlying data
      scale_y_continuous(trans = "log10", breaks = seq(unique(temp_grp$y_lim_min), unique(temp_grp$y_lim_max), 0.1)) +
      theme_stata() + theme(axis.text = element_text(size = 14), title = element_text(size = 14),
                            axis.title = element_text(size = 14), axis.text.y = element_text(angle = 0),
                            axis.text.x = element_text(angle = 45, hjust = 1, size = 12), axis.ticks.x = element_blank(),
                            strip.text = element_text(size = 12), plot.background = element_blank(),
                            panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                            legend.title = element_blank(), axis.title.x = element_blank(),  
                            legend.text = element_text(size = 12), panel.border = element_rect(color = NA, fill = NA),
                            strip.text.x = element_text(size = 14), strip.background = element_rect(fill = NA),
                            strip.text.y = element_text(size = 14), plot.margin = margin(0,0,0,5, unit = "pt")) +
      guides(fill = "none", color = "none", pattern = "none", alpha = "none")
    
    if(unique(temp_grp$measure) != "General Skills") {
      d_out <- d_out + theme(axis.title.y = element_blank())
    }
    d_out <- d_out + labs(y = "Gender Gaps")
  })

gender_fig <- (d_temp %>% wrap_plots(ncol = 5)) #& #+ plot_layout(guides = "collect") &



((race_fig / gender_fig) +
    plot_layout(heights = c(1,1), guides = "collect") &
    plot_annotation(tag_levels = "a") &
    theme(plot.tag = element_text(size = 14, face = "bold"), legend.position = "bottom"))


