#*******************************************************************************************************************
#
# 0. Identification ---------------------------------------------------
# Title: Data preparation for research paper
# Author: Andreas Laffert            
# Overview: Preparation of the EDUMER Students Data Wave 1        
# Date: 13-066-2024            
#
#******************************************************************************************************************

# 1. Packages ---------------------------------------------------------
if (!require("pacman")) install.packages("pacman")

pacman::p_load(tidyverse,
               sjlabelled, 
               sjmisc, 
               sjPlot,
               here,
               naniar)

options(scipen=999)
rm(list = ls())

# 2. Data --------------------------------------------------------------

load(url("https://github.com/educacion-meritocracia/edumer-data/raw/main/output/data/db_proc_students.RData"))

db <- db_students %>% as_tibble()

names(db)
glimpse(db)

# 3. Processing -----------------------------------------------------------

# select ----

db <- db %>% 
  select(consent = consentimiento,
         curse_level = nivel_estudiante,
         perc_effort = p1_1,
         perc_talent = p1_2,
         perc_rich_parents = p1_3,
         perc_contact = p1_4,
         pref_effort = p1_5,
         pref_talent = p1_6,
         pref_rich_parents = p1_7,
         pref_contact = p1_8,
         just_educ = p9_3,
         just_health = p9_4,
         just_pension = p9_5)

# filter ----

db <- db %>% filter(consent == 1) %>% select(-consent)

# recode and transform ----

frq(db$curse_level)
frq(db$perc_effort)
frq(db$perc_talent)
frq(db$perc_rich_parents)
frq(db$perc_contact)
frq(db$pref_effort)
frq(db$pref_talent)
frq(db$pref_rich_parents)
frq(db$pref_contact)
frq(db$just_educ)
frq(db$just_health)
frq(db$just_pension)

db$curse_level <- car::recode(db$curse_level, 
                              recodes = c("1:2 = 'Básica'; 3:4 = 'Media'"),
                              as.factor = T,
                              levels = c("Básica", "Media"))
  
  
db <- db %>% 
  mutate(
    across(
      .cols = -c(curse_level),
      .fns = ~ set_na(., na = c(88,99))
    )
  )

db <- db %>% 
  mutate(
    across(
      .cols = -c(curse_level),
      .fns = ~ sjmisc::rec(., rec = "rev")
    )
  )

db$mjp <- rowMeans(x = db[10:12], na.rm = T)

db$mjp <- if_else(is.nan(db$mjp), NA, db$mjp)

# missings ----

colSums(is.na(db))

na.omit(db) 

db <- naniar::add_n_miss(db)

any_na(db)

n_miss(db)

prop_miss(db[c(1:13)])

naniar::gg_miss_var(db)

miss_var_summary(db)

miss_var_table(db)

miss_case_summary(db)

miss_case_table(db)

vis_miss(db) + theme(axis.text.x = element_text(angle=80))

db <- na.omit(db)

# 4. Save -----------------------------------------------------------------

sapply(db, class)
save(db, file = here("output/data/db_proc.RData"))
