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
load(url("https://github.com/educacion-meritocracia/edumer-data/raw/main/output/data/edumer_students_long.RData"))

db1 <- db_students %>% as_tibble()
db_long <- edumer_students_long

rm(db_students, edumer_students_long)

glimpse(db1)
glimpse(db_long)

# 3. Processing -----------------------------------------------------------

# select ----

db1 <- db1 %>% 
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

db_long <- db_long %>% 
  select(id_estudiante,
         ola,
         consent = consentimiento,
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

db1 <- db1 %>% filter(consent == 1) %>% select(-consent)
db_long <- db_long %>% filter(consent == 1) %>% select(-consent)

# recode and transform ----

frq(db1$curse_level)
frq(db1$perc_effort)
frq(db1$perc_talent)
frq(db1$perc_rich_parents)
frq(db1$perc_contact)
frq(db1$pref_effort)
frq(db1$pref_talent)
frq(db1$pref_rich_parents)
frq(db1$pref_contact)
frq(db1$just_educ)
frq(db1$just_health)
frq(db1$just_pension)

db1$curse_level <- car::recode(db1$curse_level, 
                              recodes = c("1:2 = 'Básica'; 3:4 = 'Media'"),
                              as.factor = T,
                              levels = c("Básica", "Media"))

  
db1 <- db1 %>% 
  mutate(
    across(
      .cols = -c(curse_level),
      .fns = ~ set_na(., na = c(88,99))
    )
  )

db1$mjp <- rowMeans(x = db1[10:12], na.rm = T)

db1$mjp <- if_else(is.nan(db1$mjp), NA, db1$mjp)

frq(db_long$curse_level)
frq(db_long$perc_effort)
frq(db_long$perc_talent)
frq(db_long$perc_rich_parents)
frq(db_long$perc_contact)
frq(db_long$pref_effort)
frq(db_long$pref_talent)
frq(db_long$pref_rich_parents)
frq(db_long$pref_contact)
frq(db_long$just_educ)
frq(db_long$just_health)
frq(db_long$just_pension)


db_long$curse_level <- car::recode(db_long$curse_level, 
                               recodes = c("1:2 = 'Básica'; 3:4 = 'Media'"),
                               as.factor = T,
                               levels = c("Básica", "Media"))


db_long <- db_long %>% 
  mutate(
    across(
      .cols = -c(id_estudiante,ola,curse_level),
      .fns = ~ set_na(., na = c(88,99))
    )
  )


# missings ----

colSums(is.na(db1))

na.omit(db1) 

db1 <- naniar::add_n_miss(db1)

any_na(db1)

n_miss(db1)

prop_miss(db1[c(1:13)])

naniar::gg_miss_var(db1)

miss_var_summary(db1)

miss_var_table(db1)

miss_case_summary(db1)

miss_case_table(db1)

vis_miss(db1) + theme(axis.text.x = element_text(angle=80))

db1 <- na.omit(db1)

colSums(is.na(db_long))

na.omit(db_long) 

db_long <- naniar::add_n_miss(db_long)

any_na(db_long)

n_miss(db_long)

prop_miss(db_long[c(4:14)])

naniar::gg_miss_var(db_long)

miss_var_summary(db_long)

miss_var_table(db_long)

miss_case_summary(db_long)

miss_case_table(db_long)

vis_miss(db_long) + theme(axis.text.x = element_text(angle=80))

db_long <- na.omit(db_long)

# 4. Save -----------------------------------------------------------------

db1 <- db1 %>% select(-n_miss_all)
db_long <- db_long %>% select(-n_miss_all)

sapply(db1, class)
save(db1, file = here("output/data/db1_proc.RData"))

sapply(db_long, class)
save(db_long, file = here("output/data/db_long_proc.RData"))
