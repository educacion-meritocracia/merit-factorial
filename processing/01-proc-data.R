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

load(url("https://github.com/educacion-meritocracia/edumer-data/raw/main/output/data/edumer_students_long.RData"))

db_long <- edumer_students_long

rm(edumer_students_long)

glimpse(db_long)

# 3. Processing -----------------------------------------------------------

# select ----

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
         pref_contact = p1_8)



db_long <- db_long %>% filter(consent == 1) %>% select(-consent)

# recode and transform ----

# level
frq(db_long$curse_level)

db_long <- db_long %>% 
  mutate(
    cohort_level = if_else(curse_level %in% c("6to", "7mo"), "Primary", "Secondary"),
    cohort_level = factor(cohort_level, 
                          labels = c("Primary", "Secondary")),
    curse_level = factor(curse_level, 
                         levels = c("6to", "7mo", "1ro", "2do"))
  )

# merit-scale society
frq(db_long$perc_effort)
frq(db_long$perc_talent)
frq(db_long$perc_rich_parents)
frq(db_long$perc_contact)
frq(db_long$pref_effort)
frq(db_long$pref_talent)
frq(db_long$pref_rich_parents)
frq(db_long$pref_contact)



labels1 <- c("Muy en desacuerdo" = 1, 
             "En desacuerdo" = 2, 
             "De acuerdo" = 3, 
             "Muy de acuerdo" = 4, 
             "No sabe" = 88, 
             "No responde" = 99)

db_long <- db_long %>% 
  mutate_at(.vars = (4:11),.funs = ~ sjlabelled::set_labels(., labels = labels1))


db_long <- db_long %>% 
  mutate(
    across(
      .cols = -c(id_estudiante,ola,curse_level, cohort_level),
      .fns = ~ set_na(., na = c(88,99))
    )
  )


# missings ----

colSums(is.na(db_long))

na.omit(db_long) 

db_long <- naniar::add_n_miss(db_long)

any_na(db_long)

n_miss(db_long)

prop_miss(db_long[c(4:11)])

naniar::gg_miss_var(db_long)

miss_var_summary(db_long)

miss_var_table(db_long)

miss_case_summary(db_long)

miss_case_table(db_long)

vis_miss(db_long) + theme(axis.text.x = element_text(angle=80))

#db_long <- na.omit(db_long)

# 4. Save -----------------------------------------------------------------

db_long <- db_long %>% select(-n_miss_all)

sapply(db_long, class)
save(db_long, file = here("output/data/db_long_proc.RData"))
