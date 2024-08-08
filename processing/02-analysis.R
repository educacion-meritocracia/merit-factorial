#*******************************************************************************************************************
#
# 0. Identification ---------------------------------------------------
# Title: Data analysis for research paper
# Author: Andreas Laffert            
# Overview: Analysis of the EDUMER Students Data Wave 1        
# Date: 13-066-2024            
#
#******************************************************************************************************************

# 1. Packages ---------------------------------------------------------
if (!require("pacman")) install.packages("pacman")

pacman::p_load(tidyverse,
               sjmisc, 
               sjPlot,
               here,
               lavaan,
               psych,
               corrplot,
               semPlot,
               vtable,
               ggdist,
               patchwork)

options(scipen=999)
rm(list = ls())

# 2. Data --------------------------------------------------------------

load(file = here("output", "data", "db_proc.RData"))

names(db)
glimpse(db)

# 3. Analysis -------------------------------------------------------------

db <- db %>% select(-n_miss_all)

theme_set(theme_ggdist())
# Descriptive ----

sjmisc::descr(db, show = "all")

db %>% 
  psych::describe() %>% 
  as.data.frame() %>% 
  dplyr::select("Mean"=mean,"SD"=sd,"Min"=min,"Max"=max) %>% 
  round(.,2)


db %>% 
  mutate_all(~ as.numeric(.)) %>% 
  vtable::st()

a <- db %>% 
  select(starts_with("perc")) %>% 
  mutate_all(~ sjmisc::rec(., rec = "rev")) %>% 
  sjPlot::plot_likert(geom.colors = "RdBu",
                      title = c("a. Perceptions"),
                      geom.size = 0.8,
                      axis.labels = c("Effort", "Talent", "Rich Parents", "Contacts"),
                      catcount = 4,
                      values  =  "sum.outside",
                      reverse.colors = F,
                      reverse.scale = T,
                      show.n = FALSE
                      ) +
  ggplot2::theme(legend.position = "none")

b <- db %>% 
  select(starts_with("pref")) %>% 
  mutate_all(~ sjmisc::rec(., rec = "rev")) %>% 
  sjPlot::plot_likert(geom.colors = "RdBu",
                      title = c("b. Preferences"),
                      geom.size = 0.8,
                      axis.labels = c("Effort", "Talent", "Rich Parents", "Contacts"),
                      catcount = 4,
                      values  =  "sum.outside",
                      reverse.colors = F,
                      reverse.scale = T,
                      show.n = FALSE
  ) +
  ggplot2::theme(legend.position = "bottom")

likerplot <- a / b + plot_annotation(caption = paste0("Source: Authors calculations based on EDUMER data"," (n = ",dim(db)[1],")"
))

# Bivariate ----

M <- psych::polychoric(db)

diag(M$rho) <- NA

rownames(M$rho) <- c("A. Perception Effort",
                     "B. Perception Talent",
                     "C. Perception Rich Parents",
                     "D. Perception Contacts",
                     "E. Preferences Effort",
                     "F. Preferences Talent",
                     "G. Preferences Rich Parents",
                     "H. Preferences Contacts")

#set Column names of the matrix
colnames(M$rho) <-c("(A)", "(B)","(C)","(D)","(E)","(F)","(G)",
                       "(H)")

testp <- cor.mtest(M$rho, conf.level = 0.95)

#Plot the matrix using corrplot
corrplot::corrplot(M$rho,
                   method = "color",
                   addCoef.col = "black",
                   type = "upper",
                   tl.col = "black",
                   col = colorRampPalette(c("#E16462", "white", "#0D0887"))(12),
                   bg = "white",
                   na.label = "-")

# Measurement model ----

names(db)

# model
model_cfa <- '
  perc_merit = ~ perc_effort + perc_talent
  perc_nmerit = ~ perc_rich_parents + perc_contact
  pref_merit = ~ pref_effort + pref_talent
  pref_nmerit = ~ pref_rich_parents + pref_contact
  '

# estimation for each order set

m1_cfa <- cfa(model = model_cfa, 
              data = db,
              estimator = "MLR", 
              std.lv = F) # Continuous/ estimator ML Robust

m2_cfa <- cfa(model = model_cfa, 
              data = db, 
              estimator = "DWLS",
              ordered = T,
              std.lv = F)

summary(m1_cfa, fit.measures = T, standardized = T, rsquare = T, modindices = T) 

summary(m2_cfa, fit.measures = T, standardized = T, rsquare = T, modindices = T) 


cnames <- c("Factor","Indicator","Loading (MLR)","Loading (DWLS)")
kable(left_join(x = standardizedsolution(m1_cfa) %>% 
                  filter(op=="=~") %>% 
                  select(lhs,rhs,est.std),y = standardizedsolution(m2_cfa) %>% 
                  filter(op=="=~") %>%
                  select(lhs,rhs,est.std),c("lhs","rhs")),
      format = "markdown",digits = 2,col.names = cnames, caption = "Factor loadings")

# Invariance 



