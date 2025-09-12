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
               ggdist,
               patchwork,
               semTable,
               semTools,
               gtools,
               kableExtra)

options(scipen=999)
rm(list = ls())

# 2. Data --------------------------------------------------------------

load(file = here("output", "data", "db_proc.RData"))

names(db)
glimpse(db)
theme_set(theme_ggdist())

# 3. Analysis -------------------------------------------------------------

db <- db %>% select(-n_miss_all)

# Descriptive ----

sjmisc::descr(db, show = "all")

db %>% 
  psych::describe() %>% 
  as.data.frame() %>% 
  dplyr::select("Mean"=mean,"SD"=sd,"Min"=min,"Max"=max) %>% 
  round(.,2)

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

M <- psych::polychoric(db[c(2:9,13)])

P <- cor(db[c(2:9,13)], method = "spearman")

diag(M$rho) <- NA

diag(P) <- NA

M$rho[9,] <- P[9,]

rownames(M$rho) <- c("A. Perception Effort",
                     "B. Perception Talent",
                     "C. Perception Rich Parents",
                     "D. Perception Contacts",
                     "E. Preferences Effort",
                     "F. Preferences Talent",
                     "G. Preferences Rich Parents",
                     "H. Preferences Contacts",
                     "I. Market Justice Preferences")

#set Column names of the matrix
colnames(M$rho) <-c("(A)", "(B)","(C)","(D)","(E)","(F)","(G)",
                       "(H)","(I)")

rownames(P) <- c("A. Perception Effort",
                     "B. Perception Talent",
                     "C. Perception Rich Parents",
                     "D. Perception Contacts",
                     "E. Preferences Effort",
                     "F. Preferences Talent",
                     "G. Preferences Rich Parents",
                     "H. Preferences Contacts",
                     "I. Market Justice Preferences")

#set Column names of the matrix
colnames(P) <-c("(A)", "(B)","(C)","(D)","(E)","(F)","(G)",
                    "(H)","(I)")

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


# Market justice index alpha
matriz <- db %>% select(just_health,just_pension, just_educ)
matriz_poly <- polychoric(matriz) 
psych::alpha(matriz_poly$rho) # coef = 0.83

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

modin <- modificationIndices(m2_cfa)

modin %>% 
  filter(mi > 3.84)


## Modelos por separado para basica y media

fit.conf <- cfa(model = model_cfa, 
                data = db, 
                group = "curse_level",
                estimator = "DWLS",
                ordered = T,
                std.lv = F)

summary(fit.conf, fit.measures = T, standardized = T, rsquare = T, modindices = T) 


mbasica_cfa <- cfa(model = model_cfa, 
                   data = subset(db, curse_level == "Básica"), 
                   estimator = "DWLS",
                   ordered = T,
                   std.lv = F)

mmedia_cfa <- cfa(model = model_cfa, 
                   data = subset(db, curse_level == "Media"), 
                   estimator = "DWLS",
                   ordered = T,
                   std.lv = F)

summary(mbasica_cfa, fit.measures = T, standardized = T, rsquare = T, modindices = T) 
summary(mmedia_cfa, fit.measures = T, standardized = T, rsquare = T, modindices = T) 


# SEM

## Especificar el modelo: medición y estructural
m_sem1 <- '
# Modelo medición
perc_merit = ~ perc_effort + perc_talent
perc_nmerit = ~ perc_rich_parents + perc_contact
pref_merit = ~ pref_effort + pref_talent
pref_nmerit = ~ pref_rich_parents + pref_contact

  # Modelo estructural
mjp ~  perc_merit + perc_nmerit + pref_merit + pref_nmerit
'

## Ajustar modelo
f_sem1 <- sem(m_sem1, data = db)

## Ver resultados completos
summary(f_sem1, fit.measures = T, standardized = T, 
        rsquare = T, modindices = T)

## Exportar tablas
### Ajustar versión estandarizada

semTable(f_sem1, type = "html", columns = c ("est" , "estsestars"),
         paramSets = "all",
         file = here("processing/tabla_sem1"))

## Especificar el modelo: medición y estructural
m_sem2 <- '
# Modelo medición
perc_merit = ~ perc_effort + perc_talent
perc_nmerit = ~ perc_rich_parents + perc_contact
pref_merit = ~ pref_effort + pref_talent
pref_nmerit = ~ pref_rich_parents + pref_contact

  # Modelo estructural
just_health ~  perc_merit + perc_nmerit + pref_merit + pref_nmerit
'

## Ajustar modelo
f_sem2 <- sem(m_sem2, data = db)

## Ver resultados completos
summary(f_sem2, fit.measures = T, standardized = T, 
        rsquare = T, modindices = T)

## Exportar tablas
### Ajustar versión estandarizada

semTable(f_sem2, type = "html", columns = c ("est" , "estsestars"),
         paramSets = "all",
         file = here("processing/tabla_sem2"))

## Especificar el modelo: medición y estructural
m_sem3 <- '
# Modelo medición
perc_merit = ~ perc_effort + perc_talent
perc_nmerit = ~ perc_rich_parents + perc_contact
pref_merit = ~ pref_effort + pref_talent
pref_nmerit = ~ pref_rich_parents + pref_contact

  # Modelo estructural
just_pension ~  perc_merit + perc_nmerit + pref_merit + pref_nmerit
'

## Ajustar modelo
f_sem3 <- sem(m_sem3, data = db)

## Ver resultados completos
summary(f_sem3, fit.measures = T, standardized = T, 
        rsquare = T, modindices = T)

## Exportar tablas
### Ajustar versión estandarizada

semTable(f_sem3, type = "html", columns = c ("est" , "estsestars"),
         paramSets = "all",
         file = here("processing/tabla_sem3"))

## Especificar el modelo: medición y estructural
m_sem4 <- '
# Modelo medición
perc_merit = ~ perc_effort + perc_talent
perc_nmerit = ~ perc_rich_parents + perc_contact
pref_merit = ~ pref_effort + pref_talent
pref_nmerit = ~ pref_rich_parents + pref_contact

  # Modelo estructural
just_educ ~  perc_merit + perc_nmerit + pref_merit + pref_nmerit
'

## Ajustar modelo
f_sem4 <- sem(m_sem4, data = db)

## Ver resultados completos
summary(f_sem4, fit.measures = T, standardized = T, 
        rsquare = T, modindices = T)

## Exportar tablas
### Ajustar versión estandarizada

semTable(f_sem4, type = "html", columns = c ("est" , "estsestars"),
         paramSets = "all",
         file = here("processing/tabla_sem4"))

## Invarianza

inv01 <- semTools::measurementInvariance(model = model_cfa,
                                         data = db,
                                         group = "curse_level",
                                         estimator = "ML",
                                         strict = T,
                                         quiet = T)

conf <- inv01$fit.configural
weak <- inv01$fit.loadings
strong <- inv01$fit.intercepts
strict <- inv01$fit.residuals


tab01 <- lavaan::anova(conf,weak,strong,strict,SB.classic=TRUE) %>%
  dplyr::as_tibble() %>%
  dplyr::select("Chisq","Df","chisq_diff"=`Chisq diff`,"df_diff"=`Df diff`,"pvalue"=`Pr(>Chisq)`) %>%
  dplyr::mutate(stars=gtools::stars.pval(pvalue),
                chisqt=paste0(round(Chisq,2)," (",Df,") "),
                decision=ifelse(pvalue>0.05,yes = "Accept",no = "Reject"),
                model=c("Configural","Weak","Strong","Strict"))


fit.meas <- dplyr::bind_rows(lavaan::fitmeasures(inv01$fit.configural,output ="matrix")[c("chisq","df","cfi","rmsea","rmsea.ci.lower","rmsea.ci.upper"),],
                            lavaan::fitmeasures(inv01$fit.loadings,  output ="matrix")[c("chisq","df","cfi","rmsea","rmsea.ci.lower","rmsea.ci.upper"),],
                            lavaan::fitmeasures(inv01$fit.intercepts,output ="matrix")[c("chisq","df","cfi","rmsea","rmsea.ci.lower","rmsea.ci.upper"),],
                            lavaan::fitmeasures(inv01$fit.residuals, output ="matrix")[c("chisq","df","cfi","rmsea","rmsea.ci.lower","rmsea.ci.upper"),])

# compute differences in chisq, df, cfi and rmsea (90%, lower.ci - upper.ci )
fit.meas <- fit.meas %>%
  dplyr::mutate(diff.chi2 = chisq    - lag(chisq,default = dplyr::first(chisq)),
                diff.df   = df       - lag(df,   default = dplyr::first(df)),
                diff.cfi  = cfi      - lag(cfi,  default = dplyr::first(cfi)),
                diff.rmsea   = rmsea - lag(rmsea,default = dplyr::first(rmsea))) %>%
  round(3) %>%
  dplyr::mutate(rmsea.ci=paste0(rmsea," \n ", "(",rmsea.ci.lower,"-",rmsea.ci.upper,")"))

tab.inv <- dplyr::bind_cols(tab01,fit.meas) %>%
  dplyr::select(model,chisqt,cfi,rmsea.ci,diff.chi2,diff.df,diff.cfi,diff.rmsea,stars,decision) %>%
  dplyr::mutate(diff.chi2=paste0(diff.chi2," (",diff.df,") ",stars)) %>%
  dplyr::select(model,chisqt,cfi,rmsea.ci,diff.chi2,diff.cfi,diff.rmsea,decision)

#clean values
tab.inv[tab.inv == c("0 (0) ")] <- NA
tab.inv[tab.inv == c(0)] <- NA

col.nam <- c("Model","$\\chi^2 (\\text{df})$","CFI","RMSEA (90 CI)",
             "$\\Delta \\chi^2 (\\Delta \\text{df}$)","$\\Delta \\text{CFI}$","$\\Delta \\text{RMSEA}$","Decision")
footnote <- paste0("N = 839; Group 1, n = ",conf@Data@nobs[[1]],"; Group 2, n = ",conf@Data@nobs[[2]])

knitr::kable(tab.inv, col.names = col.nam, align = "l",
             booktabs=TRUE,format = "html",escape = FALSE,
             caption = "t") %>%
  kableExtra::kable_styling(full_width = FALSE,
                            latex_options = "HOLD_position",
                            bootstrap_options=c("striped", "bordered"),
                            font_size = 10) %>%
  kableExtra::footnote(general = footnote, footnote_as_chunk = T)
