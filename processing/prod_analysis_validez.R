#******************************************************************************************************************************************************
# 0. Identification -------------------------------------------------------
# Title: Analysis code for a research paper on "Housing wealth and social cohesion: Evidence from Chile"
# Responsable: Technical assistant
# Executive Summary: This script contains the code to perform regression analysis
# Date: December 22, 2025
#******************************************************************************************************************************************************

options(scipen = 999)
rm(list = ls())


# 1. Libraries ------------------------------------------------------------

if (!require("pacman")) install.packages("pacman")

pacman::p_load(
  tidyverse,
  sjmisc,
  sjPlot,
  here,
  lavaan,
  psych,
  corrplot,
  ggdist,
  patchwork,
  semTools,
  gtools,
  kableExtra
)


# 2. Data -----------------------------------------------------------------

load(file = here("output", "data", "db_long_proc2.RData"))

names(db_long)
glimpse(db_long)
dim(db_long)

# 3. Processing -----------------------------------------------------------

db_invariance <- db_long %>%
  group_by(id_estudiante) %>%
  mutate(
    cohort_level = first(cohort_level),
    cohort_dummy = case_when(
      cohort_level == "Primary" ~ 0,
      cohort_level == "Secondary" ~ 1,
      TRUE ~ NA_real_
    )
  ) %>%
  ungroup() %>%
  select(id_estudiante, cohort_dummy, ola, starts_with(c("perc", "pref")), mjp_educ, mjp_health, mjp_pension) %>%
  pivot_wider(
    id_cols = c(id_estudiante, cohort_dummy),
    names_from = ola,
    names_glue = "{.value}{ola}",
    values_from = c(
      perc_effort, perc_talent,
      perc_rich_parents, perc_contact,
      pref_effort, pref_talent,
      pref_rich_parents, pref_contact,
      mjp_educ, mjp_health, mjp_pension
    )
  ) %>%
  #na.omit() %>%
  rename_with(~ str_replace_all(., "_", ""))

# Reescalar variables y convertir a ordinales
db_invariance <- db_invariance %>% 
  mutate(
    across(
      .cols = -c(idestudiante, cohortdummy),
      .fns = ~ case_when(. == 1 ~ 0,
                         . == 2 ~ 1,
                         . == 3 ~ 2,
                         . == 4 ~ 3)
    )
  ) %>%
  mutate(
    across(
      .cols = -c(idestudiante),
      .fns = ~ ordered(.)
    )
  )

# 4. Longitudinal invariance ----------------------------------------------

# First, define the configural model, using the repeated measures factors and
# indicators
baseline_model_smt <- ('

###########################################
# Definición de factores (1 marcador por factor)
###########################################

percmerit1  =~ 1*perceffort1     + perctalent1
percmerit2  =~ 1*perceffort2     + perctalent2

percnmerit1 =~ 1*percrichparents1 + perccontact1
percnmerit2 =~ 1*percrichparents2 + perccontact2

prefmerit1  =~ 1*prefeffort1     + preftalent1
prefmerit2  =~ 1*prefeffort2     + preftalent2

prefnmerit1 =~ 1*prefrichparents1 + prefcontact1
prefnmerit2 =~ 1*prefrichparents2 + prefcontact2


###########################################
# Covarianzas entre errores (mismo ítem, distintas olas)
###########################################

perceffort1 ~~ perceffort2
perctalent1 ~~ perctalent2

percrichparents1 ~~ percrichparents2
perccontact1     ~~ perccontact2

prefeffort1 ~~ prefeffort2
preftalent1 ~~ preftalent2

prefrichparents1 ~~ prefrichparents2
prefcontact1     ~~ prefcontact2
')



# Model Estimation
fit_baseline <- cfa(baseline_model_smt, data = db_invariance,
                    ordered = c("perceffort1", "perctalent1", "perceffort2", "perctalent2",
                                "percrichparents1", "perccontact1", "percrichparents2", "perccontact2",
                                "prefeffort1", "preftalent1", "prefeffort2", "preftalent2",
                                "prefrichparents1", "prefcontact1", "prefrichparents2", "prefcontact2"),
                    parameterization = "theta",
                    estimator = "WLSMV")

loadinginv_model_smt <- ('

# Igualación de cargas (invarianza débil)

percmerit1  =~ 1*perceffort1 + pe_loading*perctalent1
percmerit2  =~ 1*perceffort2 + pe_loading*perctalent2

percnmerit1 =~ 1*percrichparents1 + prc_loading*perccontact1
percnmerit2 =~ 1*percrichparents2 + prc_loading*perccontact2

prefmerit1  =~ 1*prefeffort1 + pfe_loading*preftalent1
prefmerit2  =~ 1*prefeffort2 + pfe_loading*preftalent2

prefnmerit1 =~ 1*prefrichparents1 + pfrc_loading*prefcontact1
prefnmerit2 =~ 1*prefrichparents2 + pfrc_loading*prefcontact2


# Varianzas y covarianzas latentes

percmerit1  ~~ percmerit1 + percmerit2
percmerit2  ~~ percmerit2
percnmerit1 ~~ percnmerit1 + percnmerit2
percnmerit2 ~~ percnmerit2
prefmerit1  ~~ prefmerit1 + prefmerit2
prefmerit2  ~~ prefmerit2
prefnmerit1 ~~ prefnmerit1 + prefnmerit2
prefnmerit2 ~~ prefnmerit2


# Umbrales (uno igualado por ítem)

# perceffort1 | pe1t1*t1 + pe1t2*t2
# perceffort2 | pe2t1*t1 + pe2t2*t2
# perctalent1 | pt1t1*t1 + pt1t2*t2
# perctalent2 | pt2t1*t1 + pt2t2*t2
# percrichparents1 | prp1t1*t1 + prp1t2*t2
# percrichparents2 | prp2t1*t1 + prp2t2*t2
# perccontact1 | pc1t1*t1 + pc1t2*t2
# perccontact2 | pc2t1*t1 + pc2t2*t2
# prefeffort1 | pre1t1*t1 + pre1t2*t2
# prefeffort2 | pre2t1*t1 + pre2t2*t2
# preftalent1 | prt1t1*t1 + prt1t2*t2
# preftalent2 | prt2t1*t1 + prt2t2*t2
# prefrichparents1 | pfrp1t1*t1 + pfrp1t2*t2
# prefrichparents2 | pfrp2t1*t1 + pfrp2t2*t2
# prefcontact1 | pfc1t1*t1 + pfc1t2*t2
# prefcontact2 | pfc2t1*t1 + pfc2t2*t2
 

# Covarianzas entre errores

perceffort1 ~~ perceffort2
perctalent1 ~~ perctalent2
percrichparents1 ~~ percrichparents2
perccontact1 ~~ perccontact2
prefeffort1 ~~ prefeffort2
preftalent1 ~~ preftalent2
prefrichparents1 ~~ prefrichparents2
prefcontact1 ~~ prefcontact2
')


fit_loadinginv <- cfa(loadinginv_model_smt, data = db_invariance,
                      ordered = c("perceffort1", "perctalent1", "perceffort2", "perctalent2",
                                  "percrichparents1", "perccontact1", "percrichparents2", "perccontact2",
                                  "prefeffort1", "preftalent1", "prefeffort2", "preftalent2",
                                  "prefrichparents1", "prefcontact1", "prefrichparents2", "prefcontact2"),
                      parameterization = "theta",
                      estimator = "WLSMV")

thresholdinv_model_smt <- ('

# Igualación de cargas (como en el modelo débil)

percmerit1  =~ 1*perceffort1 + pe_loading*perctalent1
percmerit2  =~ 1*perceffort2 + pe_loading*perctalent2

percnmerit1 =~ 1*percrichparents1 + prc_loading*perccontact1
percnmerit2 =~ 1*percrichparents2 + prc_loading*perccontact2

prefmerit1  =~ 1*prefeffort1 + pfe_loading*preftalent1
prefmerit2  =~ 1*prefeffort2 + pfe_loading*preftalent2

prefnmerit1 =~ 1*prefrichparents1 + pfrc_loading*prefcontact1
prefnmerit2 =~ 1*prefrichparents2 + pfrc_loading*prefcontact2


# Varianzas y covarianzas latentes

percmerit1  ~~ percmerit1 + percmerit2
percmerit2  ~~ percmerit2
percnmerit1 ~~ percnmerit1 + percnmerit2
percnmerit2 ~~ percnmerit2
prefmerit1  ~~ prefmerit1 + prefmerit2
prefmerit2  ~~ prefmerit2
prefnmerit1 ~~ prefnmerit1 + prefnmerit2
prefnmerit2 ~~ prefnmerit2


# Thresholds iguales entre olas (invarianza fuerte)

perceffort1     | pe_t1*t1 + pe_t2*t2
perceffort2     | pe_t1*t1 + pe_t2*t2
perctalent1     | pt_t1*t1 + pt_t2*t2
perctalent2     | pt_t1*t1 + pt_t2*t2
percrichparents1| prp_t1*t1 + prp_t2*t2
percrichparents2| prp_t1*t1 + prp_t2*t2
perccontact1    | pc_t1*t1 + pc_t2*t2
perccontact2    | pc_t1*t1 + pc_t2*t2
prefeffort1     | pre_t1*t1 + pre_t2*t2
prefeffort2     | pre_t1*t1 + pre_t2*t2
preftalent1     | prt_t1*t1 + prt_t2*t2
preftalent2     | prt_t1*t1 + prt_t2*t2
prefrichparents1| pfrp_t1*t1 + pfrp_t2*t2
prefrichparents2| pfrp_t1*t1 + pfrp_t2*t2
prefcontact1    | pfc_t1*t1 + pfc_t2*t2
prefcontact2    | pfc_t1*t1 + pfc_t2*t2


# Interceptos fijos

perceffort1 + perctalent1 ~ 0*1
perceffort2 + perctalent2 ~ 0*1
percrichparents1 + perccontact1 ~ 0*1
percrichparents2 + perccontact2 ~ 0*1
prefeffort1 + preftalent1 ~ 0*1
prefeffort2 + preftalent2 ~ 0*1
prefrichparents1 + prefcontact1 ~ 0*1
prefrichparents2 + prefcontact2 ~ 0*1


# Varianzas únicas

perceffort1 ~~ 1*perceffort1
perctalent1 ~~ 1*perctalent1
percrichparents1 ~~ 1*percrichparents1
perccontact1 ~~ 1*perccontact1
prefeffort1 ~~ 1*prefeffort1
preftalent1 ~~ 1*preftalent1
prefrichparents1 ~~ 1*prefrichparents1
prefcontact1 ~~ 1*prefcontact1

perceffort2 ~~ NA*perceffort2
perctalent2 ~~ NA*perctalent2
percrichparents2 ~~ NA*percrichparents2
perccontact2 ~~ NA*perccontact2
prefeffort2 ~~ NA*prefeffort2
preftalent2 ~~ NA*preftalent2
prefrichparents2 ~~ NA*prefrichparents2
prefcontact2 ~~ NA*prefcontact2


# Covarianzas entre errores longitudinales

perceffort1 ~~ perceffort2
perctalent1 ~~ perctalent2
percrichparents1 ~~ percrichparents2
perccontact1 ~~ perccontact2
prefeffort1 ~~ prefeffort2
preftalent1 ~~ preftalent2
prefrichparents1 ~~ prefrichparents2
prefcontact1 ~~ prefcontact2
')

fit_thresholdinv <- cfa(thresholdinv_model_smt, data = db_invariance,
                        ordered = c("perceffort1", "perctalent1", "perceffort2", "perctalent2",
                                    "percrichparents1", "perccontact1", "percrichparents2", "perccontact2",
                                    "prefeffort1", "preftalent1", "prefeffort2", "preftalent2",
                                    "prefrichparents1", "prefcontact1", "prefrichparents2", "prefcontact2"),
                        parameterization = "theta",
                        estimator = "WLSMV")

uniquenessinv_model_smt <- ('

# Cargas factoriales (invarianza débil)

percmerit1  =~ 1*perceffort1 + pe_loading*perctalent1
percmerit2  =~ 1*perceffort2 + pe_loading*perctalent2

percnmerit1 =~ 1*percrichparents1 + prc_loading*perccontact1
percnmerit2 =~ 1*percrichparents2 + prc_loading*perccontact2

prefmerit1  =~ 1*prefeffort1 + pfe_loading*preftalent1
prefmerit2  =~ 1*prefeffort2 + pfe_loading*preftalent2

prefnmerit1 =~ 1*prefrichparents1 + pfrc_loading*prefcontact1
prefnmerit2 =~ 1*prefrichparents2 + pfrc_loading*prefcontact2


# Varianzas y covarianzas de factores latentes

percmerit1  ~~ percmerit1 + percmerit2
percmerit2  ~~ percmerit2
percnmerit1 ~~ percnmerit1 + percnmerit2
percnmerit2 ~~ percnmerit2
prefmerit1  ~~ prefmerit1 + prefmerit2
prefmerit2  ~~ prefmerit2
prefnmerit1 ~~ prefnmerit1 + prefnmerit2
prefnmerit2 ~~ prefnmerit2


# Medias latentes

percmerit1  ~ 0*1
percmerit2  ~ 1
percnmerit1 ~ 0*1
percnmerit2 ~ 1
prefmerit1  ~ 0*1
prefmerit2  ~ 1
prefnmerit1 ~ 0*1
prefnmerit2 ~ 1


# Umbrales (invarianza fuerte)

perceffort1     | pe_t1*t1 + pe_t2*t2
perceffort2     | pe_t1*t1 + pe_t2*t2
perctalent1     | pt_t1*t1 + pt_t2*t2
perctalent2     | pt_t1*t1 + pt_t2*t2
percrichparents1| prp_t1*t1 + prp_t2*t2
percrichparents2| prp_t1*t1 + prp_t2*t2
perccontact1    | pc_t1*t1 + pc_t2*t2
perccontact2    | pc_t1*t1 + pc_t2*t2
prefeffort1     | pre_t1*t1 + pre_t2*t2
prefeffort2     | pre_t1*t1 + pre_t2*t2
preftalent1     | prt_t1*t1 + prt_t2*t2
preftalent2     | prt_t1*t1 + prt_t2*t2
prefrichparents1| pfrp_t1*t1 + pfrp_t2*t2
prefrichparents2| pfrp_t1*t1 + pfrp_t2*t2
prefcontact1    | pfc_t1*t1 + pfc_t2*t2
prefcontact2    | pfc_t1*t1 + pfc_t2*t2


# Interceptos fijos

perceffort1 + perctalent1 ~ 0*1
perceffort2 + perctalent2 ~ 0*1
percrichparents1 + perccontact1 ~ 0*1
percrichparents2 + perccontact2 ~ 0*1
prefeffort1 + preftalent1 ~ 0*1
prefeffort2 + preftalent2 ~ 0*1
prefrichparents1 + prefcontact1 ~ 0*1
prefrichparents2 + prefcontact2 ~ 0*1


# Varianzas únicas (invarianza estricta)
# Fijadas a 1 en todas las olas

perceffort1 ~~ 1*perceffort1
perceffort2 ~~ 1*perceffort2
perctalent1 ~~ 1*perctalent1
perctalent2 ~~ 1*perctalent2

percrichparents1 ~~ 1*percrichparents1
percrichparents2 ~~ 1*percrichparents2
perccontact1 ~~ 1*perccontact1
perccontact2 ~~ 1*perccontact2

prefeffort1 ~~ 1*prefeffort1
prefeffort2 ~~ 1*prefeffort2
preftalent1 ~~ 1*preftalent1
preftalent2 ~~ 1*preftalent2

prefrichparents1 ~~ 1*prefrichparents1
prefrichparents2 ~~ 1*prefrichparents2
prefcontact1 ~~ 1*prefcontact1
prefcontact2 ~~ 1*prefcontact2


# Covarianzas entre errores (mismo ítem, diferente ola)

perceffort1 ~~ perceffort2
perctalent1 ~~ perctalent2
percrichparents1 ~~ percrichparents2
perccontact1 ~~ perccontact2
prefeffort1 ~~ prefeffort2
preftalent1 ~~ preftalent2
prefrichparents1 ~~ prefrichparents2
prefcontact1 ~~ prefcontact2
')

fit_uniquenessinv <- cfa(uniquenessinv_model_smt, data = db_invariance,
                         ordered = c("perceffort1", "perctalent1", "perceffort2", "perctalent2",
                                     "percrichparents1", "perccontact1", "percrichparents2", "perccontact2",
                                     "prefeffort1", "preftalent1", "prefeffort2", "preftalent2",
                                     "prefrichparents1", "prefcontact1", "prefrichparents2", "prefcontact2"),
                         parameterization = "theta",
                         estimator = "WLSMV")


# Compare fit

an1 <- anova(fit_baseline, fit_loadinginv)
an2 <- anova(fit_loadinginv, fit_thresholdinv)
an3 <- anova(fit_thresholdinv, fit_uniquenessinv)

tab01 <- bind_rows(
  as_tibble(an1)[2,],
  as_tibble(an2)[2,],
  as_tibble(an3)[2,]
) %>%
  select("Chisq", "Df", chisq_diff = `Chisq diff`, df_diff = `Df diff`, pvalue = `Pr(>Chisq)`) %>%
  mutate(
    stars = stars.pval(pvalue),
    chisqt = paste0(round(Chisq, 2), " (", Df, ")"),
    decision = ifelse(pvalue > 0.05, "Accept", "Reject"),
    model = c("Weak", "Strong", "Strict")
  ) %>%
  bind_rows(
    tibble(Chisq = an1$Chisq[1], Df = an1$Df[1], chisq_diff = NA, df_diff = NA,
           pvalue = NA, stars = "", chisqt = paste0(round(an1$Chisq[1], 2), " (", an1$Df[1], ")"),
           decision = "Reference", model = "Configural")
  ) %>%
  select(model, chisqt, chisq_diff, df_diff, pvalue, stars, decision) %>%
  mutate(model = factor(model, levels = c("Configural", "Weak", "Strong", "Strict"))) %>%
  arrange(model)


fit.meas <- dplyr::bind_rows(lavaan::fitmeasures(fit_baseline, output ="matrix")[c("chisq","df","cfi","rmsea","rmsea.ci.lower","rmsea.ci.upper"),],
                             lavaan::fitmeasures(fit_loadinginv,  output ="matrix")[c("chisq","df","cfi","rmsea","rmsea.ci.lower","rmsea.ci.upper"),],
                             lavaan::fitmeasures(fit_thresholdinv, output ="matrix")[c("chisq","df","cfi","rmsea","rmsea.ci.lower","rmsea.ci.upper"),],
                             lavaan::fitmeasures(fit_uniquenessinv, output ="matrix")[c("chisq","df","cfi","rmsea","rmsea.ci.lower","rmsea.ci.upper"),])

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


col.nam <- c("Model","&chi;^2 (df)","CFI","RMSEA (90 CI)",
             "&Delta; &chi;^2 (&Delta; df)","&Delta; CFI","&Delta; RMSEA","Decision")

#tab.inv %>% 
#  kableExtra::kable(format = "html",
#                    align = "c",
#                    booktabs = T,
#                    escape = F,
#                    caption = NULL,
#                    col.names = col.nam) %>%
#  kableExtra::kable_styling(full_width = T,
#                            latex_options = "hold_position",
#                            bootstrap_options=c("striped", "bordered", "condensed"),
#                            font_size = 23) %>%
#  kableExtra::column_spec(c(1,8), width = "3.5cm") %>% 
#  kableExtra::column_spec(2:7, width = "4cm") %>% 
#  kableExtra::column_spec(4, width = "5cm")
#


# 5. EFA Market justice preferences ---------------------------------------

items_w1 <- c("mjpeduc1", "mjphealth1", "mjppension1")
items_w2 <- c("mjpeduc2", "mjphealth2", "mjppension2")
items_all <- c(items_w1, items_w2)

df_mjp <- db_invariance %>%
  select(all_of(items_all)) %>%
  mutate(across(all_of(items_all), ~ as.numeric(.)))  

# --- EFA wave 1 ---

# Correlation matrix

M_w1 <- psych::polychoric(df_mjp[, items_w1], na.rm = T)
M_w1

# KMO

KMO(r = M_w1$rho) # 0.68

# Bartlett sphericity test

cortest.bartlett(df_mjp[, items_w1]) # p < 0.05

# Kaiser-Guttman rule

plot(scree(df_mjp[, items_w1])) # 1 factor con autovalor sobre 1

# Extract explained variance

fa(df_mjp[, items_w1],
   nfactors = 1,
   fm = "ml",
   rotate = "none",
   fa = "fa")$Vaccounted # 1 factor explica el 54% de la varianza comun de los items

# Parallel analysis

set.seed(231018)

psych::fa.parallel(M_w1$rho, n.obs = nrow(df_mjp), fm = "ml", fa = "fa") # se sugiere 1 factor

# Estimate EFA
efa_w1 <- psych::fa(r = df_mjp[, items_w1], nfactors = 1, fm = "ml", rotate = "varimax")
print(efa_w1)

# --- EFA wave 2 (opcional) ---

# Correlation matrix
M_w2 <- psych::polychoric(df_mjp[, items_w2], na.rm = T)
M_w2

# KMO

KMO(r = M_w2$rho) # 0.66

# Bartlett sphericity test

cortest.bartlett(df_mjp[, items_w2]) # p < 0.05

# Kaiser-Guttman rule

plot(scree(df_mjp[, items_w2])) # 1 factor con autovalor sobre 1

# Extract explained variance

fa(df_mjp[, items_w2],
   nfactors = 1,
   fm = "ml",
   rotate = "none",
   fa = "fa")$Vaccounted # 1 factor explica el 54% de la varianza comun de los items

# Parallel analysis

set.seed(12345)

psych::fa.parallel(M_w2$rho, n.obs = nrow(df_mjp), fm = "ml", fa = "fa") # se sugiere 1 factor

# Estimate EFA
efa_w2 <- psych::fa(r = df_mjp[, items_w2], nfactors = 1, fm = "ml", rotate = "varimax")
print(efa_w2)

# 6. CFA Market justice preferences ------------------------------------------

# model
model_cfa1 <- c('
  mjp1 =~ mjpeduc1 + mjphealth1 + mjppension1
  ')

model_cfa2 <- c('
  mjp2 =~ mjpeduc2 + mjphealth2 + mjppension2
  ')

# estimation for each order set

fit_cfa_w1 <- cfa(model = model_cfa1, 
              data = df_mjp[, items_w1],
              estimator = "DWLS",
              ordered = T,
              std.lv = F) 

fit_cfa_w2 <- cfa(model = model_cfa2, 
              data = df_mjp[, items_w2],
              estimator = "DWLS",
              ordered = T,
              std.lv = F) 


summary(fit_cfa_w1, fit.measures = TRUE, standardized = TRUE)
summary(fit_cfa_w2, fit.measures = TRUE, standardized = TRUE)

psych::alpha(df_mjp[, items_w1]) # 0.77
psych::alpha(df_mjp[, items_w2]) # 0.76


# 7. Longitudinal invariance MJP ------------------------------------------

baseline_model_smt_mjp <- ('

###########################################
# Definición de factores (1 marcador por factor)
###########################################

mjp1  =~ 1*mjpeduc1 + mjphealth1 + mjppension1
mjp2  =~ 1*mjpeduc2 + mjphealth2 + mjppension2

###########################################
# Covarianzas entre errores (mismo ítem, distintas olas)
###########################################

mjpeduc1 ~~ mjpeduc2
mjphealth1 ~~ mjphealth2
mjppension1 ~~ mjppension2

')


# Model Estimation
fit_baseline_mjp <- cfa(baseline_model_smt_mjp, data = df_mjp,
                        ordered = c("mjpeduc1", "mjphealth1", "mjppension1",
                                    "mjpeduc2", "mjphealth2", "mjppension2"),
                        parameterization = "theta",
                        estimator = "WLSMV")

loadinginv_model_smt_mjp <- ('

# Igualación de cargas (invarianza débil)

mjp1  =~ 1*mjpeduc1 + health_loading*mjphealth1 + pension_loading*mjppension1
mjp2  =~ 1*mjpeduc2 + health_loading*mjphealth2 + pension_loading*mjppension2

# Varianzas y covarianzas latentes

mjp1 ~~ mjp1 + mjp2
mjp2 ~~ mjp2

# Umbrales (uno igualado por ítem)

# mjpeduc1    | e1t1*t1 + e1t2*t2
# mjpeduc2    | e2t1*t1 + e2t2*t2
# mjphealth1  | h1t1*t1 + h1t2*t2
# mjphealth2  | h2t1*t1 + h2t2*t2
# mjppension1 | p1t1*t1 + p1t2*t2
# mjppension2 | p2t1*t1 + p2t2*t2
 
# Covarianzas entre errores

mjpeduc1 ~~ mjpeduc2
mjphealth1 ~~ mjphealth2
mjppension1 ~~ mjppension2

')


fit_loadinginv_mjp <- cfa(loadinginv_model_smt_mjp, data = df_mjp,
                          ordered = c("mjpeduc1", "mjphealth1", "mjppension1",
                                      "mjpeduc2", "mjphealth2", "mjppension2"),
                          parameterization = "theta",
                          estimator = "WLSMV")

thresholdinv_model_smt_mjp <- ('

# Igualación de cargas (como en el modelo débil)

mjp1  =~ 1*mjpeduc1 + health_loading*mjphealth1 + pension_loading*mjppension1
mjp2  =~ 1*mjpeduc2 + health_loading*mjphealth2 + pension_loading*mjppension2

# Varianzas y covarianzas latentes

mjp1 ~~ mjp1 + mjp2
mjp2 ~~ mjp2

# Thresholds iguales entre olas (invarianza fuerte)

mjpeduc1     | edu_t1*t1 + edu_t2*t2
mjpeduc2     | edu_t1*t1 + edu_t2*t2
mjphealth1   | health_t1*t1 + health_t2*t2
mjphealth2   | health_t1*t1 + health_t2*t2
mjppension1  | pens_t1*t1 + pens_t2*t2
mjppension2  | pens_t1*t1 + pens_t2*t2

# Interceptos fijos

mjpeduc1 + mjphealth1 + mjppension1 ~ 0*1
mjpeduc2 + mjphealth2 + mjppension2 ~ 0*1

# Varianzas únicas

mjpeduc1 ~~ 1*mjpeduc1
mjphealth1 ~~ 1*mjphealth1
mjppension1 ~~ 1*mjppension1

mjpeduc2 ~~ NA*mjpeduc2
mjphealth2 ~~ NA*mjphealth2
mjppension2 ~~ NA*mjppension2

# Covarianzas entre errores longitudinales

mjpeduc1 ~~ mjpeduc2
mjphealth1 ~~ mjphealth2
mjppension1 ~~ mjppension2

')

fit_thresholdinv_mjp <- cfa(thresholdinv_model_smt_mjp,  data = df_mjp,
                            ordered = c("mjpeduc1", "mjphealth1", "mjppension1",
                                        "mjpeduc2", "mjphealth2", "mjppension2"),
                            parameterization = "theta",
                            estimator = "WLSMV")

uniquenessinv_model_smt_mjp <- ('

# Cargas factoriales (invarianza débil)


mjp1  =~ 1*mjpeduc1 + health_loading*mjphealth1 + pension_loading*mjppension1
mjp2  =~ 1*mjpeduc2 + health_loading*mjphealth2 + pension_loading*mjppension2


# Varianzas y covarianzas de factores latentes

mjp1 ~~ mjp1 + mjp2
mjp2 ~~ mjp2

# Medias latentes

mjp1  ~ 0*1
mjp2  ~ 1


# Umbrales (invarianza fuerte)

mjpeduc1     | edu_t1*t1 + edu_t2*t2
mjpeduc2     | edu_t1*t1 + edu_t2*t2
mjphealth1   | health_t1*t1 + health_t2*t2
mjphealth2   | health_t1*t1 + health_t2*t2
mjppension1  | pens_t1*t1 + pens_t2*t2
mjppension2  | pens_t1*t1 + pens_t2*t2


# Interceptos fijos

mjpeduc1 + mjphealth1 + mjppension1 ~ 0*1
mjpeduc2 + mjphealth2 + mjppension2 ~ 0*1

# Varianzas únicas (invarianza estricta)
# Fijadas a 1 en todas las olas

mjpeduc1 ~~ 1*mjpeduc1
mjpeduc2 ~~ 1*mjpeduc2


mjphealth1 ~~ 1*mjphealth1
mjphealth2 ~~ 1*mjphealth2

mjppension1 ~~ 1*mjppension1
mjppension2 ~~ 1*mjppension2

# Covarianzas entre errores (mismo ítem, diferente ola)

mjpeduc1 ~~ mjpeduc2
mjphealth1 ~~ mjphealth2
mjppension1 ~~ mjppension2

')

fit_uniquenessinv_mjp <- cfa(uniquenessinv_model_smt_mjp, data = df_mjp,
                             ordered = c("mjpeduc1", "mjphealth1", "mjppension1",
                                         "mjpeduc2", "mjphealth2", "mjppension2"),
                             parameterization = "theta",
                             estimator = "WLSMV")


# Compare fit

lavTestLRT(
  fit_baseline_mjp,
  fit_loadinginv_mjp,
  fit_thresholdinv_mjp,
  fit_uniquenessinv_mjp
)

sapply(list(fit_baseline_mjp, fit_loadinginv_mjp, fit_thresholdinv_mjp, fit_uniquenessinv_mjp),
       lavInspect, "converged")

sapply(list(fit_baseline_mjp, fit_loadinginv_mjp, fit_thresholdinv_mjp, fit_uniquenessinv_mjp),
       lavInspect, "post.check")  # TRUE = solución admisible



an1_mjp <- anova(fit_baseline_mjp, fit_loadinginv_mjp)
an2_mjp <- anova(fit_loadinginv_mjp, fit_thresholdinv_mjp)
an3_mjp <- anova(fit_thresholdinv_mjp, fit_uniquenessinv_mjp)

tab02 <- bind_rows(
  as_tibble(an1_mjp)[2,],
  as_tibble(an2_mjp)[2,],
  as_tibble(an3_mjp)[2,]
) %>%
  select("Chisq", "Df", chisq_diff = `Chisq diff`, df_diff = `Df diff`, pvalue = `Pr(>Chisq)`) %>%
  mutate(
    stars = stars.pval(pvalue),
    chisqt = paste0(round(Chisq, 2), " (", Df, ")"),
    decision = ifelse(pvalue > 0.05, "Accept", "Reject"),
    model = c("Weak", "Strong", "Strict")
  ) %>%
  bind_rows(
    tibble(Chisq = an1$Chisq[1], Df = an1$Df[1], chisq_diff = NA, df_diff = NA,
           pvalue = NA, stars = "", chisqt = paste0(round(an1$Chisq[1], 2), " (", an1$Df[1], ")"),
           decision = "Reference", model = "Configural")
  ) %>%
  select(model, chisqt, chisq_diff, df_diff, pvalue, stars, decision) %>%
  mutate(model = factor(model, levels = c("Configural", "Weak", "Strong", "Strict"))) %>%
  arrange(model)


fit.meas2 <- dplyr::bind_rows(lavaan::fitmeasures(fit_baseline_mjp, output ="matrix")[c("chisq.scaled","df","cfi.scaled","rmsea.scaled","rmsea.ci.lower.scaled","rmsea.ci.upper.scaled"),],
                             lavaan::fitmeasures(fit_loadinginv_mjp,  output ="matrix")[c("chisq.scaled","df","cfi.scaled","rmsea.scaled","rmsea.ci.lower.scaled","rmsea.ci.upper.scaled"),],
                             lavaan::fitmeasures(fit_thresholdinv_mjp, output ="matrix")[c("chisq.scaled","df","cfi.scaled","rmsea.scaled","rmsea.ci.lower.scaled","rmsea.ci.upper.scaled"),],
                             lavaan::fitmeasures(fit_uniquenessinv_mjp, output ="matrix")[c("chisq.scaled","df","cfi.scaled","rmsea.scaled","rmsea.ci.lower.scaled","rmsea.ci.upper.scaled"),])

fit.meas2 <- fit.meas2 %>%
  dplyr::mutate(diff.chi2 = chisq.scaled    - lag(chisq.scaled,default = dplyr::first(chisq.scaled)),
                diff.df   = df       - lag(df,   default = dplyr::first(df)),
                diff.cfi  = cfi.scaled      - lag(cfi.scaled,  default = dplyr::first(cfi.scaled)),
                diff.rmsea   = rmsea.scaled - lag(rmsea.scaled,default = dplyr::first(rmsea.scaled))) %>%
  round(3) %>%
  dplyr::mutate(rmsea.ci.scaled=paste0(rmsea.scaled," \n ", "(",rmsea.ci.lower.scaled,"-",rmsea.ci.upper.scaled,")"))

tab.inv2 <- dplyr::bind_cols(tab02,fit.meas2) %>%
  rename_with(~str_remove(., ".scaled")) %>% 
  dplyr::select(model,chisqt,df,cfi,rmsea.ci,diff.chi2,diff.df,diff.cfi,diff.rmsea,stars,decision) %>%
  dplyr::mutate(diff.chi2=paste0(diff.chi2," (",diff.df,") ",stars)) %>%
  dplyr::select(model,df,cfi,rmsea.ci,diff.cfi,diff.rmsea,decision)


col.nam <- c("Model","df","CFI","RMSEA (90 CI)",
             "&Delta; CFI","&Delta; RMSEA","Decision")

#tab.inv2 %>% 
#  kableExtra::kable(format = "html",
#                    align = "c",
#                    booktabs = T,
#                    escape = F,
#                    caption = NULL,
#                    col.names = col.nam) %>%
#  kableExtra::kable_styling(full_width = T,
#                            latex_options = "hold_position",
#                            bootstrap_options=c("striped", "bordered", "condensed"),
#                            font_size = 23) 


# 8. Latent correlations --------------------------------------------------


validity_model_free <- paste0(
  thresholdinv_model_smt,  # meritocracia (threshold-invariant)
  "\n\n",
  thresholdinv_model_smt_mjp,  # MJP (threshold-invariant)
  "\n\n",
  "

###########################################
# Correlaciones latentes (validez convergente) - libres por ola
###########################################

mjp1 ~~ r_pe_w1*percmerit1
mjp2 ~~ r_pe_w2*percmerit2

mjp1 ~~ r_pn_w1*percnmerit1
mjp2 ~~ r_pn_w2*percnmerit2

mjp1 ~~ r_pm_w1*prefmerit1
mjp2 ~~ r_pm_w2*prefmerit2

mjp1 ~~ r_pnm_w1*prefnmerit1
mjp2 ~~ r_pnm_w2*prefnmerit2
"
)

validity_model_equal <- paste0(
  thresholdinv_model_smt,
  "\n\n",
  thresholdinv_model_smt_mjp,
  "\n\n",
  "
###########################################
# Correlaciones latentes (validez convergente) - fijas por ola
###########################################

mjp1 ~~ r_pe*percmerit1
mjp2 ~~ r_pe*percmerit2

mjp1 ~~ r_pn*percnmerit1
mjp2 ~~ r_pn*percnmerit2

mjp1 ~~ r_pm*prefmerit1
mjp2 ~~ r_pm*prefmerit2

mjp1 ~~ r_pnm*prefnmerit1
mjp2 ~~ r_pnm*prefnmerit2
"
)

ord_all <- c(
  "perceffort1", "perctalent1", "perceffort2", "perctalent2",
  "percrichparents1", "perccontact1", "percrichparents2", "perccontact2",
  "prefeffort1", "preftalent1", "prefeffort2", "preftalent2",
  "prefrichparents1", "prefcontact1", "prefrichparents2", "prefcontact2",
  "mjpeduc1", "mjphealth1", "mjppension1",
  "mjpeduc2", "mjphealth2", "mjppension2"
)

# Libres por ola

fit_validity_free <- cfa(
  validity_model_free,
  data = db_invariance,
  ordered = ord_all,
  parameterization = "theta",
  estimator = "WLSMV"
)

summary(fit_validity_free, standardized = TRUE, fit.measures = TRUE)

# Fijas por ola

fit_validity_equal <- cfa(
  validity_model_equal,
  data = db_invariance,
  ordered = ord_all,
  parameterization = "theta",
  estimator = "WLSMV"
)

summary(fit_validity_equal, standardized = TRUE, fit.measures = TRUE)

lavTestLRT(fit_validity_free, fit_validity_equal)
# Si p > .05 → no hay evidencia de que imponer igualdad empeore: correlaciones estables.




  
 
