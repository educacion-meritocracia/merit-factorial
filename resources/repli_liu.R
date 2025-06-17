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
  patchwork, #Combina múltiples gráficos de ggplot2 en una sola figura
  semTools,
  gtools,
  kableExtra
)

options(scipen = 999)
rm(list = ls())

load(file = here("output", "data", "db_long_proc.RData"))

names(db_long)
glimpse(db_long)
dim(db_long)

db_invariance <-
  db_long %>%
  select(id_estudiante, ola, starts_with(c("perc", "pref"))) %>%
  pivot_wider(
    id_cols = id_estudiante,
    names_from = ola,
    names_glue = "{.value}{ola}",
    values_from = c(
      perc_effort, perc_talent,
      perc_rich_parents, perc_contact,
      pref_effort, pref_talent,
      pref_rich_parents, pref_contact
    )
  ) %>%
  na.omit() %>%
  rename_with(~ str_replace_all(., "_", ""))

names(db_invariance)
names(db_long)

db_invariance <- db_invariance %>% 
  mutate(
    across(
      .cols = -idestudiante,
      .fns = ~ case_when(. == 1 ~ 0,
                         . == 2 ~ 1,
                         . == 3 ~ 2,
                         . == 4 ~ 3)
    )
  )

db_invariance <- db_invariance %>% 
  mutate(
    across(
      .cols = -idestudiante,
      .fns = ~ ordered(.)
    )
  ) 

baseline_model_smt <- ('
# Definición de factores

percmerit1  =~ 1*perceffort1 + 1*perctalent1
percmerit2  =~ 1*perceffort2 + 1*perctalent2

percnmerit1 =~ 1*percrichparents1 + 1*perccontact1
percnmerit2 =~ 1*percrichparents2 + 1*perccontact2

prefmerit1  =~ 1*prefeffort1 + 1*preftalent1
prefmerit2  =~ 1*prefeffort2 + 1*preftalent2

prefnmerit1 =~ 1*prefrichparents1 + 1*prefcontact1
prefnmerit2 =~ 1*prefrichparents2 + 1*prefcontact2

# Varianzas y covarianzas latentes

percmerit1  ~~ percmerit1 + percmerit2
percmerit2  ~~ percmerit2

percnmerit1 ~~ percnmerit1 + percnmerit2
percnmerit2 ~~ percnmerit2

prefmerit1  ~~ prefmerit1 + prefmerit2
prefmerit2  ~~ prefmerit2

prefnmerit1 ~~ prefnmerit1 + prefnmerit2
prefnmerit2 ~~ prefnmerit2

# Medias latentes (identificación)

percmerit1  ~ 0*1
percmerit2  ~ 1

percnmerit1 ~ 0*1
percnmerit2 ~ 1

prefmerit1  ~ 0*1
prefmerit2  ~ 1

prefnmerit1 ~ 0*1
prefnmerit2 ~ 1

# Umbrales (uno igualado por ítem)

perceffort1     | pe1t1*t1 + pe1t2*t2
perceffort2     | pe2t1*t1 + pe2t2*t2

perctalent1     | pt1t1*t1 + pt1t2*t2
perctalent2     | pt2t1*t1 + pt2t2*t2

percrichparents1| prp1t1*t1 + prp1t2*t2
percrichparents2| prp2t1*t1 + prp2t2*t2

perccontact1    | pc1t1*t1 + pc1t2*t2
perccontact2    | pc2t1*t1 + pc2t2*t2

prefeffort1     | pre1t1*t1 + pre1t2*t2
prefeffort2     | pre2t1*t1 + pre2t2*t2

preftalent1     | prt1t1*t1 + prt1t2*t2
preftalent2     | prt2t1*t1 + prt2t2*t2

prefrichparents1| pfrp1t1*t1 + pfrp1t2*t2
prefrichparents2| pfrp2t1*t1 + pfrp2t2*t2

prefcontact1    | pfc1t1*t1 + pfc1t2*t2
prefcontact2    | pfc2t1*t1 + pfc2t2*t2

# Interceptos fijos a 0 (por identificación con umbrales)

perceffort1 + perctalent1 ~ 0*1
perceffort2 + perctalent2 ~ 0*1

percrichparents1 + perccontact1 ~ 0*1
percrichparents2 + perccontact2 ~ 0*1

prefeffort1 + preftalent1 ~ 0*1
prefeffort2 + preftalent2 ~ 0*1

prefrichparents1 + prefcontact1 ~ 0*1
prefrichparents2 + prefcontact2 ~ 0*1

# Varianzas únicas
# Fijas a 1 en ola 1, libres en ola 2

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

# Covarianzas entre errores (misma variable, diferentes tiempos)

perceffort1 ~~ perceffort2
perctalent1 ~~ perctalent2

percrichparents1 ~~ percrichparents2
perccontact1 ~~ perccontact2

prefeffort1 ~~ prefeffort2
preftalent1 ~~ preftalent2

prefrichparents1 ~~ prefrichparents2
prefcontact1 ~~ prefcontact2
')

# Estimación del modelo
fit_baseline <- cfa(baseline_model_smt, data = db_invariance,
                    ordered = c("perceffort1", "perctalent1", "perceffort2", "perctalent2",
                                "percrichparents1", "perccontact1", "percrichparents2", "perccontact2",
                                "prefeffort1", "preftalent1", "prefeffort2", "preftalent2",
                                "prefrichparents1", "prefcontact1", "prefrichparents2", "prefcontact2"),
                    parameterization = "theta",
                    estimator = "WLSMV")

# Resultados
summary(fit_baseline, fit.measures = TRUE)

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


# Medias latentes

percmerit1  ~ 0*1
percmerit2  ~ 1
percnmerit1 ~ 0*1
percnmerit2 ~ 1
prefmerit1  ~ 0*1
prefmerit2  ~ 1
prefnmerit1 ~ 0*1
prefnmerit2 ~ 1


# Umbrales (uno igualado por ítem)

perceffort1 | pe1t1*t1 + pe1t2*t2
perceffort2 | pe2t1*t1 + pe2t2*t2
perctalent1 | pt1t1*t1 + pt1t2*t2
perctalent2 | pt2t1*t1 + pt2t2*t2
percrichparents1 | prp1t1*t1 + prp1t2*t2
percrichparents2 | prp2t1*t1 + prp2t2*t2
perccontact1 | pc1t1*t1 + pc1t2*t2
perccontact2 | pc2t1*t1 + pc2t2*t2
prefeffort1 | pre1t1*t1 + pre1t2*t2
prefeffort2 | pre2t1*t1 + pre2t2*t2
preftalent1 | prt1t1*t1 + prt1t2*t2
preftalent2 | prt2t1*t1 + prt2t2*t2
prefrichparents1 | pfrp1t1*t1 + pfrp1t2*t2
prefrichparents2 | pfrp2t1*t1 + pfrp2t2*t2
prefcontact1 | pfc1t1*t1 + pfc1t2*t2
prefcontact2 | pfc2t1*t1 + pfc2t2*t2


# Interceptos

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

summary(fit_loadinginv, fit.measures = TRUE)

anova(fit_baseline, fit_loadinginv)

lavTestLRT(fit_baseline, fit_loadinginv)

mi_loadinginv <- modindices(fit_loadinginv)
subset(mi_loadinginv, mi >= 10)

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


# Medias latentes

percmerit1  ~ 0*1
percmerit2  ~ 1
percnmerit1 ~ 0*1
percnmerit2 ~ 1
prefmerit1  ~ 0*1
prefmerit2  ~ 1
prefnmerit1 ~ 0*1
prefnmerit2 ~ 1


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

summary(fit_thresholdinv, fit.measures = TRUE)

anova(fit_loadinginv, fit_thresholdinv)

mi_threshold <- modindices(fit_thresholdinv)
subset(mi_threshold, mi >= 10)

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

# Ver resumen
summary(fit_uniquenessinv, fit.measures = TRUE)

# Comparar con modelo anterior (invarianza fuerte)
anova(fit_thresholdinv, fit_uniquenessinv)

# Ver modification indices
mi_uniqueness <- modindices(fit_uniquenessinv)
subset(mi_uniqueness, mi >= 10)

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

tab.inv %>% 
  kableExtra::kable(format = "html",
                    align = "c",
                    booktabs = T,
                    escape = F,
                    caption = NULL,
                    col.names = col.nam) %>%
  kableExtra::kable_styling(full_width = T,
                            latex_options = "hold_position",
                            bootstrap_options=c("striped", "bordered", "condensed"),
                            font_size = 23) %>%
  kableExtra::column_spec(c(1,8), width = "3.5cm") %>% 
  kableExtra::column_spec(2:7, width = "4cm") %>% 
  kableExtra::column_spec(4, width = "5cm")

