

library(psych)
library(sjPlot)

db_efa <- db1[,c(2:9)] %>% drop_na()

sjPlot::tab_corr(db_efa, triangle = "lower")

KMO(db_efa)

cortest.bartlett(db_efa)

plot(scree(db_efa))

fa(db_efa, # datos
   nfactors = 4, # n factores
   fm = "ml", # método estimación
   rotate = "varimax")$Vaccounted

set.seed(231018) # Resultado reproducible
n.facts <- fa.parallel(db_efa,
                       fm = 'ml', fa = 'fa') # son 4 factores


fa(db_efa, # datos
   nfactors = 4, # n factores
   fm = "ml", # método estimación
   rotate = "varimax")
