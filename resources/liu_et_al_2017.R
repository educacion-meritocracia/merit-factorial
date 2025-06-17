#Supplemental Materials 3A
#lavaan Code for the Empirical Illustration: Baseline Model 
library(lavaan)

Baseline.Model <- ' 
# Latent common factor definitions
C1FAMo =~ NA*V1T1 + V2T1 + 1*V3T1 + V4T1 + V5T1
C2FAMo =~ NA*V1T2 + V2T2 + 1*V3T2 + V4T2 + V5T2 
C3FAMo =~ NA*V1T3 + V2T3 + 1*V3T3 + V4T3 + V5T3
C4FAMo =~ NA*V1T4 + V2T4 + 1*V3T4 + V4T4 + V5T4

# Latent common factor variances and covariances
C1FAMo ~~ C1FAMo + C2FAMo + C3FAMo + C4FAMo
C2FAMo ~~ C2FAMo + C3FAMo + C4FAMo
C3FAMo ~~ C3FAMo + C4FAMo
C4FAMo ~~ C4FAMo

# Latent common factor means
# Fix T1 common factor mean to zero
C1FAMo ~ 0*1
# Freely estimate T2-T4 common factor means
C2FAMo ~ 1
C3FAMo ~ 1
C4FAMo ~ 1

# Thresholds
# t1, t2, and t3 after * represent the first, second, and third 
#    thresholds of an indicator, respectively
# Threshold parameters that are freely estimated have labels VaTbtc,
#   with a representing indicator number, b representing time,
#   c representing ordering of the threshold (first, second, third)
# Threshold parameters that are constrained to be equal over time
#   are given the labels Vatc, with a representing indicator number, 
#   c representing ordering of the threshold (first, second, third)
V1T1 | V1T1t1*t1 + V1t2*t2 + V1T1t3*t3
V1T2 | V1T2t1*t1 + V1t2*t2 + V1T2t3*t3
V1T3 | V1T3t1*t1 + V1t2*t2 + V1T3t3*t3
V1T4 | V1T4t1*t1 + V1t2*t2 + V1T4t3*t3

V2T1 | V2T1t1*t1 + V2t2*t2 + V2T1t3*t3
V2T2 | V2T2t1*t1 + V2t2*t2 + V2T2t3*t3
V2T3 | V2T3t1*t1 + V2t2*t2 + V2T3t3*t3
V2T4 | V2T4t1*t1 + V2t2*t2 + V2T4t3*t3

V3T1 | V3T1t1*t1 + V3t2*t2 + V3t3*t3
V3T2 | V3T2t1*t1 + V3t2*t2 + V3t3*t3
V3T3 | V3T3t1*t1 + V3t2*t2 + V3t3*t3
V3T4 | V3T4t1*t1 + V3t2*t2 + V3t3*t3

V4T1 | V4T1t1*t1 + V4T1t2*t2 + V4t3*t3
V4T2 | V4T2t1*t1 + V4T2t2*t2 + V4t3*t3
V4T3 | V4T3t1*t1 + V4T3t2*t2 + V4t3*t3
V4T4 | V4T4t1*t1 + V4T4t2*t2 + V4t3*t3

V5T1 | V5T1t1*t1 + V5t2*t2 + V5T1t3*t3
V5T2 | V5T2t1*t1 + V5t2*t2 + V5T2t3*t3
V5T3 | V5T3t1*t1 + V5t2*t2 + V5T3t3*t3
V5T4 | V5T4t1*t1 + V5t2*t2 + V5T4t3*t3

# Intercepts
# Fix all intercepts to zero
V1T1 + V2T1 + V3T1 + V4T1 + V5T1 ~ 0*1
V1T2 + V2T2 + V3T2 + V4T2 + V5T2 ~ 0*1
V1T3 + V2T3 + V3T3 + V4T3 + V5T3 ~ 0*1
V1T4 + V2T4 + V3T4 + V4T4 + V5T4 ~ 0*1

# Unique variances
# Fix unique variances at T1 to 1.00
V1T1 ~~ 1*V1T1
V2T1 ~~ 1*V2T1
V3T1 ~~ 1*V3T1
V4T1 ~~ 1*V4T1
V5T1 ~~ 1*V5T1

# lavaan constrains all indicator unique variances to 1.00 by default
# Free this constraint at T2-T4
V1T2 ~~ NA*V1T2
V2T2 ~~ NA*V2T2
V3T2 ~~ NA*V3T2
V4T2 ~~ NA*V4T2
V5T2 ~~ NA*V5T2

V1T3 ~~ NA*V1T3
V2T3 ~~ NA*V2T3
V3T3 ~~ NA*V3T3
V4T3 ~~ NA*V4T3
V5T3 ~~ NA*V5T3

V1T4 ~~ NA*V1T4
V2T4 ~~ NA*V2T4
V3T4 ~~ NA*V3T4
V4T4 ~~ NA*V4T4
V5T4 ~~ NA*V5T4

# Lagged unique factor covariances
V1T1 ~~ V1T2 + V1T3 + V1T4
V1T2 ~~ V1T3 + V1T4
V1T3 ~~ V1T4

V2T1 ~~ V2T2 + V2T3 + V1T4
V2T2 ~~ V2T3 + V1T4
V2T3 ~~ V1T4

V3T1 ~~ V3T2 + V3T3 + V3T4
V3T2 ~~ V3T3 + V3T4
V3T3 ~~ V3T4

V4T1 ~~ V4T2 + V4T3 + V4T4
V4T2 ~~ V4T3 + V4T4
V4T3 ~~ V4T4

V5T1 ~~ V5T2 + V5T3 + V5T4
V5T2 ~~ V5T3 + V5T4
V5T3 ~~ V5T4
'

Fit.Baseline.Model <- cfa(Baseline.Model, data = ChildFam,
                          ordered=c("V1T1", "V2T1", "V3T1", "V4T1", "V5T1",
                                    "V1T2", "V2T2", "V3T2", "V4T2", "V5T2",
                                    "V1T3", "V2T3", "V3T3", "V4T3", "V5T3",
                                    "V1T4", "V2T4", "V3T4", "V4T4", "V5T4"),
                          parameterization = "theta", estimator = "wlsmv",
                          missing = "pairwise"
                          # Default is missing = "listwise"
)

# Print out parameter estimates and fit statistics
summary(Fit.Baseline.Model, fit.measures = TRUE)

#Supplemental Materials 3B
#lavaan Code for the Empirical Illustration: Loading Invariance Model 

LoadingInv.Model <- ' 
# Latent common factor definitions
# Give factor loadings the same label over time to constrain them to be equal
# The first indicators of each measurement occasion are mentioned twice;
#   the first time freeing the default constraint (1.00),
#   the second time giving them the same label;
C1FAMo =~ NA*V1T1 + V1Loading*V1T1 + V2Loading*V2T1 + 1*V3T1 + V4Loading*V4T1 + V5Loading*V5T1
C2FAMo =~ NA*V1T2 + V1Loading*V1T2 + V2Loading*V2T2 + 1*V3T2 + V4Loading*V4T2 + V5Loading*V5T2 
C3FAMo =~ NA*V1T3 + V1Loading*V1T3 + V2Loading*V2T3 + 1*V3T3 + V4Loading*V4T3 + V5Loading*V5T3
C4FAMo =~ NA*V1T4 + V1Loading*V1T4 + V2Loading*V2T4 + 1*V3T4 + V4Loading*V4T4 + V5Loading*V5T4

# Latent common factor variances and covariances
C1FAMo ~~ C1FAMo + C2FAMo + C3FAMo + C4FAMo
C2FAMo ~~ C2FAMo + C3FAMo + C4FAMo
C3FAMo ~~ C3FAMo + C4FAMo
C4FAMo ~~ C4FAMo

# Latent common factor means
# Fix T1 common factor mean to zero
C1FAMo ~ 0*1
# Freely estimate T2-T4 common factor means
C2FAMo ~ 1
C3FAMo ~ 1
C4FAMo ~ 1

# Thresholds
# t1, t2, and t3 after * represent the first, second, and third thresholds of an indicator, 
#   respectively
# Threshold parameters that are freely estimated have labels VaTbtc, with a representing indicator 
#   number, b representing time, and c representing ordering of the threshold (first, second, 
#   third)
# Threshold parameters that are constrained to be equal over time are given the labels Vatc, with 
#   a representing indicator number and c representing ordering of the threshold (first, second, 
#   third)
V1T1 | V1T1t1*t1 + V1t2*t2 + V1T1t3*t3
V1T2 | V1T2t1*t1 + V1t2*t2 + V1T2t3*t3
V1T3 | V1T3t1*t1 + V1t2*t2 + V1T3t3*t3
V1T4 | V1T4t1*t1 + V1t2*t2 + V1T4t3*t3

V2T1 | V2T1t1*t1 + V2t2*t2 + V2T1t3*t3
V2T2 | V2T2t1*t1 + V2t2*t2 + V2T2t3*t3
V2T3 | V2T3t1*t1 + V2t2*t2 + V2T3t3*t3
V2T4 | V2T4t1*t1 + V2t2*t2 + V2T4t3*t3

V3T1 | V3T1t1*t1 + V3t2*t2 + V3t3*t3
V3T2 | V3T2t1*t1 + V3t2*t2 + V3t3*t3
V3T3 | V3T3t1*t1 + V3t2*t2 + V3t3*t3
V3T4 | V3T4t1*t1 + V3t2*t2 + V3t3*t3

V4T1 | V4T1t1*t1 + V4T1t2*t2 + V4t3*t3
V4T2 | V4T2t1*t1 + V4T2t2*t2 + V4t3*t3
V4T3 | V4T3t1*t1 + V4T3t2*t2 + V4t3*t3
V4T4 | V4T4t1*t1 + V4T4t2*t2 + V4t3*t3

V5T1 | V5T1t1*t1 + V5t2*t2 + V5T1t3*t3
V5T2 | V5T2t1*t1 + V5t2*t2 + V5T2t3*t3
V5T3 | V5T3t1*t1 + V5t2*t2 + V5T3t3*t3
V5T4 | V5T4t1*t1 + V5t2*t2 + V5T4t3*t3

# Intercepts
# Fix all intercepts to zero
V1T1 + V2T1 + V3T1 + V4T1 + V5T1 ~ 0*1
V1T2 + V2T2 + V3T2 + V4T2 + V5T2 ~ 0*1
V1T3 + V2T3 + V3T3 + V4T3 + V5T3 ~ 0*1
V1T4 + V2T4 + V3T4 + V4T4 + V5T4 ~ 0*1

# Unique variances
# Fix unique variances at T1 to 1.00
V1T1 ~~ 1*V1T1
V2T1 ~~ 1*V2T1
V3T1 ~~ 1*V3T1
V4T1 ~~ 1*V4T1
V5T1 ~~ 1*V5T1

# lavaan constrains all the indicator unique variances to be 1.00 by default.
# Free this constraint at T2-T4
V1T2 ~~ NA*V1T2
V2T2 ~~ NA*V2T2
V3T2 ~~ NA*V3T2
V4T2 ~~ NA*V4T2
V5T2 ~~ NA*V5T2

V1T3 ~~ NA*V1T3
V2T3 ~~ NA*V2T3
V3T3 ~~ NA*V3T3
V4T3 ~~ NA*V4T3
V5T3 ~~ NA*V5T3

V1T4 ~~ NA*V1T4
V2T4 ~~ NA*V2T4
V3T4 ~~ NA*V3T4
V4T4 ~~ NA*V4T4
V5T4 ~~ NA*V5T4

# Lagged unique factor covariances
V1T1 ~~ V1T2 + V1T3 + V1T4
V1T2 ~~ V1T3 + V1T4
V1T3 ~~ V1T4

V2T1 ~~ V2T2 + V2T3 + V1T4
V2T2 ~~ V2T3 + V1T4
V2T3 ~~ V1T4

V3T1 ~~ V3T2 + V3T3 + V3T4
V3T2 ~~ V3T3 + V3T4
V3T3 ~~ V3T4

V4T1 ~~ V4T2 + V4T3 + V4T4
V4T2 ~~ V4T3 + V4T4
V4T3 ~~ V4T4

V5T1 ~~ V5T2 + V5T3 + V5T4
V5T2 ~~ V5T3 + V5T4
V5T3 ~~ V5T4
'

Fit.LoadingInv.Model <- cfa(LoadingInv.Model, data = ChildFam,
                            ordered=c("V1T1", "V2T1", "V3T1", "V4T1", "V5T1",
                                      "V1T2", "V2T2", "V3T2", "V4T2", "V5T2",
                                      "V1T3", "V2T3", "V3T3", "V4T3", "V5T3",
                                      "V1T4", "V2T4", "V3T4", "V4T4", "V5T4"),
                            parameterization = "theta", estimator = "wlsmv",
                            missing = "pairwise"
                            # Default is missing = "listwise"
)

# Print out parameter estimates and fit statistics
summary(Fit.LoadingInv.Model, fit.measures = TRUE)

# Request nested model test
anova(Fit.Baseline.Model, Fit.LoadingInv.Model)

# Request modification indices
# The modindices() function returns a data frame
LoadingInv.Model.MI <- modindices(Fit.LoadingInv.Model)

# Select those rows with modification indices greater than 10
LoadingInv.Model.MI[LoadingInv.Model.MI$mi >= 10,]

#Supplemental Materials 3C
#lavaan Code for the Empirical Illustration: Threshold Invariance Model 

ThresholdInv.Model <- ' 
# Latent common factor definitions
# Give factor loadings the same label over time to constrain them to be equal
# The first indicators of each measurement occasion are mentioned twice;
#   the first time freeing the default constraint (1.00),
#   the second time giving them the same label;
C1FAMo =~ NA*V1T1 + V1Loading*V1T1 + V2Loading*V2T1 + 1*V3T1 + V4Loading*V4T1 + V5Loading*V5T1
C2FAMo =~ NA*V1T2 + V1Loading*V1T2 + V2Loading*V2T2 + 1*V3T2 + V4Loading*V4T2 + V5Loading*V5T2 
C3FAMo =~ NA*V1T3 + V1Loading*V1T3 + V2Loading*V2T3 + 1*V3T3 + V4Loading*V4T3 + V5Loading*V5T3
C4FAMo =~ NA*V1T4 + V1Loading*V1T4 + V2Loading*V2T4 + 1*V3T4 + V4Loading*V4T4 + V5Loading*V5T4

# Latent common factor variances and covariances
C1FAMo ~~ C1FAMo + C2FAMo + C3FAMo + C4FAMo
C2FAMo ~~ C2FAMo + C3FAMo + C4FAMo
C3FAMo ~~ C3FAMo + C4FAMo
C4FAMo ~~ C4FAMo

# Latent common factor means
# Fix T1 common factor mean to zero
C1FAMo ~ 0*1
# Freely estimate T2-T4 common factor means
C2FAMo ~ 1
C3FAMo ~ 1
C4FAMo ~ 1

# Thresholds
# t1, t2, and t3 after * represent the first, second, and third thresholds, respectively
# Threshold parameters that are constrained to be equal over time are given the labels Vatc, with 
#   a representing indicator number and c representing ordering of the threshold (first, second, 
#   third)
V1T1 | V1t1*t1 + V1t2*t2 + V1t3*t3
V1T2 | V1t1*t1 + V1t2*t2 + V1t3*t3
V1T3 | V1t1*t1 + V1t2*t2 + V1t3*t3
V1T4 | V1t1*t1 + V1t2*t2 + V1t3*t3

V2T1 | V2t1*t1 + V2t2*t2 + V2t3*t3
V2T2 | V2t1*t1 + V2t2*t2 + V2t3*t3
V2T3 | V2t1*t1 + V2t2*t2 + V2t3*t3
V2T4 | V2t1*t1 + V2t2*t2 + V2t3*t3

V3T1 | V3t1*t1 + V3t2*t2 + V3t3*t3
V3T2 | V3t1*t1 + V3t2*t2 + V3t3*t3
V3T3 | V3t1*t1 + V3t2*t2 + V3t3*t3
V3T4 | V3t1*t1 + V3t2*t2 + V3t3*t3

V4T1 | V4t1*t1 + V4t2*t2 + V4t3*t3
V4T2 | V4t1*t1 + V4t2*t2 + V4t3*t3
V4T3 | V4t1*t1 + V4t2*t2 + V4t3*t3
V4T4 | V4t1*t1 + V4t2*t2 + V4t3*t3

V5T1 | V5t1*t1 + V5t2*t2 + V5t3*t3
V5T2 | V5t1*t1 + V5t2*t2 + V5t3*t3
V5T3 | V5t1*t1 + V5t2*t2 + V5t3*t3
V5T4 | V5t1*t1 + V5t2*t2 + V5t3*t3

# Intercepts
# Fix all intercepts to zero
V1T1 + V2T1 + V3T1 + V4T1 + V5T1 ~ 0*1
V1T2 + V2T2 + V3T2 + V4T2 + V5T2 ~ 0*1
V1T3 + V2T3 + V3T3 + V4T3 + V5T3 ~ 0*1
V1T4 + V2T4 + V3T4 + V4T4 + V5T4 ~ 0*1

# Unique variances
# Fix unique variances at T1 to 1.00
V1T1 ~~ 1*V1T1
V2T1 ~~ 1*V2T1
V3T1 ~~ 1*V3T1
V4T1 ~~ 1*V4T1
V5T1 ~~ 1*V5T1

# lavaan constrains all the indicator unique variances to be 1.00 by default.
# Free this constraint at T2-T4
V1T2 ~~ NA*V1T2
V2T2 ~~ NA*V2T2
V3T2 ~~ NA*V3T2
V4T2 ~~ NA*V4T2
V5T2 ~~ NA*V5T2

V1T3 ~~ NA*V1T3
V2T3 ~~ NA*V2T3
V3T3 ~~ NA*V3T3
V4T3 ~~ NA*V4T3
V5T3 ~~ NA*V5T3

V1T4 ~~ NA*V1T4
V2T4 ~~ NA*V2T4
V3T4 ~~ NA*V3T4
V4T4 ~~ NA*V4T4
V5T4 ~~ NA*V5T4

# Lagged unique factor covariances
V1T1 ~~ V1T2 + V1T3 + V1T4
V1T2 ~~ V1T3 + V1T4
V1T3 ~~ V1T4

V2T1 ~~ V2T2 + V2T3 + V1T4
V2T2 ~~ V2T3 + V1T4
V2T3 ~~ V1T4

V3T1 ~~ V3T2 + V3T3 + V3T4
V3T2 ~~ V3T3 + V3T4
V3T3 ~~ V3T4

V4T1 ~~ V4T2 + V4T3 + V4T4
V4T2 ~~ V4T3 + V4T4
V4T3 ~~ V4T4

V5T1 ~~ V5T2 + V5T3 + V5T4
V5T2 ~~ V5T3 + V5T4
V5T3 ~~ V5T4
'

Fit.ThresholdInv.Model <- cfa(ThresholdInv.Model, data = ChildFam,
                              ordered=c("V1T1", "V2T1", "V3T1", "V4T1", "V5T1",
                                        "V1T2", "V2T2", "V3T2", "V4T2", "V5T2",
                                        "V1T3", "V2T3", "V3T3", "V4T3", "V5T3",
                                        "V1T4", "V2T4", "V3T4", "V4T4", "V5T4"),
                              parameterization = "theta", estimator = "wlsmv",
                              missing = "pairwise"
                              # Default is missing = "listwise"
)

# Print out parameter estimates and fit statistics
summary(Fit.ThresholdInv.Model, fit.measures = TRUE)

# Request nested model test
anova(Fit.LoadingInv.Model, Fit.ThresholdInv.Model)

# Request modification indices
# The modindices() function returns a data frame
ThresholdInv.Model.MI <- modindices(Fit.ThresholdInv.Model)

# Select those rows with modification indices greater than 10
ThresholdInv.Model.MI[ThresholdInv.Model.MI$mi >= 10,]

#Supplemental Materials 3D
#lavaan Code for the Empirical Illustration: Unique Factor Invariance Model 

UniquenessInv.Model <- ' 
# Latent common factor definitions
# Give factor loadings the same label over time to constrain them to be equal
# The first indicators of each measurement occasion are mentioned twice;
#   the first time freeing the default constraint (1.00),
#   the second time giving them the same label;
C1FAMo =~ NA*V1T1 + V1Loading*V1T1 + V2Loading*V2T1 + 1*V3T1 + V4Loading*V4T1 + V5Loading*V5T1
C2FAMo =~ NA*V1T2 + V1Loading*V1T2 + V2Loading*V2T2 + 1*V3T2 + V4Loading*V4T2 + V5Loading*V5T2 
C3FAMo =~ NA*V1T3 + V1Loading*V1T3 + V2Loading*V2T3 + 1*V3T3 + V4Loading*V4T3 + V5Loading*V5T3
C4FAMo =~ NA*V1T4 + V1Loading*V1T4 + V2Loading*V2T4 + 1*V3T4 + V4Loading*V4T4 + V5Loading*V5T4

# Latent common factor variances and covariances
C1FAMo ~~ C1FAMo + C2FAMo + C3FAMo + C4FAMo
C2FAMo ~~ C2FAMo + C3FAMo + C4FAMo
C3FAMo ~~ C3FAMo + C4FAMo
C4FAMo ~~ C4FAMo

# Latent common factor means
# Fix T1 common factor mean to zero
C1FAMo ~ 0*1
# Freely estimate T2-T4 common factor means
C2FAMo ~ 1
C3FAMo ~ 1
C4FAMo ~ 1

# Thresholds
# t1, t2, and t3 after * represent the first, second, and third thresholds, respectively
# Threshold parameters that are constrained to be equal over time are given the labels Vatc, with 
#   a representing indicator number and c representing ordering of the threshold (first, second, 
#   third)
V1T1 | V1t1*t1 + V1t2*t2 + V1t3*t3
V1T2 | V1t1*t1 + V1t2*t2 + V1t3*t3
V1T3 | V1t1*t1 + V1t2*t2 + V1t3*t3
V1T4 | V1t1*t1 + V1t2*t2 + V1t3*t3

V2T1 | V2t1*t1 + V2t2*t2 + V2t3*t3
V2T2 | V2t1*t1 + V2t2*t2 + V2t3*t3
V2T3 | V2t1*t1 + V2t2*t2 + V2t3*t3
V2T4 | V2t1*t1 + V2t2*t2 + V2t3*t3

V3T1 | V3t1*t1 + V3t2*t2 + V3t3*t3
V3T2 | V3t1*t1 + V3t2*t2 + V3t3*t3
V3T3 | V3t1*t1 + V3t2*t2 + V3t3*t3
V3T4 | V3t1*t1 + V3t2*t2 + V3t3*t3

V4T1 | V4t1*t1 + V4t2*t2 + V4t3*t3
V4T2 | V4t1*t1 + V4t2*t2 + V4t3*t3
V4T3 | V4t1*t1 + V4t2*t2 + V4t3*t3
V4T4 | V4t1*t1 + V4t2*t2 + V4t3*t3

V5T1 | V5t1*t1 + V5t2*t2 + V5t3*t3
V5T2 | V5t1*t1 + V5t2*t2 + V5t3*t3
V5T3 | V5t1*t1 + V5t2*t2 + V5t3*t3
V5T4 | V5t1*t1 + V5t2*t2 + V5t3*t3

# Intercepts
# Fix all intercepts to zero
V1T1 + V2T1 + V3T1 + V4T1 + V5T1 ~ 0*1
V1T2 + V2T2 + V3T2 + V4T2 + V5T2 ~ 0*1
V1T3 + V2T3 + V3T3 + V4T3 + V5T3 ~ 0*1
V1T4 + V2T4 + V3T4 + V4T4 + V5T4 ~ 0*1

# Unique variances
# Constrain unique variances to be equal across time
# Since unique variances are fixed to 1.00 at T1, 
# 	essentially this model constrains all unique variance at all occasions to 1.00
V1T1 ~~ 1*V1T1
V2T1 ~~ 1*V2T1
V3T1 ~~ 1*V3T1
V4T1 ~~ 1*V4T1
V5T1 ~~ 1*V5T1

V1T2 ~~ 1*V1T2
V2T2 ~~ 1*V2T2
V3T2 ~~ 1*V3T2
V4T2 ~~ 1*V4T2
V5T2 ~~ 1*V5T2

V1T3 ~~ 1*V1T3
V2T3 ~~ 1*V2T3
V3T3 ~~ 1*V3T3
V4T3 ~~ 1*V4T3
V5T3 ~~ 1*V5T3

V1T4 ~~ 1*V1T4
V2T4 ~~ 1*V2T4
V3T4 ~~ 1*V3T4
V4T4 ~~ 1*V4T4
V5T4 ~~ 1*V5T4

# Lagged unique factor covariances
V1T1 ~~ V1T2 + V1T3 + V1T4
V1T2 ~~ V1T3 + V1T4
V1T3 ~~ V1T4

V2T1 ~~ V2T2 + V2T3 + V1T4
V2T2 ~~ V2T3 + V1T4
V2T3 ~~ V1T4

V3T1 ~~ V3T2 + V3T3 + V3T4
V3T2 ~~ V3T3 + V3T4
V3T3 ~~ V3T4

V4T1 ~~ V4T2 + V4T3 + V4T4
V4T2 ~~ V4T3 + V4T4
V4T3 ~~ V4T4

V5T1 ~~ V5T2 + V5T3 + V5T4
V5T2 ~~ V5T3 + V5T4
V5T3 ~~ V5T4
'

Fit.UniquenessInv.Model <- cfa(UniquenessInv.Model, data = ChildFam,
                               ordered=c("V1T1", "V2T1", "V3T1", "V4T1", "V5T1",
                                         "V1T2", "V2T2", "V3T2", "V4T2", "V5T2",
                                         "V1T3", "V2T3", "V3T3", "V4T3", "V5T3",
                                         "V1T4", "V2T4", "V3T4", "V4T4", "V5T4"),
                               parameterization = "theta", estimator = "wlsmv",
                               missing = "pairwise"
                               # Default is missing = "listwise"
)

# Print out parameter estimates and fit statistics
summary(Fit.UniquenessInv.Model, fit.measures = TRUE)

# Request nested model test
anova(Fit.ThresholdInv.Model, Fit.UniquenessInv.Model)

# Request modification indices
# The modindices() function returns a data frame
UniquenessInv.Model.MI <- modindices(Fit.UniquenessInv.Model)

# Select those rows with modification indices greater than 10
UniquenessInv.Model.MI[UniquenessInv.Model.MI$mi >= 10,] 

