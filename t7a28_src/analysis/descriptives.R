## load dataset
source("preprocessing.R")

## reliability estimates

library(MBESS)

alpha_SAA <- ci.reliability(data[i_cross, items_list$SAA], type = "alpha", conf.level = 0.95, interval.type = "ml")
omega_SAA <- ci.reliability(data[i_cross, items_list$SAA], type = "omega", conf.level = 0.95, interval.type = "ml")
alpha_SFS <- ci.reliability(data[i_cross, items_list$SFS], type = "alpha", conf.level = 0.95, interval.type = "ml")
omega_SFS <- ci.reliability(data[i_cross, items_list$SFS], type = "omega", conf.level = 0.95, interval.type = "ml")
alpha_SKB <- ci.reliability(data[i_cross, items_list$SKB], type = "alpha", conf.level = 0.95, interval.type = "ml")
omega_SKB <- ci.reliability(data[i_cross, items_list$SKB], type = "omega", conf.level = 0.95, interval.type = "ml")
alpha_SAU <- ci.reliability(data[i_cross, items_list$SAU], type = "alpha", conf.level = 0.95, interval.type = "ml")
omega_SAU <- ci.reliability(data[i_cross, items_list$SAU], type = "omega", conf.level = 0.95, interval.type = "ml")
alpha_SINF <- ci.reliability(data[i_cross, items_list$SINF], type = "alpha", conf.level = 0.95, interval.type = "ml")
omega_SINF <- ci.reliability(data[i_cross, items_list$SINF], type = "omega", conf.level = 0.95, interval.type = "ml")
alpha_SSU <- ci.reliability(data[i_cross, items_list$SSU], type = "alpha", conf.level = 0.95, interval.type = "ml")
omega_SSU <- ci.reliability(data[i_cross, items_list$SSU], type = "omega", conf.level = 0.95, interval.type = "ml")
alpha_SUN <- ci.reliability(data[i_cross, items_list$SUN], type = "alpha", conf.level = 0.95, interval.type = "ml")
omega_SUN <- ci.reliability(data[i_cross, items_list$SUN], type = "omega", conf.level = 0.95, interval.type = "ml")
alpha_SHS <- ci.reliability(data[i_cross, items_list$SHS], type = "alpha", conf.level = 0.95, interval.type = "ml")
omega_SHS <- ci.reliability(data[i_cross, items_list$SHS], type = "omega", conf.level = 0.95, interval.type = "ml")
alpha_SIM <- ci.reliability(data[i_cross, items_list$SIM], type = "alpha", conf.level = 0.95, interval.type = "ml")
omega_SIM <- ci.reliability(data[i_cross, items_list$SIM], type = "omega", conf.level = 0.95, interval.type = "ml")
alpha_SRESI <- ci.reliability(data[i_cross, items_list$SRESI], type = "alpha", conf.level = 0.95, interval.type = "ml")
omega_SRESI <- ci.reliability(data[i_cross, items_list$SRESI], type = "omega", conf.level = 0.95, interval.type = "ml")
alpha_SRIS <- ci.reliability(data[i_cross, items_list$SRIS], type = "alpha", conf.level = 0.95, interval.type = "ml")
omega_SRIS <- ci.reliability(data[i_cross, items_list$SRIS], type = "omega", conf.level = 0.95, interval.type = "ml")

## CFA at t1

library(lavaan)

mehq <- "
SAA =~ B12 + B11 + B5 + B7 + B20 + B15 + B13 + B6
SFS =~ G2_10_rec + G2_11_rec + G2_16_rec + G2_8_rec + G2_17_rec
SKB =~ G2_6 + G2_19 + G2_15 + G2_7 + G2_3 + G2_9 + G2_13 + F1 + F13
SAU =~ A5 + A2 + A3 + A6 + A1
SINF =~ E6_rec + E5_rec + E8_rec
SSU =~ G2_4_rec + G2_2_rec + G2_5_rec + G2_1_rec
SUN =~ E2 + E4 + E1
SHS =~ C5_rec + C1 + C2
SIM =~ F7_rec + F14_rec + F9_rec + D3_rec + D2_rec
SRESI =~ H8 + H7 + H1 + H9 + H2
SRIS =~ H11 + H6 + H10 + H5_rec + H4
"

model <- cfa(mehq, data = data[i_cross, items], estimator = "WLSMV", mimic = "Mplus")
summary(model, fit.measures = TRUE, standardized = TRUE)

## outcome correlations

# t1
cor(data[i_cross, c("SBF_12", "O1_rec", "F5", "O2cat_12")], 
  use = "pairwise.complete.obs")
# t2
cor(data[i_long_strain_health_turnover, c("SBF_3", "O1_3_rec", "F5_3", "O2cat_3")], 
  use = "pairwise.complete.obs")
# t1 with t2
cor(data[i_long_strain_health_turnover, c("SBF_12", "O1_rec", "F5", "O2cat_12", "SBF_3", "O1_3_rec", "F5_3", "O2cat_3")], 
  use = "pairwise.complete.obs")

## descriptive statistics covariates
library(data.table)

descr <- as.data.table(data)
# wave 1
descr[i_cross & WAVE == 1, .N] # sample size after exclusion
descr[i_cross & WAVE == 1, sum(Gender == "female")/.N] # proportion of women
descr[i_cross & WAVE == 1, table(Age)/.N] # age distribution
descr[i_cross & WAVE == 1, table(Tenure)/.N] # tenure distribution
descr[i_long_strain_health_turnover & WAVE == 1, median(Duration)] # median duration
# wave 2
descr[i_cross & WAVE == 2, .N] # sample size after exclusion
descr[i_cross & WAVE == 2, sum(Gender == "female")/.N] # proportion of women
descr[i_cross & WAVE == 2, table(Age)/.N] # age distribution
descr[i_cross & WAVE == 2, table(Tenure)/.N] # tenure distribution
descr[i_long_strain_health_turnover & WAVE == 2, median(Duration)] # median duration
# wave 3
descr[i_long_strain_health_turnover, .N] # sample size after exclusion
descr[i_long_strain_health_turnover, sum(Gender == "female")/.N] # proportion of women
descr[i_long_strain_health_turnover, table(Age)/.N] # age distribution
descr[i_long_strain_health_turnover, table(Tenure)/.N] # tenure distribution

## analyse turnover intention of participants who changed jobs 
## (not included in the manuscript)

# only participants included in the main analyses of turnover at t1
data_change_job <- na.omit(data[data$SPSL_W12 == 0, c(items, "F5", "ChangeJob")])
# frequency of job change
table(data_change_job$ChangeJob)
# welsh test
t.test(formula = F5 ~ ChangeJob, data = data_change_job, 
  paired = FALSE, var.equal = FALSE, alternative = "two.sided")
# confidence interval for cohens's d 
library(MBESS)
mu1 <- mean(data_change_job[data_change_job$ChangeJob == 1, "F5"])
mu2 <- mean(data_change_job[data_change_job$ChangeJob == 0, "F5"])
n1 <- length(data_change_job[data_change_job$ChangeJob == 1, "F5"])
n2 <- length(data_change_job[data_change_job$ChangeJob == 0, "F5"])
n <- nrow(data_change_job)
var1 <- var(data_change_job[data_change_job$ChangeJob == 1, "F5"])
var2 <- var(data_change_job[data_change_job$ChangeJob == 0, "F5"])
smd <- (mu1 - mu2) / 
  sqrt(((n1 - 1) * var1 + (n2 - 1) * var2) / (n - 2))
ci.smd(smd = smd, n.1 = n1, n.2 = n2, conf.level = 0.95)
