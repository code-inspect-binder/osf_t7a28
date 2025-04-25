# load packages
library(batchtools)
library(mlr)
library(ggplot2)

# load aggregated results from all benchmark analyses
reg <- loadRegistry("mental_risk_registry/")
bmr <- reduceBatchmarkResults(keep.pred = TRUE)

# create variables for analysis conditions
perf <- as.data.table(getBMRAggrPerformances(bmr, as.df = TRUE))
perf[, task.id := as.character(task.id)]
perf[, c("target") := tstrsplit(task.id, "_", keep = 1L)]
perf[, c("predictors") := tstrsplit(task.id, "_", keep = 2L)]
perf[, c("control") := tstrsplit(task.id, "_", keep = 3L)]
perf[, c("design") := tstrsplit(task.id, "_", keep = 4L)]

# compute variables for sd wiskers
perf[, `:=` (lower = rsq.test.mean - rsq.test.sd, upper = rsq.test.mean + rsq.test.sd)]
perf[target != "sickdays", `:=` (lower = sapply(lower, function(x) max(x, -0.05)))]
perf[target == "sickdays", `:=` (lower = sapply(lower, function(x) max(x, -0.25)))]

# create new factor variables
perf[, `:=` (design = factor(design, levels = c("cross", "long"), 
  labels = c("Cross-sectional", "Longitudinal")),
  control = factor(control, levels = c("cov", "nocov"), 
    labels = c("Covariates included", "Covariates excluded")),
  predictors = factor(predictors, levels = c("combined", "items", "sumscores", "only"), 
    labels = c("Combined", "Items", "Sum scores", "only")),
  learner.id = factor(learner.id, levels = c("regr.featureless", "regr.lm", "regr.cvglmnet.tuned", "regr.ranger.mtry.perc.tuned"),
    labels = c("Baseline", "Linear regression", "Elastic net", "Random forest")))]

## plot benchmark results
ggplot(data = as.data.frame(perf[target == "health" & predictors != "only"]), 
  aes(x = predictors, y = rsq.test.mean, color = learner.id)) + 
  geom_pointrange(aes(ymin = lower, ymax = upper), position = position_dodge(width = 0.5)) + 
  facet_grid(design ~ control) + theme_bw() + labs(y = expression(italic(R^2))) + 
  theme(text=element_text(size = 12, family = "Arial")) + 
  theme(legend.position = "bottom", axis.title.x = element_blank(), legend.title = element_blank())

ggplot(data = as.data.frame(perf[target == "strain" & predictors != "only"]), 
  aes(x = predictors, y = rsq.test.mean, color = learner.id)) + 
  geom_pointrange(aes(ymin = lower, ymax = upper), position = position_dodge(width = 0.5)) + 
  facet_grid(design ~ control) + theme_bw() + labs(y = expression(italic(R^2))) + 
  theme(text=element_text(size = 12, family = "Arial")) + 
  theme(legend.position = "bottom", axis.title.x = element_blank(), legend.title = element_blank())

ggplot(data = as.data.frame(perf[target == "turnover" & predictors != "only"]), 
  aes(x = predictors, y = rsq.test.mean, color = learner.id)) + 
  geom_pointrange(aes(ymin = lower, ymax = upper), position = position_dodge(width = 0.5)) + 
  facet_grid(design ~ control) + theme_bw() + labs(y = expression(italic(R^2))) + 
  theme(text=element_text(size = 12, family = "Arial")) + 
  theme(legend.position = "bottom", axis.title.x = element_blank(), legend.title = element_blank())

ggplot(data = as.data.frame(perf[target == "sickdays" & predictors != "only"]), 
  aes(x = predictors, y = rsq.test.mean, color = learner.id)) + 
  geom_pointrange(aes(ymin = lower, ymax = upper), position = position_dodge(width = 0.5)) + 
  facet_grid(design ~ control) + theme_bw() + labs(y = expression(italic(R^2))) + 
  theme(text=element_text(size = 12, family = "Arial")) + 
  theme(legend.position = "bottom", axis.title.x = element_blank(), legend.title = element_blank())

## table with linear regression performance for covariates only models (very similar for other algorithms)
perf[predictors == "only" & learner.id == "Linear regression", 
  .(target, predictors, design, learner.id, rsq.test.mean, rsq.test.sd)]

## table with in and out-of-sample performance
tab <- perf[, .(target, design, control, predictors, learner.id, rsq.train.mean, rsq.test.mean, rsq.test.sd,
  mse.train.mean, mse.test.mean, mse.test.sd)]
tab[, `:=` (design = factor(design, levels = c("Cross-sectional", "Longitudinal"), 
  labels = c("cross", "long")),
  predictors = factor(predictors, levels = c("Combined", "Items", "Sum scores", "only"), 
    labels = c("combined", "items", "sum scores", "only cov")),
  learner.id = factor(learner.id, levels = c("Baseline", "Linear regression", "Elastic net", "Random forest"), 
    labels = c("baseline", "linear regr", "elastic net", "random forest")),
  control = factor(control, levels = c("Covariates included", "Covariates excluded"), 
    labels = c("included", "excluded")))]
tab[order(target, design, control, predictors)]

# increase in rsq when using elastic net with items instead of linear regression with sum scores (without covariates)
tab_delta_rsq <- merge(tab[learner.id == "elastic net" & predictors == "items" & control == "excluded", 
  .(elasticnet_items = rsq.test.mean), by = c("target", "design")], 
  tab[learner.id == "linear regr" & predictors == "sum scores" & control == "excluded", 
    .(linearregr_sumscores = rsq.test.mean), by = c("target", "design")])
tab_delta_rsq[, .(mean_delta_rsq = mean(elasticnet_items - linearregr_sumscores)), by = "design"]

# compare to r-squared increase in seeboth & mottus (2018)
delta_r2 <- c(0.013, 0.076, 0.016, 0.012, 0.015, 0.015, 0.019, 0.039, 0.040, 0.029, 0.010,
  0.007, 0.020, 0.014, 0.011, 0.004, 0.007, 0.003, 0.018, 0.006, 0.011, 0.002,
  0.015, 0.009, 0.012, 0.006, 0.004, 0.013, 0.012, 0.002, 0.016, 0.000, 0.008, 
  0.000, 0.006, 0.001, 0.005, 0.008, -0.001, 0.003)
mean(delta_r2)

# seeboth & mottus (2018) instead report % increase in rsq, which can be misleading
r2_domain <- c(0.145, 0.136, 0.107, 0.106, 0.104, 0.100, 0.099, 0.076, 0.071, 0.063,
  0.055, 0.050, 0.050, 0.048, 0.046, 0.036, 0.028, 0.028, 0.026, 0.025, 0.024, 0.024,
  0.023, 0.022, 0.021, 0.019, 0.018, 0.018, 0.016, 0.016, 0.016, 0.013, 0.011, 0.009, 
  0.008, 0.004, 0.004, 0.003, 0.002, 0.001)  
mean(delta_r2 / r2_domain)
