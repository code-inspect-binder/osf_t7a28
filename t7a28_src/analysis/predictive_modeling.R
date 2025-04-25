## load packages
library(mlr)

## load dataset
source("preprocessing.R")
## load custom random forest learner
source("regr.ranger.mtry.perc.R")

## make tasks

# cross sectional

# without covariates
task_strain_sumscores_nocov_cross <- makeRegrTask(id = "strain_sumscores_nocov_cross", 
  data = data[i_cross, c(sumscores, "SBF_12")], target = "SBF_12")
task_health_sumscores_nocov_cross <- makeRegrTask(id = "health_sumscores_nocov_cross",
  data = data[i_cross, c(sumscores, "O1_rec")], target = "O1_rec")
task_turnover_sumscores_nocov_cross <- makeRegrTask(id = "turnover_sumscores_nocov_cross",
  data = data[i_cross, c(sumscores, "F5")], target = "F5")
task_sickdays_sumscores_nocov_cross <- makeRegrTask(id = "sickdays_sumscores_nocov_cross",
  data = data[i_cross, c(sumscores, "O2cat_12")], target = "O2cat_12")

task_strain_items_nocov_cross <- makeRegrTask(id = "strain_items_nocov_cross",
  data = data[i_cross, c(items, "SBF_12")], target = "SBF_12")
task_health_items_nocov_cross <- makeRegrTask(id = "health_items_nocov_cross",
  data = data[i_cross, c(items, "O1_rec")], target = "O1_rec")
task_turnover_items_nocov_cross <- makeRegrTask(id = "turnover_items_nocov_cross",
  data = data[i_cross, c(items, "F5")], target = "F5")
task_sickdays_items_nocov_cross <- makeRegrTask(id = "sickdays_items_nocov_cross",
  data = data[i_cross, c(items, "O2cat_12")], target = "O2cat_12")

task_strain_combined_nocov_cross <- makeRegrTask(id = "strain_combined_nocov_cross",
  data = data[i_cross, c(sumscores, items, "SBF_12")], target = "SBF_12")
task_health_combined_nocov_cross <- makeRegrTask(id = "health_combined_nocov_cross",
  data = data[i_cross, c(sumscores, items, "O1_rec")], target = "O1_rec")
task_turnover_combined_nocov_cross <- makeRegrTask(id = "turnover_combined_nocov_cross",
  data = data[i_cross, c(sumscores, items, "F5")], target = "F5")
task_sickdays_combined_nocov_cross <- makeRegrTask(id = "sickdays_combined_nocov_cross",
  data = data[i_cross, c(sumscores, items, "O2cat_12")], target = "O2cat_12")

# with covariates
task_strain_sumscores_cov_cross <- makeRegrTask(id = "strain_sumscores_cov_cross",
  data = data[i_cross, c(covariates_cross, sumscores, "SBF_12")], target = "SBF_12")
task_health_sumscores_cov_cross <- makeRegrTask(id = "health_sumscores_cov_cross",
  data = data[i_cross, c(covariates_cross, sumscores, "O1_rec")], target = "O1_rec")
task_turnover_sumscores_cov_cross <- makeRegrTask(id = "turnover_sumscores_cov_cross",
  data = data[i_cross, c(covariates_cross, sumscores, "F5")], target = "F5")
task_sickdays_sumscores_cov_cross <- makeRegrTask(id = "sickdays_sumscores_cov_cross",
  data = data[i_cross, c(covariates_cross, sumscores, "O2cat_12")], target = "O2cat_12")

task_strain_items_cov_cross <- makeRegrTask(id = "strain_items_cov_cross", 
  data = data[i_cross, c(covariates_cross, items, "SBF_12")], target = "SBF_12")
task_health_items_cov_cross <- makeRegrTask(id = "health_items_cov_cross",
  data = data[i_cross, c(covariates_cross, items, "O1_rec")], target = "O1_rec")
task_turnover_items_cov_cross <- makeRegrTask(id = "turnover_items_cov_cross",
  data = data[i_cross, c(covariates_cross, items, "F5")], target = "F5")
task_sickdays_items_cov_cross <- makeRegrTask(id = "sickdays_items_cov_cross",
  data = data[i_cross, c(covariates_cross, items, "O2cat_12")], target = "O2cat_12")

task_strain_combined_cov_cross <- makeRegrTask(id = "strain_combined_cov_cross",
  data = data[i_cross, c(covariates_cross, sumscores, items, "SBF_12")], target = "SBF_12")
task_health_combined_cov_cross <- makeRegrTask(id = "health_combined_cov_cross",
  data = data[i_cross, c(covariates_cross, sumscores, items, "O1_rec")], target = "O1_rec")
task_turnover_combined_cov_cross <- makeRegrTask(id = "turnover_combined_cov_cross",
  data = data[i_cross, c(covariates_cross, sumscores, items, "F5")], target = "F5")
task_sickdays_combined_cov_cross <- makeRegrTask(id = "sickdays_combined_cov_cross",
  data = data[i_cross, c(covariates_cross, sumscores, items, "O2cat_12")], target = "O2cat_12")

# only covariates
task_strain_only_cov_cross <- makeRegrTask(id = "strain_only_cov_cross",
  data = data[i_cross, c(covariates_cross, "SBF_12")], target = "SBF_12")
task_health_only_cov_cross <- makeRegrTask(id = "health_only_cov_cross",
  data = data[i_cross, c(covariates_cross, "O1_rec")], target = "O1_rec")
task_turnover_only_cov_cross <- makeRegrTask(id = "turnover_only_cov_cross", 
  data = data[i_cross, c(covariates_cross, "F5")], target = "F5")
task_sickdays_only_cov_cross <- makeRegrTask(id = "sickdays_only_cov_cross", 
  data = data[i_cross, c(covariates_cross, "O2cat_12")], target = "O2cat_12")

# longitudinal

# without covariates
task_strain_sumscores_nocov_long <- makeRegrTask(id = "strain_sumscores_nocov_long",
  data = data[i_long_strain_health_turnover, c(sumscores, "SBF_3")], target = "SBF_3")
task_health_sumscores_nocov_long <- makeRegrTask(id = "health_sumscores_nocov_long",
  data = data[i_long_strain_health_turnover, c(sumscores, "O1_3_rec")], target = "O1_3_rec")
task_turnover_sumscores_nocov_long <- makeRegrTask(id = "turnover_sumscores_nocov_long",
  data = data[i_long_strain_health_turnover, c(sumscores, "F5_3")], target = "F5_3")
task_sickdays_sumscores_nocov_long <- makeRegrTask(id = "sickdays_sumscores_nocov_long",
  data = data[i_long_sickdays, c(sumscores, "O2cat_3")], target = "O2cat_3")

task_strain_items_nocov_long <- makeRegrTask(id = "strain_items_nocov_long",
  data = data[i_long_strain_health_turnover, c(items, "SBF_3")], target = "SBF_3")
task_health_items_nocov_long <- makeRegrTask(id = "health_items_nocov_long", 
  data = data[i_long_strain_health_turnover, c(items, "O1_3_rec")], target = "O1_3_rec")
task_turnover_items_nocov_long <- makeRegrTask(id = "turnover_items_nocov_long",
  data = data[i_long_strain_health_turnover, c(items, "F5_3")], target = "F5_3")
task_sickdays_items_nocov_long <- makeRegrTask(id = "sickdays_items_nocov_long",
  data = data[i_long_sickdays, c(items, "O2cat_3")], target = "O2cat_3")

task_strain_combined_nocov_long <- makeRegrTask(id = "strain_combined_nocov_long",
  data = data[i_long_strain_health_turnover, c(sumscores, items, "SBF_3")], target = "SBF_3")
task_health_combined_nocov_long <- makeRegrTask(id = "health_combined_nocov_long",
  data = data[i_long_strain_health_turnover, c(sumscores, items, "O1_3_rec")], target = "O1_3_rec")
task_turnover_combined_nocov_long <- makeRegrTask(id = "turnover_combined_nocov_long",
  data = data[i_long_strain_health_turnover, c(sumscores, items, "F5_3")], target = "F5_3")
task_sickdays_combined_nocov_long <- makeRegrTask(id = "sickdays_combined_nocov_long",
  data = data[i_long_sickdays, c(sumscores, items, "O2cat_3")], target = "O2cat_3")

# with covariates
task_strain_sumscores_cov_long <- makeRegrTask(id = "strain_sumscores_cov_long",
  data = data[i_long_strain_health_turnover, c(covariates_long, sumscores, "SBF_3")], target = "SBF_3")
task_health_sumscores_cov_long <- makeRegrTask(id = "health_sumscores_cov_long",
  data = data[i_long_strain_health_turnover, c(covariates_long, sumscores, "O1_3_rec")], target = "O1_3_rec")
task_turnover_sumscores_cov_long <- makeRegrTask(id = "turnover_sumscores_cov_long",
  data = data[i_long_strain_health_turnover, c(covariates_long, sumscores, "F5_3")], target = "F5_3")
task_sickdays_sumscores_cov_long <- makeRegrTask(id = "sickdays_sumscores_cov_long",
  data = data[i_long_sickdays, c(covariates_long, sumscores, "O2cat_3")], target = "O2cat_3")

task_strain_items_cov_long <- makeRegrTask(id = "strain_items_cov_long",
  data = data[i_long_strain_health_turnover, c(covariates_long, items, "SBF_3")], target = "SBF_3")
task_health_items_cov_long <- makeRegrTask(id = "health_items_cov_long",
  data = data[i_long_strain_health_turnover, c(covariates_long, items, "O1_3_rec")], target = "O1_3_rec")
task_turnover_items_cov_long <- makeRegrTask(id = "turnover_items_cov_long",
  data = data[i_long_strain_health_turnover, c(covariates_long, items, "F5_3")], target = "F5_3")
task_sickdays_items_cov_long <- makeRegrTask(id = "sickdays_items_cov_long",
  data = data[i_long_sickdays, c(covariates_long, items, "O2cat_3")], target = "O2cat_3")

# combined
task_strain_combined_cov_long <- makeRegrTask(id = "strain_combined_cov_long", 
  data = data[i_long_strain_health_turnover, c(covariates_long, sumscores, items, "SBF_3")], target = "SBF_3")
task_health_combined_cov_long <- makeRegrTask(id = "health_combined_cov_long",
  data = data[i_long_strain_health_turnover, c(covariates_long, sumscores, items, "O1_3_rec")], target = "O1_3_rec")
task_turnover_combined_cov_long <- makeRegrTask(id = "turnover_combined_cov_long",
  data = data[i_long_strain_health_turnover, c(covariates_long, sumscores, items, "F5_3")], target = "F5_3")
task_sickdays_combined_cov_long <- makeRegrTask(id = "sickdays_combined_cov_long", 
  data = data[i_long_sickdays, c(covariates_long, sumscores, items, "O2cat_3")], target = "O2cat_3")

# only covariates
task_strain_only_cov_long <- makeRegrTask(id = "strain_only_cov_long", 
  data = data[i_long_strain_health_turnover, c(covariates_long, "SBF_3")], target = "SBF_3")
task_health_only_cov_long <- makeRegrTask(id = "health_only_cov_long",
  data = data[i_long_strain_health_turnover, c(covariates_long, "O1_3_rec")], target = "O1_3_rec")
task_turnover_only_cov_long <- makeRegrTask(id = "turnover_only_cov_long",
  data = data[i_long_strain_health_turnover, c(covariates_long, "F5_3")], target = "F5_3")
task_sickdays_only_cov_long <- makeRegrTask(id = "sickdays_only_cov_long", 
  data = data[i_long_sickdays, c(covariates_long, "O2cat_3")], target = "O2cat_3")

## resample descriptions
inner_res <- makeResampleDesc("CV", iters = 10)
outer_res <- makeResampleDesc("RepCV", reps = 10, folds = 10, predict = "both")

## performance measures
mes_inner_regr <- mse
mes_outer_regr <- list(
  setAggregation(rsq, test.mean),
  setAggregation(rsq, train.mean),
  setAggregation(mse, test.mean),
  setAggregation(mse, train.mean),
  setAggregation(spearmanrho, test.mean),
  setAggregation(spearmanrho, train.mean),
  setAggregation(rsq, test.sd),
  setAggregation(mse, test.sd),
  setAggregation(spearmanrho, test.sd),
  setAggregation(rsq, testgroup.sd),
  setAggregation(mse, testgroup.sd),
  setAggregation(spearmanrho, testgroup.sd),
  timeboth
)

## define learners
# make parameter sets
ranger_ps <- makeParamSet(
  makeNumericParam("mtry.perc", lower = 0, upper = 0.5),
  makeDiscreteParam("min.node.size", values = c(5, 10, 15))
)

enet_ps <- makeParamSet(
  makeNumericParam("alpha", lower = 0, upper = 1)
)

# random Forest ranger
ranger_regr <- makeTuneWrapper(
  makeLearner("regr.ranger.mtry.perc", num.trees = 1000), 
  resampling = inner_res, 
  measures = mes_inner_regr, 
  par.set = ranger_ps, 
  control = makeTuneControlGrid(resolution = 20) # final grid: 20*3=60
)

# elastic net
enet_regr <- makeTuneWrapper(
  makeLearner("regr.cvglmnet", nlambda = 400, s = "lambda.min"),
  resampling = inner_res, 
  measures = mes_inner_regr, 
  par.set = enet_ps, 
  control = makeTuneControlGrid(resolution = 50)
)

# featureless learner
fl_regr <- makeLearner("regr.featureless")

# linear regression
linear_regr <- makeLearner("regr.lm")
