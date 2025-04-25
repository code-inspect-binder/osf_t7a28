## source predictive modeling objects
source("predictive_modeling.R")

## final elastic net models

set.seed(1)

# with covariates
enet_strain_items_cov_cross <- train(enet_regr, task_strain_items_cov_cross)
enet_health_items_cov_cross <- train(enet_regr, task_health_items_cov_cross)
enet_turnover_items_cov_cross <- train(enet_regr, task_turnover_items_cov_cross)
enet_sickdays_items_cov_cross <- train(enet_regr, task_sickdays_items_cov_cross)

enet_strain_items_cov_long <- train(enet_regr, task_strain_items_cov_long)
enet_health_items_cov_long <- train(enet_regr, task_health_items_cov_long)
enet_turnover_items_cov_long <- train(enet_regr, task_turnover_items_cov_long)
enet_sickdays_items_cov_long <- train(enet_regr, task_sickdays_items_cov_long)

enet_strain_sumscores_cov_cross <- train(enet_regr, task_strain_sumscores_cov_cross)
enet_health_sumscores_cov_cross <- train(enet_regr, task_health_sumscores_cov_cross)
enet_turnover_sumscores_cov_cross <- train(enet_regr, task_turnover_sumscores_cov_cross)
enet_sickdays_sumscores_cov_cross <- train(enet_regr, task_sickdays_sumscores_cov_cross)

enet_strain_sumscores_cov_long <- train(enet_regr, task_strain_sumscores_cov_long)
enet_health_sumscores_cov_long <- train(enet_regr, task_health_sumscores_cov_long)
enet_turnover_sumscores_cov_long <- train(enet_regr, task_turnover_sumscores_cov_long) 
enet_sickdays_sumscores_cov_long <- train(enet_regr, task_sickdays_sumscores_cov_long)

# without covariates
enet_strain_items_nocov_cross <- train(enet_regr, task_strain_items_nocov_cross)
enet_health_items_nocov_cross <- train(enet_regr, task_health_items_nocov_cross)
enet_turnover_items_nocov_cross <- train(enet_regr, task_turnover_items_nocov_cross)
enet_sickdays_items_nocov_cross <- train(enet_regr, task_sickdays_items_nocov_cross)

enet_strain_items_nocov_long <- train(enet_regr, task_strain_items_nocov_long)
enet_health_items_nocov_long <- train(enet_regr, task_health_items_nocov_long)
enet_turnover_items_nocov_long <- train(enet_regr, task_turnover_items_nocov_long)
enet_sickdays_items_nocov_long <- train(enet_regr, task_sickdays_items_nocov_long)

enet_strain_sumscores_nocov_cross <- train(enet_regr, task_strain_sumscores_nocov_cross)
enet_health_sumscores_nocov_cross <- train(enet_regr, task_health_sumscores_nocov_cross)
enet_turnover_sumscores_nocov_cross <- train(enet_regr, task_turnover_sumscores_nocov_cross)
enet_sickdays_sumscores_nocov_cross <- train(enet_regr, task_sickdays_sumscores_nocov_cross)

enet_strain_sumscores_nocov_long <- train(enet_regr, task_strain_sumscores_nocov_long)
enet_health_sumscores_nocov_long <- train(enet_regr, task_health_sumscores_nocov_long)
enet_turnover_sumscores_nocov_long <- train(enet_regr, task_turnover_sumscores_nocov_long) 
enet_sickdays_sumscores_nocov_long <- train(enet_regr, task_sickdays_sumscores_nocov_long)

# save model objects to disc (DO NOT ACCIDENTIALLY OVERWRITE THIS)
# saveRDS(mget(setdiff(grep("enet", ls(), value = TRUE), c("enet_ps", "enet_regr"))), file = "final_enet_models.rds")

# coefficient tables

# cross sectional

coefs_items_cov_cross <- cbind(
  coef(getLearnerModel(enet_strain_items_cov_cross, more.unwrap = TRUE)),
  coef(getLearnerModel(enet_health_items_cov_cross, more.unwrap = TRUE)),
  coef(getLearnerModel(enet_turnover_items_cov_cross, more.unwrap = TRUE)),
  coef(getLearnerModel(enet_sickdays_items_cov_cross, more.unwrap = TRUE))
) 
colnames(coefs_items_cov_cross) <- c("strain", "health", "turnover", "sickdays")

coefs_sumscores_cov_cross <- cbind(
  coef(getLearnerModel(enet_strain_sumscores_cov_cross, more.unwrap = TRUE)),
  coef(getLearnerModel(enet_health_sumscores_cov_cross, more.unwrap = TRUE)),
  coef(getLearnerModel(enet_turnover_sumscores_cov_cross, more.unwrap = TRUE)),
  coef(getLearnerModel(enet_sickdays_sumscores_cov_cross, more.unwrap = TRUE))
) 
colnames(coefs_sumscores_cov_cross) <- c("strain", "health", "turnover", "sickdays")

coefs_items_nocov_cross <- cbind(
  coef(getLearnerModel(enet_strain_items_nocov_cross, more.unwrap = TRUE)),
  coef(getLearnerModel(enet_health_items_nocov_cross, more.unwrap = TRUE)),
  coef(getLearnerModel(enet_turnover_items_nocov_cross, more.unwrap = TRUE)),
  coef(getLearnerModel(enet_sickdays_items_nocov_cross, more.unwrap = TRUE))
) 
colnames(coefs_items_nocov_cross) <- c("strain", "health", "turnover", "sickdays")

coefs_sumscores_nocov_cross <- cbind(
  coef(getLearnerModel(enet_strain_sumscores_nocov_cross, more.unwrap = TRUE)),
  coef(getLearnerModel(enet_health_sumscores_nocov_cross, more.unwrap = TRUE)),
  coef(getLearnerModel(enet_turnover_sumscores_nocov_cross, more.unwrap = TRUE)),
  coef(getLearnerModel(enet_sickdays_sumscores_nocov_cross, more.unwrap = TRUE))
) 
colnames(coefs_sumscores_nocov_cross) <- c("strain", "health", "turnover", "sickdays")

# longitudinal

coefs_items_cov_long <- cbind(
  coef(getLearnerModel(enet_strain_items_cov_long, more.unwrap = TRUE)),
  coef(getLearnerModel(enet_health_items_cov_long, more.unwrap = TRUE)),
  coef(getLearnerModel(enet_turnover_items_cov_long, more.unwrap = TRUE)),
  coef(getLearnerModel(enet_sickdays_items_cov_long, more.unwrap = TRUE))
) 
colnames(coefs_items_cov_long) <- c("strain", "health", "turnover", "sickdays")

coefs_sumscores_cov_long <- cbind(
  coef(getLearnerModel(enet_strain_sumscores_cov_long, more.unwrap = TRUE)),
  coef(getLearnerModel(enet_health_sumscores_cov_long, more.unwrap = TRUE)),
  coef(getLearnerModel(enet_turnover_sumscores_cov_long, more.unwrap = TRUE)),
  coef(getLearnerModel(enet_sickdays_sumscores_cov_long, more.unwrap = TRUE))
) 
colnames(coefs_sumscores_cov_long) <- c("strain", "health", "turnover", "sickdays")

coefs_items_nocov_long <- cbind(
  coef(getLearnerModel(enet_strain_items_nocov_long, more.unwrap = TRUE)),
  coef(getLearnerModel(enet_health_items_nocov_long, more.unwrap = TRUE)),
  coef(getLearnerModel(enet_turnover_items_nocov_long, more.unwrap = TRUE)),
  coef(getLearnerModel(enet_sickdays_items_nocov_long, more.unwrap = TRUE))
) 
colnames(coefs_items_nocov_long) <- c("strain", "health", "turnover", "sickdays")

coefs_sumscores_nocov_long <- cbind(
  coef(getLearnerModel(enet_strain_sumscores_nocov_long, more.unwrap = TRUE)),
  coef(getLearnerModel(enet_health_sumscores_nocov_long, more.unwrap = TRUE)),
  coef(getLearnerModel(enet_turnover_sumscores_nocov_long, more.unwrap = TRUE)),
  coef(getLearnerModel(enet_sickdays_sumscores_nocov_long, more.unwrap = TRUE))
)
colnames(coefs_sumscores_nocov_long) <- c("strain", "health", "turnover", "sickdays")

