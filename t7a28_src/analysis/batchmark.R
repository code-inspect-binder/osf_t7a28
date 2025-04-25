library(batchtools)

# source predictive modeling objects
source("predictive_modeling.R")

## make batchtools registry
mental_risk_registry <- makeExperimentRegistry(file.dir = "mental_risk_registry/", 
                                               source = "regr.ranger.mtry.perc.R", 
                                               seed = 65485699)

## define batchjobs
batchmark(learners = list(fl_regr,
                          linear_regr,
                          enet_regr, 
                          ranger_regr), 
          tasks = mget(grep("task", ls(), value = TRUE)), 
          resamplings = outer_res, 
          measures = mes_outer_regr, 
          models = FALSE, reg = mental_risk_registry)