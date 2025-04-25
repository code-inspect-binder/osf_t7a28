cluster.functions <- batchtools::makeClusterFunctionsSlurm("slurm-lrz")

default.resources <- list(walltime = 900L, 
                          memory = 1000L, 
                          ntasks = 1L, 
                          ncpus = 1L, 
                          nodes = 1L, 
                          clusters = "serial", 
                          partition = "serial_mpp2")

max.concurrent.jobs <- 1000L
