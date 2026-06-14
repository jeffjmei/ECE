library(ECE)
library(optparse)

option_list <- list(
  make_option("--task_id", type = "integer"),
  make_option("--config",  type = "character"),
  make_option("--outdir",  type = "character", default = "data/"),
  make_option("--cpus",    type = "integer",   default = 1L)
)
opt <- parse_args(OptionParser(option_list = option_list))

mirai::daemons(opt$cpus)
mirai::everywhere(library(ECE))

config_grid <- read.csv(opt$config)
config_row  <- config_grid[opt$task_id, ]

result <- run_sim_t1(config_row, N = config_row$n_sims)
save_sim(result, file.path(opt$outdir, "results-t1.csv"))
