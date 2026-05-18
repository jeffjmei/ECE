library(ECE)
library(optparse)

option_list <- list(
  make_option("--task_id", type = "integer"),
  make_option("--config",  type = "character"),
  make_option("--n_sims",  type = "integer", default = 1000),
  make_option("--outdir",  type = "character", default = "data/")
)
opt <- parse_args(OptionParser(option_list = option_list))

config_grid <- read.csv(opt$config)
config_row  <- config_grid[opt$task_id, ]

result <- run_sim(config_row, N = opt$n_sims)
save_sim(result, file.path(opt$outdir, "results.csv"))
