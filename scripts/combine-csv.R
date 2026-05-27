library(tidyverse)

results <- list.files("data/raw", pattern = "\\.csv$", full.names = TRUE) |>
  map(read_csv, show_col_types = FALSE) |>
  map(\(df) rename_with(df, \(x) str_replace(x, "^run_time$", "runtime"))) |>
  list_rbind()

write_csv(results, "data/results.csv")
message("Written ", nrow(results), " rows to data/results.csv")
