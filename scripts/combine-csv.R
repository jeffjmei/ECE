library(tidyverse)

src <- path.expand("~/Downloads/results.csv")
if (file.exists(src)) {
  timestamp <- format(Sys.time(), "%Y-%m-%d_%H-%M")
  file.rename(src, paste0("data/raw/results-", timestamp, ".csv"))
}

results <- list.files("data/raw", pattern = "\\.csv$", full.names = TRUE) |>
  map(read_csv, show_col_types = FALSE) |>
  map(\(df) rename_with(df, \(x) str_replace(x, "^run_time$", "runtime"))) |>
  list_rbind()

write_csv(results, "data/results.csv")
message("Written ", nrow(results), " rows to data/results.csv")
