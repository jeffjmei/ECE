library(tidyverse)

combine_raw <- function(raw_dir, out_file, download_name) {
  src <- path.expand(file.path("~/Downloads", download_name))
  if (file.exists(src)) {
    timestamp <- format(Sys.time(), "%Y-%m-%d_%H-%M")
    dest_name <- sub("\\.csv$", paste0("-", timestamp, ".csv"), download_name)
    file.rename(src, file.path(raw_dir, dest_name))
  }

  result <- list.files(raw_dir, pattern = "\\.csv$", full.names = TRUE) |>
    map(read_csv, show_col_types = FALSE) |>
    map(\(df) rename_with(df, \(x) str_replace(x, "^run_time$", "runtime"))) |>
    list_rbind()

  write_csv(result, out_file)
  message("Written ", nrow(result), " rows to ", out_file)
}

combine_raw("data/raw",    "data/results.csv",    "results.csv")
combine_raw("data/raw-t1", "data/results-t1.csv", "results-t1.csv")
