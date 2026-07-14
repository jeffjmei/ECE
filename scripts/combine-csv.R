library(tidyverse)

archive_raw <- function(raw_dir) {
  timestamp <- format(Sys.time(), "%Y-%m-%d_%H-%M-%S")
  dest <- file.path("data/archive", paste0(basename(raw_dir), "-", timestamp))
  dir.create(dest, recursive = TRUE)
  files <- list.files(raw_dir, pattern = "\\.csv$", full.names = TRUE)
  file.copy(files, dest)
  message("Backed up ", length(files), " files from ", raw_dir, " to ", dest)
}

split_by_method <- function(result, raw_dir, prefix) {
  stopifnot("method" %in% names(result), !anyNA(result$method))

  tmp_dir <- file.path(tempfile("split-"))
  dir.create(tmp_dir)

  result |>
    group_by(method) |>
    group_walk(\(df, key) {
      slug <- str_replace_all(key$method, "\\.", "-")
      write_csv(df, file.path(tmp_dir, paste0(prefix, "-", slug, ".csv")))
    }, .keep = TRUE)

  old_files <- list.files(raw_dir, pattern = "\\.csv$", full.names = TRUE)
  file.remove(old_files)
  file.copy(list.files(tmp_dir, full.names = TRUE), raw_dir)
  unlink(tmp_dir, recursive = TRUE)

  message("Split ", raw_dir, " into ", n_distinct(result$method), " per-method files")
}

combine_raw <- function(raw_dir, out_file, download_name, prefix) {
  src <- path.expand(file.path("~/Downloads", download_name))
  if (file.exists(src)) {
    timestamp <- format(Sys.time(), "%Y-%m-%d_%H-%M")
    dest_name <- sub("\\.csv$", paste0("-", timestamp, ".csv"), download_name)
    file.rename(src, file.path(raw_dir, dest_name))
  }

  archive_raw(raw_dir)

  result <- list.files(raw_dir, pattern = "\\.csv$", full.names = TRUE) |>
    map(read_csv, show_col_types = FALSE) |>
    map(\(df) rename_with(df, \(x) str_replace(x, "^run_time$", "runtime"))) |>
    list_rbind()

  write_csv(result, out_file)
  message("Written ", nrow(result), " rows to ", out_file)

  split_by_method(result, raw_dir, prefix)
}

combine_raw("data/raw",    "data/results.csv",    "results.csv",    "results")
combine_raw("data/raw-t1", "data/results-t1.csv", "results-t1.csv", "results-t1")
