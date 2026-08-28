# Shared project paths for all analysis scripts.
locate_project_root <- function() {
  candidates <- c(getwd(), dirname(getwd()))

  source_file <- tryCatch(
    normalizePath(sys.frame(1)$ofile, mustWork = TRUE),
    error = function(e) NA_character_
  )
  if (!is.na(source_file)) {
    candidates <- c(dirname(dirname(source_file)), candidates)
  }

  candidates <- unique(normalizePath(
    candidates, winslash = "/", mustWork = FALSE
  ))
  marker <- file.path("data", "city_level_MEE_climate.csv")
  matches <- candidates[file.exists(file.path(candidates, marker))]
  if (length(matches) == 0) {
    stop(
      "Cannot locate the project root. Open the project directory or run ",
      "run_all.R from the package root."
    )
  }
  matches[1]
}

project_root <- locate_project_root()
project_library <- file.path(project_root, "R_library")
if (dir.exists(project_library)) {
  .libPaths(c(project_library, .libPaths()))
}
data_file <- file.path(
  project_root, "data", "city_level_MEE_climate.csv"
)
results_dir <- file.path(project_root, "results")
dir.create(results_dir, recursive = TRUE, showWarnings = FALSE)
