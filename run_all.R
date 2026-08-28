# Run all analyses and regenerate Figures 3-5.
source(file.path("R", "00_project_paths.R"))

scripts <- c(
  "01_figure3_linear_regression.R",
  "02_figure3_GAM.R",
  "03_figure4_PLSR.R",
  "04_figure5_PLS_SEM.R"
)

for (script in scripts) {
  message("\nRunning ", script, " ...")
  source(file.path(project_root, "R", script), local = new.env())
}

writeLines(
  capture.output(sessionInfo()),
  file.path(results_dir, "session_info.txt")
)
message("\nAll analyses completed. Results are in: ", results_dir)
