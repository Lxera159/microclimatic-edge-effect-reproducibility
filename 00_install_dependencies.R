# Install packages required by the reproducibility workflow.
source(file.path("R", "00_project_paths.R"))

project_library <- file.path(project_root, "R_library")
dir.create(project_library, recursive = TRUE, showWarnings = FALSE)
.libPaths(c(project_library, .libPaths()))

cran_packages <- c(
  "ggplot2", "mgcv", "patchwork", "plspm", "scales", "viridis"
)
missing_packages <- cran_packages[
  !vapply(cran_packages, requireNamespace, logical(1), quietly = TRUE)
]
if (length(missing_packages) > 0) {
  install.packages(
    missing_packages,
    repos = "https://cloud.r-project.org",
    lib = project_library
  )
}

# The archived PLSR results were generated with pls 2.8-5.
required_pls_version <- "2.8.5"
pls_available <- requireNamespace("pls", quietly = TRUE)
pls_version_ok <- pls_available &&
  as.character(packageVersion("pls")) == required_pls_version

if (!pls_version_ok) {
  options(timeout = max(600, getOption("timeout")))
  if ("pls" %in% loadedNamespaces()) {
    unloadNamespace("pls")
  }
  install.packages(
    paste0(
      "https://cran.r-project.org/src/contrib/Archive/pls/",
      "pls_2.8-5.tar.gz"
    ),
    repos = NULL,
    type = "source",
    lib = project_library
  )
}

if ("pls" %in% loadedNamespaces()) {
  unloadNamespace("pls")
}
.libPaths(c(project_library, .libPaths()))
if (!requireNamespace("pls", quietly = TRUE) ||
    as.character(packageVersion("pls")) != required_pls_version) {
  stop("Installation of pls 2.8-5 failed.")
}

message("Dependencies are ready in: ", project_library)
