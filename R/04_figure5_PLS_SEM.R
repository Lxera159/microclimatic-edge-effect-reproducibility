# Reproduce the PLS-SEM analysis and path diagram for Figure 5.
source(file.path("R", "00_project_paths.R"))
output_figure <- file.path(results_dir, "Figure5_PLS_SEM.png")
output_paths <- file.path(results_dir, "Figure5_PLS_SEM_paths.csv")
output_summary <- file.path(results_dir, "Figure5_PLS_SEM_summary.csv")

if (!requireNamespace("plspm", quietly = TRUE)) {
  stop("Please install the 'plspm' package first.")
}
suppressPackageStartupMessages(library(plspm))

# 1. Read and prepare the 93-city dataset.
data <- read.csv(
  data_file,
  stringsAsFactors = FALSE,
  check.names = FALSE
)
variables <- c(
  "WMEE", "IMEE", "CE", "MPA", "FRAC",
  "SHAPE", "PRE", "NTL"
)
if (!all(variables %in% names(data))) {
  stop("Missing columns: ", paste(setdiff(variables, names(data)), collapse = ", "))
}
model_data <- data[, variables]
model_data <- model_data[complete.cases(model_data), , drop = FALSE]

# 2. Measurement model.
blocks <- list(
  MPA = "MPA",
  Fragmentation = c("FRAC", "SHAPE"),
  Precipitation = "PRE",
  NTL = "NTL",
  WMEE = "WMEE",
  IMEE = "IMEE",
  CE = "CE"
)
modes <- rep("A", length(blocks))

# 3. Structural model. Rows are outcomes and columns are predictors.
inner <- rbind(
  MPA           = c(0, 0, 0, 0, 0, 0, 0),
  Fragmentation = c(0, 0, 0, 0, 0, 0, 0),
  Precipitation = c(0, 0, 0, 0, 0, 0, 0),
  NTL           = c(0, 0, 0, 0, 0, 0, 0),
  WMEE          = c(1, 1, 1, 1, 0, 0, 0),
  IMEE          = c(1, 1, 1, 1, 0, 0, 0),
  CE            = c(1, 1, 1, 1, 1, 1, 0)
)
rownames(inner) <- colnames(inner) <- names(blocks)

# 4. Fit PLS-SEM and assess path significance using 2,000 bootstraps.
set.seed(123)
model <- plspm(
  model_data,
  path_matrix = inner,
  blocks = blocks,
  modes = modes,
  scaled = TRUE,
  scheme = "centroid",
  boot.val = TRUE,
  br = 2000
)

bootstrap <- as.data.frame(model$boot$paths)
bootstrap$path <- rownames(bootstrap)
bootstrap$from <- trimws(sub("->.*", "", bootstrap$path))
bootstrap$to <- trimws(sub(".*->", "", bootstrap$path))
bootstrap$significant <- with(
  bootstrap,
  (`perc.025` > 0 & `perc.975` > 0) |
    (`perc.025` < 0 & `perc.975` < 0)
)

coefficient_matrix <- model$path_coefs
coefficient_index <- which(coefficient_matrix != 0, arr.ind = TRUE)
paths <- data.frame(
  to = rownames(coefficient_matrix)[coefficient_index[, 1]],
  from = colnames(coefficient_matrix)[coefficient_index[, 2]],
  coefficient = coefficient_matrix[coefficient_index],
  stringsAsFactors = FALSE
)
paths <- merge(
  paths,
  bootstrap[, c(
    "from", "to", "perc.025", "perc.975", "significant"
  )],
  by = c("from", "to"),
  all.x = TRUE,
  sort = FALSE
)
write.csv(paths, output_paths, row.names = FALSE)

r2 <- model$inner_summary$R2
names(r2) <- rownames(model$inner_summary)
# Figure 5 reports the construct-level GoF:
# sqrt(mean construct AVE) * sqrt(mean endogenous R2).
endogenous_r2 <- r2[c("WMEE", "IMEE", "CE")]
gof <- sqrt(
  mean(model$inner_summary$AVE, na.rm = TRUE) *
    mean(endogenous_r2, na.rm = TRUE)
)
summary_result <- data.frame(
  statistic = c("n", "R2_WMEE", "R2_IMEE", "R2_CE", "GOF"),
  value = c(
    nrow(model_data), r2["WMEE"], r2["IMEE"], r2["CE"], gof
  )
)
write.csv(summary_result, output_summary, row.names = FALSE)

# 5. Draw only paths whose bootstrap 95% CI excludes zero.
positions <- rbind(
  MPA = c(-3.2, 2.25),
  Fragmentation = c(-1.25, 2.25),
  Precipitation = c(0.95, 2.25),
  NTL = c(2.95, 2.25),
  WMEE = c(-0.65, 0.55),
  IMEE = c(1.75, 0.55),
  CE = c(0.60, -1.05)
)
box_widths <- c(
  MPA = 1.25, Fragmentation = 1.85, Precipitation = 1.85,
  NTL = 1.25, WMEE = 1.25, IMEE = 1.25, CE = 1.25
)
box_height <- 0.70

get_path <- function(from, to) {
  paths[paths$from == from & paths$to == to, , drop = FALSE]
}

anchor <- function(node, side) {
  x <- positions[node, 1]
  y <- positions[node, 2]
  half_width <- box_widths[node] / 2
  if (side == "top") return(c(x, y + box_height / 2))
  if (side == "bottom") return(c(x, y - box_height / 2))
  if (side == "left") return(c(x - half_width, y))
  if (side == "right") return(c(x + half_width, y))
  c(x, y)
}

draw_path <- function(from, to, from_side, to_side, route = NULL,
                      label_position = NULL) {
  path <- get_path(from, to)
  if (nrow(path) == 0 || !isTRUE(path$significant[1])) {
    return(invisible(NULL))
  }
  coefficient <- path$coefficient[1]
  colour <- if (coefficient >= 0) "#3F63FF" else "#C00000"
  points <- rbind(anchor(from, from_side), route, anchor(to, to_side))

  if (nrow(points) > 2) {
    for (i in seq_len(nrow(points) - 2)) {
      segments(
        points[i, 1], points[i, 2],
        points[i + 1, 1], points[i + 1, 2],
        col = colour, lwd = 2.6
      )
    }
  }
  arrows(
    points[nrow(points) - 1, 1], points[nrow(points) - 1, 2],
    points[nrow(points), 1], points[nrow(points), 2],
    col = colour, lwd = 2.6, length = 0.11, angle = 25
  )

  if (is.null(label_position)) {
    label_position <- colMeans(
      points[c(nrow(points) - 1, nrow(points)), , drop = FALSE]
    )
  }
  text(
    label_position[1], label_position[2],
    sprintf("%.2f", coefficient),
    cex = 1.20, font = 2
  )
}

draw_node <- function(node, fill, label) {
  x <- positions[node, 1]
  y <- positions[node, 2]
  half_width <- box_widths[node] / 2
  rect(
    x - half_width, y - box_height / 2,
    x + half_width, y + box_height / 2,
    col = fill, border = "black", lwd = 1.5
  )
  text(x, y, label, cex = 1.18, font = 2)
}

png(output_figure, width = 2550, height = 1548, res = 300)
par(mar = c(0.5, 0.5, 0.5, 0.5), xpd = NA)
plot.new()
plot.window(xlim = c(-3.9, 4.05), ylim = c(-2.15, 2.9))

# Significant paths and manually routed labels.
draw_path(
  "MPA", "CE", "bottom", "left",
  route = matrix(c(-3.2, -1.05), ncol = 2),
  label_position = c(-3.2, 0.55)
)
draw_path(
  "Fragmentation", "WMEE", "bottom", "top",
  label_position = c(-1.20, 1.25)
)
draw_path(
  "Fragmentation", "IMEE", "bottom", "top",
  label_position = c(0.65, 1.08)
)
draw_path(
  "Precipitation", "WMEE", "bottom", "top",
  label_position = c(0.02, 1.08)
)
draw_path(
  "Precipitation", "IMEE", "bottom", "top",
  label_position = c(1.47, 1.24)
)
draw_path(
  "Precipitation", "CE", "top", "right",
  route = matrix(
    c(0.95, 2.75, 3.90, 2.75, 3.90, -1.05),
    ncol = 2, byrow = TRUE
  ),
  label_position = c(3.90, 0.55)
)
draw_path(
  "NTL", "IMEE", "bottom", "top",
  label_position = c(2.52, 1.25)
)
draw_path(
  "WMEE", "CE", "bottom", "top",
  label_position = c(-0.25, -0.45)
)
draw_path(
  "IMEE", "CE", "bottom", "top",
  label_position = c(1.35, -0.45)
)

# Nodes.
draw_node("MPA", "#F2C4AA", "MPA")
draw_node("Fragmentation", "#F2C4AA", "Fragmentation")
draw_node("Precipitation", "#F2C4AA", "Precipitation")
draw_node("NTL", "#F2C4AA", "NTL")
draw_node(
  "WMEE", "#CFEBC5",
  bquote(atop("WMEE", R^2 == .(sprintf("%.2f", r2["WMEE"]))))
)
draw_node(
  "IMEE", "#CFEBC5",
  bquote(atop("IMEE", R^2 == .(sprintf("%.2f", r2["IMEE"]))))
)
draw_node(
  "CE", "#BFE3F2",
  bquote(atop("CE", R^2 == .(sprintf("%.2f", r2["CE"]))))
)
text(
  0.60, -1.90,
  sprintf("GOF = %.2f, n = %d", gof, nrow(model_data)),
  cex = 1.25, font = 2
)
dev.off()

message("Saved figure: ", output_figure)
message("Saved paths: ", output_paths)
print(summary_result)
