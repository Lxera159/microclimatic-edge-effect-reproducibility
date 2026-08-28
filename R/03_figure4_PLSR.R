# Figure 4: climate-specific PLSR analysis and figure generation.
#
# Run 00_install_dependencies.R once before running this script.

source(file.path("R", "00_project_paths.R"))
project_library <- file.path(project_root, "R_library")
output_file <- file.path(results_dir, "Figure4_PLSR_coefficients.csv")
component_file <- file.path(results_dir, "Figure4_PLSR_components.csv")

.libPaths(c(project_library, .libPaths()))
if (!requireNamespace("pls", quietly = TRUE)) {
  stop("Package 'pls' is unavailable. Run 00_install_dependencies.R first.")
}
if (as.character(packageVersion("pls")) != "2.8.5") {
  stop(
    "This reproduction requires pls 2.8-5; found ",
    as.character(packageVersion("pls")), "."
  )
}
suppressPackageStartupMessages(library(pls))

data <- read.csv(
  data_file,
  stringsAsFactors = FALSE,
  check.names = FALSE
)

zones <- c("Arid", "Cold", "Temperate")
targets <- c("WMEE", "IMEE")
predictors <- c(
  "TMP", "PRE", "SHAPE", "MCH",
  "WS", "NTL", "FRAC"
)

# Component counts used in the final PLSR models.
components <- data.frame(
  climate_zone = c(
    "Arid", "Arid", "Cold", "Cold", "Temperate", "Temperate"
  ),
  target = c("WMEE", "IMEE", "WMEE", "IMEE", "WMEE", "IMEE"),
  ncomp = c(4L, 7L, 1L, 2L, 5L, 4L)
)
write.csv(components, component_file, row.names = FALSE)

zscore <- function(x) {
  standard_deviation <- sd(x, na.rm = TRUE)
  if (!is.finite(standard_deviation) || standard_deviation == 0) {
    return(rep(NA_real_, length(x)))
  }
  (x - mean(x, na.rm = TRUE)) / standard_deviation
}

vip_scores <- function(model, ncomp) {
  scores <- model$scores[, seq_len(ncomp), drop = FALSE]
  weights <- model$loading.weights[, seq_len(ncomp), drop = FALSE]
  y_loadings <- as.numeric(model$Yloadings)[seq_len(ncomp)]
  explained_ss <- colSums(scores^2) * y_loadings^2
  normalized_weights <- sweep(
    weights^2,
    2,
    colSums(weights^2),
    "/"
  )
  vip <- sqrt(
    nrow(weights) *
      rowSums(normalized_weights * rep(explained_ss, each = nrow(weights))) /
      sum(explained_ss)
  )
  names(vip) <- rownames(weights)
  vip
}

result_rows <- list()
row_index <- 1

for (zone in zones) {
  # This joint complete-case filtering is part of the original workflow.
  zone_data <- data[
    data$climate_zone == zone,
    c(targets, predictors),
    drop = FALSE
  ]
  zone_data <- zone_data[complete.cases(zone_data), , drop = FALSE]

  for (target in targets) {
    model_data <- zone_data
    model_data[[target]] <- zscore(model_data[[target]])
    for (predictor in predictors) {
      model_data[[predictor]] <- zscore(model_data[[predictor]])
    }

    ncomp <- components$ncomp[
      components$climate_zone == zone &
        components$target == target
    ]

    model <- plsr(
      reformulate(predictors, response = target),
      data = model_data,
      ncomp = ncomp,
      validation = "none",
      scale = FALSE,
      center = FALSE,
      method = "oscorespls"
    )

    beta <- drop(coef(model, ncomp = ncomp))
    vip <- vip_scores(model, ncomp)
    beta <- beta[predictors]
    vip <- vip[predictors]
    denominator <- sum(abs(beta))

    result_rows[[row_index]] <- data.frame(
      climate_zone = zone,
      target = target,
      term = predictors,
      vip = as.numeric(vip),
      beta_std = as.numeric(beta),
      direction = ifelse(beta > 0, "+", ifelse(beta < 0, "-", "0")),
      contribution_pct = abs(beta) / denominator * 100,
      stringsAsFactors = FALSE
    )
    row_index <- row_index + 1
  }
}

result <- do.call(rbind, result_rows)
result <- result[
  order(result$climate_zone, result$target, -result$vip),
]
rownames(result) <- NULL
write.csv(result, output_file, row.names = FALSE, fileEncoding = "UTF-8")

message("Saved: ", output_file)
message("Using pls ", as.character(packageVersion("pls")))

# Draw the figures directly as PNG files (no PowerPoint output).
plot_packages <- c("ggplot2", "patchwork")
missing_plot_packages <- plot_packages[
  !vapply(plot_packages, requireNamespace, logical(1), quietly = TRUE)
]
if (length(missing_plot_packages) > 0) {
  stop("Please install: ", paste(missing_plot_packages, collapse = ", "))
}

plot_data <- result
plot_data$VIP_percent <- ave(
  plot_data$vip,
  plot_data$climate_zone,
  plot_data$target,
  FUN = function(x) x / sum(x) * 100
)

term_order <- c(
  "PRE", "TMP", "WS",
  "SHAPE", "FRAC", "MCH", "NTL"
)
name_map <- c(
  PRE = "PRE", TMP = "TMP", WS = "WS",
  FRAC = "FRAC", SHAPE = "SHAPE", MCH = "MCH", NTL = "NTL"
)
term_colours <- c(
  PRE = "#9FCAD9", TMP = "#1BB7E2",
  WS = "#4E96C7", SHAPE = "#B9F2C9",
  FRAC = "#6EDFA3", MCH = "#2FBF71", NTL = "#FFC13A"
)
group_colours <- c(
  "Climatic background" = "#1BB7E2",
  "Landscape morphology" = "#6EDFA3",
  "Anthropogenic intensity" = "#FFC13A"
)
zone_colours <- c(
  Temperate = "#12A4A6", Arid = "#F3D338", Cold = "#4F81BD"
)
group_map <- c(
  PRE = "Climatic background",
  TMP = "Climatic background",
  WS = "Climatic background",
  FRAC = "Landscape morphology",
  SHAPE = "Landscape morphology",
  MCH = "Landscape morphology",
  NTL = "Anthropogenic intensity"
)
plot_data$group <- unname(group_map[plot_data$term])

make_pie <- function(zone, target) {
  x <- plot_data[
    plot_data$climate_zone == zone & plot_data$target == target,
  ]
  x$term <- factor(x$term, levels = term_order)
  x <- x[order(x$term), ]
  x$mid <- 100 - (cumsum(x$VIP_percent) - x$VIP_percent / 2)
  x$label <- name_map[as.character(x$term)]

  grouped <- aggregate(VIP_percent ~ group, x, sum)
  grouped$group <- factor(grouped$group, levels = names(group_colours))
  grouped <- grouped[order(grouped$group), ]
  grouped$xmax <- cumsum(grouped$VIP_percent)
  grouped$xmin <- c(0, head(grouped$xmax, -1))
  grouped$xmid <- (grouped$xmin + grouped$xmax) / 2
  grouped$label <- c(
    "Climatic\nbackground",
    "Landscape\nmorphology",
    "Anthropogenic\nintensity"
  )

  pie <- ggplot2::ggplot(
    x,
    ggplot2::aes(x = 1, y = VIP_percent, fill = term)
  ) +
    ggplot2::geom_col(width = 2, colour = "white", linewidth = 1) +
    ggplot2::coord_polar(theta = "y") +
    ggplot2::geom_text(
      ggplot2::aes(label = sprintf("%.1f%%", VIP_percent)),
      position = ggplot2::position_stack(vjust = 0.5),
      size = 3
    ) +
    ggplot2::geom_text(
      ggplot2::aes(x = 1.85, y = mid, label = label),
      size = 3.3
    ) +
    ggplot2::xlim(0, 2) +
    ggplot2::scale_fill_manual(values = term_colours) +
    ggplot2::labs(title = zone) +
    ggplot2::theme_void() +
    ggplot2::theme(
      legend.position = "none",
      plot.title = ggplot2::element_text(
        face = "bold", hjust = 0.5, size = 12
      )
    )

  group_bar <- ggplot2::ggplot(grouped) +
    ggplot2::geom_rect(
      ggplot2::aes(
        xmin = xmin, xmax = xmax, ymin = 0, ymax = 0.28, fill = group
      ),
      colour = "white"
    ) +
    ggplot2::geom_text(
      ggplot2::aes(
        x = xmid, y = 0.42, label = sprintf("%.1f%%", VIP_percent)
      ),
      size = 3
    ) +
    ggplot2::geom_text(
      ggplot2::aes(x = xmid, y = -0.13, label = label),
      size = 2.8
    ) +
    ggplot2::scale_fill_manual(values = group_colours) +
    ggplot2::coord_cartesian(
      xlim = c(0, 100), ylim = c(-0.3, 0.55), clip = "off"
    ) +
    ggplot2::theme_void() +
    ggplot2::theme(legend.position = "none")

  patchwork::wrap_plots(
    pie, group_bar, ncol = 1, heights = c(3.2, 1)
  )
}

make_figure <- function(target) {
  src <- plot_data[plot_data$target == target, ]
  src$term <- factor(
    src$term,
    levels = c(
      "PRE", "TMP", "WS",
      "FRAC", "SHAPE", "MCH", "NTL"
    )
  )
  src$climate_zone <- factor(
    src$climate_zone,
    levels = c("Temperate", "Arid", "Cold")
  )
  y_limits <- if (target == "WMEE") c(-0.3, 0.9) else c(-1.2, 1.2)

  src_plot <- ggplot2::ggplot(
    src,
    ggplot2::aes(x = term, y = beta_std, fill = climate_zone)
  ) +
    ggplot2::geom_col(
      position = ggplot2::position_dodge(0.78),
      width = 0.72
    ) +
    ggplot2::geom_hline(yintercept = 0, linewidth = 0.5) +
    ggplot2::scale_x_discrete(labels = name_map) +
    ggplot2::scale_fill_manual(values = zone_colours) +
    ggplot2::coord_cartesian(ylim = y_limits) +
    ggplot2::labs(x = NULL, y = "SRC", fill = NULL) +
    ggplot2::theme_classic(base_size = 13) +
    ggplot2::theme(
      legend.position = c(0.88, 0.82),
      panel.border = ggplot2::element_rect(fill = NA, colour = "black")
    )

  pies <- patchwork::wrap_plots(
    make_pie("Temperate", target),
    make_pie("Arid", target),
    make_pie("Cold", target),
    nrow = 1
  )
  patchwork::wrap_plots(
    pies, src_plot, ncol = 1, heights = c(1.25, 1)
  )
}

for (target in targets) {
  ggplot2::ggsave(
    file.path(results_dir, paste0("Figure4_PLSR_", target, ".png")),
    make_figure(target),
    width = 10,
    height = 8,
    dpi = 300
  )
}

message("Saved Figure 4 outputs in: ", results_dir)
