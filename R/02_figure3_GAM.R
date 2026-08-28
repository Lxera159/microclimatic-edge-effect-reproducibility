# Figure 3c-f: GAM analysis of the joint WMEE-IMEE effects on CE.
source(file.path("R", "00_project_paths.R"))
suppressPackageStartupMessages({
  library(mgcv)
  library(ggplot2)
  library(viridis)
})

d <- read.csv(data_file, stringsAsFactors = FALSE, check.names = FALSE)
d <- d[d$climate_zone %in% c("Temperate", "Arid", "Cold"), ]
d <- d[complete.cases(d[, c("WMEE", "IMEE", "CE", "climate_zone")]), ]

groups <- list(
  All = d,
  Temperate = d[d$climate_zone == "Temperate", ],
  Arid = d[d$climate_zone == "Arid", ],
  Cold = d[d$climate_zone == "Cold", ]
)

models <- list()
stats <- list()
surfaces <- list()

for (name in names(groups)) {
  x <- groups[[name]]
  k <- if (name == "All") c(6, 6) else c(3, 3)
  model <- gam(CE ~ te(WMEE, IMEE, k = k), data = x, method = "REML")
  fitted_ce <- predict(model, newdata = x)
  sm <- summary(model)

  models[[name]] <- model
  stats[[name]] <- data.frame(
    group = name,
    n = nrow(x),
    r_squared = sm$r.sq,
    rmse = sqrt(mean((x$CE - fitted_ce)^2)),
    deviance_explained = sm$dev.expl,
    smooth_p_value = sm$s.table[1, "p-value"]
  )

  grid <- expand.grid(
    WMEE = seq(min(x$WMEE), max(x$WMEE), length.out = 200),
    IMEE = seq(min(x$IMEE), max(x$IMEE), length.out = 200)
  )
  grid$CE <- predict(model, newdata = grid)
  grid$group <- name
  surfaces[[name]] <- grid
}

stats <- do.call(rbind, stats)
surfaces <- do.call(rbind, surfaces)
row.names(stats) <- NULL

write.csv(
  stats,
  file.path(results_dir, "Figure3_GAM_results.csv"),
  row.names = FALSE
)
print(stats, digits = 5)

fill_scale <- scale_fill_viridis_c(
  option = "plasma",
  limits = c(-0.06712357, 0.2010376),
  oob = scales::squish,
  breaks = seq(-0.05, 0.20, 0.05),
  name = expression(CE~(degree*C)),
  guide = guide_colorbar(
    direction = "horizontal",
    title.position = "left",
    barwidth = grid::unit(3, "in"),
    barheight = grid::unit(0.12, "in")
  )
)

make_panel <- function(name, letter, x_breaks, y_breaks, show_y = FALSE) {
  z <- surfaces[surfaces$group == name, ]
  s <- stats[stats$group == name, ]
  title <- paste0(
    letter, "   ", name,
    "   R²: ", sprintf("%.2f", s$r_squared),
    " RMSE: ", sprintf("%.3f", s$rmse)
  )

  ggplot(z, aes(WMEE, IMEE)) +
    geom_raster(aes(fill = CE), interpolate = TRUE) +
    geom_contour(aes(z = CE), colour = "white", alpha = 0.65,
                 bins = 12, linewidth = 0.35) +
    fill_scale +
    scale_x_continuous(expand = c(0, 0), breaks = x_breaks) +
    scale_y_continuous(expand = c(0, 0), breaks = y_breaks) +
    labs(
      title = title,
      x = "WMEE (m)",
      y = if (show_y) expression(IMEE~(degree*C)) else NULL
    ) +
    theme_classic(base_size = 11) +
    theme(
      legend.position = "none",
      plot.title = element_text(face = "bold", hjust = 0, size = 10),
      axis.text = element_text(colour = "black"),
      aspect.ratio = 1.2
    )
}

plots <- list(
  All = make_panel("All", "c", c(0, 4, 8, 12, 16),
                   seq(1.5, 3.5, 0.5), TRUE),
  Temperate = make_panel("Temperate", "d", c(0, 4, 8, 12, 16),
                         seq(1.5, 3.5, 0.5)),
  Arid = make_panel("Arid", "e", c(0, 3, 6, 9),
                    seq(1.5, 2.5, 0.5)),
  Cold = make_panel("Cold", "f", c(0, 5, 10),
                    seq(2.0, 3.0, 0.5))
)

# Save each panel separately.
for (name in names(plots)) {
  ggsave(
    file.path(results_dir, paste0("Figure3_GAM_", name, ".png")),
    plots[[name]],
    width = 3,
    height = 3.2,
    dpi = 300
  )
}

# Extract one shared horizontal legend and assemble the four panels.
legend_plot <- plots$All + theme(legend.position = "top")
g <- ggplotGrob(legend_plot)
legend_index <- which(g$layout$name %in% c("guide-box-top", "guide-box"))
legend <- g$grobs[[legend_index[1]]]

png(
  file.path(results_dir, "Figure3_GAM_WMEE_IMEE.png"),
  width = 3300,
  height = 1200,
  res = 300
)
grid::grid.newpage()
grid::pushViewport(grid::viewport(
  layout = grid::grid.layout(
    2, 4,
    heights = grid::unit(c(0.55, 3.45), "null")
  )
))
grid::pushViewport(grid::viewport(
  layout.pos.row = 1,
  layout.pos.col = 2:4
))
grid::grid.draw(legend)
grid::popViewport()
for (i in seq_along(plots)) {
  print(
    plots[[i]],
    vp = grid::viewport(layout.pos.row = 2, layout.pos.col = i)
  )
}
dev.off()
