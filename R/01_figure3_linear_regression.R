# Figure 3a-b: climate-zone linear regressions (base R only).
source(file.path("R", "00_project_paths.R"))
data <- read.csv(data_file, stringsAsFactors = FALSE, check.names = FALSE)

zones <- c("Temperate", "Arid", "Cold")
cols <- c(Temperate = "#55B98A", Arid = "#FF8A5B", Cold = "#6A9EEB")
data <- data[data$climate_zone %in% zones, ]

# Fit CE ~ WMEE and CE ~ IMEE within each climate zone
models <- list()
results <- list()
k <- 1

for (xvar in c("WMEE", "IMEE")) {
  for (zone in zones) {
    d <- data[data$climate_zone == zone, ]
    fit <- lm(reformulate(xvar, "CE"), data = d)
    s <- summary(fit)
    models[[paste(xvar, zone)]] <- fit
    results[[k]] <- data.frame(
      predictor = xvar,
      climate_zone = zone,
      n = nrow(d),
      intercept = coef(fit)[1],
      slope = coef(fit)[2],
      r_squared = s$r.squared,
      p_value = coef(s)[2, 4]
    )
    k <- k + 1
  }
}

results <- do.call(rbind, results)
row.names(results) <- NULL
write.csv(
  results,
  file.path(results_dir, "Figure3_linear_regression_results.csv"),
  row.names = FALSE
)
print(results, digits = 5)

label_text <- function(r) {
  sign <- ifelse(r$slope >= 0, " + ", " - ")
  p <- ifelse(r$p_value < 0.01, "p < 0.01",
              sprintf("p = %.3f", r$p_value))
  paste0(
    "y = ", sprintf("%.3f", r$intercept),
    sign, sprintf("%.3f", abs(r$slope)), "x, ",
    "R2 = ", sprintf("%.3f", r$r_squared), ", ", p
  )
}

draw_panel <- function(xvar, xlab) {
  xr <- range(data[[xvar]])
  yr <- range(data$CE)
  plot(NA, xlim = xr, ylim = yr, xlab = xlab, ylab = "CE (deg C)",
       bty = "l", las = 1)

  # Confidence intervals
  for (zone in zones) {
    d <- data[data$climate_zone == zone, ]
    fit <- models[[paste(xvar, zone)]]
    xnew <- seq(min(d[[xvar]]), max(d[[xvar]]), length.out = 200)
    newdata <- setNames(data.frame(xnew), xvar)
    pred <- predict(fit, newdata, interval = "confidence")
    polygon(c(xnew, rev(xnew)), c(pred[, 2], rev(pred[, 3])),
            col = adjustcolor(cols[zone], 0.16), border = NA)
  }

  # Points and regression lines
  for (zone in zones) {
    d <- data[data$climate_zone == zone, ]
    fit <- models[[paste(xvar, zone)]]
    points(d[[xvar]], d$CE, pch = 16, cex = 0.9,
           col = adjustcolor(cols[zone], 0.62))
    abline(fit, col = cols[zone], lwd = 2)
  }

  r <- results[results$predictor == xvar, ]
  xpos <- xr[1] + 0.42 * diff(xr)
  ypos <- yr[2] - c(0.02, 0.10, 0.18) * diff(yr)
  for (i in 1:3) {
    text(xpos, ypos[i], label_text(r[i, ]), col = cols[r$climate_zone[i]],
         adj = c(0, 1), cex = 0.72)
  }

  legend("bottomleft", "Climate zone", zones, col = cols[zones],
         pch = 16, lty = 1, lwd = 2, bty = "n", cex = 0.75)
}

png(file.path(results_dir, "Figure3_linear_regression.png"),
    width = 2400, height = 1050, res = 300)
par(mfrow = c(1, 2), mar = c(4.5, 4.5, 1.5, 1))
draw_panel("WMEE", "WMEE (m)")
draw_panel("IMEE", "IMEE (deg C)")
dev.off()
