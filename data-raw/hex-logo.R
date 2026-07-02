# Generate the ggchangepoint hex sticker (man/figures/logo.png).
# Pure base grid — no extra dependencies. Run from the package root:
#   Rscript data-raw/hex-logo.R

library(grid)

# ---- geometry ---------------------------------------------------------------
# pointy-top hexagon, circumradius 1, centred at (0,0)
ang <- seq(90, 390, by = 60) * pi / 180
hx <- cos(ang)
hy <- sin(ang)

# ---- palette ----------------------------------------------------------------
col_bg     <- "#1E2A4A"  # deep navy fill
col_border <- "#6C8EEF"  # periwinkle border
col_data   <- "#9DB4E8"  # muted blue data
col_fit    <- "#FFB454"  # amber step (segment means)
col_cp     <- "#FF6B6B"  # coral changepoint line
col_title  <- "#FFFFFF"
col_url    <- "#8DA3D9"

# ---- simulated series with one mean shift -----------------------------------
set.seed(7)
n  <- 56
cp <- 0.04                       # changepoint x-location
xs <- seq(-0.70, 0.62, length.out = n)
mu <- ifelse(xs < cp, -0.13, 0.30)
ys <- mu + rnorm(n, 0, 0.045)

png("man/figures/logo.png", width = 1200, height = 1386, res = 300,
    bg = "transparent", type = "cairo")
grid.newpage()
pushViewport(viewport(xscale = c(-1.05, 1.05), yscale = c(-1.15, 1.15),
                      width = unit(1, "snpc") * 0.92,
                      height = unit(1, "snpc") * 1.0))

# hexagon fill + border
grid.polygon(hx, hy, default.units = "native",
             gp = gpar(fill = col_bg, col = NA))
grid.polygon(hx, hy, default.units = "native",
             gp = gpar(fill = NA, col = col_border, lwd = 10, linejoin = "mitre"))

# changepoint vertical dashed line
grid.lines(x = c(cp, cp), y = c(-0.40, 0.52), default.units = "native",
           gp = gpar(col = col_cp, lwd = 4.5, lty = "22", lineend = "round"))

# data: thin polyline + points
grid.lines(xs, ys, default.units = "native",
           gp = gpar(col = col_data, lwd = 1.6, alpha = 0.75))
grid.points(xs, ys, default.units = "native", pch = 16, size = unit(1.7, "pt"),
            gp = gpar(col = col_data))

# amber step function (piecewise-constant fit)
grid.lines(x = c(-0.70, cp), y = c(-0.13, -0.13), default.units = "native",
           gp = gpar(col = col_fit, lwd = 7, lineend = "round"))
grid.lines(x = c(cp, 0.62), y = c(0.30, 0.30), default.units = "native",
           gp = gpar(col = col_fit, lwd = 7, lineend = "round"))

# package name
grid.text("ggchangepoint", x = 0, y = -0.62, default.units = "native",
          gp = gpar(col = col_title, fontface = 2, cex = 1.32,
                    fontfamily = "sans"))

# small url along the lower-right edge
grid.text("github.com/PursuitOfDataScience", x = 0.46, y = -0.80,
          default.units = "native", rot = 30,
          gp = gpar(col = col_url, cex = 0.36, fontfamily = "sans"))

popViewport()
dev.off()
