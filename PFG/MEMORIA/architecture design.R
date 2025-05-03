library(grid)

cajas <- c(
  "Input layer\nand data preparation",
  "Base modeling\nlayer",
  "Feature extraction\nlayer",
  "Error prediction\nlayer",
  "Ensemble layer\n(FFORMA)",
  "Global model\nlayer",
  "Results visualization\nlayer"
)

colores <- colorRampPalette(c("#dceeff", "#1d4e89"))(length(cajas))

caja_coloreada <- function(label, color) {
  grobTree(
    rectGrob(width = unit(0.9, "npc"), height = unit(0.5, "npc"),
             gp = gpar(fill = color, col = "black", lwd = 1.2)),
    textGrob(label,
             x = 0.5, y = 0.5, just = "center",
             gp = gpar(fontsize = 10, fontface = "bold"))
  )
}

cajas_grobs <- mapply(caja_coloreada, cajas, colores, SIMPLIFY = FALSE)
n_cajas <- length(cajas)
n_cols <- n_cajas * 2 - 1

grid.newpage()
pushViewport(viewport(layout = grid.layout(1, n_cols, widths = unit(rep(c(2, 0.5), n_cajas)[-n_cols-1], "null"))))

for (i in seq_along(cajas_grobs)) {
  col <- (i - 1) * 2 + 1
  pushViewport(viewport(layout.pos.row = 1, layout.pos.col = col))
  grid.draw(cajas_grobs[[i]])
  popViewport()
  
  if (i < n_cajas) {
    pushViewport(viewport(layout.pos.row = 1, layout.pos.col = col + 1))
    grid.lines(x = unit(c(0.2, 0.8), "npc"), y = unit(c(0.5, 0.5), "npc"),
               arrow = arrow(length = unit(0.1, "inches")), gp = gpar(lwd = 1.5))
    popViewport()
  }
}

grid.text("Project workflow in R", y = unit(0.95, "npc"), gp = gpar(fontsize = 16, fontface = "bold"))
