#' @importFrom ggh4x guide_axis_minor
#' @export
ggh4x::guide_axis_minor

#' @importFrom dplyr mutate
#' @export 
add_barchart <- function(
    value, brks, cols, bar_frame = NA,
    fontsize = 12,
    tck_size = 0.3,
    tck_length = unit(0.05 * 2, "cm"),
    fact = 2,
    theme = NULL, ...) {
  x <- cut(value, brks)
  dat <- as.data.frame(table(x)) %>%
    # as.data.table(table(x)) %>%
    cbind(I = 1:nrow(.)) %>%
    mutate(perc = Freq / sum(Freq))
  # mutate(perc = N/sum(N))

  n <- length(brks)
  fact2 <- floor(fact / 2)
  at_major <- seq(fact, n, fact)
  at_minor <- seq(1, n, fact2)
  labels_major <- brks[at_major]
  labels_major[is.infinite(labels_major)] <- ""

  p <- ggplot(dat, aes(I + 0.5, perc)) + theme_classic() +
    # geom_bar(aes(y=..density..), position = "dodge", width = 1)
    geom_bar(stat = "identity", fill = cols, na.rm = F, color=bar_frame) +
    # geom_histogram(breaks = brks, na.rm = F, fill = cols)
    # geom_histogram(aes(y=after_stat(count/sum(count))), breaks = brks, na.rm = F, fill = cols) +
    labs(y = "Fraction (%)", x = NULL) +
    theme(
      plot.background = element_blank(),
      panel.background = element_blank(),
      plot.margin = margin(l = 5, b = 1),
      axis.title = element_text(size = fontsize),
      axis.text.y = element_text(size = fontsize, colour = "black"),
      axis.text.x = element_blank(),
      axis.ticks.x = element_blank(),
      axis.ticks = element_line(linewidth = tck_size),
      axis.ticks.length = tck_length
    ) +
    scale_y_continuous(labels = scales::percent, expand = c(0, 0)) +
    scale_x_continuous(
      guide = "axis_minor",
      breaks = at_major,
      labels = labels_major, minor_breaks = at_minor
    )
  if (!is.null(theme)) p <- p + theme
  p
}

#' layer_barchart
#'
#' @inheritParams ggplot2::geom_bar
#' @inheritParams grid::viewport
#' @inheritParams make_colorbar
#' 
#' @example R/examples/ex-layer_barchart.R
#' @export
layer_barchart <- function(
    mapping = NULL, data = NULL,
    brks, cols,
    x = 0, y = 0, width = unit(0.5, "npc"), height = unit(0.5, "npc"), just = c(0, 0),
    fontsize = 12, theme = NULL, ...) {
  
  fun <- function(data, coords) {
    p <- add_barchart(data$z, brks, cols, fontsize = fontsize, theme = theme, ...)
    
    g <- grid::grobTree(as.grob(p),
      vp = grid::viewport(
        x = x, y = y, just = just,
        width = width, height = height
      )
    )
    g
  }
  grid_panel(fun, mapping, data)
}

#' @rdname layer_barchart
#' @export
geom_barchart = layer_barchart
