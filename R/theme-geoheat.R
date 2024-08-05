#' Base geoheat theme
#'
#' @source Builds off of `theme_statebins()`, which builds off of `theme_bw()`
#'
#' @md
#' @param legend_position fills in `legend.position`
#' @param base_family,base_size same as `theme_bw()`
#' @export
#'
#' @details
#' Maybe add details on what the main changes are? And is it used by default by the geofacet package?
#'
theme_geoheat <- function(legend_position="right",
                            base_size = 12,
                            base_family = "") {

  gg <- theme_bw(base_size = base_size, base_family = base_family)
  gg <- gg + theme(
    axis.line = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    axis.title = element_blank(),
    panel.background = element_blank(),
    panel.border = element_blank(),
    panel.grid = element_blank(),
    panel.spacing = unit(0, "lines"),
    plot.background = element_blank(),
    legend.justification = c(0, 0),
    legend.position = legend_position
  )
  gg <- gg + theme(strip.background = element_rect(color="#2b2b2b", fill="white")) #?
  gg <- gg + theme(plot.title=element_text(hjust=0.5, angle=0))
  gg <- gg + theme(axis.title.x=element_text(hjust=0.5, angle=0))
  gg <- gg + theme(legend.position=legend_position)
  gg <- gg + theme(plot.title=element_text(size=16, hjust=0))
  gg <- gg + theme(plot.margin = margin(30,30,30,30))
  gg

}
