#' Create a new ggplot-based geographical heatmap for a user-specified geographical grid
#'
#' Pass in desired data frame and grid and get back a square choropleth. The function takes
#' inspiration from the `statebins` function, modifying it to allow for non-US grids and territories,
#'  e.g. as defined in the `geofacet` package.
#' The output is a ggplot2 object to which additional layers can be added.
#'
#' @details
#'
#' Like in the statebins package, we offer the option to specify a `dark_label` color
#' and a `light_label` color. Depending on the selected colour scale function,
#' `geoheatmap` will use that information to determine what label to use on lighter/darker tiles.
#' This should in principle mean that labels never fade into the background.
#' Note that this only applies if colours are defined within function,
#' i.e. not called after the object has already been created.
#'
#' You can customize the scale function you pass in by using name parameters. All named
#' parameters not used by `geoheatmap()` itself get passed to the scale function.
#'
#' The default theme is set to `theme_void()`, but this can be either overwritten,
#' or added onto depending on intended plot purposes.
#'
#' @seealso \code{\link[statebins]{statebins}} \code{\link[geofacet]{geofacet}}
#'
#'
#' @md
#' @param facet_data data frame of facets (geographical locations) and values to plot
#' @param grid_data data frame of matching geographical grid positions
#' @param facet_col column name in \code{facet_data} that holds the facets. No duplicates;
#'        can be full names (e.g. "\code{Netherlands}") or abbreviations (e.g. "\code{NL}")
#' @param value_col column name in \code{facet_data} that holds the values to be plotted
#' @param merge_col grids can sometimes hold both native and anglophone language geographical names
#'                  (e.g. "\code{Bayern/Bavaria}". If native option is preferable, use `merge_col`;
#'                  defaults to "\code{name}".
#' @param dark_label,light_label,na_label dark/light/NA label colors. The specified color will be used
#'        when the algorithm determines labels should be inverted.
#' @param font_size font size (default = \code{3})
#' @param facet_border_col default "\code{white}" - this creates the "spaces" between boxes
#' @param facet_border_size border size
#' @param round rounded corners (default: `FALSE`)
#' @param radius if `round` is `TRUE` then use `grid::unit` to specify the corner radius.
#'        Default is `grid::unit(6, "pt")` if using rounded corners.
#' @param ggplot2_scale_function ggplot2 scale function to use. Defaults to `scale_fill_continuous`
#' @param hover if `hover` is `TRUE`, enables interactive plotly plot (see also \code{\link[plotly]{ggplotly}}).
#' Note it only works when `round` is set to `FALSE`.
#' @param ... additional parameters to the scale function
#' @return ggplot2 object
#' @import geofacet
#' @import statebins
#' @import ggplot2
#' @importFrom plotly ggplotly
#' @importFrom rlang sym
#' @export
#' @examples
#' data(internet)
#' library(geofacet)
#' library(ggplot2)
#'
#' internet_2015 <- subset(internet, year == 2015)
#' geoheatmap(facet_data= internet_2015, grid_data= europe_countries_grid1,
#'                     facet_col = "country", value_col = "users")
#' @references
#' Bob Rudis. (2022). statebins: Create United States Uniform Cartogram Heatmaps. R package version 1.4.0.
#'   URL: \url{https://CRAN.R-project.org/package=statebins}
#'
#' Ryan Hafen. (2018). geofacet: 'ggplot2' Faceting Utilities for Geographical Data.
#'   R package version 0.2.1. URL: \url{https://CRAN.R-project.org/package=geofacet}
geoheatmap <- function(facet_data = NULL,
                       grid_data = NULL,
                       facet_col = NULL,
                       value_col = NULL,
                       merge_col = NULL,
                       dark_label = "black",
                       light_label = "white",
                       na_label = "white",
                       font_size = 3,
                       facet_border_col = "white",
                       facet_border_size = 2,
                       round = FALSE,
                       radius = grid::unit(6, "pt"),
                       ggplot2_scale_function = ggplot2::scale_fill_continuous,
                       hover = FALSE,
                       ...) {

  facet_data <- data.frame(facet_data, stringsAsFactors = FALSE)

  facet_col <- facet_col

  if (!is.null(merge_col)) {
    merge.grid <- merge_col
  } else {
    if (max(nchar(facet_data[, facet_col])) <= 3) {
      merge.grid <- "code"
    } else {
      merge.grid <- "name"
    }
  }

  facet_data <- validate_facets(facet_data, grid_data, facet_col, merge.grid, ignore_dups = TRUE)

  merged_data <- mergeGridAndData(facet_data, grid_data, facet_col, merge_grid = merge.grid)

  # Hover text concatenation
  merged_data$hover_text <- paste("Location: ", merged_data$name, "<br>Value: ", merged_data[[value_col]])

  gg <- ggplot()

  if (round) {
    gg <- suppressWarnings(gg + geom_rtile(data = merged_data, radius = radius,
                                           aes(x = !!sym("x"), y = !!sym("y"), fill = !!sym(value_col),
                                               text = .data$hover_text),
                                           color = facet_border_col, size = facet_border_size))
  } else {
    gg <- suppressWarnings(gg + geom_tile(data = merged_data,
                                          aes(x = !!sym("x"), y = !!sym("y"), fill = !!sym(value_col),
                                              text = .data$hover_text),
                                          color = facet_border_col, linewidth = facet_border_size))
  }

  gg <- gg + scale_y_reverse()
  gg <- gg + ggplot2_scale_function(...)
  gg <- gg + coord_equal()
  gg <- gg + labs(x = NULL, y = NULL)

  gb <- ggplot2::ggplot_build(gg)

  gg <- gg + geom_text(data = merged_data,
                       aes(x = !!sym("x"), y = !!sym("y"), label = !!sym("code")),
                       angle = 0,
                       color = .sb_invert(gb$data[[1]]$fill, dark_label, light_label, na_label),
                       size = font_size)

  gg <- gg + theme_void()

  if (hover) {
    p <- ggplotly(gg, tooltip = "text")

    p <- plotly::layout(p = p, xaxis = list(visible = FALSE),    # Remove x-axis
                        yaxis = list(visible = FALSE),           # Remove y-axis
                        plot_bgcolor = 'rgba(0,0,0,0)',          # Remove plot background
                        paper_bgcolor = 'rgba(0,0,0,0)')         # Remove paper background
    return(p)
  } else {
    return(gg)
  }
}
