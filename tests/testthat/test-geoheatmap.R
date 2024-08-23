test_that("a simple plot gets produced", {
  require(ggplot2)
  require(geoheatmap)
  require(geofacet)

  data(internet, package = "geoheatmap")
  internet_2015 <- subset(internet, year == 2015)

  geoheatmap(facet_data = internet_2015, grid_data = europe_countries_grid1,
                           facet_col = "country", value_col = "users") -> gg

  # Expect that gg is a ggplot object
  expect_s3_class(gg, "ggplot")

  gb <- ggplot_build(gg)

  # Expect that the plot contains at least one layer
  expect_gt(length(gb$layout$panel_params), 0)
})
