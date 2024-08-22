test_that("a plot gets produced", {

  require(ggplot2)
  require(geoheatmap)
  require(geofacet)

  data(internet)

  internet_2015 <- subset(internet, year == 2015)
  geoheatmap(facet_data = internet_2015, grid_data = europe_countries_grid1,
             facet_col = "country", value_col = "users") -> gg

  gb <- ggplot_build(gg)

})
