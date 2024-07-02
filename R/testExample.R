

facet_data <- internet2015
facet_col <- "country"
library(geofacet)
grid_data <- africa_countries_grid1



ggplot(data = internet2015,
       mapping = aes(fill = users)) +
 geom_geoheat()
