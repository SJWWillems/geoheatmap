

# fix later:
# - error due to not-UTF-8 encoding
# - keep countries in grid that have NAs
library(tidyverse)

load("R/internet.rda")

internet2015 <-internet%>% filter(year == 2015)
internet2015 <- internet2015[-192,]# fix later, error due to not-UTF-8 encoding

internet2015 <-internet2015 %>% filter(!is.na(users))

facet_data <- internet2015
facet_col <- "country"
library(geofacet)
grid_data <- africa_countries_grid1



ggplot(data = internet2015,
       mapping = aes(fill = users, facet_col = country)) +
 geom_geoheat(na.rm = TRUE)
