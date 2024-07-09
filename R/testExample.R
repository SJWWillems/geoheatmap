

# fix later:
# - error due to not-UTF-8 encoding
# - keep countries in grid that have NAs
library(tidyverse)

load("R/internet2015.RData")
internet2015 <- internet2015[-192,]# fix later, error due to not-UTF-8 encoding

internet2015 <-internet2015 %>% filter(!is.na(users))

facet_data <- internet2015
facet_col <- "country"
library(geofacet)
grid_data <- africa_countries_grid1

itworks= geoheat(facet_data= internet2015,
        grid_data= grid_data,
        facet_col = facet_col,
        name="Internet Users")

# statebins(USArrests, value_col="Assault", name = "Assault")

ggplot(data = internet2015,
       mapping = aes(fill = users, facet_col = country)) +
 geom_geoheat(na.rm = TRUE)


library(ggplot2)


# Example dataset
data <- data.frame(
  country = c("USA", "Canada", "Germany", "Japan"),
  usage = c(50, 30, 45, 60)
)

# Dummy grid_data
grid_data <- data.frame(
  name = c("United States", "Canada", "Germany", "Japan"),
  code = c("US", "CA", "DE", "JP"),
  row = c(1, 2, 3, 4),
  col = c(1, 2, 3, 4)
)

facet_data <- data
facet_col <- "country"

# Example usage
ggplot(data = data,
       mapping = aes(fill = usage, facet_col = country))  +
  geom_geoheat()

