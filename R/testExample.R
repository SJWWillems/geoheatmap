

# fix later:
# - error due to not-UTF-8 encoding
# - keep countries in grid that have NAs
library(tidyverse)
library(ggplot2)

load("R/internet2015.RData")
internet2015 <- internet2015[-192,]# fix later, error due to not-UTF-8 encoding

internet2015 <-internet2015 %>% filter(!is.na(users))

facet_data <- internet2015
library(geofacet)

ita_reg_ann_data= read_csv("R/ita_reg_ann_data.csv")
italy_provinces= ita_reg_ann_data
italy_provinces= italy_provinces %>%
  mutate(den_reg = ifelse(den_reg == "Trentino Alto Adige", "Trentino-Alto Adige",
                              ifelse(den_reg == "Friuli Venezia Giulia", "Friuli-Venezia Giulia", den_reg)))

europe= europe_countries_grid2
africa= africa_countries_grid1

facet_col <- "den_reg"
grid_data= italy_grid1
facet_data= italy_provinces

example= geoheat(facet_data= internet2015,
        grid_data= europe,
        facet_col = "country",
        value_col = "users",
        name= "users",
        round= TRUE)

example+
  theme_void()+
  scale_fill_gradient(low= "red", high= "yellow")

sun= geoheat(facet_data= italy_provinces,
        grid_data= italy_grid1,
        value_col= "num_teams",
        facet_col= "den_reg",
        name= "num_teams",
        round= TRUE,
        radius= grid::unit(6, "pt"),
        low= "pink",
        high= "purple")

sun+ theme_void()+
  labs(title= "Italy provinces")


# plotly only works with round= FALSE as geom_GeomRtile() is not yet implemented in plotly
ggplotly(p= sun, tooltip= "fill",
         layer_data= 2)

itworks +
  theme_void() +
  scale_fill_gradient(low = "yellow", high = "red")

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

