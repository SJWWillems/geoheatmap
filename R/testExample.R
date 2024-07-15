# fix later:
# - error due to not-UTF-8 encoding
# - keep countries in grid that have NAs
library(tidyverse)
library(ggplot2)
library(geofacet)

###############################################################################
# GEOHEAT FUNCTION
load("R/internet2015.RData")
internet2015= internet2015[-192,]# fix later, error due to not-UTF-8 encoding
internet2015= internet2015 %>% filter(!is.na(users))

ita_reg_ann_data= read_csv("R/ita_reg_ann_data.csv")
italy_provinces= ita_reg_ann_data
italy_provinces= italy_provinces %>%
  mutate(den_reg = ifelse(den_reg == "Trentino Alto Adige", "Trentino-Alto Adige",
                              ifelse(den_reg == "Friuli Venezia Giulia", "Friuli-Venezia Giulia", den_reg)))

euroexample= geoheat(facet_data= internet2015,
        grid_data= europe_countries_grid1,
        facet_col = "country",
        value_col = "users",
        name= "users",
        round= TRUE,
        low= "red", high= "yellow")

euroexample+
  theme_void() +
  labs(title= "Internet Use by European countries") +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))


brasil_airports= read_csv("R/airports.csv")
brasil_airports= brasil_airports %>%
  rename(Passengers = `Passengers rate`)

brasil= geoheat(facet_data= brasil_airports,
                      grid_data= br_states_grid1,
                      facet_col = "UF",
                      value_col = "Passengers",
                      name= "Passengers",
                      round= TRUE,
                      low= "lightgreen", high= "darkgreen")

brasil+
  theme_void()
# :)
# this would probably be much better binned, see below for german example

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

# let's try a non-continuous scale on a German dataset
german_states= read_csv("R/german_states.csv")

# ask sanne abt this... can we somehow select the grid column that we want for mergeing?
# for now, manually use german names region name
de_states_reimagined= de_states_grid1

# have to use base r...
# remove the original 'name' column
if ("name" %in% colnames(de_states_reimagined)) {
  de_states_reimagined= de_states_reimagined[, !colnames(de_states_reimagined) == "name", drop = FALSE]
}

# rename 'name_de' column to 'name'
colnames(de_states_reimagined)[colnames(de_states_reimagined) == "name_de"]= "name"

# binning
german_states= german_states %>%
  mutate(
    density_bin= case_when(
      density <= 150 ~ "Low",
      density <= 300 ~ "Medium",
      TRUE ~ "High"  # For values greater than 300
    )
  )

pretzel= geoheat(facet_data= german_states,
                     grid_data= de_states_reimagined,
                     facet_col = "name",
                     value_col = "density_bin",
                     name= "density",
                 ggplot2_scale_function = scale_fill_manual,
                 values = c("Low" = "#edf8b1", "Medium" = "#7fcdbb", "High" = "#2c7fb8"),
                     round= TRUE)

pretzel+
  theme_void() +
  labs(title= "Population Density in German states") +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))



itworks +
  theme_void() +
  scale_fill_gradient(low = "yellow", high = "red")

###############################################################################
# GEOM_GEOHEAT
ggplot(data = internet2015,
       mapping = aes(fill = users, facet_col = country)) +
 geom_geoheat(na.rm = TRUE)

##############################################################################
library(ggplot2)

# Minimal dataset given to StackOverflow users
# Example dataset
data= data.frame(
  country = c("USA", "Canada", "Germany", "Japan"),
  usage = c(50, 30, 45, 60)
)

# Dummy grid_data
grid_data= data.frame(
  name = c("United States", "Canada", "Germany", "Japan"),
  code = c("US", "CA", "DE", "JP"),
  row = c(1, 2, 3, 4),
  col = c(1, 2, 3, 4)
)

facet_data= data
facet_col= "country"


# Example usage
ggplot(data = data,
       mapping = aes(fill = usage, facet_col = country))  +
  geom_geoheat()

