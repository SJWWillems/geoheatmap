## code to prepare `internet` dataset goes here

# Dataset contains bad encoding, identify and select the problematic one
rows_to_keep <- !startsWith(internet$country, "Korea, Dem.")

# Remove the identified rows from the data frame
internet <- internet[rows_to_keep, ]

usethis::use_data(internet, overwrite = TRUE)

