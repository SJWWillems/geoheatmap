.sb_invert <- function(hex_color, dark_color="black", light_color="white",
                       na_color="white") {

  hex_color <- gsub("#", "", hex_color)

  R <- suppressWarnings(as.integer(paste("0x", substr(hex_color,1,2), sep="")))
  G <- suppressWarnings(as.integer(paste("0x", substr(hex_color,3,4), sep="")))
  B <- suppressWarnings(as.integer(paste("0x", substr(hex_color,5,6), sep="")))

  YIQ <- ((R*299) + (G*587) + (B*114)) / 1000

  return(
    ifelse(is.na(YIQ), na_color,
      ifelse(
        YIQ >= 128, dark_color, light_color)
      )
    )
}

# Merge user data with grid data

mergeGridAndData <- function(facet_data, grid_data, facet_col)
{

  if (max(nchar(facet_data[,facet_col])) <= 3) {
    merge.grid <- "code"
  } else {
    merge.grid <- "name"
  }

  # facet_data <- validate_facets(facet_data, grid_data, facet_col, merge.grid, ignore_dups=TRUE)

  # Rename rows and columns to x and y for panel coordinates. Flip row coordinates to start at bottom left.
  # colnames(grid_data) <- c("name", "code", "y", "x")

    names(grid_data)[names(grid_data) == "row"] <- "y"
    names(grid_data)[names(grid_data) == "col"] <- "x"



  # merged_data <- merge(grid_data, facet_data,
  #                      by.x = merge.grid, by.y = "facet_col", all.x = TRUE, sort=TRUE)
  merged_data <- merge(grid_data, facet_data,
                       by.x = merge.grid, by.y = facet_col, all.x= TRUE, all.y = FALSE, sort=TRUE) # remove all.x to deletecountries from grid without data

  merged_data
}


# sanity checks for facet values
validate_facets <- function(facet_data, grid_data, facet_col, merge.grid, ignore_dups=FALSE) {

  good_facets <- facet_data[,facet_col] %in% grid_data[,merge.grid]
  if (any(!good_facets)) {
    # invalid <- facet_data[,facet_col][which(!good_facets)]
    # facet_data <- facet_data[which(good_facets),]
    # message("Found invalid state values: ", invalid)
    message("Found facets that are not in the grid. Consider checking dataset.")
  }

  if (!ignore_dups) {
    dups <- duplicated(facet_data[,facet_col])
    if (any(dups)) {
      facet_data <- facet_data[which(!dups),]
      message("Removing duplicate facet rows.")
    }
  }

  return(facet_data)

}

"%||%" <- function(a, b) { if (!is.null(a)) a else b }

.pt <- 2.84527559055118
