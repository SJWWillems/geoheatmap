#' Individuals using the Internet
#'
#' This dataset contains information on the percentage of individuals using the Internet
#' within a given population, categorized by country and year. It helps to measure the digital divide
#' and track progress internet accessibility worldwide. Internet users are defined as
#' individuals who have used the Internet (from any location) in the last 3 months.
#' The data is collected from national surveys and telecommunications ministries and is
#' regularly updated (last update: 2022) to reflect the latest available figures.
#'
#' @format A data frame with 7101 observations (long format) on the following 3 variables:
#' \describe{
#'   \item{\code{country}}{list of countries for which data was collected;
#'   there are 263 unique entries, including UN-recognized countries, dependent and autonomous territories.}
#'   \item{\code{year}}{year in which data was recorded in.}
#'   \item{\code{users}}{amount of population with active internet usage, expressed in percentage.}
#'  }
#'
#' @source The World Bank. (2024). Internet Users (\% of population) [Data file]. Retrieved from https://data.worldbank.org/indicator/IT.NET.USER.ZS
#' @details
#' Dataset contains a country name ("Democratic Republic of Korea") that has a encoding that does not comply with the UTF-8, so this observation is
#' removed from the dataset to avoid encoding issues in the example.
#' @source The World Bank Group. (2024). Internet Users (\% of population) [Data file]. Retrieved from https://data.worldbank.org/indicator/IT.NET.USER.ZS
#' @references International Telecommunication Union (ITU). (2024). ITU data (World Telecommunication/ICT Indicators Database) used in World Bank compilation.
"internet"


