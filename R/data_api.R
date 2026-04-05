# --------------------------------------------------
# Function: Get World Bank data
# --------------------------------------------------
get_wb_data <- function(country_code, indicator_code) {
  url <- paste0(
    "https://api.worldbank.org/v2/country/",
    country_code,
    "/indicator/",
    indicator_code,
    "?format=json&per_page=1000"
  )
  
  res <- GET(url)
  txt <- content(res, as = "text", encoding = "UTF-8")
  json_data <- fromJSON(txt, flatten = TRUE)
  
  if (length(json_data) < 2 || is.null(json_data[[2]])) {
    return(data.frame())
  }
  
  df <- json_data[[2]]
  
  if (is.null(df) || nrow(df) == 0) {
    return(data.frame())
  }
  
  df %>%
    transmute(
      country = country.value,
      indicator = indicator.value,
      year = as.numeric(date),
      value = as.numeric(value)
    ) %>%
    filter(!is.na(year)) %>%
    arrange(year)
}
