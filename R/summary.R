# --------------------------------------------------
# Function: Rule-based fallback summary
# --------------------------------------------------
generate_summary_rule <- function(df) {
  start_value <- round(df$value[1], 2)
  end_value <- round(df$value[nrow(df)], 2)
  start_year <- min(df$year)
  end_year <- max(df$year)
  country <- df$country[1]
  indicator <- df$indicator[1]
  
  trend <- if (end_value > start_value) {
    "an upward trend"
  } else if (end_value < start_value) {
    "a downward trend"
  } else {
    "a relatively stable pattern"
  }
  
  paste0(
    "From ", start_year, " to ", end_year, ", ", indicator, " in ", country,
    " shows ", trend, ". The value changed from ", start_value,
    " to ", end_value, "."
  )
}

# --------------------------------------------------
# Function: Generate LLM summary
# --------------------------------------------------
generate_summary_llm <- function(df) {
  api_key <- Sys.getenv("OPENAI_API_KEY")
  
  if (api_key == "") {
    stop("OPENAI_API_KEY is not set.")
  }
  
  start_value <- round(df$value[1], 2)
  end_value <- round(df$value[nrow(df)], 2)
  start_year <- min(df$year)
  end_year <- max(df$year)
  country <- df$country[1]
  indicator <- df$indicator[1]
  
  prompt <- paste0(
    "Write a short plain-language summary in 2 to 3 sentences for a general audience.\n\n",
    "Country: ", country, "\n",
    "Indicator: ", indicator, "\n",
    "Year range: ", start_year, " to ", end_year, "\n",
    "Starting value: ", start_value, "\n",
    "Ending value: ", end_value, "\n\n",
    "Explain whether the trend is increasing, decreasing, or relatively stable. ",
    "Be clear, concise, and non-technical."
  )
  
  res <- httr::POST(
    url = "https://api.openai.com/v1/responses",
    httr::add_headers(
      Authorization = paste("Bearer", api_key),
      `Content-Type` = "application/json"
    ),
    body = list(
      model = "gpt-4o-mini",
      input = prompt
    ),
    encode = "json"
  )
  
  parsed <- httr::content(res, as = "parsed", type = "application/json")
  
  if (httr::status_code(res) != 200) {
    stop(jsonlite::toJSON(parsed, auto_unbox = TRUE, pretty = TRUE))
  }
  
  if (!is.null(parsed$output_text) && nzchar(parsed$output_text)) {
    return(parsed$output_text)
  }
  
  if (!is.null(parsed$output) && length(parsed$output) > 0) {
    texts <- c()
    
    for (item in parsed$output) {
      if (!is.null(item$content) && length(item$content) > 0) {
        for (piece in item$content) {
          if (!is.null(piece$text) && nzchar(piece$text)) {
            texts <- c(texts, piece$text)
          }
        }
      }
    }
    
    if (length(texts) > 0) {
      return(paste(texts, collapse = "\n"))
    }
  }
  
  stop("Could not parse model response.")
}
