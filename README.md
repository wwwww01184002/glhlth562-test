# Glhlth562
This is my GLHLTH 562 final project.
# Global Health Indicator Explorer

## 1. Project Description

This project is an interactive data product that allows users to explore global health indicators from the World Bank. Users can select a country, indicator, and time range to visualize trends and generate a plain-language summary of the data.

The application combines real-time data retrieval, interactive visualization, and AI-generated interpretation to make global health data accessible to a general audience.

---

## 2. Data Source

The data is retrieved dynamically from the World Bank API.

* API endpoint:
  https://api.worldbank.org/v2/country/{country}/indicator/{indicator}

* Example indicators used:

  * Life Expectancy at Birth
  * Under-5 Mortality Rate
  * Health Expenditure per Capita

No local dataset is stored in the repository. All data is fetched in real time based on user input.

---

## 3. Data Ingestion

Data is retrieved using the following R packages:

* `httr` for API requests
* `jsonlite` for parsing JSON responses

Process:

1. A request URL is constructed based on selected country and indicator
2. Data is fetched via HTTP GET request
3. JSON response is parsed into a data frame

Authentication:

* No authentication is required for the World Bank API

---

## 4. Data Processing

The data is processed using `dplyr`:

* Extract relevant fields:

  * country name
  * indicator name
  * year
  * value
* Convert data types (year and value to numeric)
* Remove missing values
* Filter based on user-selected year range
* Sort data chronologically

For the AI summary:

* Key statistics are extracted (start year, end year, values)
* These are formatted into a prompt for the language model

---

## 5. Output

The application produces two main outputs:

### 1. Interactive Visualization

* Line plot of the selected indicator over time
* Built using `ggplot2`
* Updates automatically when inputs change

### 2. AI-Generated Summary

* Generated using OpenAI API (`gpt-4o-mini`)
* Provides a 2–3 sentence plain-language explanation of trends
* Triggered by user via button

Fallback:

* If the API fails, a rule-based summary is generated instead

---

## 6. How to Run the Application

### Requirements

Install required packages:

```r
install.packages(c("shiny", "httr", "jsonlite", "dplyr", "ggplot2", "bslib"))
```

### Set API Key

This project uses the OpenAI API.

Set your API key in R:

```r
Sys.setenv(OPENAI_API_KEY = "your_api_key_here")
```

⚠️ Do NOT store your API key in the code or repository.

### Run the App

```r
shiny::runApp()
```

---

## 7. Project Structure

```
project/
├── app.R              # Main Shiny application
├── R/
│   ├── data_api.R     # World Bank data retrieval
│   ├── summary.R      # AI and rule-based summaries
├── deck/              # Presentation slides
├── README.md          # Documentation
├── .gitignore
```

---

## 8. Notes

* The application relies on real-time API calls, so an internet connection is required
* Results may vary slightly depending on API availability
* The AI-generated summary is intended for general interpretation and may not be scientifically rigorous

---
