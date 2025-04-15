# This script scrapes the data from the Open FDA API

library(httr)
library(jsonlite)
library(dplyr)
library(tidyr)
library(ggplot2)
library(lubridate)


#OpenFDA API


#https://api.fda.gov/drug/event.json?search=patient.reaction.reactionmeddrapt:headache&limit=100

# Setze den API-Endpunkt
base_url <- "https://api.fda.gov/drug/event.json"

get_drug_data <- function(drug_name, limit = 100) {
  base_url <- "https://api.fda.gov/drug/event.json"
  
  query <- list(
    search = paste0("patient.drug.medicinalproduct:", drug_name),
    limit = limit
  )
  
  response <- GET(url = base_url, query = query)
  data_raw <- content(response, as = "parsed", simplifyDataFrame = TRUE)
  events <- data_raw$results
  
  # Umwandlung zu DataFrame
  injuries_df <- tibble(
    age = sapply(events$patient$patientonsetage, function(x) ifelse(is.null(x), NA, x)),
    age_unit = sapply(events$patient$patientonsetageunit, function(x) ifelse(is.null(x), NA, x)),
    sex = sapply(events$patient$patientsex, function(x) ifelse(is.null(x), NA, x)),
    drug = sapply(events$patient$drug, function(x) {
      if (is.null(x) || length(x) == 0) return(NA)
      x1 <- x[[1]]
      if (is.list(x1) && !is.null(x1$medicinalproduct)) {
        return(x1$medicinalproduct)
      } else {
        return(NA)
      }
    }),
    reaction = sapply(events$patient$reaction, function(x) {
      if (is.null(x) || length(x) == 0 || is.atomic(x)) return(NA)
      safe_reacts <- sapply(x, function(r) {
        if (is.list(r) && !is.null(r$reactionmeddrapt)) {
          return(r$reactionmeddrapt)
        } else {
          return(NA)
        }
      })
      paste(na.omit(safe_reacts), collapse = "; ")
    }),
    date = sapply(events$receiptdate, function(x) ifelse(is.null(x), NA, x))
  ) %>%
    mutate(date = as.Date(date, format = "%Y%m%d"))
  
  return(injuries_df)
}

ibuprofen_df <- get_drug_data("ibuprofen")
head(ibuprofen_df)


# 01-TappingOpenFDA-API.R
# Zweck: Daten von OpenFDA API abrufen und lokal speichern

library(httr)
library(jsonlite)

# Beispiel: Top 50 Medikamente nach Häufigkeit
url <- "https://api.fda.gov/drug/event.json"
query <- list(count = "patient.drug.medicinalproduct.exact", limit = 50)

response <- GET(url, query = query)

# Rohdaten lokal speichern
writeLines(content(response, as = "text"), "01-data_import/top50_medicines_raw.json")

# Als DataFrame extrahieren (für Vorschau)
top_meds <- fromJSON(content(response, as = "text"), flatten = TRUE)$results
top_meds <- as.data.frame(top_meds)
names(top_meds) <- c("medicinal_product", "count")

# Vorschau
head(top_meds)
