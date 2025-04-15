#Top10
top_drugs <- top_meds$medicinal_product[1:10]
all_synonyms <- lapply(top_drugs, get_gpt_synonyms)
synonyms_df <- do.call(rbind, all_synonyms)

# Speichern
write.csv(synonyms_df, "02-data/gpt_synonyms_top10.csv", row.names = FALSE)

#Top50
library(httr)
library(jsonlite)

# API-Endpunkt und Abfrage für Top 50 Medikamente
url <- "https://api.fda.gov/drug/event.json"
query <- list(count = "patient.drug.medicinalproduct.exact", limit = 50)

# API-Request senden
response <- GET(url, query = query)

# Als JSON speichern
writeLines(content(response, as = "text"),
           "01-data_import/top50_medicines_raw.json")

# Vorschau als DataFrame
top_meds <- fromJSON(content(response, as = "text"), flatten = TRUE)$results
top_meds <- as.data.frame(top_meds)
names(top_meds) <- c("medicinal_product", "count")

# Vorschau
head(top_meds)
