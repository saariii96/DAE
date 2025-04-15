# Testfile: Testet die Funktion mit einem Medikament

source("03-scripts/gpt/01-get-synonyms-gpt.R")  # lädt Funktion

# Einzeln testen
ibuprofen_synonyms <- get_gpt_synonyms("ibuprofen")
print(ibuprofen_synonyms)

# Optional: mehrere Medikamente testen
top_drugs <- c("ibuprofen", "paracetamol", "aspirin")

all_synonyms <- lapply(top_drugs, get_gpt_synonyms)
synonyms_df <- do.call(rbind, all_synonyms)

# Ergebnis speichern
write.csv(synonyms_df, "02-data/gpt_synonyms_test.csv", row.names = FALSE)

#Top10
top_drugs <- top_meds$medicinal_product[1:10]
all_synonyms <- lapply(top_drugs, get_gpt_synonyms)
synonyms_df <- do.call(rbind, all_synonyms)

# Speichern
write.csv(synonyms_df, "02-data/gpt_synonyms_top10.csv", row.names = FALSE)
