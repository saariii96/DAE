#Top50 -------------------------------------------------------------------------
# CSVs einlesen
synonyms <- read.csv("02-data/gpt_synonyms_top50.csv")
sideeffects <- read.csv("02-data/sideeffects_top50.csv")

# Zusammenführen (inner join auf drug)
combined_df <- merge(synonyms, sideeffects, by = "drug", all.x = TRUE)

# Speichern
write.csv(combined_df, "02-data/combined_synonyms_sideeffects.csv", row.names = FALSE)

# Vorschau
head(combined_df)
nrow(combined_df)


#Top100 ------------------------------------------------------------------------
# CSVs einlesen
synonyms <- read.csv("02-data/gpt_synonyms_top100.csv")
sideeffects <- read.csv("02-data/sideeffects_top100.csv")

# Zusammenführen (inner join auf drug)
combined_df <- merge(synonyms, sideeffects, by = "drug", all.x = TRUE)

# Speichern
write.csv(combined_df, "02-data/combined_synonyms_sideeffects_top100.csv", row.names = FALSE)

# Vorschau
head(combined_df)
nrow(combined_df)