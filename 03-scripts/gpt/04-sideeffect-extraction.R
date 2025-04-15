# Funktion: Nebenwirkungen für ein Medikament von der FDA-API abrufen
get_sideeffects <- function(drug_name, limit = 100) {
  base_url <- "https://api.fda.gov/drug/event.json"
  query <- list(
    search = paste0("patient.drug.medicinalproduct:", toupper(drug_name)),
    limit = limit
  )
  
  res <- GET(base_url, query = query)
  data <- content(res, as = "parsed", simplifyDataFrame = TRUE)
  
  # Falls keine Resultate vorhanden sind
  if (is.null(data$results) || length(data$results) == 0) {
    message("Keine Nebenwirkungen gefunden für: ", drug_name)
    return(data.frame(drug = drug_name, side_effects = NA))
  }
  
  # Reaktionen extrahieren
  reactions <- sapply(data$results, function(x) {
    react <- tryCatch(x$patient$reaction, error = function(e) NULL)
    if (!is.null(react)) {
      paste(sapply(react, function(r) r$reactionmeddrapt), collapse = "; ")
    } else {
      NA
    }
  })
  
  return(data.frame(drug = drug_name, side_effects = reactions))
}

# Vektor mit Medikamentennamen (z. B. aus top_drugs laden)
top_drugs <- c("humira", "enbrel", "zantac", "revlimid", "dupixent", "aspirin") # Beispiel: später ersetzen durch deine echte Liste

# Liste für alle Nebenwirkungen
sideeffects_list <- lapply(top_drugs, function(drug) {
  Sys.sleep(1.2)  # API-Rate-Limit einhalten
  tryCatch(get_sideeffects(drug), error = function(e) {
    message("Fehler bei ", drug, ": ", e$message)
    return(NULL)
  })
})

# Zusammenführen
sideeffects_df <- do.call(rbind, sideeffects_list)

# Speichern
write.csv(sideeffects_df, "02-data/sideeffects_top50.csv", row.names = FALSE)

# Vorschau
head(sideeffects_df)
nrow(sideeffects_df)

# Test-Request für humira
res <- GET("https://api.fda.gov/drug/event.json", query = list(
  search = "patient.drug.medicinalproduct:humira", 
  limit = 1
))
data <- content(res, as = "parsed", simplifyDataFrame = FALSE)

# Struktur anzeigen
str(data$results[[1]])


