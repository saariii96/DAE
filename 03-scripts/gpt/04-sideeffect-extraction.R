# Test-Request für humira
res <- GET("https://api.fda.gov/drug/event.json", query = list(
  search = "patient.drug.medicinalproduct:humira", 
  limit = 1
))
data <- content(res, as = "parsed", simplifyDataFrame = FALSE)

# Struktur anzeigen
str(data$results[[1]])

# Funktion zur Nebenwirkungsextraktion
get_sideeffects <- function(drug_name, limit = 100) {
  base_url <- "https://api.fda.gov/drug/event.json"
  query <- list(
    search = paste0("patient.drug.medicinalproduct:", drug_name),
    limit = limit
  )
  
  res <- GET(base_url, query = query)
  data <- content(res, as = "parsed", simplifyDataFrame = FALSE)
  
  # Reaktionen extrahieren
  reactions <- sapply(data$results, function(x) {
    react <- tryCatch(x[["patient"]][["reaction"]], error = function(e) NULL)
    
    if (!is.null(react)) {
      return(paste(sapply(react, function(r) r$reactionmeddrapt), collapse = "; "))
    } else {
      return(NA)
    }
  })
  
  return(data.frame(drug = drug_name, side_effects = reactions))
}

# Liste deiner Medikamente
top_drugs <- c("humira", "enbrel", "zantac", "revlimid", "dupixent", "aspirin")

# Alle Nebenwirkungen sammeln
sideeffects_list <- lapply(top_drugs, function(drug) {
  Sys.sleep(1.2)  # wichtig wegen API-Limit
  tryCatch(get_sideeffects(drug), error = function(e) {
    message("Fehler bei ", drug, ": ", e$message)
    return(NULL)
  })
})

# Kombinieren und speichern
sideeffects_df <- do.call(rbind, sideeffects_list)
write.csv(sideeffects_df, "02-data/sideeffects_top50.csv", row.names = FALSE)

# Vorschau
head(sideeffects_df)
nrow(sideeffects_df)

