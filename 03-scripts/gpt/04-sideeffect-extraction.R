get_sideeffects <- function(drug_name, limit = 100) {
  base_url <- "https://api.fda.gov/drug/event.json"
  query <- list(
    search = paste0("patient.drug.medicinalproduct:", drug_name),
    limit = limit
  )
  
  res <- GET(base_url, query = query)
  data <- content(res, as = "parsed", simplifyDataFrame = TRUE)
  
  # Reaktionen extrahieren
  reactions <- sapply(data$results, function(x) {
    react <- x$patient$reaction
    if (!is.null(react)) {
      return(paste(sapply(react, function(r) r$reactionmeddrapt), collapse = "; "))
    } else return(NA)
  })
  
  return(data.frame(drug = drug_name, side_effects = reactions))
}

sideeffects_list <- lapply(top_drugs, function(drug) {
  Sys.sleep(1.2)
  tryCatch(get_sideeffects(drug), error = function(e) NULL)
})

sideeffects_df <- do.call(rbind, sideeffects_list)
write.csv(sideeffects_df, "02-data/sideeffects_top50.csv", row.names = FALSE)
