library(httr)
library(jsonlite)

get_gpt_synonyms <- function(drug_name, model = "gpt-3.5-turbo") {
  api_key <- Sys.getenv("OPENAI_API_KEY")
  if (api_key == "") stop("Bitte setze deinen OpenAI API Key mit Sys.setenv(OPENAI_API_KEY = '...')")
  
  prompt <- paste0(
    "Give me a list of common brand names or synonyms for the drug: '", drug_name, "'. ",
    "Only return the names separated by commas, no explanation."
  )
  
  res <- POST(
    url = "https://api.openai.com/v1/chat/completions",
    add_headers(Authorization = paste("Bearer", api_key)),
    content_type_json(),
    body = toJSON(list(
      model = model,
      messages = list(
        list(role = "system", content = "You are a helpful assistant extracting drug synonyms."),
        list(role = "user", content = prompt)
      )
    ), auto_unbox = TRUE)
  )
  
  output <- content(res, as = "parsed")
  
  # Debug anzeigen
  print(output)
  
  # Versuche die Antwort zu extrahieren
  synonyms_raw <- tryCatch({
    output$choices[[1]]$message$content
  }, error = function(e) {
    warning("Konnte content aus der Antwort nicht extrahieren.")
    return(NULL)
  })
  
  if (is.null(synonyms_raw) || !is.character(synonyms_raw)) {
    warning(paste("Keine gültige GPT-Antwort für", drug_name))
    return(data.frame(drug = drug_name, synonym = NA))
  }
  
  # Aufteilen in einzelne Synonyme
  synonyms <- unlist(strsplit(synonyms_raw, ",\\s*"))
  return(data.frame(drug = drug_name, synonym = synonyms))
}

