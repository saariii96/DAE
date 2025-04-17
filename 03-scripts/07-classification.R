# Packages installieren (falls noch nicht geschehen)
# install.packages("tidyverse")
# install.packages("randomForest")
# install.packages("caret")

# Laden
library(tidyverse)
library(randomForest)
library(caret)

# CSV einlesen
df_raw <- read_csv("02-data/combined_synonyms_sideeffects_top100.csv")

# NA entfernen und Nebenwirkungen aufsplitten
df_clean <- df_raw %>%
  filter(!is.na(side_effects)) %>%
  separate_rows(side_effects, sep = ";\\s*") %>%
  rename(side_effect = side_effects)

df_clean <- df_clean %>% mutate(value = 1)

head(df_clean)

str(df_clean)


#Matrix
df_matrix <- df_clean %>%
  distinct(synonym, side_effect, .keep_all = TRUE) %>%  # Duplikate entfernen
  pivot_wider(
    id_cols = synonym,
    names_from = side_effect,
    values_from = value,
    values_fill = 0
  )

df <- df_matrix               # das ist deine 0/1-Matrix
df <- as.data.frame(df)

set.seed(42)
results <- list()

for (target_var in colnames(df)) {
  target <- df[[target_var]]
  
  # überspringen, wenn Ziel nur eine Klasse hat
  if (length(unique(target)) < 2) {
    message("⚠️ ", target_var, ": Nur eine Klasse – übersprungen.")
    next
  }
  
  # In Faktor umwandeln
  target <- as.factor(target)
  
  # Training/Test
  train_idx <- createDataPartition(target, p = 0.7, list = FALSE)
  train_data <- df[train_idx, !(names(df) %in% target_var)]
  test_data <- df[-train_idx, !(names(df) %in% target_var)]
  train_target <- target[train_idx]
  test_target <- target[-train_idx]
  
  if (nrow(test_data) == 0) {
    message("⚠️ ", target_var, ": Kein Test-Datensatz – übersprungen.")
    next
  }
  
  # Modell
  model <- randomForest(x = train_data, y = train_target, ntree = 100)
  
  # Vorhersage
  pred <- predict(model, newdata = test_data)
  acc <- mean(pred == test_target)
  
  # speichern
  results[[target_var]] <- list(
    model = model,
    accuracy = acc,
    importance = importance(model)
  )
  
  message("✅ ", target_var, " – Accuracy: ", round(acc * 100, 1), "%")
}

accuracies <- sapply(results, function(x) x$accuracy)
top_accuracies <- sort(accuracies, decreasing = TRUE)[1:10]
print(top_accuracies)

#visualization
accuracy_df <- data.frame(
  side_effect = names(accuracies),
  accuracy = round(accuracies * 100, 1)
)

top10_accuracy <- accuracy_df %>%
  arrange(desc(accuracy)) %>%
  head(10)

ggplot(top10_accuracy, aes(x = reorder(side_effect, accuracy), y = accuracy)) +
  geom_col(fill = "forestgreen") +
  coord_flip() +
  labs(title = "Top 10 Nebenwirkungen nach Vorhersage-Genauigkeit",
       x = "Nebenwirkung", y = "Accuracy (%)")




