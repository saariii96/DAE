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


# ---------------------------------------
# Wichtigste Medikamente pro Nebenwirkung
# ---------------------------------------

library(ggplot2)

for (side_effect in names(top_accuracies)) {
  model_info <- results[[side_effect]]
  
  if (is.null(model_info$importance)) next
  
  imp_df <- as.data.frame(model_info$importance)
  imp_df$medikament <- rownames(imp_df)
  
  top_features <- imp_df %>%
    arrange(desc(MeanDecreaseGini)) %>%
    slice_head(n = 10)
  
  p <- ggplot(top_features, aes(x = reorder(medikament, MeanDecreaseGini), y = MeanDecreaseGini)) +
    geom_col(fill = "tomato") +
    coord_flip() +
    labs(
      title = paste("Top 10 sideeffects of:", side_effect),
      x = "sideeffects",
      y = "Feature Importance (MeanDecreaseGini)"
    )
  
  print(p)  # damit der Plot angezeigt wird
}


----------------------------------------------------------------------------
----------------------------------------------------------------------------
#Welche Nebenwirkungen sind am stärksten mit einer bestimmten Nebenwirkung assoziiert?
  
  library(tidyverse)
library(randomForest)
library(caret)

# 1. Daten einlesen
df_raw <- read_csv("02-data/combined_synonyms_sideeffects_top100.csv")

# 2. Aufsplitten
df_clean <- df_raw %>%
  filter(!is.na(side_effects)) %>%
  separate_rows(side_effects, sep = ";\\s*") %>%
  rename(side_effect = side_effects) %>%
  mutate(value = 1)

# 3. Matrix: Zeilen = Nebenwirkungen, Spalten = Medikamente
df_matrix_med <- df_clean %>%
  distinct(side_effect, synonym, .keep_all = TRUE) %>%
  pivot_wider(
    id_cols = side_effect,
    names_from = synonym,
    values_from = value,
    values_fill = 0
  )

# 4. In Dataframe konvertieren
df <- as.data.frame(df_matrix_med)
rownames(df) <- df$side_effect
df$side_effect <- NULL

results <- list()
set.seed(42)

for (target_var in rownames(df)) {
  target <- as.numeric(df[target_var, ])
  if (length(unique(target)) < 2) next
  
  df_model <- t(df)  # Transponieren für Modelltraining
  df_model <- as.data.frame(df_model)
  target <- as.factor(target)
  
  train_idx <- createDataPartition(target, p = 0.7, list = FALSE)
  train_data <- df_model[train_idx, ]
  test_data <- df_model[-train_idx, ]
  train_target <- target[train_idx]
  test_target <- target[-train_idx]
  
  model <- randomForest(train_data, train_target, ntree = 100)
  acc <- mean(predict(model, test_data) == test_target)
  
  results[[target_var]] <- list(
    model = model,
    accuracy = acc,
    importance = importance(model)
  )
  
  message("✅ ", target_var, " – Accuracy: ", round(acc * 100, 1), "%")
}

library(ggplot2)

var <- "Hypertension"
imp_df <- as.data.frame(results[[var]]$importance)
imp_df$medikament <- rownames(imp_df)

top_features <- imp_df %>%
  arrange(desc(MeanDecreaseGini)) %>%
  slice_head(n = 10)

ggplot(top_features, aes(x = reorder(medikament, MeanDecreaseGini), y = MeanDecreaseGini)) +
  geom_col(fill = "tomato") +
  coord_flip() +
  labs(
    title = paste("Top 10 Medikamente für Vorhersage von:", var),
    x = "Medikament",
    y = "Feature Importance (MeanDecreaseGini)"
  )
