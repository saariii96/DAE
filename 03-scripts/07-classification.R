library(randomForest)
library(caret)
library(tidyverse)

df <- matrix_df          # Medikament-Nebenwirkungs-Matrix
df[is.na(df)] <- 0       # fehlende Werte als 0 interpretieren
df$drug <- NULL          # Medikamentenname entfernen (nicht als Prädiktor)

set.seed(42)  # Reproduzierbarkeit

results <- list()

for (target_var in colnames(df)) {
  target <- df[[target_var]]
  
  # Skip, wenn Zielvariable nur eine Klasse hat
  if (length(unique(target)) < 2) {
    message("⚠️ ", target_var, ": Nur eine Klasse – übersprungen.")
    next
  }
  
  # Ziel in Faktor umwandeln
  target <- as.factor(target)
  
  # Daten splitten (Train/Test)
  train_idx <- createDataPartition(target, p = 0.7, list = FALSE)
  train_data <- df[train_idx, ]
  test_data <- df[-train_idx, ]
  train_target <- target[train_idx]
  test_target <- target[-train_idx]
  
  # Sicherheitsabfrage, falls Testdaten fehlen
  if (nrow(test_data) == 0) {
    message("⚠️ ", target_var, ": Kein Test-Datensatz – übersprungen.")
    next
  }
  
  # Modell trainieren
  model <- randomForest(x = train_data, y = train_target, ntree = 100)
  
  # Vorhersage & Genauigkeit
  pred <- predict(model, newdata = test_data)
  acc <- mean(pred == test_target)
  
  # Ergebnisse speichern
  results[[target_var]] <- list(
    model = model,
    accuracy = acc,
    importance = importance(model)
  )
  
  message("✅ ", target_var, " – Accuracy: ", round(acc * 100, 1), "%")
}

#Liste anzeigen
accuracies <- sapply(results, function(x) x$accuracy)
sort(accuracies, decreasing = TRUE)[1:10]  # Top 10 Nebenwirkungen nach Vorhersagegüte


# Genauigkeiten extrahieren
accuracies <- sapply(results, function(x) x$accuracy)

# In DataFrame umwandeln
accuracy_df <- data.frame(
  side_effect = names(accuracies),
  accuracy = round(accuracies * 100, 1)
)

# Top 10 Nebenwirkungen nach Vorhersagegüte
top10_accuracy <- accuracy_df %>%
  arrange(desc(accuracy)) %>%
  head(10)

print(top10_accuracy)

#Visualisieren
library(ggplot2)

ggplot(top10_accuracy, aes(x = reorder(side_effect, accuracy), y = accuracy)) +
  geom_col(fill = "forestgreen") +
  coord_flip() +
  labs(title = "Top 10 Nebenwirkungen nach Vorhersage-Genauigkeit",
       x = "Nebenwirkung", y = "Accuracy (%)")

       
#Cross-Validation
train_control <- trainControl(method = "cv", number = 5)
model <- train(x = train_data, y = train_target,
               method = "rf", trControl = train_control)
