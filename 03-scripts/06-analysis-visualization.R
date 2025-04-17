library(dplyr)
library(ggplot2)


#Häufigstes Medikament mit Nebenwirkung
# Daten einlesen
df <- read.csv("02-data/combined_synonyms_sideeffects.csv")

# Nur Zeilen mit Nebenwirkungen
df_filtered <- df %>% filter(!is.na(side_effects))

# Häufigste Medikamente
top_drugs <- df_filtered %>%
  count(drug, sort = TRUE) %>%
  top_n(10)

# Visualisieren
ggplot(top_drugs, aes(x = reorder(drug, n), y = n)) +
  geom_col(fill = "steelblue") +
  coord_flip() +
  labs(title = "Top 10 Medikamente mit Nebenwirkungen",
       x = "Medikament", y = "Anzahl Nebenwirkungen")

#Häufigste Nebenwirkung insgesamt
# Alle Nebenwirkungen einzeln splitten
all_effects <- df_filtered %>%
  separate_rows(side_effects, sep = ";\\s*") %>%
  count(side_effects, sort = TRUE)

# Top 15 anzeigen
head(all_effects, 15)

# Plot
ggplot(all_effects[1:15, ], aes(x = reorder(side_effects, n), y = n)) +
  geom_col(fill = "darkred") +
  coord_flip() +
  labs(title = "Häufigste Nebenwirkungen", x = "Nebenwirkung", y = "Anzahl")

# Medikamente mit den meisten Nebenwirkungen
top_drugs <- df_filtered %>%
  group_by(drug) %>%
  summarise(n = sum(!is.na(side_effects))) %>%
  arrange(desc(n))

# Top 15 anzeigen
head(top_drugs, 15)

# Plot
ggplot(top_drugs[1:15,], aes(x = reorder(drug, n), y = n)) +
  geom_col(fill = "steelblue") +
  coord_flip() +
  labs(title = "Top 15 Medikamente mit Nebenwirkungen",
       x = "Medikament", y = "Anzahl Nebenwirkungen")

library(tidyr)

# Nebenwirkungen aufsplitten
matrix_df <- df_filtered %>%
  filter(!is.na(side_effects)) %>%
  separate_rows(side_effects, sep = ";\\s*") %>%
  count(drug, side_effects) %>%
  pivot_wider(names_from = side_effects, values_from = n, values_fill = 0)

# Vorschau
View(matrix_df)


#Heatmap -------------------------------------------------------------------
#install.packages("pheatmap")

library(pheatmap)

# Zeilen: Medikamente, Spalten: Nebenwirkungen
drug_matrix <- as.matrix(matrix_df[,-1])
rownames(drug_matrix) <- matrix_df$drug

# Heatmap 
pheatmap(drug_matrix, cluster_rows = TRUE, cluster_cols = TRUE,
         fontsize_row = 8, show_rownames = TRUE)
