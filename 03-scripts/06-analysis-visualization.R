# -----------------------------
# Load required packages
# -----------------------------
library(dplyr)
library(tidyr)
library(ggplot2)
library(pheatmap)

# -----------------------------
# Load and clean the dataset
# -----------------------------
df <- read.csv("02-data/combined_synonyms_sideeffects_top100.csv")
df_filtered <- df %>% filter(!is.na(side_effects))

# -----------------------------
# Top 100 drugs by side effect frequency
# -----------------------------
top100_drugs <- df_filtered %>%
  separate_rows(side_effects, sep = ";\\s*") %>%
  count(drug, sort = TRUE) %>%
  slice_head(n = 100) %>%
  pull(drug)

df_top100 <- df_filtered %>% filter(drug %in% top100_drugs)

# -----------------------------
# Side effect matrix for heatmap (Top 30 drugs × Top 30 effects)
# -----------------------------
df_split <- df_top100 %>% separate_rows(side_effects, sep = ";\\s*")

top30_drugs <- df_split %>%
  count(drug, sort = TRUE) %>%
  slice_head(n = 30) %>%
  pull(drug)

top30_effects <- df_split %>%
  count(side_effects, sort = TRUE) %>%
  slice_head(n = 30) %>%
  pull(side_effects)

df_top30 <- df_split %>%
  filter(drug %in% top30_drugs, side_effects %in% top30_effects)

matrix_df <- df_top30 %>%
  count(drug, side_effects) %>%
  pivot_wider(names_from = side_effects, values_from = n, values_fill = 0)

drug_matrix_top30 <- as.matrix(matrix_df[,-1])
rownames(drug_matrix_top30) <- matrix_df$drug

# -----------------------------
# Heatmap
# -----------------------------
pheatmap(
  drug_matrix_top30,
  cluster_rows = TRUE,
  cluster_cols = TRUE,
  fontsize_row = 8,
  fontsize_col = 8,
  main = "Heatmap: Top 30 Drugs × Top 30 Side Effects",
  xlab = "Side Effects",
  ylab = "Drugs",
  show_rownames = TRUE,
  show_colnames = TRUE
)

# -----------------------------
# Top 15 most frequent side effects
# -----------------------------
all_effects <- df_top100 %>%
  separate_rows(side_effects, sep = ";\\s*") %>%
  count(side_effects, sort = TRUE)

ggplot(all_effects[1:15, ], aes(x = reorder(side_effects, n), y = n)) +
  geom_col(fill = "darkred") +
  coord_flip() +
  labs(title = "Top 15 Most Frequent Side Effects",
       x = "Side Effect", y = "Count")

# -----------------------------
# Top 15 drugs with the most side effects
# -----------------------------
top_drugs <- df_top100 %>%
  group_by(drug) %>%
  summarise(n = sum(!is.na(side_effects))) %>%
  arrange(desc(n)) %>%
  slice_head(n = 15)

ggplot(top_drugs, aes(x = reorder(drug, n), y = n)) +
  geom_col(fill = "steelblue") +
  coord_flip() +
  labs(title = "Top 15 Drugs by Number of Side Effects",
       x = "Drug", y = "Count")

# -----------------------------
# Top 3 side effects per drug
# -----------------------------
top3_sideeffects_per_drug <- df_top100 %>%
  separate_rows(side_effects, sep = ";\\s*") %>%
  count(drug, side_effects, sort = TRUE) %>%
  group_by(drug) %>%
  slice_max(n, n = 3) %>%
  ungroup()

print(top3_sideeffects_per_drug)

# -----------------------------
# Top 20 drug × side effect combinations
# -----------------------------
top_combinations <- df %>%
  filter(!is.na(side_effects)) %>%
  separate_rows(side_effects, sep = ";\\s*") %>%
  count(drug, side_effects, sort = TRUE) %>%
  slice_max(n, n = 20)

ggplot(top_combinations, aes(x = reorder(paste(drug, side_effects, sep = " × "), n), y = n)) +
  geom_col(fill = "steelblue") +
  coord_flip() +
  labs(title = "Top 20 Drug × Side Effect Combinations",
       x = "Drug × Side Effect", y = "Frequency")

# -----------------------------
# Bubble Plot: Top 10 drugs × Top 10 effects
# -----------------------------
top10_drugs <- df %>%
  filter(!is.na(side_effects)) %>%
  separate_rows(side_effects, sep = ";\\s*") %>%
  count(drug, sort = TRUE) %>%
  slice_max(order_by = n, n = 10) %>%
  pull(drug)

top10_effects <- df %>%
  filter(!is.na(side_effects)) %>%
  separate_rows(side_effects, sep = ";\\s*") %>%
  count(side_effects, sort = TRUE) %>%
  slice_max(order_by = n, n = 10) %>%
  pull(side_effects)

bubble_data <- df %>%
  filter(drug %in% top10_drugs, !is.na(side_effects)) %>%
  separate_rows(side_effects, sep = ";\\s*") %>%
  filter(side_effects %in% top10_effects) %>%
  count(drug, side_effects)

ggplot(bubble_data, aes(x = side_effects, y = drug, size = n)) +
  geom_point(color = "darkred", alpha = 0.7) +
  labs(
    title = "Drug × Side Effect Association",
    subtitle = "Bubble size indicates frequency of co-occurrence",
    x = "Side Effect", y = "Drug", size = "Count"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# -----------------------------
# Optional: CSV Export
# -----------------------------
write.csv(top3_sideeffects_per_drug, "04-output/top3_sideeffects_per_drug.csv", row.names = FALSE)
write.csv(all_effects, "04-output/top_sideeffects_overall.csv", row.names = FALSE)
write.csv(top_drugs, "04-output/top_drugs_by_sideeffect_count.csv", row.names = FALSE)
write.csv(top_combinations, "04-output/top20_combinations.csv", row.names = FALSE)
write.csv(bubble_data, "04-output/bubble_matrix_top10.csv", row.names = FALSE)
write.csv(matrix_df, "04-output/full_drug_sideeffect_matrix.csv", row.names = FALSE)
write.csv(drug_matrix_top30, "04-output/matrix_top30x30.csv")

# ----------------------------------------
# Dendrogram: Cluster assignment (row side)
# ----------------------------------------

# Erzeuge pheatmap-Objekt (NICHT direkt plotten!)
heatmap_obj <- pheatmap(
  drug_matrix_top30,
  cluster_rows = TRUE,
  cluster_cols = TRUE,
  fontsize_row = 8,
  fontsize_col = 8,
  main = "Heatmap: Top 30 Drugs × Top 30 Side Effects",
  show_rownames = TRUE,
  show_colnames = TRUE,
  silent = TRUE # wichtig: verhindert Anzeige, gibt Objekt zurück
)

# Extrahiere die Reihenfolge und Cluster-Zugehörigkeit
# Wir nutzen die Row-Dendrogramm-Zweige
dend <- heatmap_obj$tree_row
clusters <- cutree(dend, k = 4)  # Wähle z. B. 4 Cluster – anpassbar

# Tabelle: Drug → Cluster
drug_cluster_df <- data.frame(
  drug = rownames(drug_matrix_top30),
  cluster = clusters[rownames(drug_matrix_top30)]
)

# Vorschau
print(drug_cluster_df)

# Exportieren als CSV
write.csv(drug_cluster_df, "04-output/drug_dendrogram_clusters.csv", row.names = FALSE)

# ----------------------------------------
# Dendrogram: Cluster assignment (row side)
# ----------------------------------------
# Installieren (falls noch nicht vorhanden)
#install.packages("plotly")

library(plotly)

# Heatmap-Daten vorbereiten
plot_matrix <- drug_matrix_top30  # von vorherigem Code
drug_names <- rownames(plot_matrix)
side_effects <- colnames(plot_matrix)

# In long format für Plotly umwandeln
plot_data <- as.data.frame(plot_matrix) %>%
  mutate(drug = rownames(plot_matrix)) %>%
  pivot_longer(-drug, names_to = "side_effect", values_to = "count")

# Plotly Heatmap erzeugen
plot_ly(
  data = plot_data,
  x = ~side_effect,
  y = ~drug,
  z = ~count,
  type = "heatmap",
  colorscale = "Reds",
  showscale = TRUE,
  hovertemplate = paste(
    "<b>Drug:</b> %{y}<br>",
    "<b>Side Effect:</b> %{x}<br>",
    "<b>Count:</b> %{z}<extra></extra>"
  )
) %>%
  layout(
    title = "Top 30 Drugs × Side Effects (Interactive Heatmap)",
    xaxis = list(title = "Side Effect", tickangle = 45),
    yaxis = list(title = "Drug", automargin = TRUE)
  )
--------------------------
#Figures
--------------------------
# Top 15 side effects
p1 <- ggplot(all_effects[1:15, ], aes(x = reorder(side_effects, n), y = n)) +
  geom_col(fill = "darkred") +
  coord_flip() +
  labs(title = "Top 15 Most Frequent Side Effects",
       x = "Side Effect", y = "Count")
ggsave("07-figures/fig01_top15_sideeffects.png", p1, width = 8, height = 5, dpi = 300)

# Top 15 drugs with most side effects
p2 <- ggplot(top_drugs, aes(x = reorder(drug, n), y = n)) +
  geom_col(fill = "steelblue") +
  coord_flip() +
  labs(title = "Top 15 Drugs by Number of Side Effects",
       x = "Drug", y = "Count")
ggsave("07-figures/fig02_top15_drugs.png", p2, width = 8, height = 5, dpi = 300)

# Top 20 drug × side effect combinations
p3 <- ggplot(top_combinations, aes(x = reorder(paste(drug, side_effects, sep = " × "), n), y = n)) +
  geom_col(fill = "steelblue") +
  coord_flip() +
  labs(title = "Top 20 Drug × Side Effect Combinations",
       x = "Drug × Side Effect", y = "Frequency")
ggsave("07-figures/fig03_top20_combinations.png", p3, width = 10, height = 6, dpi = 300)

# Bubble plot: top 10 drugs × top 10 effects
p4 <- ggplot(bubble_data, aes(x = side_effects, y = drug, size = n)) +
  geom_point(color = "darkred", alpha = 0.7) +
  labs(title = "Drug × Side Effect Association",
       subtitle = "Bubble size indicates frequency of co-occurrence",
       x = "Side Effect", y = "Drug", size = "Count") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
ggsave("07-figures/fig04_bubble_matrix.png", p4, width = 8, height = 6, dpi = 300)

#Heatmap
png("07-figures/fig05_heatmap_top30x30.png", width = 1000, height = 1000)
pheatmap(drug_matrix_top30,
         cluster_rows = TRUE,
         cluster_cols = TRUE,
         fontsize_row = 8,
         fontsize_col = 8,
         main = "Heatmap: Top 30 Drugs × Top 30 Side Effects")
dev.off()
