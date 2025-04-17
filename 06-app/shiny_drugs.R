library(shiny)
library(tidyverse)
library(plotly)
library(DT)
library(pheatmap)

# Load dataset
df <- read.csv("02-data/combined_synonyms_sideeffects_top100.csv")

# Clean data
df_clean <- df %>%
  filter(!is.na(side_effects)) %>%
  separate_rows(side_effects, sep = ";\\s*")

# All unique synonyms
all_synonyms <- sort(unique(df_clean$synonym))

# Analysis datasets (must exist in 04-output)
top3 <- read.csv("04-output/top3_sideeffects_per_drug.csv")
side_overall <- read.csv("04-output/top_sideeffects_overall.csv")
top_drugs <- read.csv("04-output/top_drugs_by_sideeffect_count.csv")
combinations <- read.csv("04-output/top20_combinations.csv")
bubble <- read.csv("04-output/bubble_matrix_top10.csv")
matrix <- read.csv("04-output/full_drug_sideeffect_matrix.csv")

# UI
ui <- navbarPage("Drug Side Effect Explorer",
                 
                 # 1. Search tab
                 tabPanel("🔍 Drug Lookup",
                          sidebarLayout(
                            sidebarPanel(
                              selectInput("selected_synonym", "Select Synonym:",
                                          choices = all_synonyms, selected = all_synonyms[1])
                            ),
                            mainPanel(
                              h4("Mapped Drug"),
                              verbatimTextOutput("drug_output"),
                              h4("Associated Side Effects"),
                              dataTableOutput("effects_table")
                            )
                          )
                 ),
                 
                 # 2. Analysis tabs
                 tabPanel("📊 Top Side Effects Overall",
                          plotlyOutput("plot_overall")
                 ),
                 
                 tabPanel("💊 Top Drugs by Side Effect Count",
                          plotlyOutput("plot_topdrugs")
                 ),
                 
                 tabPanel("📌 Top 3 Side Effects per Drug",
                          sidebarLayout(
                            sidebarPanel(
                              selectInput("selected_drug", "Select Drug:",
                                          choices = sort(unique(top3$drug)))
                            ),
                            mainPanel(
                              plotlyOutput("plot_top3")
                            )
                          )
                 ),
                 
                 tabPanel("🔗 Top 20 Drug × Side Effect",
                          plotlyOutput("plot_combinations")
                 ),
                 
                 tabPanel("🫧 Bubble Chart (Top 10 × Top 10)",
                          plotlyOutput("plot_bubble")
                 ),
                 
                 tabPanel("🔥 Heatmap (Top 30 × 30)",
                          plotOutput("heatmap", height = "600px")
                 )
)

# Server
server <- function(input, output, session) {
  
  # Mapped drug from synonym
  selected_drug <- reactive({
    df_clean %>%
      filter(synonym == input$selected_synonym) %>%
      pull(drug) %>%
      unique() %>%
      first()
  })
  
  output$drug_output <- renderText({
    paste("Drug:", selected_drug())
  })
  
  output$effects_table <- renderDataTable({
    df_clean %>%
      filter(synonym == input$selected_synonym) %>%
      select(side_effects) %>%
      distinct()
  })
  
  output$plot_overall <- renderPlotly({
    p <- ggplot(side_overall[1:15, ], aes(x = reorder(side_effects, n), y = n)) +
      geom_col(fill = "darkred") +
      coord_flip() +
      labs(title = "Top 15 Most Frequent Side Effects",
           x = "Side Effect", y = "Count")
    ggplotly(p)
  })
  
  output$plot_topdrugs <- renderPlotly({
    p <- ggplot(top_drugs[1:15,], aes(x = reorder(drug, n), y = n)) +
      geom_col(fill = "steelblue") +
      coord_flip() +
      labs(title = "Top 15 Drugs with Most Side Effects",
           x = "Drug", y = "Side Effect Count")
    ggplotly(p)
  })
  
  output$plot_top3 <- renderPlotly({
    p <- ggplot(top3 %>% filter(drug == input$selected_drug),
                aes(x = reorder(side_effects, n), y = n)) +
      geom_col(fill = "orange") +
      coord_flip() +
      labs(title = paste("Top 3 Side Effects for", input$selected_drug),
           x = "Side Effect", y = "Count")
    ggplotly(p)
  })
  
  output$plot_combinations <- renderPlotly({
    p <- ggplot(combinations,
                aes(x = reorder(paste(drug, side_effects, sep = " × "), n), y = n)) +
      geom_col(fill = "steelblue") +
      coord_flip() +
      labs(title = "Top 20 Drug × Side Effect Combinations",
           x = "Drug × Side Effect", y = "Frequency")
    ggplotly(p)
  })
  
  output$plot_bubble <- renderPlotly({
    p <- ggplot(bubble, aes(x = side_effects, y = drug, size = n)) +
      geom_point(color = "darkred", alpha = 0.7) +
      labs(title = "Drug × Side Effect (Top 10 × Top 10)",
           subtitle = "Bubble size = frequency",
           x = "Side Effect", y = "Drug") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
    ggplotly(p)
  })
  
  output$heatmap <- renderPlot({
    mat_matrix <- as.matrix(matrix[,-1])
    rownames(mat_matrix) <- matrix$drug
    pheatmap(mat_matrix,
             cluster_rows = TRUE,
             cluster_cols = TRUE,
             fontsize_row = 8,
             fontsize_col = 8,
             main = "Heatmap: Top 30 Drugs × Top 30 Side Effects",
             show_rownames = TRUE,
             show_colnames = TRUE)
  })
}

# Run App
shinyApp(ui, server)
