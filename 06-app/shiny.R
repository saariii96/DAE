# ------------------------------------------
# app.R — Interactive Drug & Synonym Explorer
# ------------------------------------------

library(shiny)
library(tidyverse)
library(plotly)
library(DT)
library(pheatmap)

# Load data
df <- read.csv("02-data/combined_synonyms_sideeffects_top100.csv")

# Prepare clean version
df_clean <- df %>%
  filter(!is.na(side_effects)) %>%
  separate_rows(side_effects, sep = ";\\s*")

# List of unique synonyms for dropdown
all_synonyms <- sort(unique(df_clean$synonym))

# UI
ui <- fluidPage(
  titlePanel("Drug & Synonym Side Effect Explorer"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput("selected_synonym", "Select Synonym:",
                  choices = all_synonyms,
                  selected = "aspirin"),
      selectInput("selected_analysis", "Select Analysis:",
                  choices = c("Top Side Effects Overall",
                              "Top Drugs with Side Effects",
                              "Top 3 Side Effects per Drug",
                              "Top 20 Drug × Side Effect Combinations",
                              "Top 10 × Top 10 Bubble Chart",
                              "Top 30 × Top 30 Heatmap"))
    ),
    
    mainPanel(
      h4("Mapped Drug & Side Effects"),
      verbatimTextOutput("drug_output"),
      dataTableOutput("effects_table"),
      br(),
      h4("Visualization"),
      plotlyOutput("plot", height = "600px"),
      h4("Heatmap"),
      plotOutput("heatmap", height = "600px")
    )
  )
)

# Server
server <- function(input, output, session) {
  
  # Load analysis data
  top3 <- read.csv("04-output/top3_sideeffects_per_drug.csv")
  side_overall <- read.csv("04-output/top_sideeffects_overall.csv")
  top_drugs <- read.csv("04-output/top_drugs_by_sideeffect_count.csv")
  combinations <- read.csv("04-output/top20_combinations.csv")
  bubble <- read.csv("04-output/bubble_matrix_top10.csv")
  matrix <- read.csv("04-output/full_drug_sideeffect_matrix.csv")
  
  # Drug name for selected synonym
  selected_drug <- reactive({
    df_clean %>%
      filter(synonym == input$selected_synonym) %>%
      pull(drug) %>%
      unique() %>%
      first()
  })
  
  # Output the actual mapped drug
  output$drug_output <- renderText({
    paste("Drug:", selected_drug())
  })
  
  # Show side effects table
  output$effects_table <- renderDataTable({
    df_clean %>%
      filter(synonym == input$selected_synonym) %>%
      select(side_effects) %>%
      distinct()
  })
  
  # Main plot
  output$plot <- renderPlotly({
    analysis <- input$selected_analysis
    sel_drug <- selected_drug()
    
    if (analysis == "Top Side Effects Overall") {
      p <- ggplot(side_overall[1:15, ], aes(x = reorder(side_effects, n), y = n)) +
        geom_col(fill = "darkred") +
        coord_flip() +
        labs(title = "Top 15 Most Frequent Side Effects", x = "Side Effect", y = "Count")
    }
    
    else if (analysis == "Top Drugs with Side Effects") {
      p <- ggplot(top_drugs[1:15,], aes(x = reorder(drug, n), y = n)) +
        geom_col(fill = "steelblue") +
        coord_flip() +
        labs(title = "Top 15 Drugs with Side Effects", x = "Drug", y = "Count")
    }
    
    else if (analysis == "Top 3 Side Effects per Drug") {
      p <- ggplot(top3 %>% filter(drug == sel_drug),
                  aes(x = reorder(side_effects, n), y = n)) +
        geom_col(fill = "orange") +
        coord_flip() +
        labs(title = paste("Top 3 Side Effects for", sel_drug),
             x = "Side Effect", y = "Count")
    }
    
    else if (analysis == "Top 20 Drug × Side Effect Combinations") {
      p <- ggplot(combinations,
                  aes(x = reorder(paste(drug, side_effects, sep = " × "), n), y = n)) +
        geom_col(fill = "steelblue") +
        coord_flip() +
        labs(title = "Top 20 Drug × Side Effect Combinations",
             x = "Drug × Side Effect", y = "Frequency")
    }
    
    else if (analysis == "Top 10 × Top 10 Bubble Chart") {
      p <- ggplot(bubble, aes(x = side_effects, y = drug, size = n)) +
        geom_point(color = "darkred", alpha = 0.7) +
        labs(title = "Drug × Side Effect Association",
             subtitle = "Bubble size = Frequency of co-occurrence",
             x = "Side Effect", y = "Drug", size = "Count") +
        theme_minimal() +
        theme(axis.text.x = element_text(angle = 45, hjust = 1))
    }
    
    ggplotly(p)
  })
  
  # Static heatmap
  output$heatmap <- renderPlot({
    req(input$selected_analysis == "Top 30 × Top 30 Heatmap")
    
    mat <- matrix
    mat_matrix <- as.matrix(mat[,-1])
    rownames(mat_matrix) <- mat$drug
    
    pheatmap(
      mat_matrix,
      cluster_rows = TRUE,
      cluster_cols = TRUE,
      fontsize_row = 8,
      fontsize_col = 8,
      main = "Heatmap: Top 30 Drugs × Top 30 Side Effects",
      show_rownames = TRUE,
      show_colnames = TRUE
    )
  })
}

# Launch
shinyApp(ui, server)

