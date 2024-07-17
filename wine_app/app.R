library(shiny)
library(shinydashboard)
library(shinythemes)
library(lime)
library(gt)
library(plotly)
library(markdown)
library(wordcloud)
library(RColorBrewer)
library(DT)
library(bslib)
library(dplyr)
library(thematic)
library(bsicons)
library(FNN)
library(caret)
thematic_shiny()

model <- readRDS("model.rds")
x <- readRDS("words.rds")
X_reduced <- readRDS("X_reduced.rds")
data<-X_reduced[,-c(1:4)]
data_numeric <- data %>% mutate(across(everything(), as.numeric))

# Separate the target column and features
target <- data_numeric[, 1]
features <- data_numeric[, -1]

# Ensure the column types match those in the training data
factor_columns <- sapply(data, is.factor)
factor_levels <- lapply(data[, factor_columns], levels)

# Remove "Diagnose" column from data for feature selection
feature_columns <- setdiff(colnames(data), "Diagnose")

# Create lime explainer using the training data
explainer <- lime(train, model)

# Function to compute Hamming distance
hamming_distance <- function(case1, case2) {
  sum(case1 != case2)
}

# Function to find similar cases
find_similar_cases <- function(new_case, data, top_n = 5) {
  # Exclude the first column (X1/X0 labels) for distance computation
  data_matrix <- data[, -1]
  
  # Compute Hamming distance for all cases
  distances <- apply(data_matrix, 1, function(x) hamming_distance(new_case, x))
  
  # Find the indices of the top N most similar cases
  similar_indices <- order(distances)[1:top_n]
  
  # Return the most similar cases
  similar_cases <- data[similar_indices, ]
  return(similar_cases)
}

# Define UI
ui <- page_navbar(
  title = "Wine Finder",
  theme = bs_theme(    bootswatch = "darkly",
                       success = "#400804",
                       primary = "#400804",
                       secondary = "#400804",
                       danger = "white",
                       "table-color" = "white",
                       base_font = (font_google("Lato")),
                       bg = "rgb(34,34,34)",
                       fg = "rgb(255,255,255)",
                       "enable-gradients" = TRUE,
                       "enable-shadows" = TRUE
                       
  ),
  nav_panel(    title = "",
                layout_columns(
                  navset_card_tab(
                    height = 300,
                    sidebar = sidebar(class = "bg-secondary",
                                      "Select 3 words that describe your wine best.",
                                      accordion(class = "bg-secondary",
                                                accordion_panel(class = "bg-secondary",
                                                                "Features", icon = bsicons::bs_icon("menu-app"),
                                                                selectInput(
                                                                  inputId = "selected_columns",
                                                                  label = "Select Columns:",
                                                                  choices = feature_columns,
                                                                  multiple = TRUE
                                                                ),
                                                                actionButton("find_btn", "Find Wine",
                                                                             class = "btn-primary rounded-0"),
                                                                actionButton("predict_btn", "Predict Score",
                                                                             class = "btn-primary rounded-0")
                                                )
                                      )
                    ),
                    nav_panel(title = "Find Your Wine", dataTableOutput("found_similar_cases")),
                    nav_panel(title ="Predict Wine's Score", dataTableOutput("prediction_table"),
                              markdown("If your wine predicted X1 that means it's above +90 wine score enjoy.
                  Otherwise, try to enjoy... For more information about wine scoring please 
                  visit this [link](https://www.wine-searcher.com/wine-scores).
                           "))
                  ),
                  card(
                    full_screen = TRUE,
                    card_header("Wordcloud"),
                    layout_sidebar(
                      sidebar = sidebar(sliderInput(
                        "freq",
                        "Minimum Frequency:",
                        min = 1, max = 8000, value = 15
                      ),
                      sliderInput(
                        "max",
                        "Maximum Number of Words:",
                        min = 1, max = 138, value = 15
                      ),
                      actionButton(
                        "plus90", "Most used main words from wine enthusiasts for above 90 score wines",
                        class = "btn-primary rounded-0"
                      ),
                      actionButton(
                        "minus89", "Most used main words from wine enthusiasts for below 89 score wines",
                        class = "btn-primary rounded-0"
                      )),
                      height = 300, full_screen = TRUE,
                      card_body(
                        min_height = 150,
                        class = "gap-2 container",
                        plotOutput("plot")
                      )
                    )
                    
                  ),
                  card(
                    card_header(
                      class = "bg-dark",
                      "Most Used Main Words by Critics"
                    ),
                    value_box(title = "Finish", value =10545),
                    value_box(title = "Fruit", value = 6015),
                    value_box(title = "Great", value = 5730),
                    value_box(title = "Plum", value = 5478)
                  ),
                  card(
                    card_header("Why system choose this?"),
                    plotlyOutput("explanation_plot")
                  ),
                  
                  
                  
                  
                  card(
                    max_height = 600,
                    full_screen = TRUE,
                    card_header(
                      "How to understand critics?"
                    ),
                    includeMarkdown("deneme.Rmd")
                  ),
                  col_widths = c(5, 5, 2,6, 6),
                  row_heights = c(6,5)
                )
  ),
  nav_spacer(),
  nav_menu(
    title = "Links",
    nav_item(markdown("[GitHub](https://github.com/SETOsfk)"),
             class = "bg-danger")
    ,
  )
)

# Define server function
server <- function(input, output, session) {
  
  # Word Cloud Reactive Values and Events
  word_data <- reactiveVal(data.frame())
  observeEvent(input$plus90, {
    word_data(data.frame(Word = x$Word1, Count = x$Count1))
  })
  observeEvent(input$minus89, {
    word_data(data.frame(Word = x$Word0, Count = x$Count0))
  })
  
  # Make the wordcloud drawing predictable during a session
  wordcloud_rep <- repeatable(wordcloud)
  output$plot <- renderPlot({
    req(word_data())
    wordcloud_rep(words = word_data()$Word,
                  freq = word_data()$Count,
                  scale = c(4, 0.5),
                  min.freq = input$freq,
                  max.words = input$max,
                  colors = brewer.pal(8, "Dark2"))
  })
  
  # Reactive value to store the selected columns
  selected_columns <- reactiveVal(character())
  
  observeEvent(input$selected_columns, {
    selected_columns(input$selected_columns)
  })
  
  output$selected_columns <- renderText({
    paste(selected_columns(), collapse = ", ")
  })
  
  data_reactive <- reactiveVal(data.frame(matrix(0, ncol = ncol(data), nrow = 1, 
                                                 dimnames = list(NULL, colnames(data)))))
  
  observeEvent(input$predict_btn, {
    df <- data.frame(matrix(0, ncol = ncol(data), nrow = 1, dimnames = list(NULL, colnames(data))))
    df[1, selected_columns()] <- 1
    
    # Convert columns to factors with the same levels as in the training data
    for (col in names(factor_levels)) {
      df[[col]] <- factor(df[[col]], levels = factor_levels[[col]])
    }
    
    data_reactive(df)
    
    # Make prediction using the selected columns
    prediction <- predict(model, newdata = df, type = "prob")
    prediction_probabilities <- as.data.frame(prediction)
    
    
    # Create gt table with prediction probabilities
    output$prediction_table <- renderDataTable({
      prediction_table <- data.frame(
        Prediction = c("X1", "X0"),
        Probability = c(prediction_probabilities$X1, prediction_probabilities$X0)
      )
      
      datatable(prediction_table, options = list(pageLength = 5, searching = FALSE))
    })
    
    # Explain the prediction using lime with dynamic feature selection method
    n_features <- 5  # Default number of features for lime explain
    explanation <- lime::explain(df, explainer, n_labels = 1, n_features = n_features, 
                                 feature_selection = "auto")
    
    output$explanation_plot <- renderPlotly({
      ggplotly(plot_features(explanation))
    })
  })
  
  observeEvent(input$find_btn, {
    df <- data.frame(matrix(0, ncol = ncol(data), nrow = 1, dimnames = list(NULL, colnames(data))))
    df[1, selected_columns()] <- 1
    
    # Convert columns to factors with the same levels as in the training data
    for (col in names(factor_levels)) {
      df[[col]] <- factor(df[[col]], levels = factor_levels[[col]])
    }
    
    df_n <- df %>% mutate(across(everything(), as.numeric))
    k <- 5
    nn <- get.knnx(data = features, query = df_n[, -1], k = k, algorithm = "brute")
    nn_indices <- nn$nn.index
    
    similar_wines <- X_reduced[nn_indices,c(1:3) ]
    
    output$found_similar_cases <- renderDataTable({
      datatable(similar_wines[, 1:3], options = list(pageLength = 5, searching = FALSE))
    })
  })
}

# Run the application
shinyApp(ui = ui, server = server)